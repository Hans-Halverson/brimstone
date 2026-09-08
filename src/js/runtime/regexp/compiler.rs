use std::collections::{HashMap, HashSet};

use brimstone_icu_collections::{has_case_closure_non_unicode_set, has_case_closure_unicode_set};
use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};
use num_traits::ToPrimitive;

use crate::{
    common::{
        string::StringWidth,
        unicode::{
            CodePoint, MAX_CODE_POINT, MAX_LATIN1_CODE_POINT, is_latin1, is_surrogate_code_point,
            try_encode_surrogate_pair,
        },
        wtf_8::{Wtf8Cow, Wtf8Str},
    },
    parser::{
        ast::AstStr,
        regexp::{
            Alternative, AnonymousGroup, Assertion, CaptureGroup, CaptureGroupRange,
            CharacterClass, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags, Term,
        },
    },
    runtime::{
        Context, Handle,
        bytecode::generator::{EmitError, EmitResult},
        debug_print::DebugPrintMode,
        regexp::{
            code_point_set::{CodePointSetFlags, EncodedCodePointSet},
            code_point_set_builder::{
                CodePointSetBuilder, NEWLINE_SET, WORD_CASE_INSENSITIVE_UNICODE_SET, WORD_SET,
            },
            compiled_regexp::CompiledRegExp,
            graphviz::save_regexp_dotfile_if_needed,
            instruction::{
                AcceptInstruction, AssertEndInstruction, AssertEndOrNewlineInstruction,
                AssertStartInstruction, AssertStartOrNewlineInstruction,
                AssertWordBoundaryInstruction, BackreferenceInstruction, BranchInstruction,
                ClearCaptureInstruction, CodePointLiteralInstruction, CodePointSetInstruction,
                FailInstruction, GreedyLoopInstruction, Instruction, InstructionIterator,
                InstructionIteratorMut, JumpInstruction, LookaroundInstruction, LoopInstruction,
                MarkCapturePointInstruction, OneByteStringLiteralInstruction, OpCode,
                ProgressInstruction, SetProgressInstruction, TwoByteStringLiteralInstruction,
                WildcardInstruction, WildcardNoNewlineInstruction,
            },
            match_start_filter::{MatchStartAnalyzer, MatchStartFilter},
            required_literal_filter::{RequiredLiteralAnalyzer, RequiredLiteralFilter},
        },
        string_value::StringValue,
    },
};

type BlockId = usize;

struct RegExpCompiler {
    blocks: Vec<Vec<u32>>,
    flags: RegExpFlagsStack,
    constants: ConstantTableBuilder,
    source: Handle<StringValue>,
    current_block_id: BlockId,
    num_progress_points: u32,
    num_loop_registers: u32,
    /// Nonempty stack of directions denoting the set of possibly nested direction contexts created
    /// by lookaround. The top of the stack is the current direction.
    direction_stack: Vec<Direction>,
    /// Incremented every time we enter the body of a quantifier that may have at least 2
    /// repetitions and decremented when we leave. Used to detect if we are within the body of a
    /// quantifier that may be repeated 2+ times, which require special consideration.
    repetition_depth: u64,
}

#[derive(PartialEq)]
enum Direction {
    Forward,
    Backward,
}

/// Maximum number of repititons within a quantifier that will have their terms inlined, vs using
/// loop instructions.
const MAX_INLINED_REPETITIONS: u64 = 10;

/// Minimum length of a string literal in code points that will be emitted as a single string
/// literal instruction.
const MIN_STRING_LITERAL_CODE_POINTS: usize = 2;

/// Maximum length of a string literal in code points that will be emitted as a single string
/// literal instruction. Each code point is encoded as at most two code units, so this guarantees
/// that the length in code units fits in the instruction's packed u24 operand.
const MAX_STRING_LITERAL_CODE_POINTS: usize = (1 << 23) - 1;

impl RegExpCompiler {
    fn new(regexp: &RegExp, source: Handle<StringValue>) -> Self {
        Self {
            blocks: vec![],
            flags: RegExpFlagsStack::new(regexp.flags),
            constants: ConstantTableBuilder::new(),
            source,
            current_block_id: 0,
            num_progress_points: 0,
            num_loop_registers: 0,
            direction_stack: vec![Direction::Forward],
            repetition_depth: 0,
        }
    }

    fn new_block(&mut self) -> BlockId {
        let block_id = self.blocks.len();
        self.blocks.push(vec![]);
        block_id
    }

    fn set_current_block(&mut self, block_id: BlockId) {
        self.current_block_id = block_id;
    }

    fn enter_direction_context(&mut self, direction: Direction) {
        self.direction_stack.push(direction)
    }

    fn exit_direction_context(&mut self) {
        self.direction_stack.pop();
    }

    fn is_forwards(&self) -> bool {
        *self.direction_stack.last().unwrap() == Direction::Forward
    }

    fn enter_repetition_context(&mut self) {
        self.repetition_depth += 1;
    }

    fn exit_repetition_context(&mut self) {
        self.repetition_depth -= 1;
    }

    fn is_in_repetition(&self) -> bool {
        self.repetition_depth > 0
    }

    fn current_block_buf(&mut self) -> &mut Vec<u32> {
        &mut self.blocks[self.current_block_id]
    }

    fn current_flags(&self) -> RegExpFlags {
        self.flags.current()
    }

    fn emit_code_point_literal_instruction(&mut self, code_point: CodePoint) {
        CodePointLiteralInstruction::write(self.current_block_buf(), code_point)
    }

    fn emit_one_byte_string_literal_instruction(&mut self, code_units: &[u8]) -> EmitResult<()> {
        let constant_offset = self.constants.add_one_byte(code_units)?;
        let length = code_units.len().to_u32().unwrap();
        OneByteStringLiteralInstruction::write(self.current_block_buf(), length, constant_offset);

        Ok(())
    }

    fn emit_two_byte_string_literal_instruction(&mut self, code_units: &[u16]) -> EmitResult<()> {
        let constant_offset = self.constants.add_two_byte(code_units)?;
        let length = code_units.len().to_u32().unwrap();
        TwoByteStringLiteralInstruction::write(self.current_block_buf(), length, constant_offset);

        Ok(())
    }

    fn emit_code_point_set_instruction(&mut self, flags: CodePointSetFlags, set_offset: u32) {
        CodePointSetInstruction::write(self.current_block_buf(), flags, set_offset)
    }

    fn emit_greedy_loop_instruction(&mut self, flags: CodePointSetFlags, set_offset: u32) {
        GreedyLoopInstruction::write(self.current_block_buf(), flags, set_offset)
    }

    fn emit_wildcard_instruction(&mut self) {
        WildcardInstruction::write(self.current_block_buf())
    }

    fn emit_wildcard_no_newline_instruction(&mut self) {
        WildcardNoNewlineInstruction::write(self.current_block_buf())
    }

    fn emit_jump_instruction(&mut self, block_id: BlockId) {
        JumpInstruction::write(self.current_block_buf(), block_id as u32)
    }

    fn emit_branch_instruction(&mut self, first_block_id: BlockId, second_block_id: BlockId) {
        BranchInstruction::write(
            self.current_block_buf(),
            first_block_id as u32,
            second_block_id as u32,
        )
    }

    fn emit_accept_instruction(&mut self) {
        AcceptInstruction::write(self.current_block_buf())
    }

    fn emit_fail_instruction(&mut self) {
        FailInstruction::write(self.current_block_buf())
    }

    fn emit_mark_capture_point_instruction(&mut self, capture_point_index: u32) {
        MarkCapturePointInstruction::write(self.current_block_buf(), capture_point_index)
    }

    fn emit_clear_capture_instruction(&mut self, capture_group_index: u32) {
        ClearCaptureInstruction::write(self.current_block_buf(), capture_group_index)
    }

    fn new_progress_point(&mut self) -> u32 {
        let index = self.num_progress_points;
        self.num_progress_points += 1;
        index
    }

    fn emit_progress_instruction(&mut self, progress_index: u32) {
        ProgressInstruction::write(self.current_block_buf(), progress_index);
    }

    fn emit_set_progress_instruction(&mut self, progress_index: u32) {
        SetProgressInstruction::write(self.current_block_buf(), progress_index);
    }

    fn emit_loop_instruction(
        &mut self,
        loop_register_index: u32,
        loop_max_value: u32,
        end_branch: u32,
    ) {
        LoopInstruction::write(
            self.current_block_buf(),
            loop_register_index,
            loop_max_value,
            end_branch,
        )
    }

    fn emit_assert_start_instruction(&mut self) {
        AssertStartInstruction::write(self.current_block_buf())
    }

    fn emit_assert_end_instruction(&mut self) {
        AssertEndInstruction::write(self.current_block_buf())
    }

    fn emit_assert_start_or_newline_instruction(&mut self) {
        AssertStartOrNewlineInstruction::write(self.current_block_buf())
    }

    fn emit_assert_end_or_newline_instruction(&mut self) {
        AssertEndOrNewlineInstruction::write(self.current_block_buf())
    }

    fn emit_assert_word_boundary_instruction(
        &mut self,
        is_negated: bool,
        set: &CodePointInversionList,
    ) -> EmitResult<()> {
        let (flags, set_data) = EncodedCodePointSet::encode(set);
        let set_offset = self.constants.add_code_point_set(&set_data)?;
        AssertWordBoundaryInstruction::write(
            self.current_block_buf(),
            is_negated,
            flags,
            set_offset,
        );

        Ok(())
    }

    fn emit_backreference_instruction(
        &mut self,
        is_case_insensitive: bool,
        capture_group_index: u32,
    ) {
        BackreferenceInstruction::write(
            self.current_block_buf(),
            is_case_insensitive,
            capture_group_index,
        )
    }

    fn emit_lookaround_instruction(&mut self, is_ahead: bool, is_positive: bool, body_branch: u32) {
        LookaroundInstruction::write(self.current_block_buf(), is_ahead, is_positive, body_branch)
    }

    fn next_loop_register(&mut self) -> u32 {
        let next_register = self.num_loop_registers;
        self.num_loop_registers += 1;
        next_register
    }

    fn compile(
        &mut self,
        cx: Context,
        regexp: &RegExp,
        match_start_filter: MatchStartFilter,
        required_literal_filter: RequiredLiteralFilter,
    ) -> EmitResult<Handle<CompiledRegExp>> {
        // Prime with new block
        self.new_block();

        self.emit_disjunction(&regexp.disjunction)?;
        self.emit_accept_instruction();

        let instructions = self.flatten_and_fix_indices();

        let compiled_regexp = CompiledRegExp::new(
            cx,
            &instructions,
            &self.constants.bytes,
            regexp,
            self.source,
            self.num_progress_points,
            self.num_loop_registers,
            match_start_filter,
            required_literal_filter,
        )?;

        Ok(compiled_regexp)
    }

    fn emit_disjunction(&mut self, disjunction: &Disjunction) -> EmitResult<()> {
        if disjunction.alternatives.len() == 1 {
            self.emit_alternative(&disjunction.alternatives[0])
        } else {
            // Set up blocks for the branch instructions between alternatives. First branch can
            // always occur in the current block.
            let mut branch_block_ids = vec![self.current_block_id];
            for _ in 0..disjunction.alternatives.len() - 2 {
                branch_block_ids.push(self.new_block())
            }

            // Block that all alternatives join to at the end
            let join_block_id = self.new_block();

            // Emit all alternative blocks
            struct AlternativeBlock {
                entry_block: usize,
                exit_block: usize,
                captures: Option<CaptureGroupRange>,
            }
            let mut alternative_blocks: Vec<AlternativeBlock> = vec![];

            for alternative in disjunction.alternatives.iter() {
                let alternative_block_id = self.new_block();
                self.set_current_block(alternative_block_id);

                self.emit_alternative(alternative)?;

                alternative_blocks.push(AlternativeBlock {
                    entry_block: alternative_block_id,
                    exit_block: self.current_block_id,
                    captures: alternative.captures,
                });
            }

            // If this disjunction is in a repetition we must clear the captures for all
            // alternatives not taken in case they were previously matched.
            if self.is_in_repetition() {
                // All alternatives but the last two have their captures cleared at the start of the
                // next branch block, since all successful paths that don't match the previous
                // alternative will necessarily pass through the next branch block.
                for i in 1..alternative_blocks.len() - 1 {
                    if alternative_blocks[i - 1].captures.is_some() {
                        let prev_alternative_captures = &alternative_blocks[i - 1].captures;
                        self.set_current_block(branch_block_ids[i]);
                        for capture_index in (*prev_alternative_captures).into_iter().flatten() {
                            self.emit_clear_capture_instruction(capture_index);
                        }
                    }
                }

                // The second to last alternative emits its captures as part of the last alternative
                // block, since either the last alternative proceeds or the entire disjunction fails
                // to match,
                let penultimate_alternative_captures =
                    &alternative_blocks[alternative_blocks.len() - 2].captures;
                for capture_index in (*penultimate_alternative_captures).into_iter().flatten() {
                    self.set_current_block(
                        alternative_blocks[alternative_blocks.len() - 1].exit_block,
                    );
                    self.emit_clear_capture_instruction(capture_index);
                }

                // Note that the last alternative does not need its captures cleared when entering
                // another block because there is no other block to enter. Either the last
                // alternative matches or the entire disjunction fails to match.
            }

            // Blocks each alternative joins to, in reverse order. Note that the last alternative
            // always jumps to the final join block.
            let mut join_blocks_rev = vec![join_block_id];

            // If in a repetition, create a chain of a chain of clear blocks that clear all captures
            // from the first alternative to the last alternative. Each alternative jumps to the
            // clear block that clears the captures for all later alternatives.
            for i in (1..alternative_blocks.len()).rev() {
                let alternative_captures = &alternative_blocks[i].captures;
                if self.is_in_repetition() && alternative_captures.is_some() {
                    let clear_capture_block_id = self.new_block();

                    // Emit this clear block and set it as the current one
                    self.set_current_block(clear_capture_block_id);
                    for capture_index in (*alternative_captures).into_iter().flatten() {
                        self.emit_clear_capture_instruction(capture_index);
                    }

                    // Link to the next clear (or join) block in the chain
                    self.emit_jump_instruction(join_blocks_rev[join_blocks_rev.len() - 1]);

                    join_blocks_rev.push(clear_capture_block_id);
                } else {
                    join_blocks_rev.push(join_blocks_rev[join_blocks_rev.len() - 1]);
                }
            }

            // Link all alternative blocks to their corresponding join block or link in the clear
            // block chain
            for (alternative_block, join_block) in alternative_blocks
                .iter()
                .zip(join_blocks_rev.into_iter().rev())
            {
                self.set_current_block(alternative_block.exit_block);
                self.emit_jump_instruction(join_block);
            }

            // Emit branch chain for all but the last two alternatives
            for i in 0..alternative_blocks.len() - 2 {
                // Branch between this alternative and the next branch block
                self.set_current_block(branch_block_ids[i]);
                self.emit_branch_instruction(
                    alternative_blocks[i].entry_block,
                    branch_block_ids[i + 1],
                );
            }

            // Emit branch between the last two alternatives
            self.set_current_block(branch_block_ids[branch_block_ids.len() - 1]);
            self.emit_branch_instruction(
                alternative_blocks[alternative_blocks.len() - 2].entry_block,
                alternative_blocks[alternative_blocks.len() - 1].entry_block,
            );

            // Disjunction ends at start of join block
            self.set_current_block(join_block_id);

            Ok(())
        }
    }

    fn emit_alternative(&mut self, alternative: &Alternative) -> EmitResult<()> {
        if self.is_forwards() {
            for term in alternative.terms.iter() {
                self.emit_term(term)?;
            }
        } else {
            // When emitting backwards, emit concatenation of terms in reverse order
            for term in alternative.terms.iter().rev() {
                self.emit_term(term)?;
            }
        }

        Ok(())
    }

    fn emit_term(&mut self, term: &Term) -> EmitResult<()> {
        match term {
            Term::Literal(string) => self.emit_literal_string(string),
            Term::Wildcard => {
                self.emit_wildcard();
                Ok(())
            }
            Term::Quantifier(quantifier) => self.emit_quantifier(quantifier),
            Term::Assertion(assertion) => self.emit_assertion(assertion),
            Term::CaptureGroup(group) => self.emit_capture_group(group),
            Term::AnonymousGroup(group) => self.emit_anonymous_group(group),
            Term::CharacterClass(character_class) => self.emit_character_class(character_class),
            Term::Lookaround(lookaround) => self.emit_lookaround(lookaround),
            Term::Backreference(backreference) => {
                self.emit_backreference_instruction(
                    self.current_flags().is_case_insensitive(),
                    backreference.index,
                );
                Ok(())
            }
        }
    }

    fn emit_literal_string(&mut self, string: AstStr) -> EmitResult<()> {
        let parts = self.split_literal_parts(string);

        if self.is_forwards() {
            self.emit_literal_parts(parts.into_iter())
        } else {
            self.emit_literal_parts(parts.into_iter().rev())
        }
    }

    /// Split a literal string into parts that will be individually emitted, preferring runs of code
    /// points that can be emitted as a single string literal instruction where possible.
    fn split_literal_parts(&mut self, string: AstStr) -> Vec<LiteralPart> {
        let flags = self.current_flags();

        let mut parts = vec![];
        let mut current_run = vec![];
        let mut current_run_width = StringWidth::OneByte;

        for code_point in string.iter_code_points() {
            // Runs are broken when:
            // - A code point may match multiple other code points due to case sensitivity, i.e. the
            //   code point has a case closure containing code points other than itself.
            // - An unpaired surrogate is encountered in any unicode mode. This requires a more
            //   sophisticated runtime check than a simple code unit slice comparison to avoid
            //   incorrectly matching a paired surrogate.
            if flags.is_case_insensitive() {
                let has_case_closure = if flags.has_any_unicode_flag() {
                    has_case_closure_unicode_set().contains32(code_point)
                } else {
                    has_case_closure_non_unicode_set().contains32(code_point)
                };

                if has_case_closure {
                    Self::finish_run(&mut parts, &mut current_run, &mut current_run_width);
                    parts.push(LiteralPart::CaseClosure(code_point));
                    continue;
                }
            }

            if flags.has_any_unicode_flag() && is_surrogate_code_point(code_point) {
                Self::finish_run(&mut parts, &mut current_run, &mut current_run_width);
                parts.push(LiteralPart::CodePoint(code_point));
                continue;
            }

            if !is_latin1(code_point) {
                current_run_width = StringWidth::TwoByte;
            }

            current_run.push(code_point);

            // Split runs that are too long to fit in a single string literal instruction
            if current_run.len() == MAX_STRING_LITERAL_CODE_POINTS {
                Self::finish_run(&mut parts, &mut current_run, &mut current_run_width);
            }
        }

        Self::finish_run(&mut parts, &mut current_run, &mut current_run_width);

        parts
    }

    fn finish_run(
        parts: &mut Vec<LiteralPart>,
        current_run: &mut Vec<CodePoint>,
        current_run_width: &mut StringWidth,
    ) {
        if current_run.is_empty() {
            return;
        }

        let code_points = std::mem::take(current_run);
        let width = std::mem::replace(current_run_width, StringWidth::OneByte);

        if code_points.len() >= MIN_STRING_LITERAL_CODE_POINTS {
            parts.push(LiteralPart::Run { code_points, width });
        } else {
            for code_point in code_points {
                parts.push(LiteralPart::CodePoint(code_point));
            }
        }
    }

    fn emit_literal_parts(&mut self, parts: impl Iterator<Item = LiteralPart>) -> EmitResult<()> {
        for part in parts {
            match part {
                LiteralPart::Run { code_points, width } => {
                    if width == StringWidth::OneByte {
                        let code_units =
                            code_points.iter().map(|&cp| cp as u8).collect::<Vec<u8>>();
                        self.emit_one_byte_string_literal_instruction(&code_units)?;
                    } else {
                        let mut code_units = vec![];
                        for code_point in code_points {
                            match try_encode_surrogate_pair(code_point) {
                                Some((high, low)) => {
                                    code_units.push(high);
                                    code_units.push(low);
                                }
                                None => code_units.push(code_point as u16),
                            }
                        }

                        self.emit_two_byte_string_literal_instruction(&code_units)?;
                    }
                }
                LiteralPart::CodePoint(code_point) => {
                    self.emit_code_point_literal_instruction(code_point);
                }
                LiteralPart::CaseClosure(code_point) => {
                    let set =
                        CodePointSetBuilder::code_point_to_set(code_point, self.current_flags());
                    self.emit_code_point_set(&set, /* is_inverted */ false)?;
                }
            }
        }

        Ok(())
    }

    fn emit_wildcard(&mut self) {
        if self.current_flags().is_dot_all() {
            self.emit_wildcard_instruction()
        } else {
            self.emit_wildcard_no_newline_instruction()
        }
    }

    fn emit_assertion(&mut self, assertion: &Assertion) -> EmitResult<()> {
        match assertion {
            Assertion::Start => {
                if self.current_flags().is_multiline() {
                    self.emit_assert_start_or_newline_instruction()
                } else {
                    self.emit_assert_start_instruction()
                }

                Ok(())
            }
            Assertion::End => {
                if self.current_flags().is_multiline() {
                    self.emit_assert_end_or_newline_instruction()
                } else {
                    self.emit_assert_end_instruction()
                }

                Ok(())
            }
            Assertion::WordBoundary => self.emit_assert_word_boundary(/* is_negated */ false),
            Assertion::NotWordBoundary => {
                self.emit_assert_word_boundary(/* is_negated */ true)
            }
        }
    }

    fn emit_assert_word_boundary(&mut self, is_negated: bool) -> EmitResult<()> {
        let flags = self.current_flags();
        let word_set = if flags.is_case_insensitive() && flags.has_any_unicode_flag() {
            &WORD_CASE_INSENSITIVE_UNICODE_SET
        } else {
            &WORD_SET
        };

        self.emit_assert_word_boundary_instruction(is_negated, word_set)
    }

    fn emit_quantifier(&mut self, quantifier: &Quantifier) -> EmitResult<()> {
        // A repetition is any quantifier that can be run at least twice
        let is_repetition = match quantifier.max {
            None => true,
            Some(max) => max > 1,
        };

        if is_repetition {
            self.enter_repetition_context();
        }

        // Can inline a small number of repetitions otherwise use a loop
        if quantifier.min != 0 && quantifier.min <= MAX_INLINED_REPETITIONS {
            // Emit term min times for repetitions that must be present. Clear captures from the
            // previous iteration (if any).
            for i in 0..quantifier.min {
                if i == 0 {
                    self.emit_term(&quantifier.term)?;
                } else {
                    self.emit_quantified_term_with_cleared_captures(quantifier)?;
                }
            }
        } else if quantifier.min > u32::MAX as u64 && quantifier.always_consumes {
            // The minimum number of repetitions is greater than the max possible string length.
            // Each repetition must consume at least one character, so we know this quantifier will
            // fail to match.
            self.emit_fail_instruction();
        } else if quantifier.min != 0 {
            // Jump to a new loop block for the minimum repetitions
            let loop_block_id = self.new_block();
            let loop_end_block_id = self.new_block();

            self.emit_jump_instruction(loop_block_id);
            self.set_current_block(loop_block_id);

            // If min is out of range clamp to the largest allowed number of repetitions
            let clamped_min = quantifier.min.to_u32().unwrap_or(u32::MAX);

            // Loop block consists of loop instruction, term, then loops back to start of block
            let loop_register_index = self.next_loop_register();
            self.emit_loop_instruction(loop_register_index, clamped_min, loop_end_block_id as u32);

            self.emit_quantified_term_with_cleared_captures(quantifier)?;
            self.emit_jump_instruction(loop_block_id);

            // Start emitting in the loop end block after loop finishes
            self.set_current_block(loop_end_block_id);
        }

        // Exact number of repetitions have been matched, we are done
        if let Some(max) = quantifier.max
            && max == quantifier.min
        {
            if is_repetition {
                self.exit_repetition_context();
            }

            return Ok(());
        }

        // Optional repetitions cannot match the empty string. Implemented as a progress instruction
        // after each optional repetition. Initialize the progress point before first repetition.
        let progress_index = if !quantifier.always_consumes {
            let progress_index = self.new_progress_point();
            self.emit_set_progress_instruction(progress_index);
            Some(progress_index)
        } else {
            None
        };

        if let Some(max) = quantifier.max {
            let num_optional_repetitions = max - quantifier.min;

            let join_block_id = self.new_block();

            // Can inline a small number of optional repetitions otherwise use a loop
            if num_optional_repetitions <= MAX_INLINED_REPETITIONS {
                // Emit term blocks max - min times, each is optional and is preceded by a branch to
                // the join block.
                for i in quantifier.min..max {
                    let term_block_id = self.new_block();
                    self.emit_quantifier_optional_branch(quantifier, term_block_id, join_block_id);

                    // Emit term block clearing captures from the previous iteration (if any)
                    self.set_current_block(term_block_id);

                    if i == 0 {
                        self.emit_term(&quantifier.term)?;
                    } else {
                        self.emit_quantified_term_with_cleared_captures(quantifier)?;
                    };

                    // Ensure that each repetition makes progress, if necessary
                    if let Some(progress_index) = progress_index {
                        self.emit_progress_instruction(progress_index);
                    }
                }

                // Last term block always proceeds to the join block
                self.emit_jump_instruction(join_block_id);
            } else {
                let loop_block_id = self.new_block();

                self.emit_quantifier_optional_branch(quantifier, loop_block_id, join_block_id);

                // If min is out of range clamp to the largest allowed number of repetitions
                let clamped_repetitions = num_optional_repetitions.to_u32().unwrap_or(u32::MAX);

                // Loop block consists of loop instruction, term, then branches back to start of block
                self.set_current_block(loop_block_id);
                let loop_register_index = self.next_loop_register();
                self.emit_loop_instruction(
                    loop_register_index,
                    clamped_repetitions,
                    join_block_id as u32,
                );

                self.emit_quantified_term_with_cleared_captures(quantifier)?;

                // Ensure that each repetition makes progress, if necessary
                if let Some(progress_index) = progress_index {
                    self.emit_progress_instruction(progress_index);
                }

                self.emit_quantifier_optional_branch(quantifier, loop_block_id, join_block_id);
            }

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        } else if quantifier.is_greedy
            && let Some((set, is_inverted)) = self.as_simple_greedy_loop_body(&quantifier.term)
        {
            // Emit a greedy loop instruction for a simple body that consumes exactly one code point
            // which can be matched by a single code point set.
            let (flags, set_offset) = self.add_encoded_code_point_set(&set, is_inverted)?;
            self.emit_greedy_loop_instruction(flags, set_offset);
        } else {
            // Any number of future repetitions
            let term_block_id = self.new_block();
            let join_block_id = self.new_block();

            self.emit_quantifier_optional_branch(quantifier, term_block_id, join_block_id);

            // Emit term block
            self.set_current_block(term_block_id);
            self.emit_quantified_term_with_cleared_captures(quantifier)?;

            // Ensure that each repetition makes progress, if necessary
            if let Some(progress_index) = progress_index {
                self.emit_progress_instruction(progress_index);
            }

            // Term block optionally loops back to itself
            self.emit_quantifier_optional_branch(quantifier, term_block_id, join_block_id);

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        }

        if is_repetition {
            self.exit_repetition_context();
        }

        Ok(())
    }

    fn emit_quantifier_optional_branch(
        &mut self,
        quantifier: &Quantifier,
        term_block_id: BlockId,
        join_block_id: BlockId,
    ) {
        if quantifier.is_greedy {
            self.emit_branch_instruction(term_block_id, join_block_id)
        } else {
            self.emit_branch_instruction(join_block_id, term_block_id)
        }
    }

    /// Emit a term with a prefix that clears all captures in the term.
    ///
    /// Used for terms in quantifiers, since all captures are cleared at the start of each
    /// repetition.
    fn emit_quantified_term_with_cleared_captures(
        &mut self,
        quantifier: &Quantifier,
    ) -> EmitResult<()> {
        for capture_index in quantifier.captures.into_iter().flatten() {
            self.emit_clear_capture_instruction(capture_index);
        }

        self.emit_term(&quantifier.term)
    }

    /// Simple greedy loop bodies consume exactly one code point and can be matched by a single code
    /// point set. This allows for the more efficient GreedyLoop instruction to be used.
    ///
    /// Return the code point set and whether the set is inverted if the term is a simple greedy
    /// loop body, otherwise return None.
    fn as_simple_greedy_loop_body<'a>(
        &self,
        term: &Term,
    ) -> Option<(CodePointInversionList<'a>, bool)> {
        let flags = self.current_flags();
        match term {
            // Literals can be a greedy loop body if they have exactly one code point
            Term::Literal(string) => {
                let mut code_points = string.iter_code_points();
                if let Some(code_point) = code_points.next()
                    && code_points.next().is_none()
                {
                    let set = CodePointSetBuilder::code_point_to_set(code_point, flags);
                    Some((set, /* is_inverted */ false))
                } else {
                    None
                }
            }
            // Character classes without strings are always a greedy loop body
            Term::CharacterClass(character_class) if !character_class.may_contain_strings => {
                let (set, _) = CodePointSetBuilder::character_class_to_set(character_class, flags);

                // In unicode sets mode the set was eagerly inverted instead of inverting at the end
                let is_inverted = character_class.is_inverted && !flags.has_unicode_sets_flag();

                Some((set, is_inverted))
            }
            // Wildcards can be a greedy loop body, and are represented as either the set of all
            // code points or the set of all code points except newlines depending on the dotAll
            // flag.
            Term::Wildcard => {
                let mut set_builder = CodePointInversionListBuilder::new();

                if !flags.is_dot_all() {
                    set_builder.add_set(&NEWLINE_SET);
                }

                Some((set_builder.build(), /* is_inverted */ true))
            }
            // Descend into simple anonymous groups
            Term::AnonymousGroup(group)
                if group.positive_modifiers.is_empty() && group.negative_modifiers.is_empty() =>
            {
                if let [alternative] = group.disjunction.alternatives.as_ref()
                    && let [term] = alternative.terms.as_ref()
                {
                    self.as_simple_greedy_loop_body(term)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn emit_capture_group(&mut self, group: &CaptureGroup) -> EmitResult<()> {
        // Calculate capture point indices from capture group
        let mut capture_start_index = group.index * 2;
        let mut capture_end_index = capture_start_index + 1;

        // Reverse order of capture indices when emitting backwards
        if !self.is_forwards() {
            std::mem::swap(&mut capture_start_index, &mut capture_end_index);
        }

        self.emit_mark_capture_point_instruction(capture_start_index);
        self.emit_disjunction(&group.disjunction)?;
        self.emit_mark_capture_point_instruction(capture_end_index);

        Ok(())
    }

    fn emit_anonymous_group(&mut self, group: &AnonymousGroup) -> EmitResult<()> {
        // Update the set of current flags if any modifiers are present in this group
        let updated_flags = self.flags.push_group_flags(group);

        self.emit_disjunction(&group.disjunction)?;

        if updated_flags {
            self.flags.pop_group_flags();
        }

        Ok(())
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) -> EmitResult<()> {
        let flags = self.current_flags();

        let (set, mut strings) =
            CodePointSetBuilder::character_class_to_set(character_class, flags);

        struct StringDisjunctionInfo {
            join_block_id: BlockId,
            has_empty_string: bool,
        }

        // First check non-empty strings if there are any. The empty string is handled separately
        // from other strings since it must be checked after individual code points.
        let string_disjunction_info = if !strings.is_empty() {
            let join_block_id = self.new_block();
            let empty_string = Wtf8Cow::Borrowed(Wtf8Str::from_str(""));
            let has_empty_string = strings.remove(&empty_string);

            if !strings.is_empty() {
                self.emit_class_string_disjunction(&strings, join_block_id)?;
            }

            Some(StringDisjunctionInfo { join_block_id, has_empty_string })
        } else {
            None
        };

        // In unicode sets mode the set was eagerly inverted instead of inverting at the end
        let is_check_inverted = character_class.is_inverted && !flags.has_unicode_sets_flag();

        // If a string disjunction had the empty string then we will always match the empty string
        // iff no individual code points match. Note the order since we match longer strings first.
        if let Some(StringDisjunctionInfo { has_empty_string: true, join_block_id }) =
            string_disjunction_info
        {
            let code_point_check_block = self.new_block();
            self.emit_branch_instruction(code_point_check_block, join_block_id);
            self.set_current_block(code_point_check_block);
        }

        // Check individual code points
        self.emit_code_point_set(&set, is_check_inverted)?;

        // If there is a string disjunction then proceed to the final join block
        if let Some(StringDisjunctionInfo { join_block_id, .. }) = string_disjunction_info {
            self.emit_jump_instruction(join_block_id);
            self.set_current_block(join_block_id);
        }

        Ok(())
    }

    fn emit_code_point_set(
        &mut self,
        set: &CodePointInversionList,
        is_inverted: bool,
    ) -> EmitResult<()> {
        // Can emit a literal instruction if we are matching a single code point
        if set.size() == 1 && !is_inverted {
            let single_range = set.iter_ranges().next();
            let single_code_point = *single_range.unwrap().start();
            self.emit_code_point_literal_instruction(single_code_point);
            return Ok(());
        }

        let (flags, set_offset) = self.add_encoded_code_point_set(set, is_inverted)?;
        self.emit_code_point_set_instruction(flags, set_offset);

        Ok(())
    }

    /// Encode a code point set and add it to the constant table, returning the set instruction
    /// flags and the offset of the encoded set in the constant table.
    fn add_encoded_code_point_set(
        &mut self,
        set: &CodePointInversionList,
        is_inverted: bool,
    ) -> EmitResult<(CodePointSetFlags, u32)> {
        // We can choose to encode the set or its complement so we choose whichever is cheaper
        // (i.e. has fewer non-Latin1 ranges) and flip the inversion flag if necessary.
        //
        // The complement has one fewer range exactly when the set contains both endpoints of the
        // non-Latin1 range.
        let is_complement_cheaper =
            set.contains32(MAX_LATIN1_CODE_POINT + 1) && set.contains32(MAX_CODE_POINT);

        let complement;
        let (set, is_inverted) = if is_complement_cheaper {
            let mut complement_builder = CodePointInversionListBuilder::new();
            complement_builder.add_set(set);
            complement_builder.complement();
            complement = complement_builder.build();

            (&complement, !is_inverted)
        } else {
            (set, is_inverted)
        };

        let (mut flags, set_data) = EncodedCodePointSet::encode(set);
        let set_offset = self.constants.add_code_point_set(&set_data)?;

        if is_inverted {
            flags |= CodePointSetFlags::IS_INVERTED;
        }

        Ok((flags, set_offset))
    }

    fn emit_class_string_disjunction(
        &mut self,
        strings: &HashSet<Wtf8Cow>,
        success_block: BlockId,
    ) -> EmitResult<()> {
        let mut strings = strings.iter().collect::<Vec<_>>();

        // Order strings by length, checking the longest first. Break ties consistently by comparing
        // the strings as bytes.
        strings.sort_by(|a, b| {
            let a = a.as_str();
            let b = b.as_str();
            let len_cmp = b.len().cmp(&a.len());
            len_cmp.then_with(|| a.as_bytes().cmp(b.as_bytes()))
        });

        // Set up blocks for the branch instructions between alternatives. First branch can always
        // occur in the current block.
        let mut branch_block_ids = vec![self.current_block_id];
        for _ in 0..strings.len() - 1 {
            branch_block_ids.push(self.new_block())
        }

        // Set up blocks for each alternative
        let mut alternative_block_ids = vec![];
        for _ in 0..strings.len() {
            alternative_block_ids.push(self.new_block())
        }

        // Block that all alternatives join to at the end
        let join_block_id = self.new_block();

        // Emit branch chain for all but the last alternative
        for i in 0..strings.len() - 1 {
            // Branch between this alternative and the next branch block
            self.set_current_block(branch_block_ids[i]);
            self.emit_branch_instruction(alternative_block_ids[i], branch_block_ids[i + 1]);
        }

        // Emit branch between the last alternative and the join block
        self.set_current_block(branch_block_ids[branch_block_ids.len() - 1]);
        self.emit_branch_instruction(alternative_block_ids[strings.len() - 1], join_block_id);

        // Emit each alternative block, trying to match the literal and proceeding to the success
        // block if successful.
        for (i, string) in strings.iter().enumerate() {
            self.set_current_block(alternative_block_ids[i]);
            self.emit_literal_string(string.as_str())?;
            self.emit_jump_instruction(success_block);
        }

        // Disjunction ends at start of join block
        self.set_current_block(join_block_id);

        Ok(())
    }

    fn emit_lookaround(&mut self, lookaround: &Lookaround) -> EmitResult<()> {
        let body_block_id = self.new_block();
        self.emit_lookaround_instruction(
            lookaround.is_ahead,
            lookaround.is_positive,
            body_block_id as u32,
        );

        // The body of the lookaround is generated in a new direction context to allow for emitting
        // backwards matches.
        let lookaround_direction = if lookaround.is_ahead {
            Direction::Forward
        } else {
            Direction::Backward
        };
        self.enter_direction_context(lookaround_direction);

        // Emit the body of the lookaround instruction, ending with an accept
        let current_block_id = self.current_block_id;
        self.set_current_block(body_block_id);

        // Emit the body of the lookaround, keeping track of captures
        self.emit_disjunction(&lookaround.disjunction)?;
        self.emit_accept_instruction();

        self.exit_direction_context();

        self.set_current_block(current_block_id);

        Ok(())
    }

    /// Convert the list of blocks to a flat list of instructions. Branch and jump instructions
    /// originally use block ids as their operands - replace these with u32 indices into the encoded
    /// flat array.
    fn flatten_and_fix_indices(&mut self) -> Vec<u32> {
        let num_instructions = self.blocks.iter().map(|block| block.len()).sum();
        let mut instructions = Vec::with_capacity(num_instructions);

        // Map from block ids to u32 indices in the flattened array
        let mut id_map = Vec::with_capacity(self.blocks.len());

        // Flatten blocks into instruction array
        for (i, block) in self.blocks.iter().enumerate() {
            id_map.push(instructions.len() as u32);

            // If a block ends with an unconditional jump to the immediately following block, omit
            // the jump and let execution continue directly into the next block.
            if let Some(last) = InstructionIterator::new(block).last() {
                if Self::is_jump_to_block_id(last, i + 1) {
                    let num_u32_before_last_jump = block.len() - last.size();
                    instructions.extend(&block[..num_u32_before_last_jump]);
                    continue;
                }
            }

            instructions.extend(block.iter());
        }

        // Fix up branch targets
        for instr in InstructionIteratorMut::new(&mut instructions) {
            match instr.opcode() {
                OpCode::Branch => {
                    let instr = instr.cast_mut::<BranchInstruction>();
                    instr.set_first_branch(id_map[instr.first_branch() as usize]);
                    instr.set_second_branch(id_map[instr.second_branch() as usize]);
                }
                OpCode::Jump => {
                    let instr = instr.cast_mut::<JumpInstruction>();
                    instr.set_target(id_map[instr.target() as usize]);
                }
                OpCode::Lookaround => {
                    let instr = instr.cast_mut::<LookaroundInstruction>();
                    instr.set_body_branch(id_map[instr.body_branch() as usize]);
                }
                OpCode::Loop => {
                    let instr = instr.cast_mut::<LoopInstruction>();
                    instr.set_end_branch(id_map[instr.end_branch() as usize]);
                }
                _ => {}
            }
        }

        instructions
    }

    fn is_jump_to_block_id(instruction: &Instruction, block_id: BlockId) -> bool {
        matches!(instruction.opcode(), OpCode::Jump)
            && instruction.cast::<JumpInstruction>().target() == block_id as u32
    }
}

/// Stack of flags that are active in the current context. The topmost set of flags in the stack
/// is the current set of flags.
pub struct RegExpFlagsStack {
    flags: Vec<RegExpFlags>,
}

impl RegExpFlagsStack {
    pub fn new(flags: RegExpFlags) -> Self {
        Self { flags: vec![flags] }
    }

    pub fn current(&self) -> RegExpFlags {
        *self.flags.last().unwrap()
    }

    /// Push the flags for an anonymous group onto the stack of current flags. Return whether any
    /// flags were pushed (and require a corresponding pop).
    pub fn push_group_flags(&mut self, group: &AnonymousGroup) -> bool {
        // Update the set of current flags if any modifiers are present in this group
        if group.positive_modifiers.is_empty() && group.negative_modifiers.is_empty() {
            return false;
        }

        let new_flags = (self.current() | group.positive_modifiers) & !group.negative_modifiers;
        self.flags.push(new_flags);

        true
    }

    pub fn pop_group_flags(&mut self) {
        self.flags.pop();
    }
}

/// Set of constants generated for this regular expression. Dedupes constants, storing both original
/// data and the full encoded data for all constants.
struct ConstantTableBuilder {
    /// Constants along with their offset into the encoded bytes.
    constants: HashMap<RegExpConstant, u32>,
    /// Encoded data section containing all constants generated so far.
    bytes: Vec<u8>,
}

#[derive(Eq, PartialEq, Hash)]
enum RegExpConstant {
    /// A one-byte string constant encoded as a [u8].
    OneByteString(Vec<u8>),
    /// A two-byte string constant encoded as a [u16].
    TwoByteString(Vec<u16>),
    /// A code point set's data encoded as a [u32].
    CodePointSet(Vec<u32>),
}

impl ConstantTableBuilder {
    fn new() -> Self {
        Self { constants: HashMap::new(), bytes: Vec::new() }
    }

    /// Add a constant, returning the deduped offset of the constant in the encoded data. Adds
    /// padding to guarantee that constant will have same alignment as type T.
    fn insert_with<T>(
        &mut self,
        constant: RegExpConstant,
        mut add_bytes: impl FnMut(&mut Vec<u8>),
    ) -> EmitResult<u32> {
        if let Some(existing) = self.constants.get(&constant) {
            return Ok(*existing);
        }

        // Add padding for alignment if necessary
        let aligned_size = self.bytes.len().next_multiple_of(align_of::<T>());
        self.bytes.resize(aligned_size, 0);

        // Size of the constant table is limited to fit in a u32, which is enforced after every
        // constant is added.
        let offset = self.bytes.len().to_u32().unwrap();

        add_bytes(&mut self.bytes);

        if self.bytes.len().to_u32().is_none() {
            return Err(EmitError::ConstantTableTooLarge);
        }

        self.constants.insert(constant, offset);

        Ok(offset)
    }

    fn add_one_byte(&mut self, string_data: &[u8]) -> EmitResult<u32> {
        self.insert_with::<u8>(RegExpConstant::OneByteString(string_data.to_vec()), |bytes| {
            bytes.extend_from_slice(string_data);
        })
    }

    fn add_two_byte(&mut self, string_data: &[u16]) -> EmitResult<u32> {
        self.insert_with::<u16>(RegExpConstant::TwoByteString(string_data.to_vec()), |bytes| {
            for code_unit in string_data {
                bytes.extend_from_slice(&code_unit.to_ne_bytes());
            }
        })
    }

    fn add_code_point_set(&mut self, set_data: &[u32]) -> EmitResult<u32> {
        self.insert_with::<u32>(RegExpConstant::CodePointSet(set_data.to_vec()), |bytes| {
            for word in set_data {
                bytes.extend_from_slice(&word.to_ne_bytes());
            }
        })
    }
}

/// A part of a literal string that will be emitted separately. Keeps runs of code points that can
/// be emitted as a single string literal where possible.
enum LiteralPart {
    Run { code_points: Vec<CodePoint>, width: StringWidth },
    CodePoint(CodePoint),
    CaseClosure(CodePoint),
}

pub fn compile_regexp(
    cx: Context,
    regexp: &RegExp,
    source: Handle<StringValue>,
) -> EmitResult<Handle<CompiledRegExp>> {
    let match_start_analysis = MatchStartAnalyzer::analyze(regexp);
    let match_start_filter = MatchStartFilter::new(&match_start_analysis);
    let required_literal = RequiredLiteralAnalyzer::analyze(regexp);

    let mut compiler = RegExpCompiler::new(regexp, source);
    let compiled_regexp = compiler.compile(cx, regexp, match_start_filter, required_literal)?;

    if cx.options.print_regexp_bytecode {
        let bytecode_string =
            compiled_regexp.debug_print(DebugPrintMode::Verbose, Some(&match_start_analysis));
        cx.print_or_add_to_dump_buffer(&bytecode_string);
    }

    save_regexp_dotfile_if_needed(cx, *compiled_regexp);

    Ok(compiled_regexp)
}
