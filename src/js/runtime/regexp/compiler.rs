use std::collections::HashSet;

use icu_collections::codepointinvlist::CodePointInversionList;
use num_traits::ToPrimitive;

use crate::{
    common::{
        unicode::CodePoint,
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
        alloc_error::AllocResult,
        debug_print::DebugPrintMode,
        regexp::{
            code_point_set::{CodePointSetBuilder, WORD_CASE_INSENSITIVE_UNICODE_SET, WORD_SET},
            compiled_regexp::CompiledRegExp,
            graphviz::save_regexp_dotfile_if_needed,
            instruction::{
                AcceptInstruction, AssertEndInstruction, AssertEndOrNewlineInstruction,
                AssertNotWordBoundaryInstruction, AssertStartInstruction,
                AssertStartOrNewlineInstruction, AssertWordBoundaryInstruction,
                BackreferenceInstruction, BranchInstruction, ClearCaptureInstruction,
                CompareBetweenInstruction, CompareEqualsInstruction, ConsumeIfFalseInstruction,
                ConsumeIfTrueInstruction, FailInstruction, Instruction, InstructionIterator,
                InstructionIteratorMut, JumpInstruction, LiteralInstruction, LookaroundInstruction,
                LoopInstruction, MarkCapturePointInstruction, OpCode, ProgressInstruction,
                SetProgressInstruction, WildcardInstruction, WildcardNoNewlineInstruction,
                WordBoundaryMoveToPreviousInstruction,
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

impl RegExpCompiler {
    fn new(regexp: &RegExp, source: Handle<StringValue>) -> Self {
        Self {
            blocks: vec![],
            flags: RegExpFlagsStack::new(regexp.flags),
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

    fn emit_literal_instruction(&mut self, code_point: CodePoint) {
        LiteralInstruction::write(self.current_block_buf(), code_point)
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

    fn emit_word_boundary_move_to_previous_instruction(&mut self) {
        WordBoundaryMoveToPreviousInstruction::write(self.current_block_buf())
    }

    fn emit_assert_word_boundary_instruction(&mut self) {
        AssertWordBoundaryInstruction::write(self.current_block_buf())
    }

    fn emit_assert_not_word_boundary_instruction(&mut self) {
        AssertNotWordBoundaryInstruction::write(self.current_block_buf())
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

    fn emit_consume_if_true_instruction(&mut self) {
        ConsumeIfTrueInstruction::write(self.current_block_buf())
    }

    fn emit_consume_if_false_instruction(&mut self) {
        ConsumeIfFalseInstruction::write(self.current_block_buf())
    }

    fn emit_compare_equals_instruction(&mut self, code_point: CodePoint) {
        CompareEqualsInstruction::write(self.current_block_buf(), code_point)
    }

    fn emit_compare_between_instruction(&mut self, start: CodePoint, end: CodePoint) {
        CompareBetweenInstruction::write(self.current_block_buf(), start, end)
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
    ) -> AllocResult<Handle<CompiledRegExp>> {
        // Prime with new block
        self.new_block();

        // Wrap the entire pattern in the 0'th capture group
        self.emit_mark_capture_point_instruction(0);
        self.emit_disjunction(&regexp.disjunction);
        self.emit_mark_capture_point_instruction(1);

        self.emit_accept_instruction();

        let instructions = self.flatten_and_fix_indices();

        CompiledRegExp::new(
            cx,
            instructions,
            regexp,
            self.source,
            self.num_progress_points,
            self.num_loop_registers,
            match_start_filter,
            required_literal_filter,
        )
    }

    fn emit_disjunction(&mut self, disjunction: &Disjunction) {
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

                self.emit_alternative(alternative);

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
        }
    }

    fn emit_alternative(&mut self, alternative: &Alternative) {
        if self.is_forwards() {
            for term in alternative.terms.iter() {
                self.emit_term(term);
            }
        } else {
            // When emitting backwards, emit concatenation of terms in reverse order
            for term in alternative.terms.iter().rev() {
                self.emit_term(term);
            }
        }
    }

    fn emit_term(&mut self, term: &Term) {
        match term {
            Term::Literal(string) => {
                self.emit_literal(string);
            }
            Term::Wildcard => {
                self.emit_wildcard();
            }
            Term::Quantifier(quantifier) => self.emit_quantifier(quantifier),
            Term::Assertion(assertion) => {
                self.emit_assertion(assertion);
            }
            Term::CaptureGroup(group) => self.emit_capture_group(group),
            Term::AnonymousGroup(group) => self.emit_anonymous_group(group),
            Term::CharacterClass(character_class) => {
                self.emit_character_class(character_class);
            }
            Term::Lookaround(lookaround) => self.emit_lookaround(lookaround),
            Term::Backreference(backreference) => {
                self.emit_backreference_instruction(
                    self.current_flags().is_case_insensitive(),
                    backreference.index,
                );
            }
        }
    }

    fn emit_code_point_literal(&mut self, code_point: CodePoint) {
        if self.current_flags().is_case_insensitive() {
            let set = CodePointSetBuilder::code_point_to_set(code_point, self.current_flags());
            self.emit_code_point_set(&set, /* is_inverted */ false);
        } else {
            self.emit_literal_instruction(code_point);
        }
    }

    fn emit_literal(&mut self, string: AstStr) {
        if self.is_forwards() {
            for code_point in string.iter_code_points() {
                self.emit_code_point_literal(code_point)
            }
        } else {
            // When emitting backwards, emit concatenation of literals in reverse order
            let code_points = string.iter_code_points().collect::<Vec<_>>();
            for code_point in code_points.iter().rev() {
                self.emit_code_point_literal(*code_point)
            }
        }
    }

    fn emit_wildcard(&mut self) {
        if self.current_flags().is_dot_all() {
            self.emit_wildcard_instruction()
        } else {
            self.emit_wildcard_no_newline_instruction()
        }
    }

    fn emit_assertion(&mut self, assertion: &Assertion) {
        match assertion {
            Assertion::Start => {
                if self.current_flags().is_multiline() {
                    self.emit_assert_start_or_newline_instruction()
                } else {
                    self.emit_assert_start_instruction()
                }
            }
            Assertion::End => {
                if self.current_flags().is_multiline() {
                    self.emit_assert_end_or_newline_instruction()
                } else {
                    self.emit_assert_end_instruction()
                }
            }
            Assertion::WordBoundary => self.emit_assert_word_boundary(),
            Assertion::NotWordBoundary => self.emit_assert_not_word_boundary(),
        }
    }

    fn emit_assert_word_boundary(&mut self) {
        self.emit_word_comparison();
        self.emit_word_boundary_move_to_previous_instruction();
        self.emit_word_comparison();
        self.emit_assert_word_boundary_instruction()
    }

    fn emit_assert_not_word_boundary(&mut self) {
        self.emit_word_comparison();
        self.emit_word_boundary_move_to_previous_instruction();
        self.emit_word_comparison();
        self.emit_assert_not_word_boundary_instruction()
    }

    fn emit_word_comparison(&mut self) {
        let flags = self.current_flags();
        let word_set = if flags.is_case_insensitive() && flags.has_any_unicode_flag() {
            &WORD_CASE_INSENSITIVE_UNICODE_SET
        } else {
            &WORD_SET
        };

        self.emit_set_comparisons(word_set);
    }

    fn emit_quantifier(&mut self, quantifier: &Quantifier) {
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
                    self.emit_term(&quantifier.term)
                } else {
                    self.emit_quantified_term_with_cleared_captures(quantifier)
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

            self.emit_quantified_term_with_cleared_captures(quantifier);
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

            return;
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
                        self.emit_term(&quantifier.term)
                    } else {
                        self.emit_quantified_term_with_cleared_captures(quantifier)
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

                self.emit_quantified_term_with_cleared_captures(quantifier);

                // Ensure that each repetition makes progress, if necessary
                if let Some(progress_index) = progress_index {
                    self.emit_progress_instruction(progress_index);
                }

                self.emit_quantifier_optional_branch(quantifier, loop_block_id, join_block_id);
            }

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        } else {
            // Any number of future repetitions
            let term_block_id = self.new_block();
            let join_block_id = self.new_block();

            self.emit_quantifier_optional_branch(quantifier, term_block_id, join_block_id);

            // Emit term block
            self.set_current_block(term_block_id);
            self.emit_quantified_term_with_cleared_captures(quantifier);

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
    fn emit_quantified_term_with_cleared_captures(&mut self, quantifier: &Quantifier) {
        for capture_index in quantifier.captures.into_iter().flatten() {
            self.emit_clear_capture_instruction(capture_index);
        }

        self.emit_term(&quantifier.term);
    }

    fn emit_capture_group(&mut self, group: &CaptureGroup) {
        // Calculate capture point indices from capture group
        let mut capture_start_index = group.index * 2;
        let mut capture_end_index = capture_start_index + 1;

        // Reverse order of capture indices when emitting backwards
        if !self.is_forwards() {
            std::mem::swap(&mut capture_start_index, &mut capture_end_index);
        }

        self.emit_mark_capture_point_instruction(capture_start_index);
        self.emit_disjunction(&group.disjunction);
        self.emit_mark_capture_point_instruction(capture_end_index);
    }

    fn emit_anonymous_group(&mut self, group: &AnonymousGroup) {
        // Update the set of current flags if any modifiers are present in this group
        let updated_flags = self.flags.push_group_flags(group);

        self.emit_disjunction(&group.disjunction);

        if updated_flags {
            self.flags.pop_group_flags();
        }
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) {
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
                self.emit_class_string_disjunction(&strings, join_block_id);
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
        self.emit_code_point_set(&set, is_check_inverted);

        // If there is a string disjunction then proceed to the final join block
        if let Some(StringDisjunctionInfo { join_block_id, .. }) = string_disjunction_info {
            self.emit_jump_instruction(join_block_id);
            self.set_current_block(join_block_id);
        }
    }

    fn emit_code_point_set(&mut self, set: &CodePointInversionList, is_inverted: bool) {
        // Can emit a literal instruction if we are matching a single code point
        if set.size() == 1 && !is_inverted {
            let single_range = set.iter_ranges().next();
            let single_code_point = *single_range.unwrap().start();
            self.emit_literal_instruction(single_code_point);
            return;
        }

        self.emit_set_comparisons(set);

        // Emit the final consume instruction, noting whether to invert
        if is_inverted {
            self.emit_consume_if_false_instruction();
        } else {
            self.emit_consume_if_true_instruction();
        }
    }

    fn emit_set_comparisons(&mut self, set: &CodePointInversionList) {
        // Emit all range comparisons in the set. Iterates over inclusive ranges.
        for range in set.iter_ranges() {
            let start = *range.start();
            let end = *range.end();

            if start == end {
                self.emit_compare_equals_instruction(start);
            } else {
                self.emit_compare_between_instruction(start, end);
            }
        }
    }

    fn emit_class_string_disjunction(
        &mut self,
        strings: &HashSet<Wtf8Cow>,
        success_block: BlockId,
    ) {
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
            self.emit_literal(string.as_str());
            self.emit_jump_instruction(success_block);
        }

        // Disjunction ends at start of join block
        self.set_current_block(join_block_id);
    }

    fn emit_lookaround(&mut self, lookaround: &Lookaround) {
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
        self.emit_disjunction(&lookaround.disjunction);
        self.emit_accept_instruction();

        self.exit_direction_context();

        self.set_current_block(current_block_id);
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

pub fn compile_regexp(
    cx: Context,
    regexp: &RegExp,
    source: Handle<StringValue>,
) -> AllocResult<Handle<CompiledRegExp>> {
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
