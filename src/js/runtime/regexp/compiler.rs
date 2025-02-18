use std::{collections::HashSet, sync::LazyLock};

use brimstone_icu_collections::{
    all_case_folded_set, get_case_closure_override, has_case_closure_override,
};
use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};

use crate::js::{
    common::{
        icu::ICU,
        unicode::{is_surrogate_code_point, CodePoint, MAX_CODE_POINT},
        wtf_8::Wtf8String,
    },
    parser::regexp::{
        Alternative, AnonymousGroup, Assertion, CaptureGroup, CaptureGroupIndex, CharacterClass,
        ClassExpressionType, ClassRange, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags,
        Term,
    },
    runtime::{
        debug_print::{DebugPrint, DebugPrintMode},
        string_value::StringValue,
        Context, Handle,
    },
};

use super::{
    compiled_regexp::CompiledRegExpObject,
    instruction::{
        AcceptInstruction, AssertEndInstruction, AssertEndOrNewlineInstruction,
        AssertNotWordBoundaryInstruction, AssertStartInstruction, AssertStartOrNewlineInstruction,
        AssertWordBoundaryInstruction, BackreferenceInstruction, BranchInstruction,
        ClearCaptureInstruction, CompareBetweenInstruction, CompareEqualsInstruction,
        ConsumeIfFalseInstruction, ConsumeIfTrueInstruction, FailInstruction,
        InstructionIteratorMut, JumpInstruction, LiteralInstruction, LookaroundInstruction,
        LoopInstruction, MarkCapturePointInstruction, OpCode, ProgressInstruction,
        WildcardInstruction, WildcardNoNewlineInstruction, WordBoundaryMoveToPreviousInstruction,
    },
};

type BlockId = usize;

struct CompiledRegExpBuilder {
    blocks: Vec<Vec<u32>>,
    /// Stack of flags that are active in the current context. The topmost set of flags in the stack
    /// is the current set of flags.
    flags: Vec<RegExpFlags>,
    source: Handle<StringValue>,
    current_block_id: BlockId,
    num_progress_points: u32,
    num_loop_registers: u32,
    /// Nonempty stack of directions denoting the set of possibly nested direction contexts created
    /// by lookaround. The top of the stack is the current direction.
    direction_stack: Vec<Direction>,
    /// Incremented every time we enter the body of a quantifier that may have at least 2
    /// repititions and decremented when we leave. Used to detect if we are within the body of a
    /// quantifier that may be repeated 2+ times, which require special consideration.
    repitition_depth: u64,
}

#[derive(PartialEq)]
enum Direction {
    Forward,
    Backward,
}

/// Information gathered about a compiled subexpression that is needed in compilation of the parent.
struct SubExpressionInfo {
    /// Whether a character is guaranteed to be consumed on all paths
    always_consumes: bool,
    /// All capture groups in the subexpression
    captures: Vec<CaptureGroupIndex>,
}

impl SubExpressionInfo {
    fn no_captures(always_consumes: bool) -> Self {
        Self { always_consumes, captures: vec![] }
    }

    fn with_captures(always_consumes: bool, captures: Vec<CaptureGroupIndex>) -> Self {
        Self { always_consumes, captures }
    }
}

/// Maximum number of repititons within a quantifier that will have their terms inlined, vs using
/// loop instructions.
const MAX_INLINED_REPITITIONS: u64 = 10;

impl CompiledRegExpBuilder {
    fn new(regexp: &RegExp, source: Handle<StringValue>) -> Self {
        Self {
            blocks: vec![],
            flags: vec![regexp.flags],
            source,
            current_block_id: 0,
            num_progress_points: 0,
            num_loop_registers: 0,
            direction_stack: vec![Direction::Forward],
            repitition_depth: 0,
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

    fn enter_repitition_context(&mut self) {
        self.repitition_depth += 1;
    }

    fn exit_repitition_context(&mut self) {
        self.repitition_depth -= 1;
    }

    fn is_in_repitition(&self) -> bool {
        self.repitition_depth > 0
    }

    fn current_block_buf(&mut self) -> &mut Vec<u32> {
        &mut self.blocks[self.current_block_id]
    }

    fn current_flags(&self) -> RegExpFlags {
        *self.flags.last().unwrap()
    }

    fn is_case_insensitive_unicode_sets(&self) -> bool {
        let flags = self.current_flags();
        flags.has_unicode_sets_flag() && flags.is_case_insensitive()
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

    fn compile(&mut self, cx: Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
        // Prime with new block
        self.new_block();

        // Emit preamble allowing match to start at any point in the string
        if !self.current_flags().is_sticky() {
            self.emit_preamble();
        }

        // Wrap the entire pattern in the 0'th capture group
        self.emit_mark_capture_point_instruction(0);
        self.emit_disjunction(&regexp.disjunction);
        self.emit_mark_capture_point_instruction(1);

        self.emit_accept_instruction();

        let instructions = self.flatten_and_fix_indices();

        CompiledRegExpObject::new(
            cx,
            instructions,
            regexp,
            self.source,
            self.num_progress_points,
            self.num_loop_registers,
        )
    }

    /// Emit the preable for the regexp, which allows starting the match at any point in the string.
    /// Equivalent to prefixing the regexp with `.*?`. Note the laziness as we want the leftmost
    /// match.
    fn emit_preamble(&mut self) {
        // Optionally enter wildcard block
        let wildcard_block = self.new_block();
        let join_block = self.new_block();
        self.emit_branch_instruction(join_block, wildcard_block);

        // Repeat wildcard block
        self.set_current_block(wildcard_block);
        self.emit_wildcard_instruction();
        self.emit_branch_instruction(join_block, wildcard_block);

        // Resume at the join block where pattern will start being emitted
        self.set_current_block(join_block);
    }

    fn emit_disjunction(&mut self, disjunction: &Disjunction) -> SubExpressionInfo {
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

            // Disjunction always consumes if all alternatives always consume
            let mut always_consumes = true;

            // Combine captures from all alternatives
            let mut captures = vec![];

            // Emit all alternative blocks
            struct AlternativeBlock {
                entry_block: usize,
                exit_block: usize,
                captures: Vec<CaptureGroupIndex>,
            }
            let mut alternative_blocks: Vec<AlternativeBlock> = vec![];

            for alternative in &disjunction.alternatives {
                let alternative_block_id = self.new_block();
                self.set_current_block(alternative_block_id);

                let info = self.emit_alternative(alternative);

                alternative_blocks.push(AlternativeBlock {
                    entry_block: alternative_block_id,
                    exit_block: self.current_block_id,
                    captures: info.captures.clone(),
                });

                always_consumes &= info.always_consumes;
                captures.extend(info.captures);
            }

            // If this disjunction is in a repitition we must clear the captures for all
            // alternatives not taken in case they were previously matched.
            if self.is_in_repitition() {
                // All alternatives but the last two have their captures cleared at the start of the
                // next branch block, since all successful paths that don't match the previous
                // alternative will necessarily pass through the next branch block.
                for i in 1..alternative_blocks.len() - 1 {
                    if !alternative_blocks[i - 1].captures.is_empty() {
                        let prev_alternative_captures = &alternative_blocks[i - 1].captures;
                        self.set_current_block(branch_block_ids[i]);
                        for capture_index in prev_alternative_captures {
                            self.emit_clear_capture_instruction(*capture_index);
                        }
                    }
                }

                // The second to last alternative emits its captures as part of the last alternative
                // block, since either the last alternative proceeds or the entire disjunction fails
                // to match,
                let penultimate_alternative_captures =
                    &alternative_blocks[alternative_blocks.len() - 2].captures;
                for capture_index in penultimate_alternative_captures {
                    self.set_current_block(
                        alternative_blocks[alternative_blocks.len() - 1].exit_block,
                    );
                    self.emit_clear_capture_instruction(*capture_index);
                }

                // Note that the last alternative does not need its captures cleared when entering
                // another block because there is no other block to enter. Either the last
                // alternative matches or the entire disjunction fails to match.
            }

            // Blocks each alternative joins to, in reverse order. Note that the last alternative
            // always jumps to the final join block.
            let mut join_blocks_rev = vec![join_block_id];

            // If in a repitition, create a chain of a chain of clear blocks that clear all captures
            // from the first alternative to the last alternative. Each alternative jumps to the
            // clear block that clears the captures for all later alternatives.
            for i in (1..alternative_blocks.len()).rev() {
                let alternative_captures = &alternative_blocks[i].captures;
                if self.is_in_repitition() && !alternative_captures.is_empty() {
                    let clear_capture_block_id = self.new_block();

                    // Emit this clear block and set it as the current one
                    self.set_current_block(clear_capture_block_id);
                    for capture_index in alternative_captures {
                        self.emit_clear_capture_instruction(*capture_index);
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

            SubExpressionInfo::with_captures(always_consumes, captures)
        }
    }

    fn emit_alternative(&mut self, alternative: &Alternative) -> SubExpressionInfo {
        if self.is_forwards() {
            self.emit_alternative_terms(alternative.terms.iter())
        } else {
            // When emitting backwards, emit concatenation of terms in reverse order
            self.emit_alternative_terms(alternative.terms.iter().rev())
        }
    }

    fn emit_alternative_terms<'a>(
        &mut self,
        iter: impl Iterator<Item = &'a Term>,
    ) -> SubExpressionInfo {
        // If any term always consumes then the alternative always consumes
        let mut always_consumes = false;

        // Accumulate all captures from terms
        let mut captures = vec![];

        for term in iter {
            let info = self.emit_term(term);
            always_consumes |= info.always_consumes;
            captures.extend(info.captures);
        }

        SubExpressionInfo::with_captures(always_consumes, captures)
    }

    fn emit_term(&mut self, term: &Term) -> SubExpressionInfo {
        match term {
            Term::Literal(string) => {
                self.emit_literal(string);

                // Literals are non-empty so they always consume a character
                SubExpressionInfo::no_captures(true)
            }
            Term::Wildcard => {
                self.emit_wildcard();

                // The wildcard always consumes a character
                SubExpressionInfo::no_captures(true)
            }
            Term::Quantifier(quantifier) => self.emit_quantifier(quantifier),
            Term::Assertion(assertion) => {
                self.emit_assertion(assertion);

                // Assertions never consume a character
                SubExpressionInfo::no_captures(false)
            }
            Term::CaptureGroup(group) => self.emit_capture_group(group),
            Term::AnonymousGroup(group) => self.emit_anonymous_group(group),
            Term::CharacterClass(character_class) => {
                self.emit_character_class(character_class);

                // Character classes always consume a character
                SubExpressionInfo::no_captures(true)
            }
            Term::Lookaround(lookaround) => self.emit_lookaround(lookaround),
            Term::Backreference(backreference) => {
                self.emit_backreference_instruction(
                    self.current_flags().is_case_insensitive(),
                    backreference.index,
                );

                // Backreferences may be empty depending on what was captured
                SubExpressionInfo::no_captures(false)
            }
        }
    }

    fn emit_code_point_literal(&mut self, code_point: CodePoint) {
        if self.current_flags().is_case_insensitive() && !is_surrogate_code_point(code_point) {
            // Under case insensitive mode, emit a comparison against any code points in the case
            // insensitive closure of the code point. This will check for any code points which
            // canonicalize to the same value as the literal code point.
            let mut closure_set_builder = CodePointInversionListBuilder::new();
            self.add_case_closure(&mut closure_set_builder, char::from_u32(code_point).unwrap());
            let closure_set = closure_set_builder.build();

            self.emit_code_point_set(&closure_set, /* is_inverted */ false);
        } else {
            self.emit_literal_instruction(code_point)
        }
    }

    fn emit_literal(&mut self, string: &Wtf8String) {
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

    fn in_block<R>(&mut self, block_id: BlockId, f: impl FnOnce(&mut Self) -> R) -> R {
        let current_block_id = self.current_block_id;
        self.set_current_block(block_id);
        let result = f(self);
        self.set_current_block(current_block_id);

        result
    }

    fn emit_quantifier(&mut self, quantifier: &Quantifier) -> SubExpressionInfo {
        let is_inside_repitition = self.is_in_repitition();

        // A repitition is any quantifier that can be run at least twice
        let is_repitition = match quantifier.max {
            None => true,
            Some(max) => max > 1,
        };

        if is_repitition {
            self.enter_repitition_context();
        }

        // Start a new block, saving a reference to the block before the quantifier so we can
        // potentially patch in instructions later.
        let quantifier_patch_block = self.new_block();
        let quantifier_start_block = self.new_block();
        self.emit_jump_instruction(quantifier_patch_block);
        self.set_current_block(quantifier_start_block);

        // Quantifier always has the same captures as its wrapped term. But quantifier only has the
        // same always consume status as its wrapped term when there are minimum repititions.
        // Otherwise the quantifier is never guaranteed to consume.
        let mut quantifier_info = SubExpressionInfo::no_captures(false);

        // Can inline a small number of repititions otherwise use a loop
        if quantifier.min != 0 && quantifier.min <= MAX_INLINED_REPITITIONS {
            // Emit term min times for repititions that must be present
            for _ in 0..quantifier.min {
                quantifier_info = self.emit_term(&quantifier.term);
            }
        } else if quantifier.min > u32::MAX as u64 {
            // The minimum number of repititions is greater than the max possible string length.
            // Each repitition must consume at least one character, so we know this quantifier will
            // fail to match.
            self.emit_fail_instruction();
        } else if quantifier.min != 0 {
            // Jump to a new loop block for the minimum repititions
            let loop_block_id = self.new_block();
            let loop_end_block_id = self.new_block();

            self.emit_jump_instruction(loop_block_id);

            // Loop block consists of loop instruction, term, then loops back to start of block
            self.in_block(loop_block_id, |this| {
                let loop_register_index = this.next_loop_register();
                this.emit_loop_instruction(
                    loop_register_index,
                    quantifier.min as u32,
                    loop_end_block_id as u32,
                );

                quantifier_info = this.emit_term(&quantifier.term);

                this.emit_jump_instruction(loop_block_id);
            });

            // Start emitting in the loop end block after loop finishes
            self.set_current_block(loop_end_block_id);
        }

        if let Some(max) = quantifier.max {
            let num_remaining_repititions = max - quantifier.min;

            // Exact number of repititions
            if num_remaining_repititions == 0 {
                // Clean up any necessary contexts
                if is_repitition {
                    self.exit_repitition_context();
                }

                return quantifier_info;
            }

            let join_block_id = self.new_block();

            // Can inline a small number of optional repititions otherwise use a loop
            if num_remaining_repititions <= MAX_INLINED_REPITITIONS {
                let mut progress_index = None;

                // Emit term blocks max - min times, each is optional and is preceded by a branch to
                // the join block.
                for i in quantifier.min..max {
                    let pred_block_id = self.current_block_id;

                    // Emit term block
                    let term_block_id = self.new_block();
                    self.set_current_block(term_block_id);
                    let info = self.emit_term(&quantifier.term);

                    // Each optional iteration of quantifier must consume at least one character.
                    // We can ensure this by emitting a progress instruction which checks that
                    // progress has been made since the last execution of the progress instruction.
                    if !info.always_consumes {
                        // Lazily create the shared progress point, shared between the inlined
                        // term blocks.
                        let progress_index =
                            progress_index.get_or_insert_with(|| self.new_progress_point());
                        self.emit_progress_instruction(*progress_index);
                    }

                    // Emit branch between term block and join block in predecessor
                    self.in_block(pred_block_id, |this| {
                        // If we are in a repitition but never enter a term block with captures even
                        // once, we must clear those captures in case they were previously matched.
                        if i == 0 && is_inside_repitition && !info.captures.is_empty() {
                            this.emit_quantifier_clear_captures_branch(
                                quantifier,
                                &info,
                                term_block_id,
                                join_block_id,
                            )
                        } else {
                            this.emit_quantifier_branch(quantifier, term_block_id, join_block_id);
                        }
                    });

                    quantifier_info.captures = info.captures;
                }

                // Last term block always proceeds to the join block
                self.emit_jump_instruction(join_block_id);
            } else if num_remaining_repititions > u32::MAX as u64 {
                // The minimum number of repititions is greater than the max possible string length.
                // Each repitition must consume at least one character, so we know this quantifier
                // will fail to match.
                self.emit_fail_instruction();
            } else {
                let predecessor_block_id = self.current_block_id;
                let loop_block_id = self.new_block();

                // Will emit the branch to the loop block later

                // Loop block consists of loop instruction, term, then branches back to start of block
                let info = self.in_block(loop_block_id, |this| {
                    let loop_register_index = this.next_loop_register();
                    this.emit_loop_instruction(
                        loop_register_index,
                        num_remaining_repititions as u32,
                        join_block_id as u32,
                    );

                    let info = this.emit_term(&quantifier.term);

                    // Each optional iteration of quantifier must consume at least one character.
                    // We can ensure this by emitting a progress instruction which checks that
                    // progress has been made since the last execution of the progress instruction.
                    if !info.always_consumes {
                        let progress_index = this.new_progress_point();
                        this.emit_progress_instruction(progress_index);
                    }

                    this.emit_quantifier_branch(quantifier, loop_block_id, join_block_id);

                    info
                });

                // Emit the branch to the loop block from predecessor
                self.set_current_block(predecessor_block_id);

                // If we are in a repitition but never enter a term block with captures even once,
                // we must clear those captures in case they were previously matched.
                if quantifier.min == 0 && is_inside_repitition && !info.captures.is_empty() {
                    self.emit_quantifier_clear_captures_branch(
                        quantifier,
                        &info,
                        loop_block_id,
                        join_block_id,
                    )
                } else {
                    self.emit_quantifier_branch(quantifier, loop_block_id, join_block_id);
                }

                quantifier_info.captures = info.captures;
            }

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        } else {
            // Any number of future repititions
            let term_block_id = self.new_block();
            let join_block_id = self.new_block();

            let pred_block_id = self.current_block_id;

            // Emit term block
            self.set_current_block(term_block_id);
            let info = self.emit_term(&quantifier.term);

            // Optionally enter term block from predecessor block
            self.in_block(pred_block_id, |this| {
                // If we are in a repitition but never enter a term block with captures, we must
                // clear those captures in case they were previously matched.
                if is_inside_repitition && !info.captures.is_empty() {
                    this.emit_quantifier_clear_captures_branch(
                        quantifier,
                        &info,
                        term_block_id,
                        join_block_id,
                    );
                } else {
                    this.emit_quantifier_branch(quantifier, term_block_id, join_block_id);
                }
            });

            quantifier_info.captures = info.captures;

            // Each optional iteration of quantifier must consume at least one character. We can
            // ensure this by emitting a progress instruction which checks that progress has been
            // made since the last execution of the progress instruction.
            if !info.always_consumes {
                let progress_index = self.new_progress_point();
                self.emit_progress_instruction(progress_index);
            }

            // Term block optionally loops back to itself
            self.emit_quantifier_branch(quantifier, term_block_id, join_block_id);

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        }

        if is_repitition {
            self.exit_repitition_context();
        }

        // If we are in a quantifier with a minimum of 0 repititions, then 0-length matches of that
        // quantifier should not set captures. This is implemented by detecting this case with a
        // progress instruction to make sure the quantifier consumed, and if not clearing all
        // captures in the subexpression.
        //
        // Note that the progress instruction must be patched before the start of the quantifier.
        if quantifier.min == 0
            && !quantifier_info.always_consumes
            && !quantifier_info.captures.is_empty()
        {
            // Emit a progress instruction before the quantifier starts
            let progress_index = self.new_progress_point();
            self.in_block(quantifier_patch_block, |this| {
                this.emit_progress_instruction(progress_index);
            });

            // Then after the quantifier completes
            let check_progress_and_continue_block = self.new_block();
            let clear_captures_block = self.new_block();
            let join_block = self.new_block();
            self.emit_branch_instruction(check_progress_and_continue_block, clear_captures_block);

            // Start by checking progress, and if we pass then proceed to the join block with
            // captures intact.
            self.in_block(check_progress_and_continue_block, |this| {
                this.emit_progress_instruction(progress_index);
                this.emit_jump_instruction(join_block);
            });

            // If checking progress failed then we will end up in this path and shold clear all
            // captures before proceeding.
            self.in_block(clear_captures_block, |this| {
                for capture_index in &quantifier_info.captures {
                    this.emit_clear_capture_instruction(*capture_index);
                }
                this.emit_jump_instruction(join_block);
            });

            self.set_current_block(join_block);
        }

        quantifier_info
    }

    fn emit_quantifier_branch(
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

    /// Emit a quantifier branch that clears all the provided captures along the edge to the
    /// provided join block.
    fn emit_quantifier_clear_captures_branch(
        &mut self,
        quantifier: &Quantifier,
        info: &SubExpressionInfo,
        term_block_id: BlockId,
        join_block_id: BlockId,
    ) {
        // First jump to the clear block, which clears all relevant captures, then
        // proceeds to the join block.
        let clear_block_id = self.new_block();
        self.emit_quantifier_branch(quantifier, term_block_id, clear_block_id);

        self.set_current_block(clear_block_id);
        for capture_index in &info.captures {
            self.emit_clear_capture_instruction(*capture_index);
        }

        self.emit_jump_instruction(join_block_id);
    }

    fn emit_capture_group(&mut self, group: &CaptureGroup) -> SubExpressionInfo {
        // Calculate capture point indices from capture group
        let mut capture_start_index = group.index * 2;
        let mut capture_end_index = capture_start_index + 1;

        // Reverse order of capture indices when emitting backwards
        if !self.is_forwards() {
            std::mem::swap(&mut capture_start_index, &mut capture_end_index);
        }

        self.emit_mark_capture_point_instruction(capture_start_index);
        let mut info = self.emit_disjunction(&group.disjunction);
        self.emit_mark_capture_point_instruction(capture_end_index);

        // Add this capture group to the set of captures
        info.captures.push(group.index);

        info
    }

    fn emit_anonymous_group(&mut self, group: &AnonymousGroup) -> SubExpressionInfo {
        // Update the set of current flags if any modifiers are present in this group
        let has_modifiers =
            !group.positive_modifiers.is_empty() || !group.negative_modifiers.is_empty();
        if has_modifiers {
            let new_flags =
                (self.current_flags() | group.positive_modifiers) & !group.negative_modifiers;
            self.flags.push(new_flags);
        }

        let info = self.emit_disjunction(&group.disjunction);

        if has_modifiers {
            self.flags.pop();
        }

        info
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) {
        let flags = self.current_flags();
        let (set, strings) = self.character_class_to_set(character_class);

        // First check strings if there are any
        let mut join_block = None;
        if !strings.is_empty() {
            let join_block_id = self.new_block();
            self.emit_class_string_disjunction(&strings, join_block_id);
            join_block = Some(join_block_id);
        }

        // If comparison is case insensitive then expand the set of code points to include the
        // case insensitive closure of all code points in the set.
        let set = if flags.is_case_insensitive() {
            self.case_close_over(&set)
        } else {
            set
        };

        // In unicode sets mode the set was eagerly inverted instead of inverting at the end
        let is_check_inverted = character_class.is_inverted && !flags.has_unicode_sets_flag();

        self.emit_code_point_set(&set, is_check_inverted);

        // If there is a join block (needed from string disjunction) then proceed to it
        if let Some(join_block) = join_block {
            self.emit_jump_instruction(join_block);
            self.set_current_block(join_block);
        }
    }

    fn character_class_to_set<'a, 'b>(
        &self,
        character_class: &'b CharacterClass,
    ) -> (CodePointInversionList<'a>, HashSet<&'b Wtf8String>) {
        let mut set_builder = CodePointInversionListBuilder::new();
        let mut strings = HashSet::new();

        let set = match character_class.expression_type {
            ClassExpressionType::Union => {
                // Add code points and strings that are in any operand
                for class_range in &character_class.operands {
                    self.add_character_class_range_to_set(
                        class_range,
                        &mut set_builder,
                        &mut strings,
                    );
                }

                self.maybe_simple_case_folding(set_builder.build())
            }
            ClassExpressionType::Intersection => {
                // Initialize sets with the first operand
                let (first_set, first_strings) =
                    self.character_class_range_to_set(&character_class.operands[0]);
                set_builder.add_set(&first_set);
                strings = first_strings;

                // Only retain code points and srings that are in all operands
                for class_range in &character_class.operands[1..] {
                    let (other_set, other_strings) = self.character_class_range_to_set(class_range);
                    set_builder.retain_set(&other_set);
                    strings.retain(|string| other_strings.contains(string));
                }

                set_builder.build()
            }
            ClassExpressionType::Difference => {
                // Initialize sets with the first operand
                let (first_set, first_strings) =
                    self.character_class_range_to_set(&character_class.operands[0]);
                set_builder.add_set(&first_set);
                strings = first_strings;

                // Remove code points and strings that are in later operands
                for class_range in &character_class.operands[1..] {
                    let (other_set, other_strings) = self.character_class_range_to_set(class_range);

                    set_builder.remove_set(&other_set);

                    for string in other_strings {
                        strings.remove(string);
                    }
                }

                set_builder.build()
            }
        };

        // Eagerly invert the set if in unicode sets mode
        let flags = self.current_flags();
        let set = if character_class.is_inverted && flags.has_unicode_sets_flag() {
            self.complement_set(&set)
        } else {
            set
        };

        (set, strings)
    }

    /// Return the complement of a set when in unicode sets mode.
    fn complement_set(&self, set: &CodePointInversionList) -> CodePointInversionList<'static> {
        let mut complement_builder = CodePointInversionListBuilder::new();

        // Start with set of all code points. Only including the canonical case folded set if in
        // case insensitive mode.
        if self.current_flags().is_case_insensitive() {
            complement_builder.add_set(all_case_folded_set());
        } else {
            complement_builder.add_set(&CodePointInversionList::all());
        }

        // Remove the target set from the set of all code points
        complement_builder.remove_set(set);

        complement_builder.build()
    }

    fn character_class_range_to_set<'a>(
        &self,
        class_range: &'a ClassRange,
    ) -> (CodePointInversionList<'static>, HashSet<&'a Wtf8String>) {
        let mut set_builder = CodePointInversionListBuilder::new();
        let mut strings = HashSet::new();

        self.add_character_class_range_to_set(class_range, &mut set_builder, &mut strings);
        let code_point_set = self.maybe_simple_case_folding(set_builder.build());

        (code_point_set, strings)
    }

    /// MaybeSimpleCaseFolding (https://tc39.es/ecma262/#sec-maybesimplecasefolding)
    fn maybe_simple_case_folding(
        &self,
        set: CodePointInversionList<'static>,
    ) -> CodePointInversionList<'static> {
        if !self.is_case_insensitive_unicode_sets() {
            return set;
        }

        let mut case_folded_set = CodePointInversionListBuilder::new();

        for code_point in set.iter_chars() {
            case_folded_set.add_char(ICU.case_mapper.simple_fold(code_point))
        }

        case_folded_set.build()
    }

    fn add_character_class_range_to_set<'a>(
        &self,
        class_range: &'a ClassRange,
        set_builder: &mut CodePointInversionListBuilder,
        strings_set_builder: &mut HashSet<&'a Wtf8String>,
    ) {
        match class_range {
            // Accumulate single and range char ranges
            ClassRange::Single(code_point) => {
                set_builder.add32(*code_point);
            }
            ClassRange::Range(start, end) => {
                // Otherwise can add the range directly
                set_builder.add_range32(*start..=*end);
            }
            // Use the precomputed word set. This is valid in case insensitive `u` mode because
            // the case closure will be created by the caller. This is valid in case sensitive `v`
            // mode becase MaybeSimpleCaseFolding will be applied by the caller.
            ClassRange::Word => set_builder.add_set(&WORD_SET),
            // Use the precomputed not word set if possible. In case insensitive `v` mode we must
            // construct the complement ourselves.
            ClassRange::NotWord => {
                let flags = self.current_flags();
                if flags.is_case_insensitive() && flags.has_any_unicode_flag() {
                    if flags.has_unicode_sets_flag() {
                        let set = self.complement_set(&WORD_CASE_INSENSITIVE_UNICODE_SET);
                        set_builder.add_set(&set);
                    } else {
                        set_builder.add_set(&NOT_WORD_CASE_INSENSITIVE_UNICODE_SET);
                    }
                } else {
                    set_builder.add_set(&NOT_WORD_SET);
                }
            }
            // Use the precomputed whitespace set
            ClassRange::Whitespace => set_builder.add_set(&WHITESPACE_SET),
            // Use the precomputed not whitespace set if possible. In case insensitive `v` mode we
            // must construct the complement ourselves.
            ClassRange::NotWhitespace => {
                if self.is_case_insensitive_unicode_sets() {
                    set_builder.add_set(&self.complement_set(&WHITESPACE_SET));
                } else {
                    set_builder.add_set(&NOT_WHITESPACE_SET)
                }
            }
            // Decimal ranges are simple so they are hardcoded
            ClassRange::Digit => {
                set_builder.add_range('0'..='9');
            }
            // Use the hardcoded simple decimal ranges when possible. In case insensitive `v` mode
            // we must construct the complement ourselves.
            ClassRange::NotDigit => {
                if self.is_case_insensitive_unicode_sets() {
                    let mut digits_set = CodePointInversionListBuilder::new();
                    digits_set.add_range('0'..='9');
                    set_builder.add_set(&self.complement_set(&digits_set.build()));
                } else {
                    set_builder.add_range32(0..('0' as u32));
                    set_builder.add_range32(('9' as u32 + 1)..=MAX_CODE_POINT);
                }
            }
            ClassRange::UnicodeProperty(property) => {
                // MaybeSimpleCaseFolding will be applied by the caller
                property.add_to_set(set_builder);
            }
            // Construct the complement of the unicode property set
            ClassRange::NotUnicodeProperty(property) => {
                let property_complement = if self.is_case_insensitive_unicode_sets() {
                    // In case insensitive unicode sets mode we must perform case folding before
                    // taking the complement.
                    let mut property_set = CodePointInversionListBuilder::new();
                    property.add_to_set(&mut property_set);
                    let property_set = self.maybe_simple_case_folding(property_set.build());
                    self.complement_set(&property_set)
                } else {
                    // Otherwise create the complement set directly. MaybeSimpleCaseFolding will
                    // be applied by the caller.
                    let mut property_complement = CodePointInversionListBuilder::new();
                    property.add_to_set(&mut property_complement);
                    property_complement.complement();
                    property_complement.build()
                };

                // Then add the complement set to the set builder
                set_builder.add_set(&property_complement);
            }
            ClassRange::NestedClass(nested_class) => {
                let (code_points_set, string_set) = self.character_class_to_set(nested_class);
                set_builder.add_set(&code_points_set);
                strings_set_builder.extend(string_set.iter());
            }
            ClassRange::StringDisjunction(disjunction) => {
                for string in &disjunction.alternatives {
                    // Check if the string has exactly one code point (only need to check at most
                    // the first two code points to be sure).
                    if string.iter_code_points().take(2).count() == 1 {
                        // Treat as a regular code point instead of a string
                        set_builder.add32(string.iter_code_points().next().unwrap());
                    } else {
                        // Treat as a string
                        strings_set_builder.insert(string);
                    }
                }
            }
        }
    }

    /// Create the case insensitive closure for the given set of code points, following the
    /// Canonicalization abstract operation from the spec.
    ///
    /// This is used to match for any character which is case-insensitive-equivalent to any
    /// character in the given set.
    fn case_close_over(&self, set: &CodePointInversionList) -> CodePointInversionList<'static> {
        let mut set_builder = CodePointInversionListBuilder::new();
        for code_point in set.iter_chars() {
            self.add_case_closure(&mut set_builder, code_point);
        }

        set_builder.build()
    }

    /// Create the spec-compliant case closure set for the given code point.
    fn add_case_closure(&self, set_builder: &mut CodePointInversionListBuilder, code_point: char) {
        // Case closure sets do not contain the code point itself
        set_builder.add_char(code_point);

        // We use `add_case_closure_to` from icu4x whenever possible.
        //
        // Unicode aware RegExp canonicalization uses standard Unicode simple case mapping, so
        // `add_case_closure_to` is sufficient.
        //
        // However unicode unware RegExp canonicalization uses a slightly different procedure,
        // mapping code points using simple uppercase mapping, but not mapping code points outside
        // the Latin1 range to within the Latin1 range. This has almost the same behavior as
        // `add_case_closure_to`, so we have precomputed the code points for which the behavior
        // differs. We use the precomupted override if one exists, otherwise we use
        // `add_case_closure_to`.
        if self.current_flags().has_any_unicode_flag() || !has_case_closure_override(code_point) {
            ICU.case_mapper.add_case_closure_to(code_point, set_builder);
        } else {
            let case_closure_override = get_case_closure_override(code_point).unwrap();
            set_builder.add_set(case_closure_override);
        }
    }

    fn emit_code_point_set(&mut self, set: &CodePointInversionList, is_inverted: bool) {
        // Can emit a literal instruction if we are matching a single code point
        if set.size() == 1 && !is_inverted {
            let single_code_point = set.iter_chars().next().unwrap() as CodePoint;
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
                // Convert from inclusive end to exclusive end
                self.emit_compare_between_instruction(start, end + 1);
            }
        }
    }

    fn emit_class_string_disjunction(
        &mut self,
        strings: &HashSet<&Wtf8String>,
        success_block: BlockId,
    ) {
        let mut strings = strings.iter().collect::<Vec<_>>();

        // Order strings by length, checking the longest first. Break ties consistently by comparing
        // the strings as bytes.
        strings.sort_by(|a, b| {
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
            self.emit_literal(string);
            self.emit_jump_instruction(success_block);
        }

        // Disjunction ends at start of join block
        self.set_current_block(join_block_id);
    }

    fn emit_lookaround(&mut self, lookaround: &Lookaround) -> SubExpressionInfo {
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
        let mut info = self.emit_disjunction(&lookaround.disjunction);
        self.emit_accept_instruction();

        self.exit_direction_context();

        self.set_current_block(current_block_id);

        // Lookaround never consumes any characters
        info.always_consumes = false;

        info
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
        for block in &self.blocks {
            id_map.push(instructions.len() as u32);
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
}

/// Set of word characters to be used for word character classes and word boundary assertions when
/// in case sensitive or unicode unaware mode.
static WORD_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| create_word_set_builder().build());

/// Set of word characters to be used for word character classes and word boundary assertions when
/// in case insensitive, unicode aware mode.
static WORD_CASE_INSENSITIVE_UNICODE_SET: LazyLock<CodePointInversionList> = LazyLock::new(|| {
    let mut set_builder = create_word_set_builder();

    // Add extra code points to form the case insensitive closure of the word set
    set_builder.add_char('\u{017f}');
    set_builder.add_char('\u{212a}');

    set_builder.build()
});

/// Set of non-word characters to be used for non-word character classes when in case sensitive or
/// unicode unaware mode.
static NOT_WORD_SET: LazyLock<CodePointInversionList> = LazyLock::new(|| {
    let mut set_builder = create_word_set_builder();
    set_builder.complement();
    set_builder.build()
});

/// Set of non-word characters to be used for non-word character classes when in case insensitive,
/// unicode aware mode.
static NOT_WORD_CASE_INSENSITIVE_UNICODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let mut set_builder = create_word_set_builder();

        // Add extra code points to form the case insensitive closure of the word set
        set_builder.add_char('\u{017f}');
        set_builder.add_char('\u{212a}');

        set_builder.complement();
        set_builder.build()
    });

/// Set of whitespace characters to be used for whitespace character classes.
static WHITESPACE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| create_whitespace_set_builder().build());

/// Set of non-whitespace characters to be used for non-whitespace character classes.
static NOT_WHITESPACE_SET: LazyLock<CodePointInversionList> = LazyLock::new(|| {
    let mut set_builder = create_whitespace_set_builder();
    set_builder.complement();
    set_builder.build()
});

fn create_word_set_builder() -> CodePointInversionListBuilder {
    let mut set_builder = CodePointInversionListBuilder::new();

    set_builder.add_range('a'..='z');
    set_builder.add_range('A'..='Z');
    set_builder.add_range('0'..='9');
    set_builder.add_char('_');

    set_builder
}

fn create_whitespace_set_builder() -> CodePointInversionListBuilder {
    // All code points on the right hand side of WhiteSpace or LineTerminator productions in the
    // spec.
    let mut set_builder = CodePointInversionListBuilder::new();

    set_builder.add_range('\u{0009}'..='\u{000D}');
    set_builder.add_char('\u{0020}');
    set_builder.add_char('\u{00A0}');
    set_builder.add_char('\u{1680}');
    set_builder.add_range('\u{2000}'..='\u{200A}');
    set_builder.add_range('\u{2028}'..='\u{2029}');
    set_builder.add_char('\u{202F}');
    set_builder.add_char('\u{205F}');
    set_builder.add_char('\u{3000}');
    set_builder.add_char('\u{FEFF}');

    set_builder
}

pub fn compile_regexp(
    cx: Context,
    regexp: &RegExp,
    source: Handle<StringValue>,
) -> Handle<CompiledRegExpObject> {
    let mut builder = CompiledRegExpBuilder::new(regexp, source);
    let compiled_regexp = builder.compile(cx, regexp);

    if cx.options.print_regexp_bytecode {
        let bytecode_string = compiled_regexp.debug_print(DebugPrintMode::Verbose);
        cx.print_or_add_to_dump_buffer(&bytecode_string);
    }

    compiled_regexp
}
