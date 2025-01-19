use icu_collections::codepointinvlist::CodePointInversionListBuilder;

use crate::js::{
    common::{unicode::CodePoint, unicode_property::UnicodeProperty, wtf_8::Wtf8String},
    parser::regexp::{
        Alternative, AnonymousGroup, Assertion, CaptureGroup, CaptureGroupIndex, CharacterClass,
        ClassRange, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags, Term,
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
        CompareIsDigitInstruction, CompareIsNotDigitInstruction,
        CompareIsNotUnicodePropertyInstruction, CompareIsNotWhitespaceInstruction,
        CompareIsNotWordInstruction, CompareIsUnicodePropertyInstruction,
        CompareIsWhitespaceInstruction, CompareIsWordInstruction, ConsumeIfFalseInstruction,
        ConsumeIfTrueInstruction, FailInstruction, InstructionIteratorMut, JumpInstruction,
        LiteralInstruction, LookaroundInstruction, LoopInstruction, MarkCapturePointInstruction,
        OpCode, ProgressInstruction, WildcardInstruction, WildcardNoNewlineInstruction,
        WordBoundaryMoveToPreviousInstruction,
    },
    matcher::canonicalize,
};

type BlockId = usize;

struct CompiledRegExpBuilder {
    blocks: Vec<Vec<u32>>,
    flags: RegExpFlags,
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
            flags: regexp.flags,
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

    fn emit_backreference_instruction(&mut self, capture_group_index: u32) {
        BackreferenceInstruction::write(self.current_block_buf(), capture_group_index)
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

    fn emit_compare_is_digit_instruction(&mut self) {
        CompareIsDigitInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_not_digit_instruction(&mut self) {
        CompareIsNotDigitInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_word_instruction(&mut self) {
        CompareIsWordInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_not_word_instruction(&mut self) {
        CompareIsNotWordInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_whitespace_instruction(&mut self) {
        CompareIsWhitespaceInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_not_whitespace_instruction(&mut self) {
        CompareIsNotWhitespaceInstruction::write(self.current_block_buf())
    }

    fn emit_compare_is_unicode_property_instruction(&mut self, unicode_property: UnicodeProperty) {
        CompareIsUnicodePropertyInstruction::write(self.current_block_buf(), unicode_property)
    }

    fn emit_compare_is_not_unicode_property_instruction(
        &mut self,
        unicode_property: UnicodeProperty,
    ) {
        CompareIsNotUnicodePropertyInstruction::write(self.current_block_buf(), unicode_property)
    }

    fn emit_lookaround_instruction(&mut self, is_ahead: bool, is_positive: bool, body_branch: u32) {
        LookaroundInstruction::write(self.current_block_buf(), is_ahead, is_positive, body_branch)
    }

    fn next_loop_register(&mut self) -> u32 {
        let next_register = self.num_loop_registers;
        self.num_loop_registers += 1;
        next_register
    }

    fn canonicalize(&mut self, code_point: CodePoint) -> CodePoint {
        canonicalize(code_point, self.flags.has_any_unicode_flag())
    }

    fn compile(&mut self, cx: Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
        // Prime with new block
        self.new_block();

        // Emit preamble allowing match to start at any point in the string
        if !self.flags.is_sticky() {
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
                self.emit_backreference_instruction(backreference.index);

                // Backreferences may be empty depending on what was captured
                SubExpressionInfo::no_captures(false)
            }
        }
    }

    fn emit_code_point_literal(&mut self, code_point: CodePoint) {
        let code_point = if self.flags.is_case_insensitive() {
            self.canonicalize(code_point)
        } else {
            code_point
        };

        self.emit_literal_instruction(code_point)
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
        if self.flags.is_dot_all() {
            self.emit_wildcard_instruction()
        } else {
            self.emit_wildcard_no_newline_instruction()
        }
    }

    fn emit_assertion(&mut self, assertion: &Assertion) {
        match assertion {
            Assertion::Start => {
                if self.flags.is_multiline() {
                    self.emit_assert_start_or_newline_instruction()
                } else {
                    self.emit_assert_start_instruction()
                }
            }
            Assertion::End => {
                if self.flags.is_multiline() {
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
        self.emit_compare_is_word_instruction();
        self.emit_word_boundary_move_to_previous_instruction();
        self.emit_compare_is_word_instruction();
        self.emit_assert_word_boundary_instruction()
    }

    fn emit_assert_not_word_boundary(&mut self) {
        self.emit_compare_is_word_instruction();
        self.emit_word_boundary_move_to_previous_instruction();
        self.emit_compare_is_word_instruction();
        self.emit_assert_not_word_boundary_instruction()
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
        self.emit_disjunction(&group.disjunction)
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) {
        let is_case_insensitive = self.flags.is_case_insensitive();
        let mut set_builder = CodePointInversionListBuilder::new();

        for class_range in &character_class.ranges {
            match class_range {
                // Accumulate single and range char ranges
                ClassRange::Single(code_point) => {
                    if is_case_insensitive {
                        set_builder.add32(self.canonicalize(*code_point));
                    } else {
                        set_builder.add32(*code_point)
                    }
                }
                ClassRange::Range(start, end) => {
                    if is_case_insensitive {
                        // Canonicalize each element of range and add to set
                        for code_point in *start..=*end {
                            set_builder.add32(self.canonicalize(code_point));
                        }
                    } else {
                        // Otherwise can add the range directly
                        set_builder.add_range32(*start..=*end);
                    }
                }
                // Shorthand char ranges have own instructions
                ClassRange::Digit => self.emit_compare_is_digit_instruction(),
                ClassRange::NotDigit => self.emit_compare_is_not_digit_instruction(),
                ClassRange::Word => self.emit_compare_is_word_instruction(),
                ClassRange::NotWord => self.emit_compare_is_not_word_instruction(),
                ClassRange::Whitespace => self.emit_compare_is_whitespace_instruction(),
                ClassRange::NotWhitespace => self.emit_compare_is_not_whitespace_instruction(),
                ClassRange::UnicodeProperty(property) => {
                    self.emit_compare_is_unicode_property_instruction(*property)
                }
                ClassRange::NotUnicodeProperty(property) => {
                    self.emit_compare_is_not_unicode_property_instruction(*property)
                }
            }
        }

        let set = set_builder.build();

        // Emit char ranges in set. Iterates over inclusive ranges.
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

        // Emit the final consume instruction, noting whether to invert
        if character_class.is_inverted {
            self.emit_consume_if_false_instruction();
        } else {
            self.emit_consume_if_true_instruction();
        }
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
