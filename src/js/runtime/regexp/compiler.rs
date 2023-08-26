use crate::js::{
    common::wtf_8::Wtf8String,
    parser::regexp::{
        Alternative, AnonymousGroup, Assertion, CaptureGroup, CaptureGroupIndex, CharacterClass,
        ClassRange, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags, Term,
    },
    runtime::{Context, Handle},
};

use super::compiled_regexp::{CompiledRegExpObject, Instruction};

type BlockId = usize;

struct CompiledRegExpBuilder {
    blocks: Vec<Vec<Instruction>>,
    flags: RegExpFlags,
    current_block_id: BlockId,
    num_progress_points: u32,
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

impl CompiledRegExpBuilder {
    fn new(regexp: &RegExp) -> Self {
        Self {
            blocks: vec![],
            flags: regexp.flags,
            current_block_id: 0,
            num_progress_points: 0,
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

    fn emit_instruction(&mut self, instruction: Instruction) {
        self.blocks[self.current_block_id].push(instruction)
    }

    fn emit_jump_instruction(&mut self, block_id: BlockId) {
        self.emit_instruction(Instruction::Jump(block_id as u32))
    }

    fn emit_branch_instruction(&mut self, first_block_id: BlockId, second_block_id: BlockId) {
        self.emit_instruction(Instruction::Branch(first_block_id as u32, second_block_id as u32))
    }

    fn emit_progress_instruction(&mut self) {
        let index = self.num_progress_points;
        self.num_progress_points += 1;

        self.emit_instruction(Instruction::Progress(index));
    }

    fn compile(&mut self, cx: Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
        // Prime with new block
        self.new_block();

        // Emit preamble allowing match to start at any point in the string
        if !self.flags.contains(RegExpFlags::STICKY) {
            self.emit_preamble();
        }

        // Wrap the entire pattern in the 0'th capture group
        self.emit_instruction(Instruction::MarkCapturePoint(0));
        self.emit_disjunction(&regexp.disjunction);
        self.emit_instruction(Instruction::MarkCapturePoint(1));

        self.emit_instruction(Instruction::Accept);

        let instructions = self.flatten_and_fix_indices();

        CompiledRegExpObject::new(cx, instructions, regexp, self.num_progress_points)
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
        self.emit_instruction(Instruction::Wildcard);
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
                            self.emit_instruction(Instruction::ClearCapture(*capture_index));
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
                    self.emit_instruction(Instruction::ClearCapture(*capture_index));
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
                        self.emit_instruction(Instruction::ClearCapture(*capture_index));
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
            Term::Lookaround(lookaround) => {
                self.emit_lookaround(lookaround);

                // Lookaround never consumes any characters
                SubExpressionInfo::no_captures(false)
            }
            Term::Backreference(backreference) => {
                self.emit_instruction(Instruction::Backreference(backreference.index));

                // Backreferences may be empty depending on what was captured
                SubExpressionInfo::no_captures(false)
            }
        }
    }

    fn emit_literal(&mut self, string: &Wtf8String) {
        if self.is_forwards() {
            for code_point in string.iter_code_points() {
                self.emit_instruction(Instruction::Literal(code_point))
            }
        } else {
            // When emitting backwards, emit concatenation of literals in reverse order
            let code_points = string.iter_code_points().collect::<Vec<_>>();
            for code_point in code_points.iter().rev() {
                self.emit_instruction(Instruction::Literal(*code_point))
            }
        }
    }

    fn emit_wildcard(&mut self) {
        if self.flags.contains(RegExpFlags::DOT_ALL) {
            self.emit_instruction(Instruction::Wildcard)
        } else {
            self.emit_instruction(Instruction::WildcardNoNewline)
        }
    }

    fn emit_assertion(&mut self, assertion: &Assertion) {
        match assertion {
            Assertion::Start => {
                if self.flags.contains(RegExpFlags::MULTILINE) {
                    self.emit_instruction(Instruction::AssertStartOrNewline)
                } else {
                    self.emit_instruction(Instruction::AssertStart)
                }
            }
            Assertion::End => {
                if self.flags.contains(RegExpFlags::MULTILINE) {
                    self.emit_instruction(Instruction::AssertEndOrNewline)
                } else {
                    self.emit_instruction(Instruction::AssertEnd)
                }
            }
            Assertion::WordBoundary => self.emit_instruction(Instruction::AssertWordBoundary),
            Assertion::NotWordBoundary => self.emit_instruction(Instruction::AssertNotWordBoundary),
        }
    }

    fn in_block(&mut self, block_id: BlockId, f: impl FnOnce(&mut Self)) {
        let current_block_id = self.current_block_id;
        self.set_current_block(block_id);
        f(self);
        self.set_current_block(current_block_id);
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

        // Quantifier always has the same captures as its wrapped term. But quantifier only has the
        // same always consume status as its wrapped term when there are minimum repititions.
        // Otherwise the quantifier is never guaranteed to consume.
        let mut quantifier_info = SubExpressionInfo::no_captures(false);

        // Emit term min times for repititions that must be present
        for _ in 0..quantifier.min {
            quantifier_info = self.emit_term(&quantifier.term);
        }

        if let Some(max) = quantifier.max {
            // Exact number of repititions
            if quantifier.min == max {
                return quantifier_info;
            }

            let join_block_id = self.new_block();

            // Emit term blocks max - min times, each is optional and is preceded by a branch to
            // the join block.
            for i in quantifier.min..max {
                let pred_block_id = self.current_block_id;

                // Emit term block
                let term_block_id = self.new_block();
                self.set_current_block(term_block_id);
                let info = self.emit_term(&quantifier.term);

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

            // Term block loops back to itself to allow any number of repititions. If the term block
            // always consumes then we can directly loop back to the term block
            if info.always_consumes {
                self.emit_quantifier_branch(quantifier, term_block_id, join_block_id);
            } else {
                // When term block does not always consume the back edge first goes through a
                // progress block that ensures that progress has been made since the last entry to
                // the progress block. This prevents infinite epsilon loops.
                let progress_block_id = self.new_block();
                self.emit_quantifier_branch(quantifier, progress_block_id, join_block_id);

                self.set_current_block(progress_block_id);
                self.emit_progress_instruction();
                self.emit_jump_instruction(term_block_id);
            }

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        }

        if is_repitition {
            self.exit_repitition_context();
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

    /// Emit a quantifier bridge that clears all the provided captures along the edge to the
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
            self.emit_instruction(Instruction::ClearCapture(*capture_index));
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

        self.emit_instruction(Instruction::MarkCapturePoint(capture_start_index));
        let mut info = self.emit_disjunction(&group.disjunction);
        self.emit_instruction(Instruction::MarkCapturePoint(capture_end_index));

        // Add this capture group to the set of captures
        info.captures.push(group.index);

        info
    }

    fn emit_anonymous_group(&mut self, group: &AnonymousGroup) -> SubExpressionInfo {
        self.emit_disjunction(&group.disjunction)
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) {
        let mut all_char_ranges = vec![];
        for class_range in &character_class.ranges {
            match class_range {
                // Accumulate single and range char ranges
                ClassRange::Single(code_point) => {
                    all_char_ranges.push((*code_point, *code_point + 1));
                }
                ClassRange::Range(start, end) => {
                    all_char_ranges.push((*start, *end + 1));
                }
                // Shorthand char ranges have own instructions
                ClassRange::Digit => self.emit_instruction(Instruction::CompareIsDigit),
                ClassRange::NotDigit => self.emit_instruction(Instruction::CompareIsNotDigit),
                ClassRange::Word => self.emit_instruction(Instruction::CompareIsWord),
                ClassRange::NotWord => self.emit_instruction(Instruction::CompareIsNotWord),
                ClassRange::Whitespace => self.emit_instruction(Instruction::CompareIsWhitespace),
                ClassRange::NotWhitespace => {
                    self.emit_instruction(Instruction::CompareIsNotWhitespace)
                }
                ClassRange::UnicodeProperty(property) => {
                    self.emit_instruction(Instruction::CompareIsUnicodeProperty(*property))
                }
                ClassRange::NotUnicodeProperty(property) => {
                    self.emit_instruction(Instruction::CompareIsNotUnicodeProperty(*property))
                }
            }
        }

        // Order char ranges by starting index, then ending index
        all_char_ranges.sort_by(|(start_1, end_1), (start_2, end_2)| {
            start_1.cmp(start_2).then_with(|| end_1.cmp(end_2))
        });

        // Merge adjacent char ranges
        if !all_char_ranges.is_empty() {
            let mut merged_char_ranges = vec![];
            let mut current_char_range = all_char_ranges[0];

            for (start, end) in &all_char_ranges[1..] {
                // If next range overlaps current range then merge them
                if *start <= current_char_range.1 {
                    current_char_range.1 = u32::max(current_char_range.1, *end);
                } else {
                    // Otherwise next range doesn't overlap so emit disjoint ranges
                    merged_char_ranges.push(current_char_range);
                    current_char_range = (*start, *end);
                }
            }

            // Emit the last range
            merged_char_ranges.push(current_char_range);

            all_char_ranges = merged_char_ranges
        }

        // Emit merged char ranges
        for range in all_char_ranges {
            if range.0 + 1 == range.1 {
                self.emit_instruction(Instruction::CompareEquals(range.0));
            } else {
                self.emit_instruction(Instruction::CompareBetween(range.0, range.1));
            }
        }

        // Emit the final consume instruction, noting whether to invert
        if character_class.is_inverted {
            self.emit_instruction(Instruction::ConsumeIfFalse);
        } else {
            self.emit_instruction(Instruction::ConsumeIfTrue);
        }
    }

    fn emit_lookaround(&mut self, lookaround: &Lookaround) {
        let body_block_id = self.new_block();
        self.emit_instruction(Instruction::Lookaround(
            lookaround.is_ahead,
            lookaround.is_positive,
            body_block_id as u32,
        ));

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

        self.emit_disjunction(&lookaround.disjunction);
        self.emit_instruction(Instruction::Accept);

        self.exit_direction_context();

        self.set_current_block(current_block_id);
    }

    /// Convert the list of blocks to a flat list of instructions. Branch and jump instructions
    /// originally use block ids as their operands - replace these with instruction indices into
    /// the flat array.
    fn flatten_and_fix_indices(&mut self) -> Vec<Instruction> {
        let num_instructions = self.blocks.iter().map(|block| block.len()).sum();
        let mut instructions = Vec::with_capacity(num_instructions);

        // Map from block ids to instruction indices in the flattened array
        let mut id_map = Vec::with_capacity(self.blocks.len());

        // Flatten blocks into instruction array
        for block in &self.blocks {
            id_map.push(instructions.len());
            instructions.extend(block.iter());
        }

        // Fix up indices
        for instruction in &mut instructions {
            match instruction {
                Instruction::Branch(block_id_1, block_id_2) => {
                    *block_id_1 = id_map[(*block_id_1) as usize] as u32;
                    *block_id_2 = id_map[(*block_id_2) as usize] as u32;
                }
                Instruction::Jump(block_id) | Instruction::Lookaround(_, _, block_id) => {
                    *block_id = id_map[(*block_id) as usize] as u32;
                }
                _ => {}
            }
        }

        instructions
    }
}

pub fn compile_regexp(cx: Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
    let mut builder = CompiledRegExpBuilder::new(regexp);
    builder.compile(cx, regexp)
}
