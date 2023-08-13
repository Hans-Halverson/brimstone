use crate::js::{
    common::wtf_8::Wtf8String,
    parser::regexp::{
        Alternative, AnonymousGroup, Assertion, CaptureGroup, CharacterClass, ClassRange,
        Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags, Term,
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
}

impl CompiledRegExpBuilder {
    fn new(regexp: &RegExp) -> Self {
        Self {
            blocks: vec![],
            flags: regexp.flags,
            current_block_id: 0,
            num_progress_points: 0,
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

    fn compile(&mut self, cx: &mut Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
        // Prime with new block
        self.new_block();

        // Emit preamble allowing match to start at any point in the string
        self.emit_preamble();

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
        self.emit_wildcard();
        self.emit_branch_instruction(join_block, wildcard_block);

        // Resume at the join block where pattern will start being emitted
        self.set_current_block(join_block);
    }

    /// Emit a disjunction, returning whether a character was guaranteed to be consumed on all paths
    fn emit_disjunction(&mut self, disjunction: &Disjunction) -> bool {
        if disjunction.alternatives.len() == 1 {
            self.emit_alternative(&disjunction.alternatives[0])
        } else {
            // Set up blocks for the branch instructions between alternatives
            let mut branch_block_ids = vec![];
            for _ in 0..disjunction.alternatives.len() - 1 {
                branch_block_ids.push(self.new_block())
            }

            // Block that all alternatives join to at the end
            let join_block_id = self.new_block();

            // Disjunction always consumes if all alternatives always consume
            let mut always_consumes = true;

            for i in 0..disjunction.alternatives.len() - 2 {
                let alternative_block_id = self.new_block();

                // Branch between this alternative and the next branch block
                self.set_current_block(branch_block_ids[i]);
                self.emit_branch_instruction(alternative_block_id, branch_block_ids[i + 1]);

                // Emit the alternative
                self.set_current_block(alternative_block_id);
                always_consumes &= self.emit_alternative(&disjunction.alternatives[i]);
                self.emit_jump_instruction(join_block_id);
            }

            // Last branch block branches between the last two alternatives
            let alternative_block_id_1 = self.new_block();
            let alternative_block_id_2 = self.new_block();

            self.set_current_block(branch_block_ids[branch_block_ids.len() - 1]);
            self.emit_branch_instruction(alternative_block_id_1, alternative_block_id_2);

            // Emit the last two alternatives
            self.set_current_block(alternative_block_id_1);
            always_consumes &= self
                .emit_alternative(&disjunction.alternatives[disjunction.alternatives.len() - 2]);
            self.emit_jump_instruction(join_block_id);

            self.set_current_block(alternative_block_id_2);
            always_consumes &= self
                .emit_alternative(&disjunction.alternatives[disjunction.alternatives.len() - 1]);
            self.emit_jump_instruction(join_block_id);

            // Disjunction ends at start of join block
            self.set_current_block(join_block_id);

            always_consumes
        }
    }

    /// Emit an alternative, returning whether a character was guaranteed to be consumed on all paths
    fn emit_alternative(&mut self, alternative: &Alternative) -> bool {
        let mut always_consumes = false;

        // If any term always consumes then the alternative always consumes
        for term in &alternative.terms {
            always_consumes |= self.emit_term(term);
        }

        always_consumes
    }

    /// Emit a term, returning whether a character was guaranteed to be consumed on all paths
    fn emit_term(&mut self, term: &Term) -> bool {
        match term {
            Term::Literal(string) => {
                self.emit_literal(string);

                // Literals are non-empty so they always consume a character
                true
            }
            Term::Wildcard => {
                self.emit_wildcard();

                // The wildcard always consumes a character
                true
            }
            Term::Quantifier(quantifier) => self.emit_quantifier(quantifier),
            Term::Assertion(assertion) => {
                self.emit_assertion(assertion);

                // Assertions never consume a character
                false
            }
            Term::CaptureGroup(group) => self.emit_capture_group(group),
            Term::AnonymousGroup(group) => self.emit_anonymous_group(group),
            Term::CharacterClass(character_class) => {
                self.emit_character_class(character_class);

                // Character classes always consume a character
                true
            }
            Term::Lookaround(lookaround) => {
                self.emit_lookaround(lookaround);

                // Lookaround never consumes any characters
                false
            }
            Term::Backreference(backreference) => {
                self.emit_instruction(Instruction::Backreference(backreference.index));

                // Backreferences may be empty depending on what was captured
                false
            }
        }
    }

    fn emit_literal(&mut self, string: &Wtf8String) {
        for code_point in string.iter_code_points() {
            self.emit_instruction(Instruction::Literal(code_point))
        }
    }

    fn emit_wildcard(&mut self) {
        self.emit_instruction(Instruction::Wildcard)
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

    /// Emit a quantifier, returning whether a character was guaranteed to be consumed on all paths
    fn emit_quantifier(&mut self, quantifier: &Quantifier) -> bool {
        // Quantifier always consumes if there is a required repitition that always consumes
        let mut always_consumes = false;

        // Emit term min times for repititions that must be present
        for _ in 0..quantifier.min {
            always_consumes |= self.emit_term(&quantifier.term);
        }

        if let Some(max) = quantifier.max {
            // Exact number of repititions
            if quantifier.min == max {
                return always_consumes;
            }

            let join_block_id = self.new_block();

            // Emit term blocks max - min times, each is optional and is preceded by a branch to
            // the join block.
            for _ in quantifier.min..max {
                let term_block_id = self.new_block();

                self.emit_quantifier_branch(quantifier, term_block_id, join_block_id);

                self.set_current_block(term_block_id);
                self.emit_term(&quantifier.term);
            }

            // Last term block always proceeds to the join block
            self.emit_jump_instruction(join_block_id);

            // Quantifier ends at start of join block
            self.set_current_block(join_block_id);
        } else {
            // Any number of future repititions
            let term_block_id = self.new_block();
            let join_block_id = self.new_block();

            // Optionally enter term block in the first place
            self.emit_quantifier_branch(quantifier, term_block_id, join_block_id);

            // Term block loops back to itself to allow any number of repititions
            self.set_current_block(term_block_id);
            let term_always_consumes = self.emit_term(&quantifier.term);

            // If the term block always consumes then we can directly loop back to the term block
            if term_always_consumes {
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

        always_consumes
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

    fn emit_capture_group(&mut self, group: &CaptureGroup) -> bool {
        // Calculate capture point indices from capture group
        let capture_start_index = group.index * 2;
        let capture_end_index = capture_start_index + 1;

        self.emit_instruction(Instruction::MarkCapturePoint(capture_start_index));
        let always_consumes = self.emit_disjunction(&group.disjunction);
        self.emit_instruction(Instruction::MarkCapturePoint(capture_end_index));

        always_consumes
    }

    fn emit_anonymous_group(&mut self, group: &AnonymousGroup) -> bool {
        self.emit_disjunction(&group.disjunction)
    }

    fn emit_character_class(&mut self, character_class: &CharacterClass) {
        let mut all_char_ranges = vec![];
        for class_range in &character_class.ranges {
            match class_range {
                // Accumalate single and range char ranges
                ClassRange::Single(code_point) => {
                    all_char_ranges.push((*code_point, *code_point + 1));
                }
                ClassRange::Range(start, end) => {
                    all_char_ranges.push((*start, *end));
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
        self.emit_instruction(Instruction::Lookaround(lookaround.is_positive));

        if !lookaround.is_ahead {
            unimplemented!("RegExp lookbehind");
        }

        // Emit the body of the lookaround instruction, ending with an accept
        self.emit_disjunction(&lookaround.disjunction);
        self.emit_instruction(Instruction::Accept);
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
                Instruction::Jump(block_id) => {
                    *block_id = id_map[(*block_id) as usize] as u32;
                }
                _ => {}
            }
        }

        instructions
    }
}

pub fn compile_regexp(cx: &mut Context, regexp: &RegExp) -> Handle<CompiledRegExpObject> {
    let mut builder = CompiledRegExpBuilder::new(regexp);
    builder.compile(cx, regexp)
}
