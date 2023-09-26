use crate::js::{
    common::{
        icu::ICU,
        unicode::{
            is_ascii_alphabetic, is_decimal_digit, is_newline, is_whitespace,
            try_encode_surrogate_pair, CodePoint,
        },
    },
    parser::lexer_stream::{
        HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream, HeapTwoByteCodeUnitLexerStream,
        LexerStream, SavedLexerStreamState,
    },
    runtime::{
        string_value::{FlatString, StringValue, StringWidth},
        Handle, HeapPtr,
    },
};

use super::compiled_regexp::{CompiledRegExpObject, Instruction};

pub struct MatchEngine<T: LexerStream> {
    // Lexer over the target string with a current position
    string_lexer: T,
    // The regexp that is being matched against
    regexp: HeapPtr<CompiledRegExpObject>,
    // Index of the next instruction to execute
    instruction_index: usize,
    // Saved restore points for backtracking
    backtrack_stack: Vec<BacktrackEntry>,
    // String index for each capture point
    capture_points: Vec<usize>,
    // The most recent string index marked at each progress instruction
    progress_points: Vec<usize>,
    // The next loop iteration for each loop
    loop_registers: Vec<usize>,
    // An accumulator register for building multi-part comparisons
    compare_register: bool,
    // Backtrack stack base index for the current sub-execution. For the top-level execution this is
    // always 0, for sub-executions this is the size of the backtrack stack at the sub-execution start.
    backtrack_stack_base: usize,
}

enum BacktrackEntry {
    /// Backtrack to an (instruction, string index) state
    RestoreState(BacktrackRestoreState),
    /// Restore a capture point
    CapturePoint(CapturePoint),
    /// Restore a progress point (progress point index, string index to restore)
    ProgressPoint(u32, usize),
    /// Restore a loop register to a given value (loop register index, value)
    LoopRegister(u32, usize),
    /// Restore the backtrack stack to a given size, backtracking through all entries
    /// above that size.
    RestoreBacktrackStack(usize),
}

struct BacktrackRestoreState {
    // Index of the next instruction to execute when this restore point was created
    instruction_index: usize,
    // Current target string state when this restore point was created
    saved_string_state: SavedLexerStreamState,
}

const EMPTY_STRING_INDEX: usize = usize::MAX;

struct CapturePoint {
    // Capture point index marking the beginning or end of a capture group
    capture_point_index: u32,
    // Target string index that was marked
    string_index: usize,
}

#[derive(Debug)]
pub struct Match {
    /// Includes the implicit 0'th capture group for the entire match
    pub capture_groups: Vec<Option<Capture>>,
}

/// Bounds of a matched capture group. The start index is inclusive, the end index is exclusive.
#[derive(Debug)]
pub struct Capture {
    pub start: usize,
    pub end: usize,
}

const FORWARD: bool = true;
const BACKWARD: bool = false;

impl<T: LexerStream> MatchEngine<T> {
    fn new(regexp: HeapPtr<CompiledRegExpObject>, string_lexer: T) -> Self {
        let num_capture_points = (regexp.num_capture_groups as usize + 1) * 2;

        Self {
            regexp,
            string_lexer,
            instruction_index: 0,
            backtrack_stack: Vec::new(),
            capture_points: vec![EMPTY_STRING_INDEX; num_capture_points],
            progress_points: vec![EMPTY_STRING_INDEX; regexp.num_progress_points as usize],
            loop_registers: vec![0; regexp.num_loop_registers as usize],
            compare_register: false,
            backtrack_stack_base: 0,
        }
    }

    fn push_backtrack_restore_state(&mut self, instruction_index: usize) {
        let saved_string_state = self.string_lexer.save();
        let restore_state = BacktrackRestoreState { instruction_index, saved_string_state };

        self.backtrack_stack
            .push(BacktrackEntry::RestoreState(restore_state));
    }

    fn backtrack(&mut self) -> Result<(), ()> {
        while self.backtrack_stack.len() > self.backtrack_stack_base {
            let backtrack_entry = self.backtrack_stack.pop().unwrap();
            match backtrack_entry {
                BacktrackEntry::RestoreState(restore_state) => {
                    self.instruction_index = restore_state.instruction_index;
                    self.string_lexer.restore(&restore_state.saved_string_state);

                    return Ok(());
                }
                BacktrackEntry::CapturePoint(capture_point) => {
                    self.set_capture_point(
                        capture_point.capture_point_index,
                        capture_point.string_index,
                    );

                    // Continue backtracking
                }
                BacktrackEntry::ProgressPoint(progress_point_index, string_index) => {
                    self.set_progress_point(progress_point_index, string_index);

                    // Continue backtracking
                }
                BacktrackEntry::LoopRegister(loop_register_index, value) => {
                    self.set_loop_register(loop_register_index, value);

                    // Continue backtracking
                }
                BacktrackEntry::RestoreBacktrackStack(backtrack_stack_size) => {
                    self.backtrack_to_stack_size(backtrack_stack_size);

                    // Continue backtracking
                }
            }
        }

        Err(())
    }

    /// Restore the backtrack stack to a particular size, restoring all registers that were set
    /// along the way.
    fn backtrack_to_stack_size(&mut self, backtrack_stack_size: usize) {
        while self.backtrack_stack.len() > backtrack_stack_size {
            let backtrack_entry = self.backtrack_stack.pop().unwrap();
            match backtrack_entry {
                BacktrackEntry::CapturePoint(capture_point) => {
                    self.set_capture_point(
                        capture_point.capture_point_index,
                        capture_point.string_index,
                    );
                }
                BacktrackEntry::ProgressPoint(progress_point_index, string_index) => {
                    self.set_progress_point(progress_point_index, string_index);
                }
                BacktrackEntry::LoopRegister(loop_register_index, value) => {
                    self.set_loop_register(loop_register_index, value);
                }
                BacktrackEntry::RestoreState(_) | BacktrackEntry::RestoreBacktrackStack(_) => {}
            }
        }
    }

    #[inline]
    fn current_instruction(&self) -> Instruction {
        self.regexp.instructions()[self.instruction_index]
    }

    #[inline]
    fn advance_instruction(&mut self) {
        self.instruction_index += 1;
    }

    #[inline]
    fn set_next_instruction(&mut self, next_instruction_index: u32) {
        self.instruction_index = next_instruction_index as usize;
    }

    #[inline]
    fn get_capture_point(&self, capture_point_index: u32) -> usize {
        self.capture_points[capture_point_index as usize]
    }

    #[inline]
    fn set_capture_point(&mut self, capture_point_index: u32, string_index: usize) {
        self.capture_points[capture_point_index as usize] = string_index;
    }

    #[inline]
    fn get_progress_point(&self, progress_point_index: u32) -> usize {
        self.progress_points[progress_point_index as usize]
    }

    #[inline]
    fn set_progress_point(&mut self, progress_point_index: u32, string_index: usize) {
        self.progress_points[progress_point_index as usize] = string_index;
    }

    #[inline]
    fn get_loop_register(&self, loop_register_index: u32) -> usize {
        self.loop_registers[loop_register_index as usize]
    }

    #[inline]
    fn set_loop_register(&mut self, loop_register_index: u32, value: usize) {
        self.loop_registers[loop_register_index as usize] = value;
    }

    fn run(&mut self) -> Option<Match> {
        match self.execute_bytecode::<FORWARD>() {
            Err(_) => None,
            Ok(_) => Some(self.build_match()),
        }
    }

    fn execute_bytecode<const DIRECTION: bool>(&mut self) -> Result<(), ()> {
        loop {
            match self.current_instruction() {
                Instruction::Accept => return Ok(()),
                Instruction::Literal(code_point) => {
                    if self.string_lexer.current() != code_point {
                        self.backtrack()?;
                    } else {
                        self.advance_code_point_in_direction::<DIRECTION>();
                        self.advance_instruction();
                    }
                }
                Instruction::Wildcard => {
                    if !self.string_lexer.has_current() {
                        self.backtrack()?;
                    } else {
                        self.advance_code_point_in_direction::<DIRECTION>();
                        self.advance_instruction();
                    }
                }
                Instruction::WildcardNoNewline => {
                    if !self.string_lexer.has_current() || is_newline(self.string_lexer.current()) {
                        self.backtrack()?;
                    } else {
                        self.advance_code_point_in_direction::<DIRECTION>();
                        self.advance_instruction();
                    }
                }
                Instruction::Jump(next_instruction_index) => {
                    self.set_next_instruction(next_instruction_index);
                }
                Instruction::Branch(first_instruction_index, second_instruction_index) => {
                    self.push_backtrack_restore_state(second_instruction_index as usize);
                    self.set_next_instruction(first_instruction_index);
                }
                Instruction::MarkCapturePoint(capture_point_index) => {
                    let string_index = self.string_lexer.pos();
                    self.push_capture_point(capture_point_index, string_index);
                    self.advance_instruction();
                }
                Instruction::ClearCapture(capture_group_index) => {
                    // Clearing just the ending capture point for the group is enough
                    let capture_point_index = capture_group_index * 2 + 1;
                    self.push_capture_point(capture_point_index, EMPTY_STRING_INDEX);
                    self.advance_instruction();
                }
                Instruction::Progress(progress_index) => {
                    let string_index = self.string_lexer.pos();
                    if self.get_progress_point(progress_index) != string_index {
                        self.push_progress_point(progress_index, string_index);
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::Loop { loop_register_index, loop_max_value, end_branch } => {
                    let loop_register_value = self.get_loop_register(loop_register_index);
                    if loop_register_value < loop_max_value as usize {
                        self.push_loop_register(loop_register_index, loop_register_value + 1);
                        self.advance_instruction();
                    } else {
                        self.push_loop_register(loop_register_index, 0);
                        self.set_next_instruction(end_branch);
                    }
                }
                Instruction::AssertStart => {
                    if self.string_lexer.is_start() {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertEnd => {
                    if self.string_lexer.is_end() {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertStartOrNewline => {
                    if self.string_lexer.is_start()
                        || is_newline(self.code_point_before_current_pos::<DIRECTION>())
                    {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertEndOrNewline => {
                    if self.string_lexer.is_end()
                        || is_newline(self.code_point_after_current_pos::<DIRECTION>())
                    {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertWordBoundary => {
                    if self.is_at_word_boundary::<DIRECTION>() {
                        self.advance_instruction()
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertNotWordBoundary => {
                    if self.is_at_word_boundary::<DIRECTION>() {
                        self.backtrack()?;
                    } else {
                        self.advance_instruction()
                    }
                }
                Instruction::Backreference(capture_group_index) => {
                    self.execute_backreference::<DIRECTION>(capture_group_index)?;
                }
                Instruction::ConsumeIfTrue => {
                    if !self.compare_register || !self.string_lexer.has_current() {
                        self.backtrack()?;
                    } else {
                        self.advance_code_point_in_direction::<DIRECTION>();
                        self.advance_instruction();
                    }

                    self.compare_register = false;
                }
                Instruction::ConsumeIfFalse => {
                    if self.compare_register || !self.string_lexer.has_current() {
                        self.backtrack()?;
                    } else {
                        self.advance_code_point_in_direction::<DIRECTION>();
                        self.advance_instruction();
                    }

                    self.compare_register = false;
                }
                Instruction::CompareEquals(code_point) => {
                    if code_point == self.string_lexer.current() {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareBetween(lower, upper) => {
                    let current = self.string_lexer.current();
                    if current >= lower && current < upper {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsDigit => {
                    if is_decimal_digit(self.string_lexer.current()) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsNotDigit => {
                    if !is_decimal_digit(self.string_lexer.current()) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsWord => {
                    if is_word_code_point(self.string_lexer.current()) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsNotWord => {
                    if !is_word_code_point(self.string_lexer.current()) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsWhitespace => {
                    let current = self.string_lexer.current();
                    if is_whitespace(current) || is_newline(current) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsNotWhitespace => {
                    let current = self.string_lexer.current();
                    if !is_whitespace(current) && !is_newline(current) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsUnicodeProperty(property) => {
                    let current = self.string_lexer.current();
                    if property.is_match(current) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::CompareIsNotUnicodeProperty(property) => {
                    let current = self.string_lexer.current();
                    if !property.is_match(current) {
                        self.compare_register = true;
                    }

                    self.advance_instruction();
                }
                Instruction::Lookaround(is_ahead, is_positive, lookaround_instruction_index) => {
                    // Save lexer state for starting lookaround
                    let saved_string_state = self.string_lexer.save();

                    // Save the index of the instruction to be executed after the lookaround
                    self.advance_instruction();
                    let next_instruction_index = self.instruction_index;

                    // Save the base and size of the backtrack stack before the sub-execution starts
                    let old_backtrack_stack_size = self.backtrack_stack.len();
                    let old_backtrack_stack_base = self.backtrack_stack_base;

                    // Set up new sub-execution frame on backtrack stack
                    self.backtrack_stack_base = self.backtrack_stack.len();

                    // Execute the lookaround as a sub-execution within engine
                    self.set_next_instruction(lookaround_instruction_index);

                    let is_match = if is_ahead {
                        // Prime lexer for forwards traversal
                        self.string_lexer.advance_n(0);
                        self.execute_bytecode::<FORWARD>().is_ok()
                    } else {
                        // Prime lexer for backwards traversal
                        self.string_lexer.advance_backwards_n(0);
                        self.execute_bytecode::<BACKWARD>().is_ok()
                    };

                    // Successfully matched - we want to keep the captures for now, but must allow
                    // undoing the captures in the future when backtracking.
                    if is_match {
                        self.backtrack_stack
                            .push(BacktrackEntry::RestoreBacktrackStack(old_backtrack_stack_size))
                    } else {
                        // If did not match then backtrack stack must have been popped back to
                        // the old size.
                        debug_assert!(self.backtrack_stack.len() == old_backtrack_stack_size);
                    }

                    self.backtrack_stack_base = old_backtrack_stack_base;

                    // Check if lookaround succeeded and either restore or backtrack
                    if is_match == is_positive {
                        self.string_lexer.restore(&saved_string_state);
                        self.instruction_index = next_instruction_index;
                    } else {
                        self.backtrack()?;
                    }
                }
            }
        }
    }

    fn advance_code_point_in_direction<const DIRECTION: bool>(&mut self) {
        match DIRECTION {
            FORWARD => self.string_lexer.advance_code_point(),
            BACKWARD => self.string_lexer.advance_backwards_code_point(),
        }
    }

    fn peek_prev_code_point_in_direction<const DIRECTION: bool>(&self) -> CodePoint {
        match DIRECTION {
            FORWARD => self.string_lexer.peek_prev_code_point(),
            BACKWARD => self.string_lexer.peek_next_code_point(),
        }
    }

    fn code_point_before_current_pos<const DIRECTION: bool>(&self) -> CodePoint {
        match DIRECTION {
            // In forwards mode the current token is the code point after the pos, so we must peek
            // at the previous code point.
            FORWARD => self.string_lexer.peek_prev_code_point(),
            // In backwards mode the current token is the code point before the pos
            BACKWARD => self.string_lexer.current(),
        }
    }

    fn code_point_after_current_pos<const DIRECTION: bool>(&self) -> CodePoint {
        match DIRECTION {
            // In forwards mode the current token is the code point after the pos
            FORWARD => self.string_lexer.current(),
            // In backwards mode the current token is the code point before the pos, so we must
            // peek at the next code point.
            BACKWARD => self.string_lexer.peek_next_code_point(),
        }
    }

    fn is_at_word_boundary<const DIRECTION: bool>(&self) -> bool {
        let is_current_word = is_word_code_point(self.string_lexer.current());
        let is_prev_word =
            is_word_code_point(self.peek_prev_code_point_in_direction::<DIRECTION>());

        is_current_word != is_prev_word
    }

    fn push_capture_point(&mut self, capture_point_index: u32, string_index: usize) {
        // Save old capture point on backtrack stack
        let old_string_index = self.get_capture_point(capture_point_index);
        self.backtrack_stack
            .push(BacktrackEntry::CapturePoint(CapturePoint {
                capture_point_index,
                string_index: old_string_index,
            }));

        self.set_capture_point(capture_point_index, string_index);
    }

    fn push_progress_point(&mut self, progress_point_index: u32, string_index: usize) {
        // Save old progress point on backtrack stack
        let old_string_index = self.get_progress_point(progress_point_index);
        self.backtrack_stack
            .push(BacktrackEntry::ProgressPoint(progress_point_index, old_string_index));

        self.set_progress_point(progress_point_index, string_index);
    }

    fn push_loop_register(&mut self, loop_register_index: u32, value: usize) {
        // Save old loop register on backtrack stack
        let old_value = self.get_loop_register(loop_register_index);
        self.backtrack_stack
            .push(BacktrackEntry::LoopRegister(loop_register_index, old_value));

        self.set_loop_register(loop_register_index, value);
    }

    fn execute_backreference<const DIRECTION: bool>(
        &mut self,
        capture_group_index: u32,
    ) -> Result<(), ()> {
        match self.get_valid_capture_bounds(capture_group_index) {
            None => self.advance_instruction(),
            Some((start_index, end_index)) => {
                let captured_slice = self.string_lexer.slice(start_index, end_index);
                let captured_slice_len = end_index - start_index;

                match DIRECTION {
                    FORWARD => {
                        // Slice to check is directly after current string position
                        if self
                            .string_lexer
                            .slice_equals(self.string_lexer.pos(), captured_slice)
                        {
                            self.string_lexer.advance_n(captured_slice_len);
                            self.advance_instruction();
                        } else {
                            self.backtrack()?;
                        }
                    }
                    BACKWARD => {
                        // Slice to check is directly before current string position
                        let start_pos = self.string_lexer.pos().checked_sub(captured_slice_len);
                        if start_pos.is_some()
                            && self
                                .string_lexer
                                .slice_equals(start_pos.unwrap(), captured_slice)
                        {
                            self.string_lexer.advance_backwards_n(captured_slice_len);
                            self.advance_instruction();
                        } else {
                            self.backtrack()?;
                        }
                    }
                }
            }
        };

        Ok(())
    }

    /// Return the bounds of the current match for the given capture group index. Capture group
    /// is 1-indexed where 0 is the entire match.
    ///
    /// If either bound is empty then the capture group did not match and return None.
    fn get_valid_capture_bounds(&self, capture_group_index: u32) -> Option<(usize, usize)> {
        let start_string_index = self.get_capture_point(capture_group_index * 2);
        let end_string_index = self.get_capture_point(capture_group_index * 2 + 1);

        if start_string_index == EMPTY_STRING_INDEX || end_string_index == EMPTY_STRING_INDEX {
            return None;
        }

        Some((start_string_index, end_string_index))
    }

    fn build_match(&self) -> Match {
        let num_capture_point_pairs = self.regexp.num_capture_groups + 1;
        let mut capture_groups = Vec::with_capacity(num_capture_point_pairs as usize);

        for i in 0..num_capture_point_pairs {
            match self.get_valid_capture_bounds(i) {
                Some((start, end)) => capture_groups.push(Some(Capture { start, end })),
                None => capture_groups.push(None),
            }
        }

        Match { capture_groups }
    }
}

fn match_lexer_stream(
    mut lexer_stream: impl LexerStream,
    regexp: HeapPtr<CompiledRegExpObject>,
    start_index: usize,
) -> Option<Match> {
    lexer_stream.advance_n(start_index);
    let mut match_engine = MatchEngine::new(regexp, lexer_stream);
    match_engine.run()
}

pub fn run_matcher(
    regexp: HeapPtr<CompiledRegExpObject>,
    target_string: Handle<StringValue>,
    start_index: usize,
) -> Option<Match> {
    let flat_string = target_string.flatten();

    if regexp.flags.is_case_insensitive() {
        return run_case_insensitive_matcher(regexp, flat_string, start_index);
    }

    match flat_string.width() {
        StringWidth::OneByte => {
            let lexer_stream = HeapOneByteLexerStream::new(flat_string.as_one_byte_slice());
            match_lexer_stream(lexer_stream, regexp, start_index)
        }
        StringWidth::TwoByte => {
            if regexp.flags.has_any_unicode_flag() {
                let lexer_stream =
                    HeapTwoByteCodePointLexerStream::new(flat_string.as_two_byte_slice());
                match_lexer_stream(lexer_stream, regexp, start_index)
            } else {
                let lexer_stream =
                    HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice());
                match_lexer_stream(lexer_stream, regexp, start_index)
            }
        }
    }
}

fn run_case_insensitive_matcher(
    regexp: HeapPtr<CompiledRegExpObject>,
    target_string: Handle<FlatString>,
    start_index: usize,
) -> Option<Match> {
    // If ignoring case, canonicalize into an off-heap UTF-16 string
    let mut lowercase_string_code_units = Vec::with_capacity(target_string.len());

    if regexp.flags.has_any_unicode_flag() {
        for code_point in target_string.iter_code_points() {
            let canonical_code_point = canonicalize(code_point, true);

            match try_encode_surrogate_pair(canonical_code_point) {
                None => lowercase_string_code_units.push(canonical_code_point as u16),
                Some((high, low)) => {
                    lowercase_string_code_units.push(high);
                    lowercase_string_code_units.push(low);
                }
            }
        }

        let lexer_stream = HeapTwoByteCodePointLexerStream::new(&lowercase_string_code_units);
        match_lexer_stream(lexer_stream, regexp, start_index)
    } else {
        for code_unit in target_string.iter_code_units() {
            let canonical_code_point = canonicalize(code_unit as CodePoint, false);

            match try_encode_surrogate_pair(canonical_code_point) {
                None => lowercase_string_code_units.push(canonical_code_point as u16),
                Some((high, low)) => {
                    lowercase_string_code_units.push(high);
                    lowercase_string_code_units.push(low);
                }
            }
        }

        // We can use the match indices returned from matching the canonicalized string. This is
        // because both simple case folding and simple uppercase do not map code points outside the
        // BMP inside the BMP (and vice versa). This means that the number of code units needed per
        // code point is the same.
        let lexer_stream = HeapTwoByteCodeUnitLexerStream::new(&lowercase_string_code_units);
        match_lexer_stream(lexer_stream, regexp, start_index)
    }
}

/// Whether a code point is a word character as defined by \w or \b
fn is_word_code_point(code_point: CodePoint) -> bool {
    is_ascii_alphabetic(code_point) || is_decimal_digit(code_point) || code_point == '_' as u32
}

// 22.2.2.7.3 Canonicalize
#[inline]
pub fn canonicalize(code_point: CodePoint, is_unicode_aware: bool) -> CodePoint {
    if is_unicode_aware {
        // Use simple case folding for Unicode-aware case-insensitive matching
        match char::from_u32(code_point) {
            None => code_point,
            Some(c) => ICU.case_mapping.fold(c) as CodePoint,
        }
    } else {
        match char::from_u32(code_point) {
            None => code_point,
            Some(c) => {
                // Use simple uppercase for non-Unicode-aware case-insensitive matching
                let uppercase_code_point = ICU.case_mapping.to_uppercase(c) as CodePoint;

                // Do not allow mapping non-ASCII code points to ASCII code points
                if uppercase_code_point < 128 && code_point >= 128 {
                    code_point
                } else {
                    uppercase_code_point
                }
            }
        }
    }
}
