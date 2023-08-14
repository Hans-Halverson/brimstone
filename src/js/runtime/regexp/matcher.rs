use std::collections::HashSet;

use crate::js::{
    common::unicode::{
        is_ascii_alphabetic, is_decimal_digit, is_newline, is_whitespace, CodePoint,
    },
    parser::{
        lexer_stream::{
            HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream,
            HeapTwoByteCodeUnitLexerStream, LexerStream, SavedLexerStreamState,
        },
        regexp::RegExpFlags,
    },
    runtime::{
        string_value::{StringValue, StringWidth},
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
    backtrack_stack: Vec<BacktrackRestoreState>,
    // All capture points that have been marked
    capture_stack: Vec<CapturePoint>,
    // A saved string index for each progress instruction
    progress_points: Vec<HashSet<usize>>,
    // An accumulator register for building multi-part comparisons
    compare_register: bool,
}

struct BacktrackRestoreState {
    // Index of the next instruction to execute when this restore point was created
    instruction_index: usize,
    // Current target string state when this restore point was created
    saved_string_state: SavedLexerStreamState,
    // Length of the capture stack when this restore point was created
    capture_stack_len: usize,
}

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

impl<T: LexerStream> MatchEngine<T> {
    fn new(regexp: HeapPtr<CompiledRegExpObject>, string_lexer: T) -> Self {
        let mut progress_points = Vec::with_capacity(regexp.num_progress_points as usize);
        progress_points.resize_with(regexp.num_progress_points as usize, || HashSet::new());

        Self {
            regexp,
            string_lexer,
            instruction_index: 0,
            backtrack_stack: Vec::new(),
            capture_stack: Vec::new(),
            progress_points,
            compare_register: false,
        }
    }

    fn push_backtrack_restore_state(&mut self, instruction_index: usize) {
        let saved_string_state = self.string_lexer.save();
        let restore_state = BacktrackRestoreState {
            instruction_index,
            saved_string_state,
            capture_stack_len: self.capture_stack.len(),
        };

        self.backtrack_stack.push(restore_state);
    }

    fn backtrack(&mut self) -> Result<(), ()> {
        match self.backtrack_stack.pop() {
            None => Err(()),
            Some(restore_state) => {
                self.instruction_index = restore_state.instruction_index;
                self.string_lexer.restore(&restore_state.saved_string_state);
                self.capture_stack.truncate(restore_state.capture_stack_len);

                Ok(())
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

    fn run(&mut self) -> Option<Match> {
        match self.execute_bytecode() {
            Err(_) => None,
            Ok(_) => Some(self.build_match()),
        }
    }

    fn execute_bytecode(&mut self) -> Result<(), ()> {
        loop {
            match self.current_instruction() {
                Instruction::Accept => return Ok(()),
                Instruction::Literal(code_point) => {
                    if self.string_lexer.current() != code_point {
                        self.backtrack()?;
                    } else {
                        self.string_lexer.advance_code_point();
                        self.advance_instruction();
                    }
                }
                Instruction::Wildcard => {
                    if self.string_lexer.is_end() {
                        self.backtrack()?;
                    } else {
                        self.string_lexer.advance_code_point();
                        self.advance_instruction();
                    }
                }
                Instruction::WildcardNoNewline => {
                    if self.string_lexer.is_end() || is_newline(self.string_lexer.current()) {
                        self.backtrack()?;
                    } else {
                        self.string_lexer.advance_code_point();
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
                    let capture_point = CapturePoint { capture_point_index, string_index };

                    self.capture_stack.push(capture_point);

                    self.advance_instruction();
                }
                Instruction::Progress(progress_index) => {
                    let string_index = self.string_lexer.pos();
                    if self.progress_points[progress_index as usize].insert(string_index) {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertStart => {
                    if self.string_lexer.pos() == 0 {
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
                    if self.string_lexer.pos() == 0
                        || is_newline(self.string_lexer.peek_prev_code_point())
                    {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertEndOrNewline => {
                    if self.string_lexer.is_end() || is_newline(self.string_lexer.current()) {
                        self.advance_instruction();
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertWordBoundary => {
                    if self.is_at_word_boundary() {
                        self.advance_instruction()
                    } else {
                        self.backtrack()?;
                    }
                }
                Instruction::AssertNotWordBoundary => {
                    if self.is_at_word_boundary() {
                        self.backtrack()?;
                    } else {
                        self.advance_instruction()
                    }
                }
                Instruction::Backreference(capture_group_index) => {
                    match self.find_backreference_capture_bounds(capture_group_index) {
                        None => self.advance_instruction(),
                        Some((start_index, end_index)) => {
                            let captured_slice = self.string_lexer.slice(start_index, end_index);

                            if self
                                .string_lexer
                                .slice_equals(self.string_lexer.pos(), captured_slice)
                            {
                                self.string_lexer.advance_n(captured_slice.len());
                                self.advance_instruction();
                            } else {
                                self.backtrack()?;
                            }
                        }
                    }
                }
                Instruction::ConsumeIfTrue => {
                    if !self.compare_register || self.string_lexer.is_end() {
                        self.backtrack()?;
                    } else {
                        self.string_lexer.advance_code_point();
                        self.advance_instruction();
                    }

                    self.compare_register = false;
                }
                Instruction::ConsumeIfFalse => {
                    if self.compare_register || self.string_lexer.is_end() {
                        self.backtrack()?;
                    } else {
                        self.string_lexer.advance_code_point();
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
                    if is_word_code_point(self.string_lexer.current()) {
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
                Instruction::Lookaround(is_positive) => {
                    // Save lexer state for starting lookaround
                    let saved_string_state = self.string_lexer.save();

                    // Execute the lookaround as a sub-execution within engine
                    self.advance_instruction();
                    let is_match = self.execute_bytecode().is_ok();

                    // Check if lookaround succeeded and either restore or backtrack
                    if is_match == is_positive {
                        self.string_lexer.restore(&saved_string_state);
                    } else {
                        self.backtrack()?;
                    }
                }
            }
        }
    }

    fn is_at_word_boundary(&self) -> bool {
        let is_current_word = is_word_code_point(self.string_lexer.current());
        let is_prev_word = is_word_code_point(self.string_lexer.peek_prev_code_point());

        is_current_word != is_prev_word
    }

    /// Return the bounds of most recent match of the given capture group
    fn find_backreference_capture_bounds(
        &self,
        capture_group_index: u32,
    ) -> Option<(usize, usize)> {
        // Precalculate the start and end capture point indices to search for
        let start_capture_point_index = capture_group_index * 2;
        let end_capture_point_index = start_capture_point_index + 1;

        let mut end_string_index = None;

        // Find the last (start_index, end_index) pair in the capture stack
        for capture_point in self.capture_stack.iter().rev() {
            if capture_point.capture_point_index == end_capture_point_index {
                end_string_index = Some(capture_point.string_index);
            } else if capture_point.capture_point_index == start_capture_point_index {
                // If we find the start point and have already found the end point, return the
                // full capture. Otherwise we may have encountered the start point but not yet
                // encountered a corresponding end point.
                if let Some(end_string_index) = end_string_index {
                    return Some((capture_point.string_index, end_string_index));
                }
            }
        }

        None
    }

    fn build_match(&self) -> Match {
        let mut latest_capture_points: Vec<(Option<usize>, Option<usize>)> =
            vec![(None, None); self.regexp.num_capture_groups as usize + 1];

        // Collect the latest start and end capture points for each capture group
        for capture_point in self.capture_stack.iter().rev() {
            let capture_group_index = capture_point.capture_point_index as usize / 2;
            let is_start_point = capture_point.capture_point_index % 2 == 0;
            if is_start_point {
                if latest_capture_points[capture_group_index].0.is_none() {
                    latest_capture_points[capture_group_index].0 = Some(capture_point.string_index);
                }
            } else {
                if latest_capture_points[capture_group_index].1.is_none() {
                    latest_capture_points[capture_group_index].1 = Some(capture_point.string_index);
                }
            }
        }

        // If a start and end capture point appear then form a complete capture
        let capture_groups = latest_capture_points
            .into_iter()
            .map(|(start_index, end_index)| match (start_index, end_index) {
                (Some(start), Some(end)) => Some(Capture { start, end }),
                _ => None,
            })
            .collect();

        Match { capture_groups }
    }
}

pub fn run_matcher(
    regexp: HeapPtr<CompiledRegExpObject>,
    target_string: Handle<StringValue>,
    start_index: usize,
) -> Option<Match> {
    fn match_lexer_stream(
        lexer_stream: impl LexerStream,
        regexp: HeapPtr<CompiledRegExpObject>,
    ) -> Option<Match> {
        let mut match_engine = MatchEngine::new(regexp, lexer_stream);
        match_engine.run()
    }

    let flat_string = target_string.flatten();
    match flat_string.width() {
        StringWidth::OneByte => {
            let mut lexer_stream = HeapOneByteLexerStream::new(flat_string.as_one_byte_slice());
            lexer_stream.advance_n(start_index);

            match_lexer_stream(lexer_stream, regexp)
        }
        StringWidth::TwoByte => {
            if regexp.flags.contains(RegExpFlags::UNICODE_AWARE) {
                let mut lexer_stream =
                    HeapTwoByteCodePointLexerStream::new(flat_string.as_two_byte_slice());
                lexer_stream.advance_n(start_index);

                match_lexer_stream(lexer_stream, regexp)
            } else {
                let mut lexer_stream =
                    HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice());
                lexer_stream.advance_n(start_index);

                match_lexer_stream(lexer_stream, regexp)
            }
        }
    }
}

/// Whether a code point is a word character as defined by \w or \b
fn is_word_code_point(code_point: CodePoint) -> bool {
    is_ascii_alphabetic(code_point) || is_decimal_digit(code_point) || code_point == '_' as u32
}
