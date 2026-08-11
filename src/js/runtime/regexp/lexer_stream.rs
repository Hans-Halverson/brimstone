use crate::{
    common::unicode::{is_newline, needs_surrogate_pair, two_byte_slice_as_bytes},
    parser::{
        lexer_stream::{
            EOF_CHAR, HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream,
            HeapTwoByteCodeUnitLexerStream, LexerStream,
        },
        loc::Pos,
    },
    runtime::regexp::{
        match_start_filter::MatchStartFilter,
        required_literal_filter::{RequiredLiteralFilter, RequiredLiteralSearcher},
    },
};

/// An extension of the LexerStream trait with additional methods for use in the RegExp engine.
pub trait RegExpLexerStream: LexerStream {
    /// Maximum number of code units used to encode a single code point in this stream.
    const MAX_CODE_UNITS_PER_CODE_POINT: usize;

    /// Move backward N units in the input stream
    fn advance_backwards_n(&mut self, n: usize);

    /// Move forward one code point in the input stream
    fn advance_code_point(&mut self);

    /// Move backwards one code point in the input stream
    fn advance_backwards_code_point(&mut self);

    /// Return the code point that immediately precedes the current position, or EOF_CHAR if at the
    /// start of the stream.
    fn peek_prev_code_point(&self) -> u32;

    /// Return the code point that immediately succeeds the current position, or EOF_CHAR if at the
    /// end of the stream.
    fn peek_next_code_point(&self) -> u32;

    /// Return a slice of the underlying buffer between two indices. Note that the underlying buffer
    /// may not be a buffer of bytes but the return type is always a byte slice.
    fn slice(&self, start: Pos, end: Pos) -> &[u8];

    /// Return whether the slice of code points in the underlying buffer starting at the provided
    /// index is equal to a slice of code points represented as a byte slice returned from `slice`.
    ///
    /// Only matches if the slice in the underlying buffer contains full code points (i.e. no match
    /// if the slice would split surrogate pairs).
    ///
    /// Note that the underlying buffer may not be a buffer of bytes but we always take in a byte
    /// slice.
    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool;

    /// Whether the remaining stream matches the required literal at any position.
    fn matches_required_literal_anywhere(
        &self,
        required_literal_filter: &RequiredLiteralFilter,
    ) -> bool;

    /// Whether a match starting at the current position may contain the required literal.
    ///
    /// Uses literal's known bounds within the match to restrict search window. Conservatively
    /// returns true if there are no bounds on the literal within the match.
    fn may_match_required_literal_at_current_pos(
        &self,
        required_literal_filter: &RequiredLiteralFilter,
    ) -> bool;

    /// Create a searcher for finding the required literal in the stream, or None if this stream
    /// does not support this kind of search.
    fn required_literal_searcher(
        &self,
        required_literal_filter: &RequiredLiteralFilter,
    ) -> Option<RequiredLiteralSearcher>;

    /// Find the required literal starting from the given position, returning the position of the
    /// first occurrence if any.
    fn find_required_literal(
        &self,
        searcher: &mut RequiredLiteralSearcher,
        start_pos: Pos,
    ) -> Option<Pos>;

    /// Advance until the current code point is a member of the filter. Returns false if the end of
    /// the input was reached without finding a member.
    fn scan_to_code_point_in_filter(&mut self, filter: &MatchStartFilter) -> bool {
        while self.has_current() {
            if filter.contains(self.current()) {
                return true;
            }

            self.advance_code_point();
        }

        false
    }

    /// Advance to just past the next line terminator. Returns false if the end of the input was
    /// reached without finding a line terminator.
    fn scan_past_line_terminator(&mut self) -> bool {
        while self.has_current() {
            let is_line_terminator = is_newline(self.current());
            self.advance_code_point();

            if is_line_terminator {
                return true;
            }
        }

        false
    }
}

impl<'a> RegExpLexerStream for HeapOneByteLexerStream<'a> {
    const MAX_CODE_UNITS_PER_CODE_POINT: usize = 1;

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos().checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.set_pos(new_pos);
                self.set_current(self.code_point_before(new_pos));
            }
            _ => {
                self.set_pos(0);
                self.set_current(EOF_CHAR);
            }
        }
    }

    #[inline]
    fn advance_code_point(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    fn advance_backwards_code_point(&mut self) {
        self.advance_backwards_n(1);
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.is_start() {
            EOF_CHAR
        } else {
            self.code_point_before(self.pos())
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos())
        }
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        &self.buf()[start..end]
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        // All indices are valid code point boundaries since surrogate code units cannot appear
        let end = start + slice.len();
        if end > self.buf().len() {
            return false;
        }

        &self.buf()[start..end] == slice
    }

    fn matches_required_literal_anywhere(&self, filter: &RequiredLiteralFilter) -> bool {
        filter.matches_one_byte(&self.buf()[self.pos()..])
    }

    fn may_match_required_literal_at_current_pos(&self, filter: &RequiredLiteralFilter) -> bool {
        let buf = self.buf();

        // If the offset is known we can compare directly at the expected position
        if filter.has_exact_offset() {
            return filter.matches_one_byte_at(buf, self.pos() + filter.min_offset());
        }

        match filter.search_window(self.pos(), buf.len(), Self::MAX_CODE_UNITS_PER_CODE_POINT) {
            Some(window) => filter.matches_one_byte(&buf[window]),
            None => true,
        }
    }

    fn required_literal_searcher(
        &self,
        filter: &RequiredLiteralFilter,
    ) -> Option<RequiredLiteralSearcher> {
        Some(RequiredLiteralSearcher::new_one_byte(filter))
    }

    #[inline]
    fn find_required_literal(
        &self,
        searcher: &mut RequiredLiteralSearcher,
        start_pos: Pos,
    ) -> Option<Pos> {
        searcher.find_one_byte(self.buf(), start_pos)
    }

    fn scan_to_code_point_in_filter(&mut self, filter: &MatchStartFilter) -> bool {
        let remaining_buf = &self.buf()[self.pos()..];

        match filter.scan_to_next_latin1_member(remaining_buf) {
            Some(offset) => {
                self.advance_n(offset);
                true
            }
            None => {
                self.advance_n(remaining_buf.len());
                false
            }
        }
    }

    fn scan_past_line_terminator(&mut self) -> bool {
        let remaining_buf = &self.buf()[self.pos()..];

        // The only newline characters that can appear in a one-byte string
        match memchr::memchr2(b'\n', b'\r', remaining_buf) {
            Some(offset) => {
                self.advance_n(offset + 1);
                true
            }
            None => {
                self.advance_n(remaining_buf.len());
                false
            }
        }
    }
}

impl<'a> RegExpLexerStream for HeapTwoByteCodeUnitLexerStream<'a> {
    const MAX_CODE_UNITS_PER_CODE_POINT: usize = 1;

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos().checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.set_pos(new_pos);
                self.set_current(self.code_point_before(new_pos));
            }
            _ => {
                self.set_pos(0);
                self.set_current(EOF_CHAR);
            }
        }
    }

    #[inline]
    fn advance_code_point(&mut self) {
        self.advance_n(1)
    }

    #[inline]
    fn advance_backwards_code_point(&mut self) {
        self.advance_backwards_n(1);
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.is_start() {
            EOF_CHAR
        } else {
            self.code_point_before(self.pos())
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos())
        }
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        two_byte_slice_as_bytes(&self.buf()[start..end])
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        // All indices are valid code point boundaries since code units are never paired
        let slice =
            unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const u16, slice.len() / 2) };

        let end = start + slice.len();
        if end > self.buf().len() {
            return false;
        }

        &self.buf()[start..end] == slice
    }

    fn matches_required_literal_anywhere(&self, filter: &RequiredLiteralFilter) -> bool {
        filter.matches_two_byte(&self.buf()[self.pos()..])
    }

    fn may_match_required_literal_at_current_pos(&self, filter: &RequiredLiteralFilter) -> bool {
        let buf = self.buf();

        // If the offset is known we can compare directly at the expected position
        if filter.has_exact_offset() {
            return filter.matches_two_byte_at(buf, self.pos() + filter.min_offset());
        }

        match filter.search_window(self.pos(), buf.len(), Self::MAX_CODE_UNITS_PER_CODE_POINT) {
            Some(window) => filter.matches_two_byte(&buf[window]),
            None => true,
        }
    }

    fn required_literal_searcher(
        &self,
        filter: &RequiredLiteralFilter,
    ) -> Option<RequiredLiteralSearcher> {
        Some(RequiredLiteralSearcher::new_two_byte(filter))
    }

    #[inline]
    fn find_required_literal(
        &self,
        searcher: &mut RequiredLiteralSearcher,
        start_pos: Pos,
    ) -> Option<Pos> {
        searcher.find_two_byte(self.buf(), start_pos)
    }
}

impl<'a> RegExpLexerStream for HeapTwoByteCodePointLexerStream<'a> {
    const MAX_CODE_UNITS_PER_CODE_POINT: usize = 2;

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos().checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.set_pos(new_pos);
                let (code_point, _) = self.code_point_before(new_pos);
                self.set_current(code_point);
            }
            _ => {
                self.set_pos(0);
                self.set_current(EOF_CHAR);
            }
        }
    }

    #[inline]
    fn advance_code_point(&mut self) {
        if self.is_end() {
            return;
        }

        if needs_surrogate_pair(self.current()) {
            self.advance_n(2);
        } else {
            self.advance_n(1);
        }
    }

    #[inline]
    fn advance_backwards_code_point(&mut self) {
        if self.is_start() {
            return;
        }

        if needs_surrogate_pair(self.current()) {
            self.advance_backwards_n(2);
        } else {
            self.advance_backwards_n(1);
        }
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.is_start() {
            EOF_CHAR
        } else {
            self.code_point_before(self.pos()).0
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos()).0
        }
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        two_byte_slice_as_bytes(&self.buf()[start..end])
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        let slice =
            unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const u16, slice.len() / 2) };

        let end = start + slice.len();
        if end > self.buf().len()
            || !self.is_valid_code_point_boundary(start)
            || !self.is_valid_code_point_boundary(end)
        {
            return false;
        }

        &self.buf()[start..end] == slice
    }

    fn matches_required_literal_anywhere(&self, filter: &RequiredLiteralFilter) -> bool {
        filter.matches_two_byte(&self.buf()[self.pos()..])
    }

    fn may_match_required_literal_at_current_pos(&self, filter: &RequiredLiteralFilter) -> bool {
        let buf = self.buf();
        match filter.search_window(self.pos(), buf.len(), Self::MAX_CODE_UNITS_PER_CODE_POINT) {
            Some(window) => filter.matches_two_byte(&buf[window]),
            None => true,
        }
    }

    fn required_literal_searcher(
        &self,
        _: &RequiredLiteralFilter,
    ) -> Option<RequiredLiteralSearcher> {
        // Searchers do not support code units encoded as surrogate pairs, since they assume a 1:1
        // code unit to code point mapping.
        None
    }

    fn find_required_literal(&self, _: &mut RequiredLiteralSearcher, _: Pos) -> Option<Pos> {
        panic!("HeapTwoByteCodePointLexerStream does not support required literal scans");
    }
}
