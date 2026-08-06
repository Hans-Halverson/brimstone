use crate::{
    common::unicode::needs_surrogate_pair,
    parser::{
        lexer_stream::{
            EOF_CHAR, HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream,
            HeapTwoByteCodeUnitLexerStream, LexerStream,
        },
        loc::Pos,
    },
};

/// An extension of the LexerStream trait with additional methods for use in the RegExp engine.
pub trait RegExpLexerStream: LexerStream {
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
}

impl<'a> RegExpLexerStream for HeapOneByteLexerStream<'a> {
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
}

impl<'a> RegExpLexerStream for HeapTwoByteCodeUnitLexerStream<'a> {
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
        let u16_slice = &self.buf()[start..end];
        unsafe { std::slice::from_raw_parts(u16_slice.as_ptr() as *const u8, u16_slice.len() * 2) }
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
}

impl<'a> RegExpLexerStream for HeapTwoByteCodePointLexerStream<'a> {
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
        let u16_slice = &self.buf()[start..end];
        unsafe { std::slice::from_raw_parts(u16_slice.as_ptr() as *const u8, u16_slice.len() * 2) }
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
}
