use std::rc::Rc;

use crate::js::{
    common::unicode::{
        code_point_from_surrogate_pair, decode_wtf8_codepoint, is_ascii,
        is_high_surrogate_code_unit, is_low_surrogate_code_unit, needs_surrogate_pair, CodeUnit,
    },
    runtime::string_value::{CodePointIterator, CodeUnitIterator},
};

use super::{
    loc::{Loc, Pos},
    source::Source,
    LocalizedParseError, ParseError, ParseResult,
};

/// Character that marks an EOF. Not a valid unicode character.
pub const EOF_CHAR: u32 = 0x110000;

// Forward vs backwards lexer streams.
//
// If the most recent advance call was forward, the pos is immediately before the start of the
// current code point. If the most recent advance call was backwards, the pos is immediately after
// the end of the current code point.
//
// In both types of lexer stream pos == 0 is the start of the input and pos == len is the end of
// the input. In both of these cases current will be the EOF_CHAR.

/// A save point for the lexer stream, can be used to restore the stream to a particular position.
pub struct SavedLexerStreamState {
    current: u32,
    pos: Pos,
}

pub trait LexerStream {
    /// The current position in the input stream
    fn pos(&self) -> Pos;

    /// The current byte or EOF_CHAR in the input stream. Output can have any range, but will only
    /// be compared against bytes or EOF_CHAR.
    fn current(&self) -> u32;

    /// Length of the underlying stream in units
    fn len(&self) -> usize;

    /// Move forward N units in the input stream
    fn advance_n(&mut self, n: usize);

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

    /// Return the byte or EOF_CHAR that is N units forwards in the input stream. Output can have
    /// any range, but will only be compared against bytes or EOF_CHAR.
    fn peek_n(&self, n: usize) -> u32;

    /// Parse and move forward one code point in the input stream
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32>;

    /// Iterate through the code points in the input stream between two indices.
    fn iter_slice<'a>(&self, start: Pos, end: Pos) -> impl 'a + DoubleEndedIterator<Item = u32>;

    /// Return a slice of the underlying buffer between two indices. Note that the underlying buffer
    /// may not be a buffer of bytes but the return type is always a byte slice.
    fn slice(&self, start: Pos, end: Pos) -> &[u8];

    /// Return whether the slice of the underlying buffer starting at the provided index is equal to
    /// a byte slice returned from `slice`. Note that the underlying buffer may not be a buffer of
    /// bytes but we always take in a byte slice.
    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool;

    /// Return an error with location between start_pos and the current position
    fn error<T>(&self, start_pos: Pos, error: ParseError) -> ParseResult<T>;

    /// Save the current state
    fn save(&self) -> SavedLexerStreamState;

    /// Restore to a saved state
    fn restore(&mut self, save_state: &SavedLexerStreamState);

    /// Whether the stream is at the start of the input
    #[inline]
    fn is_start(&self) -> bool {
        self.pos() == 0
    }

    /// Whether the stream is at the end of the input
    #[inline]
    fn is_end(&self) -> bool {
        self.pos() == self.len()
    }

    /// Whether the stream has a current code point. A stream does not have an current code point if
    /// it is at the end of a forward stream, or the start of a backwards stream.
    #[inline]
    fn has_current(&self) -> bool {
        self.current() != EOF_CHAR
    }
}

/// An input stream over a valid UTF-8 string.
pub struct Utf8LexerStream<'a> {
    /// Buffer of bytes that are being parsed
    buf: &'a [u8],
    /// Location of the current byte in the buffer
    pos: Pos,
    /// Current byte or EOF_CHAR
    current: u32,
    /// Pos of the start of the buffer that is being parsed
    buf_start_pos: Pos,
    /// Source of the buffer we are parsing
    source: Rc<Source>,
}

impl<'a> Utf8LexerStream<'a> {
    pub fn new(buf_start_pos: Pos, source: Rc<Source>, buf: &'a [u8]) -> Self {
        let current = if buf.is_empty() {
            EOF_CHAR
        } else {
            buf[0].into()
        };

        Utf8LexerStream { buf, pos: 0, current, buf_start_pos, source }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf[index].into()
    }

    fn loc_from_start_pos(&self, start_pos: Pos) -> Loc {
        let start = self.buf_start_pos + start_pos;
        let end = self.buf_start_pos + self.pos;
        Loc { start, end }
    }
}

impl LexerStream for Utf8LexerStream<'_> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
    }

    #[inline]
    fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.code_point_at(self.pos);
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    fn advance_backwards_n(&mut self, _: usize) {
        panic!("Utf8LexerStream::advance_backward_n not implemented")
    }

    fn advance_code_point(&mut self) {
        panic!("Utf8LexerStream::advance_code_point not implemented")
    }

    fn advance_backwards_code_point(&mut self) {
        panic!("Utf8LexerStream::advance_backwards_code_point not implemented")
    }

    fn peek_prev_code_point(&self) -> u32 {
        panic!("Utf8LexerStream::peek_prev_code_point not implemented")
    }

    fn peek_next_code_point(&self) -> u32 {
        panic!("Utf8LexerStream::peek_next_code_point not implemented")
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u32 {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.code_point_at(next_pos)
        } else {
            EOF_CHAR
        }
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32> {
        // Must check if we are at end of buffer as decode_wtf8_codepoint assumes we are not
        if self.current == EOF_CHAR {
            return self.error(self.pos(), ParseError::UnexpectedRegExpEnd);
        }

        let current = self.current;
        if is_ascii(current) {
            self.advance_n(1);
            Ok(current)
        } else {
            let buf = &self.buf[self.pos..];
            match decode_wtf8_codepoint(buf) {
                Ok((code_point, byte_length)) => {
                    self.advance_n(byte_length);
                    Ok(code_point)
                }
                Err(byte_length) => {
                    let start_pos = self.pos;
                    self.advance_n(byte_length);
                    self.error(start_pos, ParseError::InvalidUnicode)
                }
            }
        }
    }

    #[allow(refining_impl_trait)]
    fn iter_slice<'b>(&self, _: Pos, _: Pos) -> impl 'b + DoubleEndedIterator<Item = u32> {
        // Stub implementation
        CodePointIterator::from_raw_one_byte_slice(&[])
    }

    fn slice(&self, _: Pos, _: Pos) -> &[u8] {
        panic!("Utf8LexerStream::slice not implemented")
    }

    fn slice_equals(&self, _: Pos, _: &[u8]) -> bool {
        panic!("Utf8LexerStream::slice_equals not implemented")
    }

    fn error<T>(&self, start_pos: Pos, error: ParseError) -> ParseResult<T> {
        let loc = self.loc_from_start_pos(start_pos);
        let source = self.source.clone();
        Err(LocalizedParseError { error, source_loc: Some((loc, source)) })
    }

    fn save(&self) -> SavedLexerStreamState {
        SavedLexerStreamState { current: self.current, pos: self.pos }
    }

    fn restore(&mut self, save_state: &SavedLexerStreamState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }
}

/// An input stream over the bytes of a heap allocated OneByte string.
pub struct HeapOneByteLexerStream<'a> {
    /// Buffer of bytes that are being parsed
    buf: &'a [u8],
    /// Location of the current byte in the buffer
    pos: Pos,
    /// Current byte or EOF_CHAR
    current: u32,
}

impl<'a> HeapOneByteLexerStream<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        let current = if buf.is_empty() {
            EOF_CHAR
        } else {
            buf[0].into()
        };

        HeapOneByteLexerStream { buf, pos: 0, current }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf[index].into()
    }

    #[inline]
    fn code_point_before(&self, index: usize) -> u32 {
        self.buf[index - 1].into()
    }
}

impl LexerStream for HeapOneByteLexerStream<'_> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
    }

    #[inline]
    fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.code_point_at(self.pos);
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos.checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.pos = new_pos;
                self.current = self.code_point_before(new_pos);
            }
            _ => {
                self.pos = 0;
                self.current = EOF_CHAR;
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
            self.code_point_before(self.pos)
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos)
        }
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u32 {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.code_point_at(next_pos)
        } else {
            EOF_CHAR
        }
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32> {
        // There is no code point if we are at the end of the buffer
        if self.current == EOF_CHAR {
            return self.error(self.pos(), ParseError::UnexpectedRegExpEnd);
        }

        let code_point = self.current();
        self.advance_n(1);

        Ok(code_point)
    }

    fn iter_slice<'b>(&self, start: Pos, end: Pos) -> impl 'b + DoubleEndedIterator<Item = u32> {
        CodePointIterator::from_raw_one_byte_slice(&self.buf[start..end])
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        &self.buf[start..end]
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        let end = start + slice.len();
        if end > self.buf.len() {
            return false;
        }

        &self.buf[start..end] == slice
    }

    fn error<T>(&self, _: Pos, error: ParseError) -> ParseResult<T> {
        Err(LocalizedParseError { error, source_loc: None })
    }

    fn save(&self) -> SavedLexerStreamState {
        SavedLexerStreamState { current: self.current, pos: self.pos }
    }

    fn restore(&mut self, save_state: &SavedLexerStreamState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }
}

/// An input stream over the code units of a heap allocated TwoByte string. This lexer stream is not
/// unicode aware, meaning that it will not decode surrogate pairs into code points and will instead
/// treat every 16-bit code unit as its own code point in the Basic Multilingual Plane.
pub struct HeapTwoByteCodeUnitLexerStream<'a> {
    /// Buffer of two byte code units that are being parsed
    buf: &'a [u16],
    /// Location of the current code unit in the buffer
    pos: Pos,
    /// Current code unit or EOF_CHAR
    current: u32,
    /// Source of the buffer we are parsing, if one was specified
    source: Option<Rc<Source>>,
}

impl<'a> HeapTwoByteCodeUnitLexerStream<'a> {
    pub fn new(buf: &'a [u16], source: Option<Rc<Source>>) -> Self {
        let current = if buf.is_empty() {
            EOF_CHAR
        } else {
            buf[0] as u32
        };

        HeapTwoByteCodeUnitLexerStream { buf, pos: 0, current, source }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf[index] as u32
    }

    #[inline]
    fn code_point_before(&self, index: usize) -> u32 {
        self.buf[index - 1] as u32
    }
}

impl LexerStream for HeapTwoByteCodeUnitLexerStream<'_> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
    }

    #[inline]
    fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.code_point_at(self.pos);
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos.checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.pos = new_pos;
                self.current = self.code_point_before(new_pos);
            }
            _ => {
                self.pos = 0;
                self.current = EOF_CHAR;
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
            self.code_point_before(self.pos)
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos)
        }
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u32 {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.code_point_at(next_pos)
        } else {
            EOF_CHAR
        }
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32> {
        // There is no code point if we are at the end of the buffer
        if self.current == EOF_CHAR {
            return self.error(self.pos(), ParseError::UnexpectedRegExpEnd);
        }

        let code_point = self.current();
        self.advance_n(1);

        Ok(code_point)
    }

    fn iter_slice<'b>(&self, start: Pos, end: Pos) -> impl 'b + DoubleEndedIterator<Item = u32> {
        CodeUnitIterator::from_raw_two_byte_slice(&self.buf[start..end])
            .map(|code_unit| code_unit as u32)
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        let u16_slice = &self.buf[start..end];
        unsafe { std::slice::from_raw_parts(u16_slice.as_ptr() as *const u8, u16_slice.len() * 2) }
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        let slice =
            unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const u16, slice.len() / 2) };

        let end = start + slice.len();
        if end > self.buf.len() {
            return false;
        }

        &self.buf[start..end] == slice
    }

    fn error<T>(&self, pos: Pos, error: ParseError) -> ParseResult<T> {
        let source_loc = if let Some(source) = &self.source {
            let loc = Loc { start: pos, end: pos };
            Some((loc, source.clone()))
        } else {
            None
        };

        Err(LocalizedParseError { error, source_loc })
    }

    fn save(&self) -> SavedLexerStreamState {
        SavedLexerStreamState { current: self.current, pos: self.pos }
    }

    fn restore(&mut self, save_state: &SavedLexerStreamState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }
}

/// An input stream over the code points of a heap allocated TwoByte string. This lexer stream is
/// unicode aware, meaning that it will decode paired surrogate into code points.
pub struct HeapTwoByteCodePointLexerStream<'a> {
    /// Buffer of two byte code units that are being parsed
    buf: &'a [u16],
    /// Location of the current code unit in the buffer
    pos: Pos,
    /// Current code unit or EOF_CHAR
    current: u32,
}

impl<'a> HeapTwoByteCodePointLexerStream<'a> {
    pub fn new(buf: &'a [u16]) -> Self {
        let current = if buf.is_empty() {
            EOF_CHAR
        } else {
            buf[0] as u32
        };

        HeapTwoByteCodePointLexerStream { buf, pos: 0, current }
    }

    // Return a code point and the number of code units that make up that code point.
    #[inline]
    fn code_point_at(&self, index: usize) -> (u32, usize) {
        let code_unit = self.buf[index] as CodeUnit;
        if is_high_surrogate_code_unit(code_unit) && index + 1 < self.buf.len() {
            let next_code_unit = self.buf[index + 1] as CodeUnit;
            if is_low_surrogate_code_unit(next_code_unit) {
                let code_point = code_point_from_surrogate_pair(code_unit, next_code_unit);
                return (code_point, 2);
            }
        }

        (code_unit as u32, 1)
    }

    // Return a code point ending at the given position, along with the number of code units that
    // make up that code point.
    #[inline]
    fn code_point_before(&self, index: usize) -> (u32, usize) {
        let code_unit = self.buf[index - 1];
        if is_low_surrogate_code_unit(code_unit) && index >= 2 {
            let prev_code_unit = self.buf[index - 2];
            if is_high_surrogate_code_unit(prev_code_unit) {
                let code_point = code_point_from_surrogate_pair(prev_code_unit, code_unit);
                return (code_point, 2);
            }
        }

        (code_unit as u32, 1)
    }
}

impl LexerStream for HeapTwoByteCodePointLexerStream<'_> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
    }

    #[inline]
    fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            let (code_point, _) = self.code_point_at(self.pos);
            self.current = code_point;
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    #[inline]
    fn advance_backwards_n(&mut self, n: usize) {
        match self.pos.checked_sub(n) {
            Some(new_pos) if new_pos > 0 => {
                self.pos = new_pos;
                let (code_point, _) = self.code_point_before(new_pos);
                self.current = code_point;
            }
            _ => {
                self.pos = 0;
                self.current = EOF_CHAR;
            }
        }
    }

    #[inline]
    fn advance_code_point(&mut self) {
        if self.is_end() {
            return;
        }

        if needs_surrogate_pair(self.current) {
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

        if needs_surrogate_pair(self.current) {
            self.advance_backwards_n(2);
        } else {
            self.advance_backwards_n(1);
        }

        let prev_code_unit = self.buf[self.pos - 1];
        if is_low_surrogate_code_unit(prev_code_unit) && self.pos >= 2 {
            let prev_prev_code_unit = self.buf[self.pos - 2];
            if is_high_surrogate_code_unit(prev_prev_code_unit) {
                self.pos -= 2;
                self.current = code_point_from_surrogate_pair(prev_prev_code_unit, prev_code_unit);
                return;
            }
        }

        self.pos -= 1;
        self.current = prev_code_unit as u32;
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.is_start() {
            EOF_CHAR
        } else {
            self.code_point_before(self.pos).0
        }
    }

    #[inline]
    fn peek_next_code_point(&self) -> u32 {
        if self.is_end() {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos).0
        }
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u32 {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            let (code_point, _) = self.code_point_at(self.pos);
            code_point
        } else {
            EOF_CHAR
        }
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32> {
        // There is no code point if we are at the end of the buffer
        if self.current == EOF_CHAR {
            return self.error(self.pos(), ParseError::UnexpectedRegExpEnd);
        }

        let (code_point, num_code_units) = self.code_point_at(self.pos);
        self.advance_n(num_code_units);

        Ok(code_point)
    }

    fn iter_slice<'b>(&self, start: Pos, end: Pos) -> impl 'b + DoubleEndedIterator<Item = u32> {
        CodePointIterator::from_raw_two_byte_slice(&self.buf[start..end])
    }

    fn slice(&self, start: Pos, end: Pos) -> &[u8] {
        let u16_slice = &self.buf[start..end];
        unsafe { std::slice::from_raw_parts(u16_slice.as_ptr() as *const u8, u16_slice.len() * 2) }
    }

    fn slice_equals(&self, start: Pos, slice: &[u8]) -> bool {
        let slice =
            unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const u16, slice.len() / 2) };

        let end = start + slice.len();
        if end > self.buf.len() {
            return false;
        }

        &self.buf[start..end] == slice
    }

    fn error<T>(&self, _: Pos, error: ParseError) -> ParseResult<T> {
        Err(LocalizedParseError { error, source_loc: None })
    }

    fn save(&self) -> SavedLexerStreamState {
        SavedLexerStreamState { current: self.current, pos: self.pos }
    }

    fn restore(&mut self, save_state: &SavedLexerStreamState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }
}
