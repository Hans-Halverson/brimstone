use std::rc::Rc;

use crate::js::common::unicode::{
    code_point_from_surrogate_pair, decode_utf8_codepoint, is_ascii, is_high_surrogate_code_unit,
    is_low_surrogate_code_unit, needs_surrogate_pair, CodeUnit,
};

use super::{
    loc::{Loc, Pos},
    source::Source,
    LocalizedParseError, ParseError, ParseResult,
};

/// Character that marks an EOF. Not a valid unicode character.
pub const EOF_CHAR: u32 = 0x110000;

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

    /// Move forward N units in the input stream
    fn advance_n(&mut self, n: usize);

    /// Move forward one code point in the input stream
    fn advance_code_point(&mut self);

    /// Return the code point that immediately precedes the current position, or EOF_CHAR if at the
    /// start of the stream.
    fn peek_prev_code_point(&self) -> u32;

    /// Return the byte or EOF_CHAR that is N units forwards in the input stream. Output can have
    /// any range, but will only be compared against bytes or EOF_CHAR.
    fn peek_n(&self, n: usize) -> u32;

    /// Parse and move forward one code point in the input stream
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32>;

    /// Return an error with location between start_pos and the current position
    fn error<T>(&self, start_pos: Pos, error: ParseError) -> ParseResult<T>;

    /// Save the current state
    fn save(&self) -> SavedLexerStreamState;

    /// Restore to a saved state
    fn restore(&mut self, save_state: &SavedLexerStreamState);

    /// Whether the stream is at the end
    #[inline]
    fn is_end(&self) -> bool {
        self.current() == EOF_CHAR
    }
}

/// An input stream over a valid UTF-8 string.
pub struct Utf8LexerStream<'a> {
    /// Buffer of bytes that are being parsed
    buf: &'a str,
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
    pub fn new(buf_start_pos: Pos, source: Rc<Source>, buf: &'a str) -> Self {
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        Utf8LexerStream { buf, pos: 0, current, buf_start_pos, source }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf.as_bytes()[index].into()
    }

    fn loc_from_start_pos(&self, start_pos: Pos) -> Loc {
        let start = self.buf_start_pos + start_pos;
        let end = self.buf_start_pos + self.pos;
        Loc { start, end }
    }
}

impl<'a> LexerStream for Utf8LexerStream<'a> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
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
    fn advance_code_point(&mut self) {
        panic!("Utf8LexerStream::advance_code_point not implemented")
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        panic!("Utf8LexerStream::peek_prev_code_point not implemented")
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
        // Must check if we are at end of buffer as decode_utf8_codepoint assumes we are not
        if self.current == EOF_CHAR {
            return self.error(self.pos(), ParseError::UnexpectedRegExpEnd);
        }

        let current = self.current;
        if is_ascii(current) {
            self.advance_n(1);
            Ok(current)
        } else {
            let buf = &self.buf.as_bytes()[self.pos..];
            match decode_utf8_codepoint(buf) {
                Ok((code_point, byte_length)) => {
                    self.advance_n(byte_length);
                    Ok(code_point as u32)
                }
                Err(byte_length) => {
                    let start_pos = self.pos;
                    self.advance_n(byte_length);
                    self.error(start_pos, ParseError::InvalidUnicode)
                }
            }
        }
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
        let current = if buf.len() == 0 {
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
}

impl<'a> LexerStream for HeapOneByteLexerStream<'a> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
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
    fn advance_code_point(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.pos == 0 {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos - 1)
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
}

impl<'a> HeapTwoByteCodeUnitLexerStream<'a> {
    pub fn new(buf: &'a [u16]) -> Self {
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf[0] as u32
        };

        HeapTwoByteCodeUnitLexerStream { buf, pos: 0, current }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf[index] as u32
    }
}

impl<'a> LexerStream for HeapTwoByteCodeUnitLexerStream<'a> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
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
    fn advance_code_point(&mut self) {
        self.advance_n(1)
    }

    #[inline]
    fn peek_prev_code_point(&self) -> u32 {
        if self.pos == 0 {
            EOF_CHAR
        } else {
            self.code_point_at(self.pos - 1)
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
        let current = if buf.len() == 0 {
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
        if is_high_surrogate_code_unit(code_unit) {
            let next_code_unit = self.buf[index + 1] as CodeUnit;
            if is_low_surrogate_code_unit(next_code_unit) {
                let code_point = code_point_from_surrogate_pair(code_unit, next_code_unit);
                return (code_point, 2);
            }
        }

        (code_unit as u32, 1)
    }
}

impl<'a> LexerStream for HeapTwoByteCodePointLexerStream<'a> {
    #[inline]
    fn pos(&self) -> Pos {
        self.pos
    }

    #[inline]
    fn current(&self) -> u32 {
        self.current
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
    fn peek_prev_code_point(&self) -> u32 {
        if self.pos == 0 {
            return EOF_CHAR;
        }

        let prev_code_unit = self.buf[self.pos - 1];
        if is_low_surrogate_code_unit(prev_code_unit) && self.pos >= 2 {
            let prev_prev_code_unit = self.buf[self.pos - 2];
            if is_high_surrogate_code_unit(prev_prev_code_unit) {
                return code_point_from_surrogate_pair(prev_prev_code_unit, prev_code_unit);
            }
        }

        prev_code_unit as u32
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
