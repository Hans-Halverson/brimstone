use std::rc::Rc;

use crate::js::common::unicode::{decode_utf8_codepoint, is_ascii};

use super::{
    loc::{Loc, Pos},
    source::Source,
    LocalizedParseError, ParseError, ParseResult,
};

/// Character that marks an EOF. Not a valid unicode character.
pub const EOF_CHAR: char = '\u{ffff}';

/// A save point for the lexer stream, can be used to restore the stream to a particular position.
pub struct SavedLexerStreamState {
    current: char,
    pos: Pos,
}

pub trait LexerStream {
    /// The current position in the input stream
    fn pos(&self) -> Pos;

    /// The current byte or EOF_CHAR in the input stream. Output can have any range, but will only
    /// be compared against bytes or EOF_CHAR.
    fn current(&self) -> char;

    /// Move forward N units in the input stream
    fn advance_n(&mut self, n: usize);

    /// Return the byte or EOF_CHAR that is N units forwards in the input stream. Output can have
    /// any range, but will only be compared against bytes or EOF_CHAR.
    fn peek_n(&self, n: usize) -> char;

    /// Parse and move forward one code point in the input stream
    fn parse_unicode_codepoint(&mut self) -> ParseResult<char>;

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
    /// Buffer of bytes for pattern that is being parsed
    buf: &'a str,
    /// Location of the current byte in the buffer
    pos: Pos,
    /// Current byte or EOF_CHAR
    current: char,
    /// Pos of the start of the buffer that is being parsed
    buf_start_pos: Pos,
    /// Source of the buffer we are parsing
    source: Rc<Source>,
}

impl<'a> Utf8LexerStream<'a> {
    pub fn new(buf_start_pos: Pos, source: Rc<Source>, pattern: &'a str) -> Self {
        let buf = pattern;
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        Utf8LexerStream { buf, pos: 0, current, buf_start_pos, source }
    }

    #[inline]
    fn char_at(&self, index: usize) -> char {
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
    fn current(&self) -> char {
        self.current
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.char_at(self.pos);
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    #[inline]
    fn peek_n(&self, n: usize) -> char {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.char_at(next_pos)
        } else {
            EOF_CHAR
        }
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<char> {
        // Must check if we are at end of pattern as decode_utf8_codepoint assumes we are not
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
