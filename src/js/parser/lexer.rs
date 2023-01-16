use std::rc::Rc;
use std::str::FromStr;

use super::loc::{Loc, Pos};
use super::parser::{LocalizedParseError, ParseError, ParseResult};
use super::source::Source;
use super::token::Token;
use super::unicode_tables::{ID_CONTINUE, ID_START};

pub struct Lexer<'a> {
    pub source: &'a Rc<Source>,
    buf: &'a str,
    current: char,
    pos: Pos,
    is_new_line_before_current: bool,
}

/// A save point for the lexer, can be used to restore the lexer to a particular position.
pub struct SavedLexerState {
    current: char,
    pos: Pos,
}

type LexResult = ParseResult<(Token, Loc)>;

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: char = '\u{ffff}';

/// Can this character appear as the first character of an identifier.
fn is_id_start_ascii(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '_' | '$' => true,
        _ => false,
    }
}

/// Can this character appear in an identifier (after the first character).
fn is_id_continue_ascii(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$' => true,
        _ => false,
    }
}

#[inline]
fn is_id_start_unicode(char: char) -> bool {
    ID_START.contains_char(char)
}

#[inline]
fn is_id_continue_unicode(char: char) -> bool {
    ID_CONTINUE.contains_char(char)
}

#[inline]
fn is_continuation_byte(byte: u8) -> bool {
    (byte & 0xC0) == 0x80
}

#[inline]
fn is_ascii(char: char) -> bool {
    (char as u32) < 0x80
}

fn is_decimal_digit(char: char) -> bool {
    '0' <= char && char <= '9'
}

fn get_binary_value(char: char) -> Option<u32> {
    match char {
        '0' => Some(0),
        '1' => Some(1),
        _ => None,
    }
}

fn get_octal_value(char: char) -> Option<u32> {
    match char {
        '0'..='7' => Some(char as u32 - '0' as u32),
        _ => None,
    }
}

fn get_hex_value(char: char) -> Option<u32> {
    match char {
        '0'..='9' => Some(char as u32 - '0' as u32),
        'a'..='f' => Some(char as u32 - 'a' as u32 + 10),
        'A'..='F' => Some(char as u32 - 'A' as u32 + 10),
        _ => None,
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Rc<Source>) -> Lexer<'a> {
        let buf = &source.contents;
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        Lexer {
            source: &source,
            buf: &source.contents,
            current,
            pos: 0,
            is_new_line_before_current: false,
        }
    }

    pub fn save(&self) -> SavedLexerState {
        SavedLexerState {
            current: self.current,
            pos: self.pos,
        }
    }

    pub fn restore(&mut self, save_state: &SavedLexerState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }

    pub fn is_new_line_before_current(&self) -> bool {
        self.is_new_line_before_current
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.buf.as_bytes()[self.pos].into();
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    fn advance(&mut self) {
        self.advance_n(1);
    }

    fn advance2(&mut self) {
        self.advance_n(2);
    }

    fn advance3(&mut self) {
        self.advance_n(3);
    }

    fn advance4(&mut self) {
        self.advance_n(4);
    }

    #[inline]
    fn peek_n(&self, n: usize) -> char {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.buf.as_bytes()[next_pos].into()
        } else {
            EOF_CHAR
        }
    }

    fn peek(&mut self) -> char {
        self.peek_n(1)
    }

    fn peek2(&mut self) -> char {
        self.peek_n(2)
    }

    fn peek3(&mut self) -> char {
        self.peek_n(3)
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc {
            start: start_pos,
            end: self.pos,
        }
    }

    fn emit(&self, token: Token, start_pos: Pos) -> LexResult {
        Ok((token, self.mark_loc(start_pos)))
    }

    fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        let source = (*self.source).clone();
        Err(LocalizedParseError {
            error,
            source_loc: Some((loc, source)),
        })
    }

    pub fn next(&mut self) -> LexResult {
        self.is_new_line_before_current = false;
        self.lex_token()
    }

    fn lex_token(&mut self) -> LexResult {
        // Skip whitespace
        loop {
            match self.current {
              | ' '
              | '\t'
              // Vertical tab
              | '\u{000b}'
              // Form feed
              | '\u{000c}' => self.advance(),
              | '\n'
              | '\r' => {
                self.is_new_line_before_current = true;
                self.advance();
              }
              | _ => break,
            }
        }

        let start_pos = self.pos;

        match self.current {
            '+' => match self.peek() {
                '+' => {
                    self.advance2();
                    self.emit(Token::Increment, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::AddEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Plus, start_pos)
                }
            },
            '-' => match self.peek() {
                '-' => {
                    self.advance2();
                    self.emit(Token::Decrement, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::SubtractEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Minus, start_pos)
                }
            },
            '*' => match self.peek() {
                '*' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::ExponentEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::Exponent, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::MultiplyEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Multiply, start_pos)
                }
            },
            '/' => match self.peek() {
                '/' => {
                    self.advance2();
                    self.skip_line_comment()?;
                    self.lex_token()
                }
                '*' => {
                    self.advance2();
                    self.skip_block_comment()?;
                    self.lex_token()
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::DivideEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Divide, start_pos)
                }
            },
            '%' => match self.peek() {
                '=' => {
                    self.advance2();
                    self.emit(Token::RemainderEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Remainder, start_pos)
                }
            },
            '&' => match self.peek() {
                '&' => {
                    self.advance2();
                    self.emit(Token::LogicalAnd, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::AndEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseAnd, start_pos)
                }
            },
            '|' => match self.peek() {
                '|' => {
                    self.advance2();
                    self.emit(Token::LogicalOr, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::OrEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseOr, start_pos)
                }
            },
            '?' => match self.peek() {
                '?' => {
                    self.advance2();
                    self.emit(Token::NullishCoalesce, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Question, start_pos)
                }
            },
            '^' => match self.peek() {
                '=' => {
                    self.advance2();
                    self.emit(Token::XorEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseXor, start_pos)
                }
            },
            '>' => match self.peek() {
                '>' => match self.peek2() {
                    '>' => match self.peek3() {
                        '=' => {
                            self.advance4();
                            self.emit(Token::ShiftRightLogicalEq, start_pos)
                        }
                        _ => {
                            self.advance3();
                            self.emit(Token::ShiftRightLogical, start_pos)
                        }
                    },
                    '=' => {
                        self.advance3();
                        self.emit(Token::ShiftRightArithmeticEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::ShiftRightArithmetic, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::GreaterThanOrEqual, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::GreaterThan, start_pos)
                }
            },
            '<' => match self.peek() {
                '<' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::ShiftLeftEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::ShiftLeft, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::LessThanOrEqual, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::LessThan, start_pos)
                }
            },
            '~' => {
                self.advance();
                self.emit(Token::BitwiseNot, start_pos)
            }
            '=' => match self.peek() {
                '=' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::EqEqEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::EqEq, start_pos)
                    }
                },
                '>' => {
                    self.advance2();
                    self.emit(Token::Arrow, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Equals, start_pos)
                }
            },
            '!' => match self.peek() {
                '=' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::NotEqEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::NotEq, start_pos)
                    }
                },
                _ => {
                    self.advance();
                    self.emit(Token::LogicalNot, start_pos)
                }
            },
            '(' => {
                self.advance();
                self.emit(Token::LeftParen, start_pos)
            }
            ')' => {
                self.advance();
                self.emit(Token::RightParen, start_pos)
            }
            '{' => {
                self.advance();
                self.emit(Token::LeftBrace, start_pos)
            }
            '}' => {
                self.advance();
                self.emit(Token::RightBrace, start_pos)
            }
            '[' => {
                self.advance();
                self.emit(Token::LeftBracket, start_pos)
            }
            ']' => {
                self.advance();
                self.emit(Token::RightBracket, start_pos)
            }
            ';' => {
                self.advance();
                self.emit(Token::Semicolon, start_pos)
            }
            ',' => {
                self.advance();
                self.emit(Token::Comma, start_pos)
            }
            '.' => {
                if is_decimal_digit(self.peek()) {
                    self.lex_decimal_literal()
                } else {
                    self.advance();
                    self.emit(Token::Period, start_pos)
                }
            }
            ':' => {
                self.advance();
                self.emit(Token::Colon, start_pos)
            }
            '#' => {
                self.advance();
                self.emit(Token::Hash, start_pos)
            }
            '0' => match self.peek() {
                'b' | 'B' => self.lex_binary_literal(),
                'o' | 'O' => self.lex_octal_literal(),
                'x' | 'X' => self.lex_hex_literal(),
                _ => self.lex_decimal_literal(),
            },
            '1'..='9' => self.lex_decimal_literal(),
            '"' | '\'' => self.lex_string_literal(),
            EOF_CHAR => self.emit(Token::Eof, start_pos),
            char if is_id_start_ascii(char) => self.lex_identifier_ascii(start_pos),
            // Escape sequence at the start of an identifier
            '\\' => {
                let code_point = self.lex_identifier_unicode_escape_sequence()?;
                if !is_id_start_ascii(code_point) && !is_id_start_unicode(code_point) {
                    let loc = self.mark_loc(start_pos);
                    return self.error(loc, ParseError::UnknownToken((code_point as char).into()));
                }

                self.lex_identifier_non_ascii(start_pos, code_point.into())
            }
            other => {
                if is_ascii(other) {
                    self.advance();
                    let loc = self.mark_loc(start_pos);
                    self.error(
                        loc,
                        ParseError::UnknownToken(((other as u8) as char).into()),
                    )
                } else {
                    let code_point = self.lex_utf8_codepoint()?;
                    if is_id_start_unicode(code_point) {
                        self.lex_identifier_non_ascii(start_pos, code_point.into())
                    } else {
                        let loc = self.mark_loc(start_pos);
                        self.error(loc, ParseError::UnknownToken((code_point as char).into()))
                    }
                }
            }
        }
    }

    fn skip_line_comment(&mut self) -> ParseResult<()> {
        loop {
            match self.current {
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                    return Ok(());
                }
                EOF_CHAR => return Ok(()),
                _ => {
                    self.lex_ascii_or_unicode_character()?;
                }
            }
        }
    }

    fn skip_block_comment(&mut self) -> ParseResult<()> {
        loop {
            match self.current {
                '*' => match self.peek() {
                    '/' => {
                        self.advance2();
                        break;
                    }
                    EOF_CHAR => {
                        let loc = self.mark_loc(self.pos + 1);
                        return self
                            .error(loc, ParseError::ExpectedToken(Token::Eof, Token::Divide));
                    }
                    _ => self.advance(),
                },
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                }
                EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::ExpectedToken(Token::Eof, Token::Multiply));
                }
                _ => {
                    self.lex_ascii_or_unicode_character()?;
                }
            }
        }

        Ok(())
    }

    fn error_invalid_unicode(&mut self, start_pos: Pos) -> ParseResult<char> {
        let loc = self.mark_loc(start_pos);
        return self.error(loc, ParseError::InvalidUnicode);
    }

    // Lex a non-ascii unicode codepoint encoded as utf-8
    fn lex_utf8_codepoint(&mut self) -> ParseResult<char> {
        let bytes = self.buf[self.pos..].as_bytes();
        let b1 = bytes[0];

        if (b1 & 0xE0) == 0xC0 && self.pos + 1 < self.buf.len() {
            // Two byte sequence
            self.advance2();

            let b2 = bytes[1];
            if !is_continuation_byte(b2) {
                return self.error_invalid_unicode(self.pos - 2);
            }

            let mut codepoint = (b1 as u32 & 0x1F) << 6;
            codepoint |= b2 as u32 & 0x3F;

            Ok(unsafe { char::from_u32_unchecked(codepoint) })
        } else if (b1 & 0xF0) == 0xE0 && self.pos + 2 < self.buf.len() {
            // Three byte sequence
            self.advance3();

            let b2 = bytes[1];
            let b3 = bytes[2];
            if !is_continuation_byte(b2) || !is_continuation_byte(b3) {
                return self.error_invalid_unicode(self.pos - 3);
            }

            let mut codepoint = (b1 as u32 & 0x0F) << 12;
            codepoint |= (b2 as u32 & 0x3F) << 6;
            codepoint |= b3 as u32 & 0x3F;

            // Char could be in the surrogate pair range, 0xD800 - 0xDFFF, which is not considered
            // a valid code point.
            return match char::from_u32(codepoint) {
                None => self.error_invalid_unicode(self.pos - 3),
                Some(char) => Ok(char),
            };
        } else if (b1 & 0xF8) == 0xF0 && self.pos + 3 < self.buf.len() {
            // Four byte sequence
            self.advance4();

            let b2 = bytes[1];
            let b3 = bytes[2];
            let b4 = bytes[3];
            if !is_continuation_byte(b2) || !is_continuation_byte(b3) || !is_continuation_byte(b4) {
                return self.error_invalid_unicode(self.pos - 4);
            }

            let mut codepoint = (b1 as u32 & 0x07) << 18;
            codepoint |= (b2 as u32 & 0x3F) << 12;
            codepoint |= (b3 as u32 & 0x3F) << 6;
            codepoint |= b4 as u32 & 0x3F;

            // Char could be above the code point max, 0x10FFFF
            return match char::from_u32(codepoint) {
                None => self.error_invalid_unicode(self.pos - 4),
                Some(char) => Ok(char),
            };
        } else {
            self.advance();
            return self.error_invalid_unicode(self.pos);
        }
    }

    fn skip_decimal_digits(&mut self) {
        while is_decimal_digit(self.current) {
            self.advance();
        }
    }

    fn lex_decimal_literal(&mut self) -> LexResult {
        let start_pos = self.pos;

        // Read optional digits before the decimal point
        self.skip_decimal_digits();

        // Read optional decimal point with its optional following digits
        if self.current == '.' {
            self.advance();
            self.skip_decimal_digits();
        }

        // Read optional exponent
        if self.current == 'e' || self.current == 'E' {
            self.advance();

            // Exponent has optional sign
            if self.current == '-' || self.current == '+' {
                self.advance();
            }

            if !is_decimal_digit(self.current) {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedNumericLiteral);
            }

            self.skip_decimal_digits();
        }

        // Parse float using rust stdlib
        let end_pos = self.pos;
        let value = f64::from_str(&self.buf[start_pos..end_pos]).unwrap();

        self.emit(Token::NumberLiteral(value), start_pos)
    }

    fn lex_binary_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_binary_value(self.current) {
            value <<= 1;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_octal_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_octal_value(self.current) {
            value <<= 3;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_hex_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_hex_value(self.current) {
            value <<= 4;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_string_literal(&mut self) -> LexResult {
        let quote_char = self.current;
        let start_pos = self.pos;
        self.advance();

        let mut value = String::new();

        while self.current != quote_char {
            match self.current {
                // Escape sequences
                '\\' => match self.peek() {
                    // Single character escapes
                    'n' => {
                        value.push('\n');
                        self.advance2()
                    }
                    '\\' => {
                        value.push('\\');
                        self.advance2()
                    }
                    '\'' => {
                        value.push('\'');
                        self.advance2()
                    }
                    '"' => {
                        value.push('"');
                        self.advance2()
                    }
                    't' => {
                        value.push('\t');
                        self.advance2()
                    }
                    'r' => {
                        value.push('\r');
                        self.advance2()
                    }
                    'b' => {
                        value.push('\x08');
                        self.advance2()
                    }
                    'v' => {
                        value.push('\x0B');
                        self.advance2()
                    }
                    'f' => {
                        value.push('\x0C');
                        self.advance2()
                    }
                    '0' if self.peek2() < '0' || self.peek2() > '9' => {
                        value.push('\x00');
                        self.advance2()
                    }
                    // Hex escape sequence
                    'x' => {
                        self.advance2();

                        if let Some(x1) = get_hex_value(self.current) {
                            if let Some(x2) = get_hex_value(self.peek()) {
                                let escaped_char = std::char::from_u32(x1 * 16 + x2).unwrap();
                                value.push(escaped_char);
                                self.advance2();
                            } else {
                                let loc = self.mark_loc(self.pos);
                                self.advance();
                                return self.error(loc, ParseError::MalformedEscapeSeqence);
                            }
                        } else {
                            let loc = self.mark_loc(self.pos);
                            return self.error(loc, ParseError::MalformedEscapeSeqence);
                        }
                    }
                    // Unicode escape sequence
                    'u' => {
                        let escape_start_pos = self.pos;
                        self.advance2();
                        let code_point = self.lex_unicode_escape_sequence(escape_start_pos)?;
                        value.push(code_point)
                    }
                    // Line continuations, either LF, CR, or CRLF
                    '\n' => {
                        value.push('\n');
                        self.advance2()
                    }
                    '\r' => {
                        value.push('\r');
                        self.advance2();

                        if self.current == '\n' {
                            value.push('\n');
                            self.advance()
                        }
                    }
                    // Not an escape sequence, use '/' directly
                    _ => {
                        value.push('/');
                        self.advance()
                    }
                },
                // Unterminated string literal
                '\n' | EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::UnterminatedStringLiteral);
                }
                quote if quote == quote_char => break,
                _ => {
                    value.push(self.lex_ascii_or_unicode_character()?);
                }
            }
        }

        self.advance();

        return self.emit(Token::StringLiteral(value), start_pos);
    }

    #[inline]
    fn lex_ascii_or_unicode_character(&mut self) -> ParseResult<char> {
        if is_ascii(self.current) {
            let ascii_char = self.current;
            self.advance();
            Ok(ascii_char)
        } else {
            let code_point = self.lex_utf8_codepoint()?;
            Ok(code_point)
        }
    }

    // Lex a single unicode escape sequence, called after the '\u' prefix has already been processed
    fn lex_unicode_escape_sequence(&mut self, start_pos: Pos) -> ParseResult<char> {
        // Escape sequence has form \u{HEX_DIGITS}, with at most 6 hex digits
        if self.current == '{' {
            self.advance();

            if self.current == '}' {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            let mut value = 0;
            for _ in 0..6 {
                if let Some(hex_value) = get_hex_value(self.current) {
                    self.advance();
                    value <<= 4;
                    value += hex_value;
                } else if self.current == '}' {
                    break;
                } else {
                    let loc = self.mark_loc(start_pos);
                    return self.error(loc, ParseError::MalformedEscapeSeqence);
                }
            }

            if self.current != '}' {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            self.advance();

            // Check that value is not out of range (greater than \u10FFFF)
            if let Some(code_point) = std::char::from_u32(value) {
                return Ok(code_point);
            } else {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }
        }

        // Otherwise this is \uXXXX so expect exactly four hex digits
        let mut value = 0;
        for _ in 0..4 {
            if let Some(hex_value) = get_hex_value(self.current) {
                self.advance();
                value <<= 4;
                value += hex_value;
            } else {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }
        }

        let code_point = unsafe { std::char::from_u32_unchecked(value) };
        Ok(code_point)
    }

    // Fast path for lexing a purely ASCII identifier
    fn lex_identifier_ascii(&mut self, start_pos: Pos) -> LexResult {
        self.advance();

        loop {
            if is_id_continue_ascii(self.current) {
                self.advance();
                continue;
            } else if self.current == '\\' || is_id_continue_unicode(self.current) {
                // Start of an escape sequence so bail to slow path, copying over ASCII string that
                // has been created so far.
                let string_builder = String::from(&self.buf[start_pos..self.pos]);
                return self.lex_identifier_non_ascii(start_pos, string_builder);
            } else {
                break;
            }
        }

        let id_string = &self.buf[start_pos..self.pos];

        if let Some(keyword_token) = self.id_to_keyword(id_string) {
            self.emit(keyword_token, start_pos)
        } else {
            self.emit(Token::Identifier(String::from(id_string)), start_pos)
        }
    }

    // Slow path for lexing an identifier with at least one unicode character or escape sequence.
    // Input the string that has been created so far before falling back to this slow path.
    fn lex_identifier_non_ascii(
        &mut self,
        start_pos: Pos,
        mut string_builder: String,
    ) -> LexResult {
        loop {
            // Check if ASCII
            if is_ascii(self.current) {
                if is_id_continue_ascii(self.current) {
                    string_builder.push(self.current);
                    self.advance();
                } else if self.current == '\\' {
                    let code_point = self.lex_identifier_unicode_escape_sequence()?;
                    if !is_id_continue_ascii(code_point) && !is_id_continue_unicode(code_point) {
                        let loc = self.mark_loc(self.pos);
                        return self.error(loc, ParseError::UnknownToken(code_point.into()));
                    }

                    string_builder.push(code_point);
                } else {
                    break;
                }
            } else {
                // Otherwise must be a utf-8 encoded codepoint
                let code_point = self.lex_utf8_codepoint()?;
                if is_id_continue_unicode(code_point) {
                    string_builder.push(code_point);
                } else {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::UnknownToken(code_point.into()));
                }
            }
        }

        // Escape characters can be used in keywords
        if let Some(keyword_token) = self.id_to_keyword(&string_builder) {
            self.emit(keyword_token, start_pos)
        } else {
            self.emit(Token::Identifier(string_builder), start_pos)
        }
    }

    fn lex_identifier_unicode_escape_sequence(&mut self) -> ParseResult<char> {
        let escape_start_pos = self.pos;
        self.advance();

        if self.current == 'u' {
            self.advance();

            let code_point = self.lex_unicode_escape_sequence(escape_start_pos)?;
            Ok(code_point)
        } else {
            let loc = self.mark_loc(escape_start_pos);
            self.error(loc, ParseError::MalformedEscapeSeqence)
        }
    }

    fn id_to_keyword(&mut self, id_string: &str) -> Option<Token> {
        match id_string {
            "var" => Some(Token::Var),
            "let" => Some(Token::Let),
            "const" => Some(Token::Const),
            "function" => Some(Token::Function),
            "async" => Some(Token::Async),
            "this" => Some(Token::This),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "switch" => Some(Token::Switch),
            "case" => Some(Token::Case),
            "default" => Some(Token::Default),
            "for" => Some(Token::For),
            "of" => Some(Token::Of),
            "while" => Some(Token::While),
            "do" => Some(Token::Do),
            "with" => Some(Token::With),
            "return" => Some(Token::Return),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            "try" => Some(Token::Try),
            "catch" => Some(Token::Catch),
            "finally" => Some(Token::Finally),
            "throw" => Some(Token::Throw),
            "null" => Some(Token::Null),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "in" => Some(Token::In),
            "instanceof" => Some(Token::InstanceOf),
            "new" => Some(Token::New),
            "typeof" => Some(Token::Typeof),
            "void" => Some(Token::Void),
            "delete" => Some(Token::Delete),
            "debugger" => Some(Token::Debugger),
            "static" => Some(Token::Static),
            "from" => Some(Token::From),
            "as" => Some(Token::As),
            "class" => Some(Token::Class),
            "extends" => Some(Token::Extends),
            "super" => Some(Token::Super),
            "get" => Some(Token::Get),
            "set" => Some(Token::Set),
            _ => None,
        }
    }
}
