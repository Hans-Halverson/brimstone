use std::rc::Rc;
use std::str::FromStr;

use brimstone_macros::match_u32;
use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::js::common::unicode::{
    as_id_part, as_id_part_ascii, as_id_part_unicode, as_id_start, as_id_start_unicode,
    decode_wtf8_codepoint, get_binary_value, get_hex_value, get_octal_value, is_ascii,
    is_ascii_newline, is_ascii_whitespace, is_decimal_digit, is_id_part_ascii, is_id_part_unicode,
    is_id_start_ascii, is_id_start_unicode, is_in_unicode_range, is_newline, is_unicode_newline,
    is_unicode_whitespace, to_string_or_unicode_escape_sequence, CodePoint,
};
use crate::js::common::wtf_8::Wtf8Str;

use super::ast::{AstAlloc, AstString};
use super::loc::{Loc, Pos};
use super::parse_error::{LocalizedParseError, ParseError, ParseResult};
use super::source::Source;
use super::token::Token;

pub struct Lexer<'a> {
    pub source: &'a Rc<Source>,
    buf: &'a [u8],
    current: u32,
    pos: Pos,
    is_new_line_before_current: bool,
    pub in_strict_mode: bool,
    pub allow_hashbang_comment: bool,
    alloc: AstAlloc<'a>,
}

/// A save point for the lexer, can be used to restore the lexer to a particular position.
pub struct SavedLexerState {
    current: u32,
    pos: Pos,
}

type LexResult<'a> = ParseResult<(Token<'a>, Loc)>;

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: u32 = 0x110000;

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Rc<Source>, alloc: AstAlloc<'a>) -> Lexer<'a> {
        let buf = source.contents.as_bytes();
        let current = if buf.is_empty() {
            EOF_CHAR
        } else {
            buf[0].into()
        };

        Lexer {
            source,
            buf,
            current,
            pos: 0,
            is_new_line_before_current: false,
            in_strict_mode: false,
            allow_hashbang_comment: true,
            alloc,
        }
    }

    pub fn save(&self) -> SavedLexerState {
        SavedLexerState { current: self.current, pos: self.pos }
    }

    pub fn restore(&mut self, save_state: &SavedLexerState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }

    pub fn is_new_line_before_current(&self) -> bool {
        self.is_new_line_before_current
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> u32 {
        self.buf[index].into()
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
    fn peek_n(&self, n: usize) -> u32 {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.code_point_at(next_pos)
        } else {
            EOF_CHAR
        }
    }

    fn peek(&mut self) -> u32 {
        Lexer::peek_n(self, 1)
    }

    fn peek2(&mut self) -> u32 {
        Lexer::peek_n(self, 2)
    }

    fn peek3(&mut self) -> u32 {
        Lexer::peek_n(self, 3)
    }

    #[inline]
    fn eat(&mut self, char: char) -> bool {
        if self.current == char as u32 {
            self.advance();
            true
        } else {
            false
        }
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc { start: start_pos, end: self.pos }
    }

    fn emit<'b>(&self, token: Token<'b>, start_pos: Pos) -> LexResult<'b> {
        Ok((token, self.mark_loc(start_pos)))
    }

    fn localized_parse_error(&self, loc: Loc, error: ParseError) -> LocalizedParseError {
        let source = (*self.source).clone();
        LocalizedParseError { error, source_loc: Some((loc, source)) }
    }

    fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        Err(self.localized_parse_error(loc, error))
    }

    pub fn next(&mut self) -> LexResult<'a> {
        self.is_new_line_before_current = false;
        loop {
            // Fast pass for skipping ASCII whitespace and newlines
            loop {
                if is_ascii(self.current) {
                    if is_ascii_whitespace(self.current) {
                        self.advance();
                    } else if is_ascii_newline(self.current) {
                        self.is_new_line_before_current = true;
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            let start_pos = self.pos;

            return match_u32!(match self.current {
                '+' => match_u32!(match self.peek() {
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
                }),
                '-' => match_u32!(match self.peek() {
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
                }),
                '*' => match_u32!(match self.peek() {
                    '*' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::ExponentEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::Exponent, start_pos)
                        }
                    }),
                    '=' => {
                        self.advance2();
                        self.emit(Token::MultiplyEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::Multiply, start_pos)
                    }
                }),
                '/' => match_u32!(match self.peek() {
                    '/' => {
                        self.advance2();
                        self.skip_line_comment()?;
                        continue;
                    }
                    '*' => {
                        self.advance2();
                        self.skip_block_comment()?;
                        continue;
                    }
                    '=' => {
                        self.advance2();
                        self.emit(Token::DivideEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::Divide, start_pos)
                    }
                }),
                '%' => match_u32!(match self.peek() {
                    '=' => {
                        self.advance2();
                        self.emit(Token::RemainderEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::Remainder, start_pos)
                    }
                }),
                '&' => match_u32!(match self.peek() {
                    '&' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::LogicalAndEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::LogicalAnd, start_pos)
                        }
                    }),
                    '=' => {
                        self.advance2();
                        self.emit(Token::AndEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::BitwiseAnd, start_pos)
                    }
                }),
                '|' => match_u32!(match self.peek() {
                    '|' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::LogicalOrEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::LogicalOr, start_pos)
                        }
                    }),
                    '=' => {
                        self.advance2();
                        self.emit(Token::OrEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::BitwiseOr, start_pos)
                    }
                }),
                '?' => match_u32!(match self.peek() {
                    '?' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::NullishCoalesceEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::NullishCoalesce, start_pos)
                        }
                    }),
                    // ?.d is parsed as a question mark followed by a decimal literal
                    '.' if !is_decimal_digit(self.peek2()) => {
                        self.advance2();
                        self.emit(Token::QuestionDot, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::Question, start_pos)
                    }
                }),
                '^' => match_u32!(match self.peek() {
                    '=' => {
                        self.advance2();
                        self.emit(Token::XorEq, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::BitwiseXor, start_pos)
                    }
                }),
                '>' => match_u32!(match self.peek() {
                    '>' => match_u32!(match self.peek2() {
                        '>' => match_u32!(match self.peek3() {
                            '=' => {
                                self.advance4();
                                self.emit(Token::ShiftRightLogicalEq, start_pos)
                            }
                            _ => {
                                self.advance3();
                                self.emit(Token::ShiftRightLogical, start_pos)
                            }
                        }),
                        '=' => {
                            self.advance3();
                            self.emit(Token::ShiftRightArithmeticEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::ShiftRightArithmetic, start_pos)
                        }
                    }),
                    '=' => {
                        self.advance2();
                        self.emit(Token::GreaterThanOrEqual, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::GreaterThan, start_pos)
                    }
                }),
                '<' => match_u32!(match self.peek() {
                    '<' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::ShiftLeftEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::ShiftLeft, start_pos)
                        }
                    }),
                    '=' => {
                        self.advance2();
                        self.emit(Token::LessThanOrEqual, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::LessThan, start_pos)
                    }
                }),
                '~' => {
                    self.advance();
                    self.emit(Token::BitwiseNot, start_pos)
                }
                '=' => match_u32!(match self.peek() {
                    '=' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::EqEqEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::EqEq, start_pos)
                        }
                    }),
                    '>' => {
                        self.advance2();
                        self.emit(Token::Arrow, start_pos)
                    }
                    _ => {
                        self.advance();
                        self.emit(Token::Equals, start_pos)
                    }
                }),
                '!' => match_u32!(match self.peek() {
                    '=' => match_u32!(match self.peek2() {
                        '=' => {
                            self.advance3();
                            self.emit(Token::NotEqEq, start_pos)
                        }
                        _ => {
                            self.advance2();
                            self.emit(Token::NotEq, start_pos)
                        }
                    }),
                    _ => {
                        self.advance();
                        self.emit(Token::LogicalNot, start_pos)
                    }
                }),
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
                    let next_char = self.peek();
                    if next_char == '.' as u32 && self.peek2() == '.' as u32 {
                        self.advance3();
                        self.emit(Token::Spread, start_pos)
                    } else if is_decimal_digit(next_char) {
                        let token = self.lex_decimal_literal()?;
                        self.error_if_cannot_follow_numeric_literal()?;
                        Ok(token)
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
                    // Parse hashbang comment if it starts at the first byte in the file
                    if self.pos == 0 && self.peek() == '!' as u32 && self.allow_hashbang_comment {
                        self.advance2();
                        self.skip_line_comment()?;
                        continue;
                    }

                    self.advance();
                    self.emit(Token::Hash, start_pos)
                }
                '0' => {
                    let token = match_u32!(match self.peek() {
                        'b' | 'B' => self.lex_binary_literal()?,
                        'o' | 'O' => self.lex_octal_literal()?,
                        'x' | 'X' => self.lex_hex_literal()?,
                        '0'..='9' if self.in_strict_mode => {
                            self.advance();
                            let loc = self.mark_loc(start_pos);
                            return self.error(loc, ParseError::LegacyOctalLiteralInStrictMode);
                        }
                        '0'..='9' => {
                            let start_pos = self.pos;
                            if let Some(token) = self.lex_legacy_octal_literal() {
                                self.emit(token, start_pos)?
                            } else {
                                self.lex_decimal_literal()?
                            }
                        }
                        _ => self.lex_decimal_literal()?,
                    });

                    self.error_if_cannot_follow_numeric_literal()?;
                    Ok(token)
                }
                '1'..='9' => {
                    let token = self.lex_decimal_literal()?;
                    self.error_if_cannot_follow_numeric_literal()?;
                    Ok(token)
                }
                '"' | '\'' => self.lex_string_literal(),
                '`' => {
                    let start_pos = self.pos;
                    self.advance();
                    self.lex_template_literal(start_pos, true)
                }
                EOF_CHAR => self.emit(Token::Eof, start_pos),
                char if is_id_start_ascii(char) => self.lex_identifier_ascii(start_pos),
                // Escape sequence at the start of an identifier
                '\\' => {
                    let code_point = self.lex_identifier_unicode_escape_sequence()?;

                    if let Some(char) = as_id_start(code_point) {
                        let string = AstString::from_char_in(char, self.alloc);
                        self.lex_identifier_non_ascii(start_pos, string)
                    } else {
                        let loc = self.mark_loc(start_pos);
                        let code_point_string = to_string_or_unicode_escape_sequence(code_point);
                        return self.error(loc, ParseError::new_unknown_token(code_point_string));
                    }
                }
                other => {
                    if is_ascii(other) {
                        self.advance();
                        let loc = self.mark_loc(start_pos);
                        self.error(
                            loc,
                            ParseError::new_unknown_token(((other as u8) as char).into()),
                        )
                    } else {
                        let code_point = self.lex_utf8_codepoint()?;
                        if let Some(char) = as_id_start_unicode(code_point) {
                            let string = AstString::from_char_in(char, self.alloc);
                            self.lex_identifier_non_ascii(start_pos, string)
                        } else if is_unicode_whitespace(code_point) {
                            continue;
                        } else if is_unicode_newline(code_point) {
                            self.is_new_line_before_current = true;
                            continue;
                        } else {
                            let loc = self.mark_loc(start_pos);
                            let code_point_string =
                                to_string_or_unicode_escape_sequence(code_point);
                            self.error(loc, ParseError::new_unknown_token(code_point_string))
                        }
                    }
                }
            });
        }
    }

    fn skip_line_comment(&mut self) -> ParseResult<()> {
        loop {
            match_u32!(match self.current {
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                    return Ok(());
                }
                EOF_CHAR => return Ok(()),
                other => {
                    if is_ascii(other) {
                        self.advance()
                    } else {
                        let code_point = self.lex_utf8_codepoint()?;
                        if is_unicode_newline(code_point) {
                            self.is_new_line_before_current = true;
                            return Ok(());
                        }
                    }
                }
            })
        }
    }

    fn skip_block_comment(&mut self) -> ParseResult<()> {
        loop {
            match_u32!(match self.current {
                '*' => match_u32!(match self.peek() {
                    '/' => {
                        self.advance2();
                        break;
                    }
                    EOF_CHAR => {
                        let loc = self.mark_loc(self.pos + 1);
                        return self
                            .error(loc, ParseError::new_expected_token(Token::Eof, Token::Divide));
                    }
                    _ => self.advance(),
                }),
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                }
                EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self
                        .error(loc, ParseError::new_expected_token(Token::Eof, Token::Multiply));
                }
                other => {
                    if is_ascii(other) {
                        self.advance()
                    } else {
                        let code_point = self.lex_utf8_codepoint()?;
                        if is_unicode_newline(code_point) {
                            self.is_new_line_before_current = true;
                        }
                    }
                }
            })
        }

        Ok(())
    }

    /// Skip a series of decimal digits, possibly separated by numeric separators. Numeric separators
    /// must be adjacent to a numeric digit on both sides.
    ///
    /// Return whether any numeric separator was encountered.
    fn skip_decimal_digits(&mut self, allow_numeric_separator: bool) -> ParseResult<bool> {
        // First digit must be a decimal digit
        if !is_decimal_digit(self.current) {
            return Ok(false);
        }

        self.advance();

        // Middle digits may be decimal numbers or numeric separators
        let mut has_numeric_separator = false;
        let mut is_last_char_numeric_separator = false;

        loop {
            is_last_char_numeric_separator = if is_decimal_digit(self.current) {
                false
            } else if self.current == '_' as u32 && allow_numeric_separator {
                if is_last_char_numeric_separator {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::AdjacentNumericSeparators);
                }

                has_numeric_separator = true;

                true
            } else {
                break;
            };

            self.advance()
        }

        // Last digit cannot be a separator
        if is_last_char_numeric_separator {
            let loc = self.mark_loc(self.pos - 1);
            return self.error(loc, ParseError::TrailingNumericSeparator);
        }

        Ok(has_numeric_separator)
    }

    fn lex_decimal_literal(&mut self) -> LexResult<'static> {
        let start_pos = self.pos;
        let mut has_numeric_separator = false;

        let has_leading_zero = self.current == '0' as u32;
        let allow_numeric_separator = !has_leading_zero;

        // Read optional digits before the decimal point
        has_numeric_separator |= self.skip_decimal_digits(allow_numeric_separator)?;

        // This is a bigint literal
        if self.current == 'n' as u32 {
            let digits_slice = &self.buf[start_pos..self.pos];
            let value = BigInt::parse_bytes(digits_slice, 10).unwrap();
            self.advance();

            // BigInts do not allow a leading zeros
            if has_leading_zero && self.pos - 2 != start_pos {
                let loc = self.mark_loc(self.pos);
                return self.error(loc, ParseError::BigIntLeadingZero);
            }

            return self.emit(Token::BigIntLiteral(value), start_pos);
        }

        // Read optional decimal point with its optional following digits
        if self.current == '.' as u32 {
            self.advance();
            has_numeric_separator |= self.skip_decimal_digits(allow_numeric_separator)?;
        }

        // Read optional exponent
        if self.current == 'e' as u32 || self.current == 'E' as u32 {
            self.advance();

            // Exponent has optional sign
            if self.current == '-' as u32 || self.current == '+' as u32 {
                self.advance();
            }

            if !is_decimal_digit(self.current) {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedNumericLiteral);
            }

            has_numeric_separator |= self.skip_decimal_digits(allow_numeric_separator)?;
        }

        // Parse float using rust stdlib. Rust stdlib cannot handle numeric separators, so if there
        // were numeric separators then first generate string with numeric separators removed. It is
        // safe to treat this slice as valid UTF-8 as it only contains ASCII characters.
        let end_pos = self.pos;
        let string = unsafe { std::str::from_utf8_unchecked(&self.buf[start_pos..end_pos]) };

        let value = if has_numeric_separator {
            f64::from_str(&string.replace('_', "")).unwrap()
        } else {
            f64::from_str(string).unwrap()
        };

        self.emit(Token::NumberLiteral(value), start_pos)
    }

    #[inline]
    fn lex_literal_with_base(
        &mut self,
        base: u32,
        char_to_digit: fn(u32) -> Option<u32>,
    ) -> LexResult<'static> {
        let start_pos = self.pos;
        self.advance2();

        let mut value: u64;
        let mut overflows_u64 = false;

        // First digit must be a binary digit
        if let Some(digit) = char_to_digit(self.current) {
            value = digit as u64;
            self.advance()
        } else {
            let loc = self.mark_loc(start_pos);
            return self.error(loc, ParseError::MalformedNumericLiteral);
        }

        // Middle digits may be binary numbers or numeric separators
        let mut is_last_char_numeric_separator = false;
        loop {
            is_last_char_numeric_separator = if let Some(digit) = char_to_digit(self.current) {
                // Try to apply multiplication by base, marking if it overflows
                if let Some(new_value) = value.checked_mul(base as u64) {
                    value = new_value;
                } else {
                    overflows_u64 = true;
                };

                // Try to apply addition of digit, marking if it overflows
                if let Some(new_value) = value.checked_add(digit as u64) {
                    value = new_value;
                } else {
                    overflows_u64 = true;
                };

                false
            } else if self.current == '_' as u32 {
                if is_last_char_numeric_separator {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::AdjacentNumericSeparators);
                }

                true
            } else {
                break;
            };

            self.advance()
        }

        // Last digit cannot be a separator
        if is_last_char_numeric_separator {
            let loc = self.mark_loc(self.pos - 1);
            return self.error(loc, ParseError::TrailingNumericSeparator);
        }

        // A `n` suffix indicates a BigInt literal
        if self.current == 'n' as u32 {
            let digits_slice = &self.buf[(start_pos + 2)..self.pos];
            let value = BigInt::parse_bytes(digits_slice, base).unwrap();
            self.advance();

            return self.emit(Token::BigIntLiteral(value), start_pos);
        }

        // If the value overflows then we must first reparse to a BigInt, then convert to an f64
        if overflows_u64 {
            let digits_slice = &self.buf[(start_pos + 2)..self.pos];
            let bigint_value = BigInt::parse_bytes(digits_slice, base).unwrap();

            // Never returns None, as BigInt -> float conversion goes to infinity instead of failing
            let value = bigint_value.to_f64().unwrap();
            return self.emit(Token::NumberLiteral(value), start_pos);
        }

        self.emit(Token::NumberLiteral(value as f64), start_pos)
    }

    fn lex_binary_literal(&mut self) -> LexResult<'static> {
        self.lex_literal_with_base(2, get_binary_value)
    }

    fn lex_octal_literal(&mut self) -> LexResult<'static> {
        self.lex_literal_with_base(8, get_octal_value)
    }

    fn lex_hex_literal(&mut self) -> LexResult<'static> {
        self.lex_literal_with_base(16, get_hex_value)
    }

    fn lex_legacy_octal_literal(&mut self) -> Option<Token<'static>> {
        let save_state = self.save();
        let start_pos = self.pos;

        let mut value: u64 = 0;
        let mut overflows_u64 = false;

        while let Some(digit) = get_octal_value(self.current) {
            // Try to apply multiplication by base, marking if it overflows
            if let Some(new_value) = value.checked_mul(8) {
                value = new_value;
            } else {
                overflows_u64 = true;
            };

            // Try to apply addition of digit, marking if it overflows
            if let Some(new_value) = value.checked_add(digit as u64) {
                value = new_value;
            } else {
                overflows_u64 = true;
            };

            self.advance();
        }

        // Reparse as decimal literal if we encounter a digit outside hex range
        if self.current == '8' as u32 || self.current == '9' as u32 {
            self.restore(&save_state);
            return None;
        }

        // If the value overflows then we must first reparse to a BigInt, then convert to an f64
        if overflows_u64 {
            let digits_slice = &self.buf[start_pos..self.pos];
            let bigint_value = BigInt::parse_bytes(digits_slice, 8).unwrap();

            // Never returns None, as BigInt -> float conversion goes to infinity instead of failing
            let value = bigint_value.to_f64().unwrap();
            return Some(Token::NumberLiteral(value));
        }

        Some(Token::NumberLiteral(value as f64))
    }

    fn error_if_cannot_follow_numeric_literal(&mut self) -> ParseResult<()> {
        let start_pos = self.pos;

        let cannot_follow_numeric_literal;
        let end_pos;

        if is_ascii(self.current) {
            cannot_follow_numeric_literal =
                is_id_start_ascii(self.current) || is_decimal_digit(self.current);
            end_pos = self.pos + 1;
        } else if self.current == EOF_CHAR {
            cannot_follow_numeric_literal = false;
            end_pos = self.pos;
        } else {
            // Peek at next code point
            let save_state = self.save();
            let code_point = self.lex_utf8_codepoint()?;

            cannot_follow_numeric_literal = is_id_start_unicode(code_point);
            end_pos = self.pos;

            self.restore(&save_state);
        }

        if cannot_follow_numeric_literal {
            let loc = Loc { start: start_pos, end: end_pos };
            self.error(loc, ParseError::InvalidNumericLiteralNextChar)
        } else {
            Ok(())
        }
    }

    fn lex_string_literal(&mut self) -> LexResult<'a> {
        let quote_char = self.current;
        let quote_start_pos = self.pos;
        self.advance();

        let content_start_pos = self.pos;

        // Fast path until an escape is encountered
        loop {
            match_u32!(match self.current {
                // If we find the end of the string then directly return a slice of the underlying
                // buffer.
                quote if quote == quote_char => {
                    let value =
                        Wtf8Str::from_bytes_unchecked(&self.buf[content_start_pos..self.pos]);

                    self.advance();

                    return self.emit(Token::StringLiteral(value), quote_start_pos);
                }
                // Break out to slow path if an escape sequence is encountered
                '\\' => break,
                // Check for unterminated string literals
                '\n' | '\r' | EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::UnterminatedStringLiteral);
                }
                // Otherwise skip the next character, performing UTF-8 validation
                _ => {
                    let _ = self.lex_ascii_or_unicode_character()?;
                }
            })
        }

        // Must build our own string since escaped characters exist. Can copy in the slice of the
        // string up to the first escape sequence.
        let mut value =
            AstString::from_bytes_unchecked_in(&self.buf[content_start_pos..self.pos], self.alloc);

        loop {
            match_u32!(match self.current {
                // Escape sequences
                '\\' => match_u32!(match self.peek() {
                    // Single character escapes
                    'n' => {
                        value.push_char('\n');
                        self.advance2()
                    }
                    '\\' => {
                        value.push_char('\\');
                        self.advance2()
                    }
                    '\'' => {
                        value.push_char('\'');
                        self.advance2()
                    }
                    '"' => {
                        value.push_char('"');
                        self.advance2()
                    }
                    't' => {
                        value.push_char('\t');
                        self.advance2()
                    }
                    'r' => {
                        value.push_char('\r');
                        self.advance2()
                    }
                    'b' => {
                        value.push_char('\x08');
                        self.advance2()
                    }
                    'v' => {
                        value.push_char('\x0B');
                        self.advance2()
                    }
                    'f' => {
                        value.push_char('\x0C');
                        self.advance2()
                    }
                    // Null character escape
                    '0' if !is_decimal_digit(self.peek2()) => {
                        value.push_char('\x00');
                        self.advance2()
                    }
                    // Legacy octal escape
                    first_digit @ ('0'..='7') => {
                        let start_pos = self.pos;
                        self.advance();

                        let mut octal_value = get_octal_value(first_digit).unwrap();
                        self.advance();

                        if let Some(next_digit) = get_octal_value(self.current) {
                            octal_value *= 8;
                            octal_value += next_digit;
                            self.advance();
                        }

                        if first_digit <= '3' as u32 {
                            if let Some(next_digit) = get_octal_value(self.current) {
                                octal_value *= 8;
                                octal_value += next_digit;
                                self.advance();
                            }
                        }

                        if self.in_strict_mode {
                            let loc = self.mark_loc(start_pos);
                            return self
                                .error(loc, ParseError::LegacyOctalEscapeSequenceInStrictMode);
                        }

                        value.push(octal_value)
                    }
                    // Legacy non-octal escape
                    code_point @ ('8' | '9') => {
                        self.advance2();

                        if self.in_strict_mode {
                            let loc = self.mark_loc(self.pos);
                            return self
                                .error(loc, ParseError::LegacyNonOctalEscapeSequenceInStrictMode);
                        }

                        value.push(code_point)
                    }
                    // Hex escape sequence
                    'x' => {
                        self.advance2();

                        if let Some(x1) = get_hex_value(self.current) {
                            if let Some(x2) = get_hex_value(self.peek()) {
                                // Safe to use without checking if in code point range since we
                                // know there are only two hex digits.
                                let escaped_code_point = x1 * 16 + x2;
                                value.push(escaped_code_point);
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
                    // Line continuations, either LF, CR, or CRLF. Ignored in string value.
                    '\n' => self.advance2(),
                    '\r' => {
                        self.advance2();

                        self.eat('\n');
                    }
                    // Non-escape character, use character directly
                    other => {
                        if is_ascii(other) {
                            self.advance2();
                            value.push(other);
                        } else {
                            self.advance();
                            let code_point = self.lex_utf8_codepoint()?;

                            // Unicode line continuations are ignored in string value
                            if !is_unicode_newline(code_point) {
                                value.push(code_point);
                            }
                        }
                    }
                }),
                // If we find the end of the string then return the newly allocated string
                quote if quote == quote_char => {
                    self.advance();
                    return self
                        .emit(Token::StringLiteral(value.into_arena_str()), quote_start_pos);
                }
                // Unterminated string literal
                '\n' | '\r' | EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::UnterminatedStringLiteral);
                }
                // Otherwise add
                _ => value.push(self.lex_ascii_or_unicode_character()?),
            })
        }
    }

    // Lex a regexp literal. Must be called when the previously lexed token was either a '/' or '/='
    pub fn next_regexp_literal(&mut self) -> LexResult<'a> {
        let start_pos = if self.code_point_at(self.pos - 1) == '/' as u32 {
            let start_pos = self.pos - 1;

            // RegularExpressionFirstChar
            self.lex_regex_character(false)?;

            start_pos
        } else {
            // First pattern character is a '='
            self.pos - 2
        };

        let pattern_start_pos = start_pos + 1;

        // RegularExpressionChars
        while self.current != '/' as u32 {
            self.lex_regex_character(false)?;
        }

        let pattern_end_pos = self.pos;

        self.advance();

        // Consume optional flags, which are IdentifierPartChars
        let flags_start_pos = self.pos;

        loop {
            // EOF signals the end of the flags
            if self.current == EOF_CHAR {
                break;
            }

            if is_ascii(self.current) {
                if is_id_part_ascii(self.current) {
                    self.advance();
                } else {
                    break;
                }
            } else {
                // Otherwise must be a utf-8 encoded codepoint
                let save_state = self.save();
                let code_point = self.lex_utf8_codepoint()?;
                if is_id_part_unicode(code_point) {
                    continue;
                } else {
                    // Restore to before codepoint if not part of the flags
                    self.restore(&save_state);
                    break;
                }
            }
        }

        let pattern = Wtf8Str::from_bytes_unchecked(&self.buf[pattern_start_pos..pattern_end_pos]);
        let flags = Wtf8Str::from_bytes_unchecked(&self.buf[flags_start_pos..self.pos]);
        let raw = Wtf8Str::from_bytes_unchecked(&self.buf[start_pos..self.pos]);

        self.emit(Token::RegExpLiteral { raw, pattern, flags }, start_pos)
    }

    fn lex_regex_character(&mut self, in_class: bool) -> ParseResult<()> {
        match_u32!(match self.current {
            '\\' => {
                self.advance();
                self.lex_regexp_character_non_line_terminator()?;
            }
            EOF_CHAR => {
                let loc = self.mark_loc(self.pos);
                return self.error(loc, ParseError::UnterminatedRegExpLiteral);
            }
            '[' if !in_class => {
                self.advance();

                while self.current != ']' as u32 {
                    self.lex_regex_character(true)?;
                }

                self.advance();
            }
            _ => {
                self.lex_regexp_character_non_line_terminator()?;
            }
        });

        Ok(())
    }

    fn lex_regexp_character_non_line_terminator(&mut self) -> ParseResult<()> {
        let char = self.lex_ascii_or_unicode_character()?;

        if is_newline(char) {
            let loc = self.mark_loc(self.pos);
            self.error(loc, ParseError::UnterminatedRegExpLiteral)
        } else {
            Ok(())
        }
    }

    // Get the next template part after the end of a template expression. Must be called when the
    // previously lexed token was a '}'.
    pub fn next_template_part(&mut self) -> LexResult<'a> {
        let start_pos = self.pos - 1;
        self.lex_template_literal(start_pos, false)
    }

    fn lex_template_literal(&mut self, start_pos: Pos, is_head: bool) -> LexResult<'a> {
        let mut value = AstString::new_in(self.alloc);

        let is_tail;
        let raw_start_pos = self.pos;
        let raw_end_pos;

        let mut has_cr = false;
        let mut malformed_error_loc = None;

        loop {
            match_u32!(match self.current {
                // Escape sequences
                '\\' => match_u32!(match self.peek() {
                    // Single character escapes
                    'n' => {
                        value.push_char('\n');
                        self.advance2()
                    }
                    '\\' => {
                        value.push_char('\\');
                        self.advance2()
                    }
                    '\'' => {
                        value.push_char('\'');
                        self.advance2()
                    }
                    '"' => {
                        value.push_char('"');
                        self.advance2()
                    }
                    '`' => {
                        value.push_char('`');
                        self.advance2()
                    }
                    't' => {
                        value.push_char('\t');
                        self.advance2()
                    }
                    'r' => {
                        value.push_char('\r');
                        self.advance2()
                    }
                    'b' => {
                        value.push_char('\x08');
                        self.advance2()
                    }
                    'v' => {
                        value.push_char('\x0B');
                        self.advance2()
                    }
                    'f' => {
                        value.push_char('\x0C');
                        self.advance2()
                    }
                    '0' => {
                        if self.peek2() < '0' as u32 || self.peek2() > '9' as u32 {
                            value.push_char('\x00');
                            self.advance2()
                        } else {
                            malformed_error_loc = Some(self.mark_loc(self.pos));
                            self.advance2()
                        }
                    }
                    // Invalid octal escape sequence
                    '1'..='9' => {
                        malformed_error_loc = Some(self.mark_loc(self.pos));
                        self.advance2();
                    }
                    // Hex escape sequence
                    'x' => {
                        self.advance2();

                        if let Some(x1) = get_hex_value(self.current) {
                            if let Some(x2) = get_hex_value(self.peek()) {
                                let escaped_char = x1 * 16 + x2;
                                value.push(escaped_char);
                                self.advance2();
                            } else {
                                let loc = self.mark_loc(self.pos);
                                self.advance();
                                malformed_error_loc = Some(loc);
                            }
                        } else {
                            malformed_error_loc = Some(self.mark_loc(self.pos));
                        }
                    }
                    // Unicode escape sequence
                    'u' => {
                        let escape_start_pos = self.pos;
                        self.advance2();

                        match self.lex_unicode_escape_sequence(escape_start_pos) {
                            Ok(code_point) => value.push(code_point),
                            Err(err) => malformed_error_loc = Some(err.source_loc.unwrap().0),
                        }
                    }
                    // Line continuations, either LF, CR, or CRLF, which are excluded in cooked value
                    '\n' => self.advance2(),
                    '\r' => {
                        self.advance2();

                        has_cr = true;

                        self.eat('\n');
                    }
                    // Non-escape character, use character directly
                    other => {
                        if is_ascii(other) {
                            self.advance2();
                            value.push(other);
                        } else {
                            self.advance();
                            let code_point = self.lex_utf8_codepoint()?;

                            // Unicode line continuations are ignored in string value
                            if !is_unicode_newline(code_point) {
                                value.push(code_point);
                            }
                        }
                    }
                }),
                '$' => match_u32!(match self.peek() {
                    // Start of an expression in the template literal
                    '{' => {
                        raw_end_pos = self.pos;
                        is_tail = false;

                        self.advance2();

                        break;
                    }
                    // Not an escape sequence, use '$' directly
                    _ => {
                        value.push_char('$');
                        self.advance()
                    }
                }),
                // End of the entire template literal
                '`' => {
                    raw_end_pos = self.pos;
                    is_tail = true;

                    self.advance();

                    break;
                }
                // CR and CRLF are converted to LF to both raw and cooked strings
                '\r' => {
                    self.advance();

                    has_cr = true;
                    value.push_char('\n');

                    self.eat('\n');
                }
                _ => value.push(self.lex_ascii_or_unicode_character()?),
            })
        }

        let mut raw =
            AstString::from_bytes_unchecked_in(&self.buf[raw_start_pos..raw_end_pos], self.alloc);

        // CR and CRLF are both converted to LF in raw string. This requires copying the string
        // again, so only perform the replace if a CR was encountered.
        if has_cr {
            let mut new_raw = AstString::new_in(self.alloc);
            for (i, slice) in raw.as_bytes().split(|byte| *byte == b'\r').enumerate() {
                // Replace both `\r\n` and `\r`
                let slice = if i != 0 && !slice.is_empty() && slice[0] == b'\n' {
                    &slice[1..]
                } else {
                    slice
                };

                // Replace with a single `\n`
                if i != 0 {
                    new_raw.push_char('\n');
                }

                new_raw.push_bytes_unchecked(slice);
            }

            raw = new_raw;
        }

        // Only return cooked string if a malformed error location was not found
        let cooked = match malformed_error_loc {
            None => Ok(value),
            Some(loc) => Err(loc),
        };

        self.emit(Token::TemplatePart { raw, cooked, is_head, is_tail }, start_pos)
    }

    /// Lex any valid codepoint whether it is ASCII or unicode
    #[inline]
    fn lex_ascii_or_unicode_character(&mut self) -> ParseResult<CodePoint> {
        if is_ascii(self.current) {
            let ascii_char = self.current;
            self.advance();
            Ok(ascii_char)
        } else {
            let code_point = self.lex_utf8_codepoint()?;
            Ok(code_point)
        }
    }

    #[inline]
    fn lex_utf8_codepoint(&mut self) -> ParseResult<CodePoint> {
        let buf = &self.buf[self.pos..];
        match decode_wtf8_codepoint(buf) {
            Ok((code_point, byte_length)) => {
                self.advance_n(byte_length);
                Ok(code_point)
            }
            Err(byte_length) => {
                self.advance_n(byte_length);
                let loc = self.mark_loc(self.pos - byte_length);
                self.error(loc, ParseError::InvalidUnicode)
            }
        }
    }

    // Lex a single unicode escape sequence, called after the '\u' prefix has already been processed
    fn lex_unicode_escape_sequence(&mut self, start_pos: Pos) -> ParseResult<CodePoint> {
        // Escape sequence has form \u{HEX_DIGITS}, with at most 6 hex digits
        if self.eat('{') {
            if self.eat('}') {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            let mut value = 0;
            for _ in 0..6 {
                if let Some(hex_value) = get_hex_value(self.current) {
                    self.advance();
                    value <<= 4;
                    value += hex_value;
                } else if self.current == '}' as u32 {
                    break;
                } else {
                    let loc = self.mark_loc(start_pos);
                    return self.error(loc, ParseError::MalformedEscapeSeqence);
                }
            }

            if self.current != '}' as u32 {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            self.advance();

            // Check that value is not out of range (greater than \u10FFFF)
            if is_in_unicode_range(value) {
                return Ok(value);
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

        // Guaranteed to already be in range since there are four hex digits
        Ok(value)
    }

    // Fast path for lexing a purely ASCII identifier
    fn lex_identifier_ascii(&mut self, start_pos: Pos) -> LexResult<'a> {
        // Consume the id start ASCII character
        self.advance();

        loop {
            if is_id_part_ascii(self.current) {
                self.advance();
                continue;
            } else if is_ascii(self.current) && self.current != '\\' as u32 {
                // The only remaining allowed ASCII character is the start of an escape sequence,
                // handled below.
                break;
            } else if self.current == EOF_CHAR {
                break;
            } else {
                // Peek at the next code point, which is either a unicode character or an escape
                // sequence. This may be part of the identifier.
                let save_state = self.save();
                let ascii_end_pos = self.pos;

                let code_point = if self.current == '\\' as u32 {
                    self.lex_identifier_unicode_escape_sequence()?
                } else {
                    self.lex_utf8_codepoint()?
                };

                if let Some(char) = as_id_part(code_point) {
                    // Non-ASCII character is part of the identifier so bail to slow path, copying
                    // over ASCII string and code point that has been created so far. Safe since
                    // string is ASCII only so far and therefore valid UTF-8.
                    let mut string_builder = AstString::from_bytes_unchecked_in(
                        &self.buf[start_pos..ascii_end_pos],
                        self.alloc,
                    );
                    string_builder.push_char(char);

                    return self.lex_identifier_non_ascii(start_pos, string_builder);
                } else {
                    // If not an id part then this is a pure ASCII identifier
                    self.restore(&save_state);
                    break;
                }
            }
        }

        // Safe since this slice is ASCII only and therefore valid UTF-8
        let id_str = unsafe { std::str::from_utf8_unchecked(&self.buf[start_pos..self.pos]) };

        if let Some(keyword_token) = self.ascii_id_to_keyword(id_str) {
            self.emit(keyword_token, start_pos)
        } else {
            self.emit(Token::Identifier(Wtf8Str::from_str(id_str)), start_pos)
        }
    }

    // Slow path for lexing an identifier with at least one unicode character or escape sequence.
    // Input the string that has been created so far before falling back to this slow path.
    fn lex_identifier_non_ascii(
        &mut self,
        start_pos: Pos,
        mut string_builder: AstString<'a>,
    ) -> LexResult<'a> {
        loop {
            // Check if ASCII
            if is_ascii(self.current) {
                if let Some(char) = as_id_part_ascii(self.current) {
                    string_builder.push_char(char);
                    self.advance();
                } else if self.current == '\\' as u32 {
                    let code_point = self.lex_identifier_unicode_escape_sequence()?;

                    if let Some(char) = as_id_part(code_point) {
                        string_builder.push_char(char);
                    } else {
                        let loc = self.mark_loc(self.pos);
                        let code_point_string = to_string_or_unicode_escape_sequence(code_point);
                        return self.error(loc, ParseError::new_unknown_token(code_point_string));
                    }
                } else {
                    break;
                }
            } else if self.current == EOF_CHAR {
                break;
            } else {
                // Otherwise must be a utf-8 encoded codepoint
                let save_state = self.save();
                let code_point = self.lex_utf8_codepoint()?;

                if let Some(char) = as_id_part_unicode(code_point) {
                    string_builder.push_char(char);
                } else {
                    // Restore to before codepoint if not part of the id
                    self.restore(&save_state);
                    break;
                }
            }
        }

        self.emit(Token::Identifier(string_builder.into_arena_str()), start_pos)
    }

    fn lex_identifier_unicode_escape_sequence(&mut self) -> ParseResult<CodePoint> {
        let escape_start_pos = self.pos;
        self.advance();

        if self.current == 'u' as u32 {
            self.advance();

            let code_point = self.lex_unicode_escape_sequence(escape_start_pos)?;
            Ok(code_point)
        } else {
            let loc = self.mark_loc(escape_start_pos);
            self.error(loc, ParseError::MalformedEscapeSeqence)
        }
    }

    fn ascii_id_to_keyword(&mut self, id_string: &str) -> Option<Token<'static>> {
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
            "import" => Some(Token::Import),
            "export" => Some(Token::Export),
            "await" => Some(Token::Await),
            "yield" => Some(Token::Yield),
            "target" => Some(Token::Target),
            "meta" => Some(Token::Meta),
            "enum" => Some(Token::Enum),
            _ => None,
        }
    }
}
