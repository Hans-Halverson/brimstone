use std::str::FromStr;

use num_bigint::BigInt;

use crate::js::common::unicode::{
    get_binary_value, get_decimal_value, get_hex_value, get_octal_value, is_continuation_byte,
    is_decimal_digit, is_newline, is_whitespace,
};

pub struct StringLexer<'a> {
    buf: &'a str,
    // Position to start reading from for the next character
    pos: usize,
    // Start of the previously read character (aka the character in the current field)
    prev_pos: usize,
    current: char,
}

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: char = '\u{ffff}';

impl<'a> StringLexer<'a> {
    // Returns None if the lexer could not be primed, meaning the string starts with invalid
    // unicode.
    pub fn new(buf: &str) -> Option<StringLexer> {
        let mut lexer = StringLexer { buf, pos: 0, prev_pos: 0, current: EOF_CHAR };
        lexer.advance()?;

        Some(lexer)
    }

    #[inline]
    pub fn current(&self) -> char {
        self.current
    }

    pub fn advance(&mut self) -> Option<()> {
        if self.pos >= self.buf.len() {
            self.current = EOF_CHAR;
            self.prev_pos = self.buf.len();

            return Some(());
        }

        let prev_pos = self.pos;
        self.current = read_utf8_codepoint(self.buf.as_bytes(), &mut self.pos)?;
        self.prev_pos = prev_pos;

        Some(())
    }

    fn expect(&mut self, char: char) -> Option<()> {
        if self.current == char {
            self.advance()?;
            Some(())
        } else {
            None
        }
    }

    // Peek the next char by looking at the next byte. Can only be used for ASCII chars.
    pub fn peek_ascii_char(&self) -> char {
        if self.pos >= self.buf.len() {
            EOF_CHAR
        } else {
            self.buf.as_bytes()[self.pos] as char
        }
    }

    pub fn is_end(&self) -> bool {
        self.current == EOF_CHAR
    }

    // Return the start pos of the character at lexer.current
    pub fn current_start_pos(&self) -> usize {
        self.prev_pos
    }
}

// Read a unicode codepoint encoded as utf-8. Codepoint is not validated to be in unicode range.
fn read_utf8_codepoint(buf: &[u8], pos_ref: &mut usize) -> Option<char> {
    let pos = *pos_ref;
    let bytes = &buf[pos..];
    let b1 = bytes[0];

    if b1 < 0x80 {
        // Single byte ASCII character
        *pos_ref += 1;
        Some(b1 as char)
    } else if (b1 & 0xE0) == 0xC0 && pos + 1 < buf.len() {
        // Two byte sequence
        *pos_ref += 2;

        let b2 = bytes[1];
        if !is_continuation_byte(b2) {
            return None;
        }

        let mut codepoint = (b1 as u32 & 0x1F) << 6;
        codepoint |= b2 as u32 & 0x3F;

        Some(unsafe { char::from_u32_unchecked(codepoint) })
    } else if (b1 & 0xF0) == 0xE0 && pos + 2 < buf.len() {
        // Three byte sequence
        *pos_ref += 3;

        let b2 = bytes[1];
        let b3 = bytes[2];
        if !is_continuation_byte(b2) || !is_continuation_byte(b3) {
            return None;
        }

        let mut codepoint = (b1 as u32 & 0x0F) << 12;
        codepoint |= (b2 as u32 & 0x3F) << 6;
        codepoint |= b3 as u32 & 0x3F;

        // Char could be in the surrogate pair range, which is not a valid code point.
        char::from_u32(codepoint)
    } else if (b1 & 0xF8) == 0xF0 && pos + 3 < buf.len() {
        // Four byte sequence
        *pos_ref += 4;

        let b2 = bytes[1];
        let b3 = bytes[2];
        let b4 = bytes[3];
        if !is_continuation_byte(b2) || !is_continuation_byte(b3) || !is_continuation_byte(b4) {
            return None;
        }

        let mut codepoint = (b1 as u32 & 0x07) << 18;
        codepoint |= (b2 as u32 & 0x3F) << 12;
        codepoint |= (b3 as u32 & 0x3F) << 6;
        codepoint |= b4 as u32 & 0x3F;

        // Char could be above the code point max, 0x10FFFF
        char::from_u32(codepoint)
    } else {
        None
    }
}

// Parse string to a number following the grammar in:
// 7.1.4.1 ToNumber Applied to the String Type
//
// Return None if the string does not conform to the grammar.
pub fn parse_string_to_number(str: &str) -> Option<f64> {
    let mut lexer = StringLexer::new(str)?;

    skip_string_whitespace(&mut lexer)?;

    // Empty or pure whitespace string is treated as 0
    if lexer.is_end() {
        return Some(0.0);
    }

    // Check if we have a non-decimal integer literal prefix
    if lexer.current == '0' {
        match lexer.peek_ascii_char() {
            'x' | 'X' => {
                let value = non_numeric_literal_with_base(&mut lexer, 4, get_hex_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            'o' | 'O' => {
                let value = non_numeric_literal_with_base(&mut lexer, 3, get_octal_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            'b' | 'B' => {
                let value = non_numeric_literal_with_base(&mut lexer, 1, get_binary_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            _ => {}
        }
    }

    // Parse optional leading sign
    let is_negative = match lexer.current {
        '-' => {
            lexer.advance()?;
            true
        }
        '+' => {
            lexer.advance()?;
            false
        }
        _ => false,
    };

    // Parse 'Infinity', one character at a time
    if lexer.current == 'I' {
        lexer.advance()?;
        lexer.expect('n')?;
        lexer.expect('f')?;
        lexer.expect('i')?;
        lexer.expect('n')?;
        lexer.expect('i')?;
        lexer.expect('t')?;
        lexer.expect('y')?;

        skip_string_whitespace(&mut lexer)?;

        if !lexer.is_end() {
            return None;
        }

        if is_negative {
            return Some(f64::NEG_INFINITY);
        } else {
            return Some(f64::INFINITY);
        }
    }

    let parse_float_start_pos = lexer.current_start_pos();
    parse_unsigned_decimal_literal(&mut lexer)?;
    let parse_float_end_pos = lexer.current_start_pos();

    skip_string_whitespace(&mut lexer)?;

    // We must be at end of string otherwise string was not fully parsed
    if !lexer.is_end() {
        return None;
    }

    // Parse portion of string using rust stdlib
    let number = f64::from_str(&lexer.buf[parse_float_start_pos..parse_float_end_pos]).unwrap();

    if is_negative {
        Some(-number)
    } else {
        Some(number)
    }
}

pub fn skip_string_whitespace(lexer: &mut StringLexer) -> Option<()> {
    loop {
        let char = lexer.current();
        if is_whitespace(char) || is_newline(char) {
            lexer.advance()?;
        } else {
            break;
        }
    }

    Some(())
}

fn skip_decimal_digits(lexer: &mut StringLexer) -> Option<bool> {
    let mut has_digit = false;

    while is_decimal_digit(lexer.current) {
        lexer.advance()?;
        has_digit = true;
    }

    Some(has_digit)
}

#[inline]
fn non_numeric_literal_with_base(
    lexer: &mut StringLexer,
    shift: u32,
    char_to_digit: fn(char) -> Option<u32>,
) -> Option<f64> {
    // Skip prefix
    lexer.advance()?;
    lexer.advance()?;

    let mut value: u64 = 0;
    let mut has_digit = false;

    while let Some(digit) = char_to_digit(lexer.current) {
        value <<= shift;
        value += digit as u64;

        has_digit = true;
        lexer.advance()?;
    }

    if !has_digit {
        return None;
    }

    Some(value as f64)
}

// Parse an unsigned decimal literal, returning None if an unsigned decimal literal does not appear
// at the beginning of the lexer. On success return Some, and the lexer will be one character beyond
// the end of the parsed literal.
pub fn parse_unsigned_decimal_literal(lexer: &mut StringLexer) -> Option<()> {
    // Parse digits before dot
    let has_digits_before_dot = skip_decimal_digits(lexer)?;

    // Parse optional dot followed by digits
    let has_dot = if lexer.current == '.' {
        lexer.advance()?;
        let has_digits_after_dot = skip_decimal_digits(lexer)?;

        // Invalid numeric literal '.', dot must have digits on at least one side
        if !has_digits_before_dot && !has_digits_after_dot {
            return None;
        }

        true
    } else {
        false
    };

    // Parse optional exponent, but only if there were some digits beforehand or after the dot
    if (lexer.current == 'e' || lexer.current == 'E') && (has_digits_before_dot || has_dot) {
        lexer.advance()?;

        // Optional sign in exponent
        if lexer.current == '-' || lexer.current == '+' {
            lexer.advance()?;
        }

        // Exponent must have some digits
        let has_digits_in_exponent = skip_decimal_digits(lexer)?;
        if !has_digits_in_exponent {
            return None;
        }
    }

    Some(())
}

// Parse string to a BigInt following the grammar in:
// 7.1.14.1 StringIntegerLiteral Grammar
//
// Return None if the string does not conform to the grammar.
pub fn parse_string_to_bigint(str: &str) -> Option<BigInt> {
    let mut lexer = StringLexer::new(str)?;

    skip_string_whitespace(&mut lexer)?;

    // Empty or pure whitespace string is treated as 0
    if lexer.is_end() {
        return Some(BigInt::from(0));
    }

    // Check if we have a non-decimal BigInt literal prefix
    if lexer.current == '0' {
        match lexer.peek_ascii_char() {
            'x' | 'X' => {
                lexer.advance();
                lexer.advance();

                let value = bigint_literal_with_base(&mut lexer, 16, get_hex_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            'o' | 'O' => {
                lexer.advance();
                lexer.advance();

                let value = bigint_literal_with_base(&mut lexer, 8, get_octal_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            'b' | 'B' => {
                lexer.advance();
                lexer.advance();

                let value = bigint_literal_with_base(&mut lexer, 2, get_binary_value)?;
                skip_string_whitespace(&mut lexer)?;

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            _ => {}
        }
    }

    // Parse optional leading sign
    let is_negative = match lexer.current {
        '-' => {
            lexer.advance()?;
            true
        }
        '+' => {
            lexer.advance()?;
            false
        }
        _ => false,
    };

    // Parse decimal digits, building bigint
    let value = bigint_literal_with_base(&mut lexer, 10, get_decimal_value)?;
    skip_string_whitespace(&mut lexer)?;

    // We must be at end of string otherwise string was not fully parsed
    if !lexer.is_end() {
        return None;
    }

    // Apply sign
    if is_negative {
        Some(-value)
    } else {
        Some(value)
    }
}

#[inline]
fn bigint_literal_with_base(
    lexer: &mut StringLexer,
    base: u32,
    char_to_digit: fn(char) -> Option<u32>,
) -> Option<BigInt> {
    let mut value = BigInt::from(0);
    let mut has_digit = false;

    while let Some(digit) = char_to_digit(lexer.current) {
        value *= base;
        value += digit;

        has_digit = true;
        lexer.advance()?;
    }

    if !has_digit {
        return None;
    }

    Some(value)
}
