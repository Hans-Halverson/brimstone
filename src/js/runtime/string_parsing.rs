use std::str::FromStr;

use num_bigint::BigInt;

use crate::js::common::unicode::{
    is_ascii_newline, is_ascii_whitespace, is_unicode_newline, is_unicode_whitespace, CodeUnit,
};

use super::{
    string_value::{CodeUnitIterator, StringValue},
    Gc,
};

pub struct StringLexer {
    iter: CodeUnitIterator,
    // Pointer to start of the previously read character (aka the character in the current field)
    prev_ptr: *const u8,
    current: Option<CodeUnit>,
}

impl StringLexer {
    // Returns None if the lexer could not be primed, meaning the string starts with invalid
    // unicode.
    pub fn new(string: Gc<StringValue>) -> StringLexer {
        let iter = string.iter_code_units();
        let prev_ptr = iter.ptr();
        let mut lexer = StringLexer { iter, prev_ptr, current: None };

        // Prime the lexer
        lexer.advance();

        lexer
    }

    pub fn advance(&mut self) {
        let prev_ptr = self.iter.ptr();
        self.current = self.iter.next();
        self.prev_ptr = prev_ptr;
    }

    /// Advance if the current code unit is equal to the given character. Only valid for char in
    /// the u16 range.
    fn expect(&mut self, char: char) -> Option<()> {
        match self.current {
            Some(current_code_point) if current_code_point == char as u16 => {
                self.advance();
                Some(())
            }
            _ => None,
        }
    }

    /// Is the current code unit equal to the given character. Only valid for char in the u16 range.
    #[inline]
    pub fn current_equals(&self, expected: char) -> bool {
        match self.current {
            Some(current) if current == expected as u16 => true,
            _ => false,
        }
    }

    /// Peek the next char by looking at the next byte. Can only be used for ASCII chars.
    pub fn peek_ascii_char(&self) -> Option<char> {
        match self.iter.peek() {
            Some(code_unit) if code_unit < 0x80 => Some(code_unit as u8 as char),
            _ => None,
        }
    }

    pub fn is_end(&self) -> bool {
        self.current.is_none()
    }

    /// Return the pointer to the start of the code unit at lexer.current
    pub fn current_ptr(&self) -> *const u8 {
        self.prev_ptr
    }

    pub fn current_is_decimal_digit(&self) -> bool {
        match self.current {
            Some(current) if '0' as u16 <= current && current <= '9' as u16 => true,
            _ => false,
        }
    }

    pub fn current_is_whitespace_or_newline(&self) -> bool {
        match self.current {
            // ASCII fast path
            Some(current) if current < 0x80 => {
                let char = current as u8 as char;
                is_ascii_whitespace(char) || is_ascii_newline(char)
            }
            // Slow non-ascii path for full unicode coverage
            Some(current) => match char::from_u32(current as u32) {
                None => false,
                Some(char) => is_unicode_whitespace(char) || is_unicode_newline(char),
            },
            None => false,
        }
    }

    pub fn current_decimal_value(&self) -> Option<u32> {
        match self.current {
            Some(current) if '0' as u16 <= current && current <= '9' as u16 => {
                Some(current as u32 - '0' as u32)
            }
            _ => None,
        }
    }

    pub fn current_binary_value(&self) -> Option<u32> {
        match self.current {
            Some(current) if '0' as u16 == current => Some(0),
            Some(current) if '1' as u16 == current => Some(1),
            _ => None,
        }
    }

    pub fn current_octal_value(&self) -> Option<u32> {
        match self.current {
            Some(current) if '0' as u16 <= current && current <= '7' as u16 => {
                Some(current as u32 - '0' as u32)
            }
            _ => None,
        }
    }

    pub fn current_hex_value(&self) -> Option<u32> {
        match self.current {
            Some(current) if '0' as u16 <= current && current <= '9' as u16 => {
                Some(current as u32 - '0' as u32)
            }
            Some(current) if 'a' as u16 <= current && current <= 'f' as u16 => {
                Some(current as u32 - 'a' as u32 + 10)
            }
            Some(current) if 'A' as u16 <= current && current <= 'F' as u16 => {
                Some(current as u32 - 'A' as u32 + 10)
            }
            _ => None,
        }
    }

    #[inline]
    pub fn current_digit_value(&self, lower_bound: char, upper_bound: char) -> Option<u32> {
        match self.current {
            Some(current) if lower_bound as u16 <= current && current < upper_bound as u16 => {
                Some(current as u32 - lower_bound as u32)
            }
            _ => None,
        }
    }
}

// Parse string to a number following the grammar in:
// 7.1.4.1 ToNumber Applied to the String Type
//
// Return None if the string does not conform to the grammar.
pub fn parse_string_to_number(string: Gc<StringValue>) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    skip_string_whitespace(&mut lexer);

    // Empty or pure whitespace string is treated as 0
    if lexer.is_end() {
        return Some(0.0);
    }

    // Check if we have a non-decimal integer literal prefix
    if lexer.current_equals('0') {
        match lexer.peek_ascii_char() {
            Some('x' | 'X') => {
                let value =
                    non_numeric_literal_with_base(&mut lexer, 4, StringLexer::current_hex_value)?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            Some('o' | 'O') => {
                let value =
                    non_numeric_literal_with_base(&mut lexer, 3, StringLexer::current_octal_value)?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            Some('b' | 'B') => {
                let value = non_numeric_literal_with_base(
                    &mut lexer,
                    1,
                    StringLexer::current_binary_value,
                )?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            _ => {}
        }
    }

    // Parse optional leading sign
    let is_negative = if lexer.current_equals('-') {
        lexer.advance();
        true
    } else if lexer.current_equals('+') {
        lexer.advance();
        false
    } else {
        false
    };

    // Parse 'Infinity', one character at a time
    if lexer.current_equals('I') {
        lexer.advance();
        lexer.expect('n')?;
        lexer.expect('f')?;
        lexer.expect('i')?;
        lexer.expect('n')?;
        lexer.expect('i')?;
        lexer.expect('t')?;
        lexer.expect('y')?;

        skip_string_whitespace(&mut lexer);

        if !lexer.is_end() {
            return None;
        }

        if is_negative {
            return Some(f64::NEG_INFINITY);
        } else {
            return Some(f64::INFINITY);
        }
    }

    let parse_float_start_ptr = lexer.current_ptr();
    parse_unsigned_decimal_literal(&mut lexer)?;
    let parse_float_end_ptr = lexer.current_ptr();

    skip_string_whitespace(&mut lexer);

    // We must be at end of string otherwise string was not fully parsed
    if !lexer.is_end() {
        return None;
    }

    let number = parse_between_ptrs_to_f64(parse_float_start_ptr, parse_float_end_ptr);

    if is_negative {
        Some(-number)
    } else {
        Some(number)
    }
}

pub fn skip_string_whitespace(lexer: &mut StringLexer) {
    while lexer.current_is_whitespace_or_newline() {
        lexer.advance()
    }
}

fn skip_decimal_digits(lexer: &mut StringLexer) -> bool {
    let mut has_digit = false;

    while lexer.current_is_decimal_digit() {
        lexer.advance();
        has_digit = true;
    }

    has_digit
}

#[inline]
fn non_numeric_literal_with_base(
    lexer: &mut StringLexer,
    shift: u32,
    current_digit_fn: fn(&StringLexer) -> Option<u32>,
) -> Option<f64> {
    // Skip prefix
    lexer.advance();
    lexer.advance();

    let mut value: u64 = 0;
    let mut has_digit = false;

    while let Some(digit) = current_digit_fn(lexer) {
        value <<= shift;
        value += digit as u64;

        has_digit = true;
        lexer.advance();
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
    let has_digits_before_dot = skip_decimal_digits(lexer);

    // Parse optional dot followed by digits
    let has_dot = if lexer.current_equals('.') {
        lexer.advance();
        let has_digits_after_dot = skip_decimal_digits(lexer);

        // Invalid numeric literal '.', dot must have digits on at least one side
        if !has_digits_before_dot && !has_digits_after_dot {
            return None;
        }

        true
    } else {
        false
    };

    // Parse optional exponent, but only if there were some digits beforehand or after the dot
    if (lexer.current_equals('e') || lexer.current_equals('E'))
        && (has_digits_before_dot || has_dot)
    {
        lexer.advance();

        // Optional sign in exponent
        if lexer.current_equals('-') || lexer.current_equals('+') {
            lexer.advance();
        }

        // Exponent must have some digits
        let has_digits_in_exponent = skip_decimal_digits(lexer);
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
pub fn parse_string_to_bigint(string: Gc<StringValue>) -> Option<BigInt> {
    let mut lexer = StringLexer::new(string);

    skip_string_whitespace(&mut lexer);

    // Empty or pure whitespace string is treated as 0
    if lexer.is_end() {
        return Some(BigInt::from(0));
    }

    // Check if we have a non-decimal BigInt literal prefix
    if lexer.current_equals('0') {
        match lexer.peek_ascii_char() {
            Some('x' | 'X') => {
                lexer.advance();
                lexer.advance();

                let value =
                    bigint_literal_with_base(&mut lexer, 16, StringLexer::current_hex_value)?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            Some('o' | 'O') => {
                lexer.advance();
                lexer.advance();

                let value =
                    bigint_literal_with_base(&mut lexer, 8, StringLexer::current_octal_value)?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            Some('b' | 'B') => {
                lexer.advance();
                lexer.advance();

                let value =
                    bigint_literal_with_base(&mut lexer, 2, StringLexer::current_binary_value)?;
                skip_string_whitespace(&mut lexer);

                if !lexer.is_end() {
                    return None;
                }

                return Some(value);
            }
            _ => {}
        }
    }

    // Parse optional leading sign
    let is_negative = if lexer.current_equals('-') {
        lexer.advance();
        true
    } else if lexer.current_equals('+') {
        lexer.advance();
        false
    } else {
        false
    };

    // Parse decimal digits, building bigint
    let value = bigint_literal_with_base(&mut lexer, 10, StringLexer::current_decimal_value)?;
    skip_string_whitespace(&mut lexer);

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
    current_digit_fn: fn(&StringLexer) -> Option<u32>,
) -> Option<BigInt> {
    let mut value = BigInt::from(0);
    let mut has_digit = false;

    while let Some(digit) = current_digit_fn(lexer) {
        value *= base;
        value += digit;

        has_digit = true;
        lexer.advance();
    }

    if !has_digit {
        return None;
    }

    Some(value)
}

/// Parse string to a u32,. String must be the canonical representation of a u32, and overflow is
/// checked. This is used when converting a string to an array property key if possible.
pub fn parse_string_to_u32(string: Gc<StringValue>) -> Option<u32> {
    let mut lexer = StringLexer::new(string);

    let mut result;

    if let Some(digit) = lexer.current_decimal_value() {
        lexer.advance();

        // First digit can be a 0 only if it is not a leading zero
        if digit == 0 && !lexer.is_end() {
            return None;
        }

        result = digit;
    } else {
        return None;
    }

    // Consume remaining digits, checking for overlow as they are added to the result number
    while !lexer.is_end() {
        if let Some(digit) = lexer.current_decimal_value() {
            result = result.checked_mul(10)?;
            result = result.checked_add(digit)?;

            lexer.advance();
        } else {
            return None;
        }
    }

    Some(result)
}

/// Parse portion of string between two pointers using rust stdlib
pub fn parse_between_ptrs_to_f64(start_ptr: *const u8, end_ptr: *const u8) -> f64 {
    let str = unsafe {
        let bytes = std::slice::from_raw_parts(start_ptr, end_ptr.offset_from(start_ptr) as usize);
        std::str::from_utf8_unchecked(bytes)
    };

    f64::from_str(str).unwrap()
}
