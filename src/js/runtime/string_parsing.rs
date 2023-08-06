use std::str::FromStr;

use num_bigint::BigInt;

use crate::js::common::unicode::{
    is_ascii_newline, is_ascii_whitespace, is_unicode_newline, is_unicode_whitespace, CodeUnit,
};

use super::{
    intrinsics::date_object::{
        year_month_day_to_days_since_unix_epoch, MAX_TIME_VALUE, MS_PER_DAY, MS_PER_HOUR,
        MS_PER_MINUTE, MS_PER_SECOND,
    },
    string_value::{CodeUnitIterator, GenericCodeUnitIterator, StringValue, StringWidth},
    Handle,
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
    pub fn new(string: Handle<StringValue>) -> StringLexer {
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

    /// Advance if the current code unit is equal to the given character. Only valid for char in
    /// the u16 range.
    ///
    /// Return true if the character was consumed.
    fn eat(&mut self, char: char) -> bool {
        self.expect(char).is_some()
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
                let code_point = current as u32;
                is_ascii_whitespace(code_point) || is_ascii_newline(code_point)
            }
            // Slow non-ascii path for full unicode coverage
            Some(current) => {
                let code_point = current as u32;
                is_unicode_whitespace(code_point) || is_unicode_newline(code_point)
            }
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

    pub fn width(&self) -> StringWidth {
        self.iter.width()
    }
}

// Parse string to a number following the grammar in:
// 7.1.4.1 ToNumber Applied to the String Type
//
// Return None if the string does not conform to the grammar.
pub fn parse_string_to_number(string: Handle<StringValue>) -> Option<f64> {
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
    let is_negative = if lexer.eat('-') {
        true
    } else if lexer.eat('+') {
        false
    } else {
        false
    };

    // Parse 'Infinity', one character at a time
    if lexer.eat('I') {
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

    let number = parse_between_ptrs_to_f64(&lexer, parse_float_start_ptr, parse_float_end_ptr);

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
pub fn parse_string_to_bigint(string: Handle<StringValue>) -> Option<BigInt> {
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
pub fn parse_string_to_u32(string: Handle<StringValue>) -> Option<u32> {
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
pub fn parse_between_ptrs_to_f64(
    lexer: &StringLexer,
    start_ptr: *const u8,
    end_ptr: *const u8,
) -> f64 {
    if lexer.width() == StringWidth::OneByte {
        // If string is one-byte we can directly read from a slice, treating it as UTF-8 (since it
        // is guaranteed that all code units are ASCII).
        let str = unsafe {
            let bytes =
                std::slice::from_raw_parts(start_ptr, end_ptr.offset_from(start_ptr) as usize);
            std::str::from_utf8_unchecked(bytes)
        };

        f64::from_str(str).unwrap()
    } else {
        // Otherwise we must copy string to a UTF-8 buffer before parsing
        let start_ptr = start_ptr as *const u16;
        let end_ptr = end_ptr as *const u16;
        let code_units = unsafe {
            std::slice::from_raw_parts(start_ptr, end_ptr.offset_from(start_ptr) as usize)
        };

        let mut utf8_string = String::with_capacity(code_units.len());
        for code_unit in code_units {
            utf8_string.push(*code_unit as u8 as char)
        }

        f64::from_str(&utf8_string).unwrap()
    }
}

// Parse exactly num_digits into an
fn parse_decimal_digits(lexer: &mut StringLexer, num_digits: i32) -> Option<i64> {
    let mut value = 0;

    for _ in 0..num_digits {
        if let Some(digit) = lexer.current_decimal_value() {
            value *= 10;
            value += digit as i64;

            lexer.advance();
        } else {
            return None;
        }
    }

    Some(value)
}

// 21.4.3.2 Date.parse
// Parse string to a date, accepting multiple date formats. Supports the formats generated by the
// following methods:
// - Date.prototype.toISOString
// - Date.prototype.toString
// - Date.prototype.toUTCString
pub fn parse_string_to_date(string: Handle<StringValue>) -> Option<f64> {
    if let Some(date) = parse_string_to_iso_date(string) {
        return Some(date);
    }

    parse_string_to_utc_or_default_date(string)
}

// 21.4.1.32 Date Time String Format
// Parse string to a date following the simplified ISO 8601 format
fn parse_string_to_iso_date(string: Handle<StringValue>) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    // Parse required year - must be in format:
    //  YYYY
    //  +YYYYYY
    //  -YYYYYY
    let years = if lexer.current_equals('+') {
        lexer.advance();
        parse_decimal_digits(&mut lexer, 6)?
    } else if lexer.current_equals('-') {
        lexer.advance();

        let years = parse_decimal_digits(&mut lexer, 6)?;

        // Validate that years cannot be -0
        if years == 0 {
            return None;
        }

        -years
    } else {
        parse_decimal_digits(&mut lexer, 4)?
    };

    // Parse optional month
    let months = if lexer.current_equals('-') {
        lexer.advance();
        parse_decimal_digits(&mut lexer, 2)?
    } else {
        1
    };

    // Validate that month is in range
    if months < 1 || months > 12 {
        return None;
    }

    // Parse optional days
    let days = if lexer.current_equals('-') {
        lexer.advance();
        parse_decimal_digits(&mut lexer, 2)?
    } else {
        1
    };

    // Validate that days is in range
    if days < 1 || days > 31 {
        return None;
    }

    let mut hours = 0;
    let mut minutes = 0;
    let mut seconds = 0;
    let mut milliseconds = 0;

    // Divider between date and time portions
    let has_time = lexer.current_equals('T');
    if has_time {
        lexer.advance();

        // Hours and minutes are required
        hours = parse_decimal_digits(&mut lexer, 2)?;
        lexer.expect(':')?;
        minutes = parse_decimal_digits(&mut lexer, 2)?;

        // Validate that hours and minutes are in range
        if hours > 24 || minutes > 59 {
            return None;
        }

        // Parse optional seconds
        if lexer.current_equals(':') {
            lexer.advance();
            seconds = parse_decimal_digits(&mut lexer, 2)?;
        };

        // Validate that seconds are in range
        if seconds > 59 {
            return None;
        }

        // Parse optional milliseconds
        if lexer.current_equals('.') {
            lexer.advance();
            milliseconds = parse_decimal_digits(&mut lexer, 3)?;
        }
    }

    // Parse optional timezone, calculating timezone offset in milliseconds
    let timezone_offset_milliseconds = if lexer.eat('Z') {
        0
    } else if lexer.current_equals('-') || lexer.current_equals('+') {
        let sign = if lexer.current_equals('+') { 1 } else { -1 };
        lexer.advance();

        let timezone_hour = parse_decimal_digits(&mut lexer, 2)?;
        lexer.expect(':')?;
        let timezone_minute = parse_decimal_digits(&mut lexer, 2)?;

        // Validate that hours and minutes are in range
        if timezone_hour > 23 || timezone_minute > 59 {
            return None;
        }

        sign * (timezone_hour * MS_PER_HOUR as i64 + timezone_minute * MS_PER_MINUTE as i64)
    } else {
        if has_time {
            // TODO: Use current time zone offset
            0
        } else {
            0
        }
    };

    // Make sure we are at the end of the string
    if !lexer.is_end() {
        return None;
    }

    utc_time_from_full_date_parts(
        years,
        months,
        days,
        hours,
        minutes,
        seconds,
        milliseconds,
        timezone_offset_milliseconds,
    )
}

fn utc_time_from_full_date_parts(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: i64,
    millisecond: i64,
    timezone_offset_milliseconds: i64,
) -> Option<f64> {
    let date_part_milliseconds =
        year_month_day_to_days_since_unix_epoch(year, month, day) * MS_PER_DAY as i64;

    let time_part_milliseconds = hour * MS_PER_HOUR as i64
        + minute * MS_PER_MINUTE as i64
        + second * MS_PER_SECOND as i64
        + millisecond;

    let utc_time = date_part_milliseconds + time_part_milliseconds - timezone_offset_milliseconds;

    // Check that time value is in range
    if utc_time.abs() > MAX_TIME_VALUE as i64 {
        return None;
    }

    Some(utc_time as f64)
}

// Parse the string to the date format specified in toString or toUTCString
fn parse_string_to_utc_or_default_date(string: Handle<StringValue>) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    // Both string formats start with the week day
    parse_week_day(&mut lexer)?;

    // Presence of comma indicates UTC format
    let is_utc_format = lexer.eat(',');

    lexer.expect(' ')?;

    // Order of months and days is swapped between formats
    let month; // 0-indexed
    let day;
    if is_utc_format {
        day = parse_decimal_digits(&mut lexer, 2)?;
        lexer.expect(' ')?;
        month = parse_month(&mut lexer)?;
    } else {
        month = parse_month(&mut lexer)?;
        lexer.expect(' ')?;
        day = parse_decimal_digits(&mut lexer, 2)?;
    }

    // Validate range of day
    if day < 1 || day > 31 {
        return None;
    }

    lexer.expect(' ')?;

    // Both formats have optionally negative year
    let year_sign = if lexer.eat('-') { -1 } else { 1 };
    let year = year_sign * parse_decimal_digits(&mut lexer, 4)?;

    lexer.expect(' ')?;

    // Both formats have same time specifier followed by " GMT"
    let hour = parse_decimal_digits(&mut lexer, 2)?;
    lexer.expect(':')?;
    let minute = parse_decimal_digits(&mut lexer, 2)?;
    lexer.expect(':')?;
    let second = parse_decimal_digits(&mut lexer, 2)?;

    // Validate range of hour, minute and second
    if hour > 24 || minute > 59 || second > 59 {
        return None;
    }

    lexer.expect(' ')?;
    lexer.expect('G')?;
    lexer.expect('M')?;
    lexer.expect('T')?;

    let timezone_offset_milliseconds = if is_utc_format {
        // End of UTC format
        0
    } else {
        // The toString format always ends with a time zone
        let sign = if lexer.eat('+') {
            1
        } else {
            lexer.expect('-')?;
            -1
        };

        let timezone_hour = parse_decimal_digits(&mut lexer, 2)?;
        let timezone_minute = parse_decimal_digits(&mut lexer, 2)?;

        // Validate range of timezone's hour and minute
        if timezone_hour > 23 || timezone_minute > 59 {
            return None;
        }

        // TODO: Parse time zone names
        if !lexer.is_end() {
            return None;
        }

        sign * (timezone_hour * MS_PER_HOUR as i64 + timezone_minute * MS_PER_MINUTE as i64)
    };

    // Make sure we are at the end of the string
    if !lexer.is_end() {
        return None;
    }

    utc_time_from_full_date_parts(
        year,
        month + 1,
        day,
        hour,
        minute,
        second,
        0,
        timezone_offset_milliseconds,
    )
}

fn parse_week_day(lexer: &mut StringLexer) -> Option<()> {
    if lexer.eat('S') {
        if lexer.eat('u') {
            lexer.expect('n')?;
        } else {
            lexer.expect('a')?;
            lexer.expect('t')?;
        }
    } else if lexer.eat('M') {
        lexer.expect('o')?;
        lexer.expect('n')?;
    } else if lexer.eat('T') {
        if lexer.eat('u') {
            lexer.expect('e')?;
        } else {
            lexer.expect('h')?;
            lexer.expect('u')?;
        }
    } else if lexer.eat('W') {
        lexer.expect('e')?;
        lexer.expect('d')?;
    } else {
        lexer.expect('F')?;
        lexer.expect('r')?;
        lexer.expect('i')?;
    }

    Some(())
}

fn parse_month(lexer: &mut StringLexer) -> Option<i64> {
    let month_number = if lexer.eat('J') {
        if lexer.eat('a') {
            lexer.expect('n')?;
            0
        } else {
            lexer.expect('u')?;
            if lexer.eat('u') {
                5
            } else {
                lexer.expect('l')?;
                6
            }
        }
    } else if lexer.eat('F') {
        lexer.expect('e')?;
        lexer.expect('b')?;
        1
    } else if lexer.eat('M') {
        if lexer.eat('a') {
            lexer.expect('r')?;
            2
        } else {
            lexer.expect('a')?;
            lexer.expect('y')?;
            4
        }
    } else if lexer.eat('A') {
        if lexer.eat('p') {
            lexer.expect('r')?;
            3
        } else {
            lexer.expect('u')?;
            lexer.expect('g')?;
            7
        }
    } else if lexer.eat('S') {
        lexer.expect('e')?;
        lexer.expect('p')?;
        8
    } else if lexer.eat('O') {
        lexer.expect('c')?;
        lexer.expect('t')?;
        9
    } else if lexer.eat('N') {
        lexer.expect('o')?;
        lexer.expect('v')?;
        10
    } else {
        lexer.expect('D')?;
        lexer.expect('e')?;
        lexer.expect('c')?;
        11
    };

    Some(month_number)
}
