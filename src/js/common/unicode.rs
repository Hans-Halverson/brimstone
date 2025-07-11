use core::panic;

use super::icu::ICU;

use brimstone_macros::match_u32;

/// A single unicode code unit. Value may be in the surrogate pair range.
pub type CodeUnit = u16;

/// A single unicode code point. Value may be in the surrogate pair range, but is guaranteed to
/// be within the full unicode range [0x0-0x10FFFF].
pub type CodePoint = u32;

/// Highest unicode code point
pub const MAX_CODE_POINT: CodePoint = 0x10FFFF;

/// End of the BMP range, inclusive
const BMP_END: CodePoint = 0xFFFF;

/// Start of high surrogate range, inclusive
const HIGH_SURROGATE_START: CodeUnit = 0xD800;
/// End of high surrogate range, inclusive
const HIGH_SURROGATE_END: CodeUnit = 0xDBFF;

/// Start of low surrogate range, inclusive
const LOW_SURROGATE_START: CodeUnit = 0xDC00;
/// End of low surrogate range, inclusive
const LOW_SURROGATE_END: CodeUnit = 0xDFFF;

#[inline]
pub fn is_in_unicode_range(code_point: CodePoint) -> bool {
    code_point <= MAX_CODE_POINT
}

#[inline]
pub fn is_in_bmp_range(code_point: CodePoint) -> bool {
    code_point <= BMP_END
}

#[inline]
pub fn is_surrogate_code_point(code_point: CodePoint) -> bool {
    code_point >= HIGH_SURROGATE_START as u32 && code_point <= LOW_SURROGATE_END as u32
}

#[inline]
pub fn is_high_surrogate_code_point(code_point: CodePoint) -> bool {
    code_point >= HIGH_SURROGATE_START as u32 && code_point <= HIGH_SURROGATE_END as u32
}

#[inline]
pub fn is_low_surrogate_code_point(code_point: CodePoint) -> bool {
    code_point >= LOW_SURROGATE_START as u32 && code_point <= LOW_SURROGATE_END as u32
}

#[inline]
pub fn is_high_surrogate_code_unit(code_unit: CodeUnit) -> bool {
    (HIGH_SURROGATE_START..=HIGH_SURROGATE_END).contains(&code_unit)
}

#[inline]
pub fn is_low_surrogate_code_unit(code_unit: CodeUnit) -> bool {
    (LOW_SURROGATE_START..=LOW_SURROGATE_END).contains(&code_unit)
}

pub fn needs_surrogate_pair(code_point: CodePoint) -> bool {
    code_point >= '\u{10000}' as u32
}

pub fn try_encode_surrogate_pair(code_point: CodePoint) -> Option<(CodeUnit, CodeUnit)> {
    if !needs_surrogate_pair(code_point) {
        return None;
    }

    let offset_code_point = code_point - 0x10000;

    let high_bits = ((offset_code_point) >> 10) as u16;
    let low_bits = (offset_code_point & 0x3FF) as u16;

    Some((high_bits + HIGH_SURROGATE_START, low_bits + LOW_SURROGATE_START))
}

pub fn code_point_from_surrogate_pair(high: CodeUnit, low: CodeUnit) -> CodePoint {
    // High 10 bits are encoded in the the high surrogate
    let high_bits = (high - HIGH_SURROGATE_START) as u32;

    // Low 10 bits are encoded in the low surrogate
    let low_bits = (low - LOW_SURROGATE_START) as u32;

    ((high_bits << 10) | low_bits) + 0x10000
}

#[inline]
pub fn is_continuation_byte(byte: u8) -> bool {
    (byte & 0xC0) == 0x80
}

#[inline]
pub fn is_ascii(code_point: CodePoint) -> bool {
    code_point < 0x80
}

#[inline]
pub fn is_latin1(code_point: CodePoint) -> bool {
    code_point <= 0xFF
}

#[inline]
pub fn is_decimal_digit(code_point: CodePoint) -> bool {
    '0' as u32 <= code_point && code_point <= '9' as u32
}

#[inline]
pub fn is_ascii_whitespace(code_point: CodePoint) -> bool {
    match_u32!(match code_point {
         ' '
        | '\t'
        // Vertical tab
        | '\u{000B}'
        // Form feed
        | '\u{000C}' => true,
        _ => false,
    })
}

#[inline]
pub fn is_unicode_whitespace(code_point: CodePoint) -> bool {
    match_u32!(match code_point {
        // All non-ascii characters in the unicode Space_Separator category
        '\u{00A0}'
        | '\u{1680}'
        | '\u{2000}'..='\u{200A}'
        | '\u{202F}'
        | '\u{205F}'
        | '\u{3000}'
        // And the zero width non breaking space
        | '\u{FEFF}'
        => true,
        _ => false,
    })
}

#[inline]
pub fn is_whitespace(code_point: CodePoint) -> bool {
    is_ascii_whitespace(code_point) || is_unicode_whitespace(code_point)
}

#[inline]
pub fn is_unicode_newline(code_point: CodePoint) -> bool {
    code_point == '\u{2028}' as u32 || code_point == '\u{2029}' as u32
}

#[inline]
pub fn is_ascii_newline(code_point: CodePoint) -> bool {
    match_u32!(match code_point {
        '\n' | '\r' => true,
        _ => false,
    })
}

#[inline]
pub fn is_newline(code_point: CodePoint) -> bool {
    is_ascii_newline(code_point) || is_unicode_newline(code_point)
}

#[inline]
pub fn is_ascii_alphabetic(code_point: CodePoint) -> bool {
    match_u32!(match code_point {
        'a'..='z' | 'A'..='Z' => true,
        _ => false,
    })
}

pub fn get_binary_value(code_point: CodePoint) -> Option<u32> {
    match_u32!(match code_point {
        '0' => Some(0),
        '1' => Some(1),
        _ => None,
    })
}

pub fn get_octal_value(code_point: CodePoint) -> Option<u32> {
    match_u32!(match code_point {
        '0'..='7' => Some(code_point - '0' as u32),
        _ => None,
    })
}

pub fn get_hex_value(code_point: CodePoint) -> Option<u32> {
    match_u32!(match code_point {
        '0'..='9' => Some(code_point - '0' as u32),
        'a'..='f' => Some(code_point - 'a' as u32 + 10),
        'A'..='F' => Some(code_point - 'A' as u32 + 10),
        _ => None,
    })
}

/// Lookup table for ASCII code points that can appear as the first character of an identifier.
///
/// Generated from:
///   is_ascii_alphabetic(code_point) || code_point == '_' as u32 || code_point == '$' as u32
const ASCII_ID_START: [bool; 128] = [
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false, false, false, true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
    true, true, true, true, true, true, true, true, true, true, false, false, false, false, true,
    false, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
    true, true, true, true, true, true, true, true, true, true, true, true, false, false, false,
    false, false,
];

/// Lookup table for ASCII code points that can appear in an identifier after the first character.
///
/// Generated from:
///   is_ascii_alphabetic(code_point)
///       || is_decimal_digit(code_point)
///       || code_point == '_' as u32
///       || code_point == '$' as u32
const ASCII_ID_PART: [bool; 128] = [
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false, false, false, true, false, false,
    false, false, false, false, false, false, false, false, false, true, true, true, true, true,
    true, true, true, true, true, false, false, false, false, false, false, false, true, true,
    true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
    true, true, true, true, true, true, true, true, false, false, false, false, true, false, true,
    true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
    true, true, true, true, true, true, true, true, true, false, false, false, false, false,
];

/// Can this code point appear as the first code point of an identifier.
pub fn is_id_start_ascii(code_point: CodePoint) -> bool {
    ASCII_ID_START
        .get(code_point as usize)
        .copied()
        .unwrap_or(false)
}

/// Can this code point appear in an identifier (after the first character).
pub fn is_id_part_ascii(code_point: CodePoint) -> bool {
    ASCII_ID_PART
        .get(code_point as usize)
        .copied()
        .unwrap_or(false)
}

#[inline]
pub fn is_id_start_unicode(code_point: CodePoint) -> bool {
    ICU.properties.id_start.contains32(code_point)
}

#[inline]
pub fn is_id_continue_unicode(code_point: CodePoint) -> bool {
    ICU.properties.id_continue.contains32(code_point)
}

#[inline]
pub fn is_id_part_unicode(code_point: CodePoint) -> bool {
    // Either part of the unicode ID_Continue, ZWNJ, or ZWJ
    is_id_continue_unicode(code_point)
        || code_point == ('\u{200C}' as u32)
        || code_point == ('\u{200D}' as u32)
}

#[inline]
pub fn is_id_start(code_point: CodePoint) -> bool {
    is_id_start_ascii(code_point) || is_id_start_unicode(code_point)
}

#[inline]
pub fn is_id_part(code_point: CodePoint) -> bool {
    is_id_part_ascii(code_point) || is_id_part_unicode(code_point)
}

#[inline]
pub fn as_id_start(code_point: CodePoint) -> Option<char> {
    if is_id_start(code_point) {
        // Safe as all ID start code points are valid unicode code points
        let char = unsafe { char::from_u32_unchecked(code_point) };
        Some(char)
    } else {
        None
    }
}

#[inline]
pub fn as_id_part(code_point: CodePoint) -> Option<char> {
    if is_id_part(code_point) {
        // Safe as all ID part code points are valid unicode code points
        let char = unsafe { char::from_u32_unchecked(code_point) };
        Some(char)
    } else {
        None
    }
}

#[inline]
pub fn as_id_start_unicode(code_point: CodePoint) -> Option<char> {
    if is_id_start_unicode(code_point) {
        // Safe as all ID start code points are valid unicode code points
        let char = unsafe { char::from_u32_unchecked(code_point) };
        Some(char)
    } else {
        None
    }
}

#[inline]
pub fn as_id_part_ascii(code_point: CodePoint) -> Option<char> {
    if is_id_part_ascii(code_point) {
        // Safe as all ID part code points are valid unicode code points
        let char = unsafe { char::from_u32_unchecked(code_point) };
        Some(char)
    } else {
        None
    }
}

#[inline]
pub fn as_id_part_unicode(code_point: CodePoint) -> Option<char> {
    if is_id_part_unicode(code_point) {
        // Safe as all ID part code points are valid unicode code points
        let char = unsafe { char::from_u32_unchecked(code_point) };
        Some(char)
    } else {
        None
    }
}

/// Encode a code point (including surrogate pairs) as UTF-8 into the given buffer. Must only be
/// called on code points that are in the valid unicode range [0x0-0x10FFFF]. Must only be called
/// when the buffer has room for the encoded code point.
///
/// Return the number of bytes for the encoded code point.
pub fn encode_utf8_codepoint(buf: &mut [u8], code_point: CodePoint) -> usize {
    if code_point < 0x80 && !buf.is_empty() {
        buf[0] = code_point as u8;
        1
    } else if code_point <= 0x7FF && buf.len() >= 2 {
        buf[0] = 0xC0 | (code_point >> 6) as u8;
        buf[1] = 0x80 | (code_point & 0x3F) as u8;
        2
    } else if code_point <= 0xFFFF && buf.len() >= 3 {
        buf[0] = 0xE0 | (code_point >> 12) as u8;
        buf[1] = 0x80 | ((code_point >> 6) & 0x3F) as u8;
        buf[2] = 0x80 | (code_point & 0x3F) as u8;
        3
    } else if code_point <= 0x10FFFF && buf.len() >= 4 {
        buf[0] = 0xF0 | (code_point >> 18) as u8;
        buf[1] = 0x80 | ((code_point >> 12) & 0x3F) as u8;
        buf[2] = 0x80 | ((code_point >> 6) & 0x3F) as u8;
        buf[3] = 0x80 | (code_point & 0x3F) as u8;
        4
    } else {
        panic!("Code point out of range")
    }
}

/// Return the number of bytes needed to encode the given code point in UTF-8.
pub fn utf8_byte_count(code_point: CodePoint) -> usize {
    if code_point < 0x80 {
        1
    } else if code_point <= 0x7FF {
        2
    } else if code_point <= 0xFFFF {
        3
    } else if code_point <= 0x10FFFF {
        4
    } else {
        panic!("Code point out of range")
    }
}

/// Lex a non-ascii unicode codepoint encoded as UTF-8. Must only be called when we know the
/// current byte is in-bounds and is not ASCII meaning it is the start of a UTF-8 byte sequence.
///
/// Returns the codepoint as well as its length in bytes. If no valid codepoint could be parsed,
/// return an error with the length of the invalid bytes.
pub fn decode_wtf8_codepoint(buf: &[u8]) -> Result<(CodePoint, usize), usize> {
    let b1 = buf[0];

    if (b1 & 0xE0) == 0xC0 && buf.len() >= 2 {
        // Two byte sequence
        let b2 = buf[1];

        if !is_continuation_byte(b2) {
            return Err(2);
        }

        let mut codepoint = (b1 as u32 & 0x1F) << 6;
        codepoint |= b2 as u32 & 0x3F;

        Ok((codepoint, 2))
    } else if (b1 & 0xF0) == 0xE0 && buf.len() >= 3 {
        // Three byte sequence
        let b2 = buf[1];
        let b3 = buf[2];

        if !is_continuation_byte(b2) || !is_continuation_byte(b3) {
            return Err(3);
        }

        let mut codepoint = (b1 as u32 & 0x0F) << 12;
        codepoint |= (b2 as u32 & 0x3F) << 6;
        codepoint |= b3 as u32 & 0x3F;

        Ok((codepoint, 3))
    } else if (b1 & 0xF8) == 0xF0 && buf.len() >= 4 {
        // Four byte sequence
        let b2 = buf[1];
        let b3 = buf[2];
        let b4 = buf[3];

        if !is_continuation_byte(b2) || !is_continuation_byte(b3) || !is_continuation_byte(b4) {
            return Err(4);
        }

        let mut codepoint = (b1 as u32 & 0x07) << 18;
        codepoint |= (b2 as u32 & 0x3F) << 12;
        codepoint |= (b3 as u32 & 0x3F) << 6;
        codepoint |= b4 as u32 & 0x3F;

        Ok((codepoint, 4))
    } else {
        Err(1)
    }
}

/// Encode a code point (including surrogate pairs) as UTF-16 into the given buffer. Must only be
/// called on code points that are in the valid unicode range [0x0-0x10FFFF]. Must only be called
/// when the buffer has room for the encoded code point.
///
/// Return the number of code units for the encoded code point.
pub fn encode_utf16_codepoint(buf: &mut [CodeUnit], code_point: CodePoint) -> usize {
    if is_in_bmp_range(code_point) {
        buf[0] = code_point as u16;
        1
    } else {
        let offset = code_point - 0x10000;

        let high_bits = ((offset) >> 10) as u16;
        let low_bits = (offset & 0x3FF) as u16;

        buf[0] = high_bits + HIGH_SURROGATE_START;
        buf[1] = low_bits + LOW_SURROGATE_START;

        2
    }
}

/// Return the number of ode units needed to encode the given code point in UTF-16.
pub fn utf16_code_unit_count(code_point: CodePoint) -> usize {
    if is_in_bmp_range(code_point) {
        1
    } else {
        2
    }
}

pub fn to_string_or_unicode_escape_sequence(code_point: CodePoint) -> String {
    if let Some(char) = char::from_u32(code_point) {
        return String::from(char);
    }

    // Code points in the surrogate pair range are encoded as a \uXXXX unicode escape sequence
    format!("\\u{code_point:X}")
}
