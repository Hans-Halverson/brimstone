/// A single unicode code unit. Value may be in the surrogate pair range.
pub type CodeUnit = u16;

/// A single unicode code point. Value may be in the surrogate pair range, but is guaranteed to
/// be within the full unicode range [0x0-0x10FFFF].
pub type CodePoint = u32;

// Start of high surrogate range, inclusive
const HIGH_SURROGATE_START: CodeUnit = 0xD800;
// End of high surrogate range, inclusive
const HIGH_SURROGATE_END: CodeUnit = 0xDBFF;

// Start of low surrogate range, inclusive
const LOW_SURROGATE_START: CodeUnit = 0xDC00;
// End of low surrogate range, inclusive
const LOW_SURROGATE_END: CodeUnit = 0xDFFF;

#[inline]
pub fn is_high_surrogate_code_unit(code_unit: CodeUnit) -> bool {
    code_unit >= HIGH_SURROGATE_START && code_unit <= HIGH_SURROGATE_END
}

#[inline]
pub fn is_low_surrogate_code_unit(code_unit: CodeUnit) -> bool {
    code_unit >= LOW_SURROGATE_START && code_unit <= LOW_SURROGATE_END
}

pub fn needs_surrogate_pair(char: CodePoint) -> bool {
    char >= '\u{10000}' as u32
}

pub fn try_encode_surrogate_pair(char: CodePoint) -> Option<(CodeUnit, CodeUnit)> {
    if !needs_surrogate_pair(char) {
        return None;
    }

    let offset_char = char as u32 - 0x10000;

    let high_bits = ((offset_char as u32) >> 10) as u16;
    let low_bits = (offset_char as u32 & 0x3FF) as u16;

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
pub fn is_ascii(char: char) -> bool {
    (char as u32) < 0x80
}

#[inline]
pub fn is_latin1_char(char: char) -> bool {
    is_latin1_code_point(char as CodePoint)
}

#[inline]
pub fn is_latin1_code_point(code_point: CodePoint) -> bool {
    code_point <= 0xFF
}

#[inline]
pub fn is_decimal_digit(char: char) -> bool {
    '0' <= char && char <= '9'
}

#[inline]
pub fn is_ascii_whitespace(char: char) -> bool {
    match char {
         ' '
        | '\t'
        // Vertical tab
        | '\u{000B}'
        // Form feed
        | '\u{000C}' => true,
        _ => false,
    }
}

#[inline]
pub fn is_unicode_whitespace(char: char) -> bool {
    match char {
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
    }
}

#[inline]
pub fn is_unicode_newline(char: char) -> bool {
    char == '\u{2028}' || char == '\u{2029}'
}

#[inline]
pub fn is_ascii_newline(char: char) -> bool {
    match char {
        '\n' | '\r' => true,
        _ => false,
    }
}

#[inline]
pub fn is_newline(char: char) -> bool {
    is_ascii_newline(char) || is_unicode_newline(char)
}

pub fn get_binary_value(char: char) -> Option<u32> {
    match char {
        '0' => Some(0),
        '1' => Some(1),
        _ => None,
    }
}

pub fn get_octal_value(char: char) -> Option<u32> {
    match char {
        '0'..='7' => Some(char as u32 - '0' as u32),
        _ => None,
    }
}

pub fn get_hex_value(char: char) -> Option<u32> {
    match char {
        '0'..='9' => Some(char as u32 - '0' as u32),
        'a'..='f' => Some(char as u32 - 'a' as u32 + 10),
        'A'..='F' => Some(char as u32 - 'A' as u32 + 10),
        _ => None,
    }
}
