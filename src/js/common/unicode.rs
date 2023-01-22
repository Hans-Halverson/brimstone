#[inline]
pub fn is_continuation_byte(byte: u8) -> bool {
    (byte & 0xC0) == 0x80
}

#[inline]
pub fn is_ascii(char: char) -> bool {
    (char as u32) < 0x80
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
pub fn is_whitespace(char: char) -> bool {
    is_ascii_whitespace(char) || is_unicode_whitespace(char)
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
