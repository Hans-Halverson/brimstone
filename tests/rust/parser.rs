use std::rc::Rc;

use brimstone_core::{
    common::{options::OptionsBuilder, wtf_8::Wtf8String},
    parser::{ParseContext, ParseError, parse_script, source::Source},
};

/// Parse a raw buffer of source bytes and assert the parser rejects it as invalid unicode.
fn assert_rejected_as_invalid_unicode(bytes: &[u8]) {
    let options = Rc::new(OptionsBuilder::new().build().unwrap());
    let wtf8_string = Wtf8String::from_bytes_unchecked(bytes);
    let source = Rc::new(Source::new_for_string("<test>", wtf8_string).unwrap());
    let pcx = ParseContext::new(source);

    let Err(err) = parse_script(&pcx, options) else {
        panic!("expected parser error")
    };

    assert!(matches!(err.error, ParseError::InvalidUnicode));
}

#[test]
fn overlong_two_byte_min() {
    // 0xC0 0x80 is an overlong two-byte encoding of U+0000.
    assert_rejected_as_invalid_unicode(&[0xC0, 0x80]);
}

#[test]
fn overlong_two_byte_max() {
    // 0xC1 0xBF is the largest overlong two-byte encoding, of U+007F.
    assert_rejected_as_invalid_unicode(&[0xC1, 0xBF]);
}

#[test]
fn overlong_three_byte_min() {
    // 0xE0 0x80 0x80 is an overlong three-byte encoding of U+0000.
    assert_rejected_as_invalid_unicode(&[0xE0, 0x80, 0x80]);
}

#[test]
fn overlong_three_byte_max() {
    // 0xE0 0x9F 0xBF is the largest overlong three-byte encoding, of U+07FF.
    assert_rejected_as_invalid_unicode(&[0xE0, 0x9F, 0xBF]);
}

#[test]
fn overlong_four_byte_min() {
    // 0xF0 0x80 0x80 0x80 is an overlong four-byte encoding of U+0000.
    assert_rejected_as_invalid_unicode(&[0xF0, 0x80, 0x80, 0x80]);
}

#[test]
fn overlong_four_byte_max() {
    // 0xF0 0x8F 0xBF 0xBF is the largest overlong four-byte encoding, of U+FFFF.
    assert_rejected_as_invalid_unicode(&[0xF0, 0x8F, 0xBF, 0xBF]);
}
