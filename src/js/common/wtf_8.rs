use std::{borrow::Borrow, fmt, hash};

use super::unicode::{decode_wtf8_codepoint, encode_utf8_codepoint, is_ascii};

/// A string using the WTF-8 encoding: https://simonsapin.github.io/wtf-8/.
/// Identical to UTf-8 but also allows unpaired surrogate code points.
#[derive(Clone)]
pub struct Wtf8String {
    buf: Vec<u8>,
}

impl Wtf8String {
    #[inline]
    pub fn new() -> Self {
        Wtf8String { buf: Vec::new() }
    }

    #[inline]
    pub fn from_str(string: &str) -> Self {
        Wtf8String { buf: string.as_bytes().to_vec() }
    }

    #[inline]
    pub fn from_code_point(code_point: u32) -> Self {
        let mut string = Self::new();
        string.push(code_point);

        string
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.buf
    }

    #[inline]
    pub fn push(&mut self, code_point: u32) {
        let mut buf = [0; 4];
        let byte_length = encode_utf8_codepoint(&mut buf, code_point);
        self.buf.extend_from_slice(&buf[..byte_length]);
    }

    #[inline]
    pub fn push_char(&mut self, char: char) {
        self.push(char as u32);
    }

    #[inline]
    pub fn push_wtf8_str(&mut self, string: &Wtf8String) {
        self.buf.extend_from_slice(string.as_bytes());
    }

    #[inline]
    pub fn eq_str(&self, string: &str) -> bool {
        self.buf == string.as_bytes()
    }

    #[inline]
    pub fn iter_code_points<'a>(&'a self) -> Wtf8CodePointsIterator<'a> {
        Wtf8CodePointsIterator::new(&self.buf)
    }
}

pub struct Wtf8CodePointsIterator<'a> {
    buf: &'a [u8],
}

impl<'a> Wtf8CodePointsIterator<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Wtf8CodePointsIterator { buf }
    }
}

impl Iterator for Wtf8CodePointsIterator<'_> {
    type Item = u32;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.buf.is_empty() {
            return None;
        }

        // Check for an ascii byte
        let first_byte = self.buf[0];
        if is_ascii(first_byte as u32) {
            self.buf = &self.buf[1..];
            return Some(first_byte as u32);
        }

        // Otherwise must be a multibyte code point
        match decode_wtf8_codepoint(self.buf) {
            Ok((code_point, byte_length)) => {
                self.buf = &self.buf[byte_length..];
                Some(code_point)
            }
            Err(_) => panic!("Invalid WTF-8 string"),
        }
    }
}

impl fmt::Display for Wtf8String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf8_lossy(&self.buf).to_string().fmt(f)
    }
}

impl fmt::Debug for Wtf8String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.to_string())
    }
}

impl PartialEq for Wtf8String {
    // Must use the same eq as `&[u8]` so that Wtf8String can implement Borrow<[u8]>
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl Eq for Wtf8String {}

impl PartialEq<str> for Wtf8String {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.buf == other.as_bytes()
    }
}

impl PartialEq<&str> for Wtf8String {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.buf == other.as_bytes()
    }
}

impl hash::Hash for Wtf8String {
    // Must use the same hash function as `&[u8]` so that Wtf8String can implement Borrow<[u8]>
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_bytes().hash(state);
    }
}

impl Borrow<[u8]> for Wtf8String {
    #[inline]
    fn borrow(&self) -> &[u8] {
        self.as_bytes()
    }
}
