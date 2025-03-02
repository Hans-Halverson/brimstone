use std::{borrow::Borrow, fmt, hash};

use allocator_api2::{
    alloc::{Allocator, Global},
    SliceExt,
};

use super::{
    alloc,
    unicode::{
        decode_wtf8_codepoint, encode_utf8_codepoint, is_ascii, is_high_surrogate_code_point,
        is_low_surrogate_code_point, is_surrogate_code_point,
    },
};

/// A string using the WTF-8 encoding: https://simonsapin.github.io/wtf-8/.
/// Identical to UTf-8 but also allows unpaired surrogate code points.
#[derive(Clone)]
pub struct Wtf8String<A: Allocator + Clone = Global> {
    buf: alloc::Vec<u8, A>,
}

impl Wtf8String<Global> {
    #[inline]
    pub fn new() -> Self {
        Wtf8String { buf: alloc::Vec::new() }
    }

    #[inline]
    pub fn from_string(string: String) -> Self {
        Wtf8String { buf: string.into_bytes() }
    }
}

impl<A: Allocator + Clone> Wtf8String<A> {
    #[inline]
    pub fn new_in(alloc: A) -> Self {
        Wtf8String { buf: alloc::Vec::new_in(alloc) }
    }

    #[inline]
    pub fn from_string_in(string: String, alloc: A) -> Self {
        Self::from_bytes_unchecked_in(string.as_bytes(), alloc)
    }

    #[inline]
    pub fn from_bytes_unchecked_in(bytes: &[u8], alloc: A) -> Self {
        #[allow(unstable_name_collisions)]
        Wtf8String { buf: bytes.to_vec_in(alloc) }
    }

    #[inline]
    pub fn from_char_in(c: char, alloc: A) -> Self {
        let mut buf = [0; 4];
        let byte_length = encode_utf8_codepoint(&mut buf, c as u32);

        #[allow(unstable_name_collisions)]
        Wtf8String { buf: buf[..byte_length].to_vec_in(alloc) }
    }

    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(string: &str) -> Self {
        Self::from_bytes_unchecked(string.as_bytes())
    }

    #[inline]
    pub fn from_code_point(code_point: u32) -> Self {
        let mut string = Self::new();
        string.push(code_point);

        string
    }

    #[inline]
    pub fn clone_in<A2: Allocator + Clone>(&self, alloc: A2) -> Wtf8String<A2> {
        #[allow(unstable_name_collisions)]
        Wtf8String { buf: self.buf.to_vec_in(alloc) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
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
    pub fn push_str(&mut self, string: &str) {
        self.push_bytes_unchecked(string.as_bytes())
    }

    #[inline]
    pub fn push_wtf8_str(&mut self, string: &Wtf8String) {
        self.push_bytes_unchecked(string.as_bytes())
    }

    #[inline]
    pub fn push_bytes_unchecked(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    #[inline]
    pub fn truncate(&mut self, new_length: usize) {
        self.buf.truncate(new_length);
    }

    pub fn repeat(&self, num_times: usize) -> Self {
        Wtf8String { buf: self.buf.repeat(num_times) }
    }

    /// Returns true if the string does not have any unpaired surrogates.
    ///
    /// IsWellFormedUnicode (https://tc39.es/ecma262/#sec-isstringwellformedunicode)
    pub fn is_well_formed(&self) -> bool {
        let mut iter = self.iter_code_points();

        let mut is_well_formed = true;

        while let Some(code_point) = iter.next() {
            if is_surrogate_code_point(code_point) {
                // Only way to be well formed at this point is to be a high surrogate followed by
                // a low surrogate.
                if is_high_surrogate_code_point(code_point) {
                    if let Some(next_code_point) = iter.next() {
                        if is_low_surrogate_code_point(next_code_point) {
                            // A surrogate pair is well formed so proceed to next code point
                            continue;
                        }
                    }
                }

                is_well_formed = false;
                break;
            }

            // Not a surrogate, proceed to next code point
        }

        is_well_formed
    }

    #[inline]
    pub fn iter_code_points(&self) -> Wtf8CodePointsIterator<'_> {
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

impl<A: Allocator + Clone> fmt::Display for Wtf8String<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf8_lossy(&self.buf).to_string().fmt(f)
    }
}

impl<A: Allocator + Clone> fmt::Debug for Wtf8String<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.to_string())
    }
}

impl<A1, A2> PartialEq<Wtf8String<A1>> for Wtf8String<A2>
where
    A1: Allocator + Clone,
    A2: Allocator + Clone,
{
    // Must use the same eq as `&[u8]` so that Wtf8String can implement Borrow<[u8]>
    #[inline]
    fn eq(&self, other: &Wtf8String<A1>) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl<A: Allocator + Clone> Eq for Wtf8String<A> {}

impl<A: Allocator + Clone> PartialEq<str> for Wtf8String<A> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.buf == other.as_bytes()
    }
}

impl<A: Allocator + Clone> PartialEq<&str> for Wtf8String<A> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.buf == other.as_bytes()
    }
}

impl<A: Allocator + Clone> hash::Hash for Wtf8String<A> {
    // Must use the same hash function as `&[u8]` so that Wtf8String can implement Borrow<[u8]>
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_bytes().hash(state);
    }
}

impl<A: Allocator + Clone> Borrow<[u8]> for Wtf8String<A> {
    #[inline]
    fn borrow(&self) -> &[u8] {
        self.as_bytes()
    }
}
