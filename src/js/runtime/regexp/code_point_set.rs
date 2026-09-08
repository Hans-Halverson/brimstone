use std::fmt::Write;

use bitflags::bitflags;
use icu_collections::codepointinvlist::CodePointInversionList;

use crate::{
    common::unicode::{
        CodePoint, MAX_LATIN1_CODE_POINT, is_latin1, to_string_or_unicode_escape_sequence,
    },
    runtime::regexp::lexer_stream::RegExpLexerStream,
};

bitflags! {
    #[derive(Clone, Copy)]
    pub struct CodePointSetFlags: u8 {
        /// Code point is a member iff it is not in the set.
        const IS_INVERTED = 1 << 0;
        /// Set has two-byte (non-Latin1) ranges in addition to the Latin1 bitset.
        const HAS_TWO_BYTE_RANGES = 1 << 1;
    }
}

impl CodePointSetFlags {
    #[inline]
    pub fn is_inverted(self) -> bool {
        self.contains(CodePointSetFlags::IS_INVERTED)
    }

    #[inline]
    pub fn has_two_byte_ranges(self) -> bool {
        self.contains(CodePointSetFlags::HAS_TWO_BYTE_RANGES)
    }
}

/// Packed bitset for Latin1 code points.
#[repr(C)]
pub struct Latin1BitSet {
    bitset: [u32; Self::NUM_U32_WORDS],
}

impl Latin1BitSet {
    /// Number of u32's that make up the Latin1 bitset.
    const NUM_U32_WORDS: usize = 8;

    pub fn new() -> Latin1BitSet {
        Latin1BitSet { bitset: [0; Self::NUM_U32_WORDS] }
    }

    #[inline]
    pub fn insert(&mut self, code_point: u8) {
        self.bitset[(code_point >> 5) as usize] |= 1 << (code_point & 31);
    }

    #[inline]
    pub fn contains(&self, code_point: u8) -> bool {
        self.bitset[(code_point >> 5) as usize] & (1 << (code_point & 31)) != 0
    }
}

/// An encoded representation of a code point set in the constant table. Contains utilities for
/// encoding, decoding, and querying the set.
///
/// Code point sets are encoded as [u32], where the first [u32; 8] is the Latin1 bitset, followed by
/// an optional length-prefixed list of range boundaries for non-Latin1 code points. Range
/// boundaries appear in pairs which represent half-open ranges of code points in the set.
pub struct EncodedCodePointSet {
    /// Flags describing the set's properties.
    pub flags: CodePointSetFlags,
    /// Pointer to the start of the encoded set in the constant table.
    pub encoded_base: *const u32,
}

impl EncodedCodePointSet {
    /// Encode a code point set for storage in the constant table, returning the flags and a vector
    /// of encoded u32's.
    pub fn encode(set: &CodePointInversionList) -> (CodePointSetFlags, Vec<u32>) {
        let mut latin1_bitset = Latin1BitSet::new();
        let mut boundaries: Vec<u32> = vec![];

        for range in set.iter_ranges() {
            let (start, end) = (*range.start(), *range.end());

            if is_latin1(start) {
                for code_point in start..=end.min(MAX_LATIN1_CODE_POINT) {
                    latin1_bitset.insert(code_point as u8);
                }
            }

            if !is_latin1(end) {
                boundaries.push(start.max(MAX_LATIN1_CODE_POINT + 1));
                boundaries.push(end + 1);
            }
        }

        let mut encoded = latin1_bitset.bitset.to_vec();
        let mut flags = CodePointSetFlags::empty();

        if !boundaries.is_empty() {
            flags |= CodePointSetFlags::HAS_TWO_BYTE_RANGES;
            encoded.push(boundaries.len() as u32);
            encoded.extend_from_slice(&boundaries);
        }

        (flags, encoded)
    }

    /// Reference to the Latin1 bitset in the encoded set.
    #[inline]
    fn latin1_bitset<'a>(&self) -> &'a Latin1BitSet {
        unsafe { &*(self.encoded_base.cast::<Latin1BitSet>()) }
    }

    /// Slice containing the ordered boundaries of all two-byte ranges in the encoded set.
    #[inline]
    fn two_byte_ranges<'a>(&self) -> &'a [u32] {
        unsafe {
            let num_boundaries = *self.encoded_base.add(Latin1BitSet::NUM_U32_WORDS) as usize;
            let boundaries_ptr = self.encoded_base.add(Latin1BitSet::NUM_U32_WORDS + 1);

            std::slice::from_raw_parts(boundaries_ptr, num_boundaries)
        }
    }

    /// Whether the encoded set contains a code point.
    ///
    /// Parameterized by the underlying lexer stream type to allow for specialization of the check
    /// for streams that only support Latin1 code points.
    #[inline]
    pub fn contains<T: RegExpLexerStream>(&self, code_point: u32) -> bool {
        // First check the Latin1 bitset
        let latin1_bitset = self.latin1_bitset();
        let mut hit =
            latin1_bitset.contains(code_point as u8) & (code_point <= MAX_LATIN1_CODE_POINT);

        // Then check the two-byte ranges if there are any and the underlying lexer stream needs it.
        //
        // The range boundaries are stored in sorted order, so the code point is a member iff an odd
        // number of boundaries are <= the code point, meaning the code point is within a range.
        if T::MAX_CODE_POINT > MAX_LATIN1_CODE_POINT && self.flags.has_two_byte_ranges() {
            let range_boundaries = self.two_byte_ranges();
            let num_boundaries_lte =
                range_boundaries.partition_point(|&boundary| boundary <= code_point);
            hit |= num_boundaries_lte % 2 == 1;
        }

        hit
    }

    pub fn debug_format(&self) -> String {
        let mut result = String::new();

        for (i, (start, end)) in self.decode_all_ranges().into_iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }

            let start_str = to_string_or_unicode_escape_sequence(start);
            if start == end {
                write!(result, "\"{}\"", start_str).unwrap();
            } else {
                let end_str = to_string_or_unicode_escape_sequence(end);
                write!(result, "\"{}\"-\"{}\"", start_str, end_str).unwrap();
            }
        }

        result
    }

    /// Decode all set ranges including both Latin1 and two-byte ranges.
    fn decode_all_ranges(&self) -> Vec<(CodePoint, CodePoint)> {
        let mut ranges = vec![];

        // Decode Latin1 bitset into ranges by testing every Latin1 code point
        let mut range_start: Option<u32> = None;
        let latin1_bitset = self.latin1_bitset();

        for code_point in 0..=MAX_LATIN1_CODE_POINT {
            let is_member = latin1_bitset.contains(code_point as u8);
            match (is_member, range_start) {
                (true, None) => range_start = Some(code_point),
                (false, Some(start)) => {
                    ranges.push((start, code_point - 1));
                    range_start = None;
                }
                _ => {}
            }
        }

        // Add the last range if it extends to the end of the Latin1 range
        if let Some(start) = range_start {
            ranges.push((start, MAX_LATIN1_CODE_POINT));
        }

        // Decode two-byte ranges
        if self.flags.has_two_byte_ranges() {
            for range in self.two_byte_ranges().chunks(2) {
                ranges.push((range[0], range[1] - 1));
            }
        }

        ranges
    }
}
