use icu_collections::codepointinvlist::CodePointInversionList;

use crate::{
    common::unicode::{MAX_LATIN1_CODE_POINT, is_latin1},
    runtime::regexp::compiler::RegExpMatchStart,
};

/// Maximum number of Latin1 members that are stored individually for scanning with memchr.
const MAX_MEMCHR_ARGS: usize = 3;

/// Sentinel meaning there is no memchr set (e.g. due to too many Latin1 members of the set)
const NO_MEMCHR_SET: u8 = u8::MAX;

/// If the code point set contains more than this many Latin1 members then scanning would stop too
/// frequently.
const MAX_LATIN1_DENSITY: u32 = 192;

/// Maximum number of ranges outside the Latin1 range that can be stored. Sets with more ranges
/// than this are over-approximated by treating all code points above the Latin1 range as members.
const MAX_NON_LATIN1_RANGES: usize = 8;

/// Where a RegExp match must start in the input, in an optimized form stored on a CompiledRegExp
/// and used to quickly scan for possible match start positions.
#[repr(C)]
pub struct MatchStartFilter {
    kind: MatchStartKind,
    /// Number of valid entries in the memchr set, or `NO_MEMCHR_SET`
    latin1_memchr_set_len: u8,
    /// The individual Latin1 members of the set, if there are few enough of them
    latin1_memchr_set: [u8; MAX_MEMCHR_ARGS],
    /// Number of ranges used in the non-Latin1 ranges array
    non_latin1_ranges_len: u8,
    /// Whether the set contains all code points above the Latin1 range
    all_above_latin1: bool,
    /// Membership bitset for all Latin1 code points
    latin1_bitset: Latin1BitSet,
    /// A small, sorted sequence of inclusive ranges of non-Latin1 code points in the set
    non_latin1_ranges: [(u32, u32); MAX_NON_LATIN1_RANGES],
}

/// The kind of start position filter for a RegExp.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq)]
pub enum MatchStartKind {
    /// RegExp could match at any position in the input.
    Unknown,
    /// RegExp can only match at the start of the input.
    InputStart,
    /// RegExp can only match at the start of a line.
    Line,
    /// RegExp can only match starting at a member of the code point set described by a filter.
    CodePoints,
}

impl MatchStartFilter {
    pub fn new(regexp_match_start: &RegExpMatchStart) -> Self {
        match regexp_match_start {
            RegExpMatchStart::Unknown => Self::new_empty_set(MatchStartKind::Unknown),
            RegExpMatchStart::InputStart => Self::new_empty_set(MatchStartKind::InputStart),
            RegExpMatchStart::Line => Self::new_empty_set(MatchStartKind::Line),
            RegExpMatchStart::CodePoints(set) => Self::new_code_points(set),
        }
    }

    fn new_empty_set(kind: MatchStartKind) -> Self {
        Self {
            kind,
            latin1_memchr_set_len: NO_MEMCHR_SET,
            latin1_memchr_set: [0; MAX_MEMCHR_ARGS],
            non_latin1_ranges_len: 0,
            all_above_latin1: false,
            latin1_bitset: Latin1BitSet::new(),
            non_latin1_ranges: [(0, 0); MAX_NON_LATIN1_RANGES],
        }
    }

    fn new_code_points(set: &CodePointInversionList) -> Self {
        let mut match_start = Self::new_empty_set(MatchStartKind::CodePoints);
        let mut num_latin1_code_points = 0;

        for range in set.iter_ranges() {
            let (start, end) = (*range.start(), *range.end());

            if is_latin1(start) {
                for code_point in start..=end.min(MAX_LATIN1_CODE_POINT) {
                    // Add Latin1 code points to the bitset
                    match_start.latin1_bitset.insert(code_point as u8);

                    // Add Latin1 code points to the memchr set if there are few enough of them
                    if num_latin1_code_points < MAX_MEMCHR_ARGS {
                        match_start.latin1_memchr_set[num_latin1_code_points] = code_point as u8;
                        match_start.latin1_memchr_set_len = num_latin1_code_points as u8 + 1;
                    } else {
                        match_start.latin1_memchr_set_len = NO_MEMCHR_SET;
                        match_start.latin1_memchr_set = [0; MAX_MEMCHR_ARGS];
                    }

                    num_latin1_code_points += 1;
                }
            }

            // Add non-Latin1 code points to the list of ranges. If too many ranges are added then
            // over-approximate by treating all code points above the Latin1 range as members.
            if !is_latin1(end) {
                let non_latin1_ranges_len = match_start.non_latin1_ranges_len as usize;
                if non_latin1_ranges_len < MAX_NON_LATIN1_RANGES {
                    match_start.non_latin1_ranges[non_latin1_ranges_len] =
                        (start.max(MAX_LATIN1_CODE_POINT + 1), end);
                    match_start.non_latin1_ranges_len += 1;
                } else {
                    match_start.all_above_latin1 = true;
                }
            }
        }

        // Fall back to scanning every code point if we have too many Latin1 members in the filter
        if num_latin1_code_points > MAX_LATIN1_DENSITY as usize {
            return Self::new_empty_set(MatchStartKind::Unknown);
        }

        match_start
    }

    #[inline]
    pub fn kind(&self) -> MatchStartKind {
        self.kind
    }

    /// Whether the set contains the given code point.
    #[inline]
    pub fn contains(&self, code_point: u32) -> bool {
        if code_point < 256 {
            self.latin1_bitset.contains(code_point as u8)
        } else if self.all_above_latin1 {
            true
        } else {
            let non_latin1_ranges = &self.non_latin1_ranges[..self.non_latin1_ranges_len as usize];
            non_latin1_ranges
                .iter()
                .any(|(start, end)| (*start..=*end).contains(&code_point))
        }
    }

    /// Scan a Latin1 buffer for the next member of the set, returning its offset if found.
    #[inline]
    pub fn scan_to_next_latin1_member(&self, buf: &[u8]) -> Option<usize> {
        // Use SIMD accelerated search when there are only a few members to scan for, otherwise
        // test each byte against the bitmap.
        if self.latin1_memchr_set_len != NO_MEMCHR_SET {
            let latin1_memchr_set = &self.latin1_memchr_set[..self.latin1_memchr_set_len as usize];
            match latin1_memchr_set {
                [] => None,
                [m1] => memchr::memchr(*m1, buf),
                [m1, m2] => memchr::memchr2(*m1, *m2, buf),
                [m1, m2, m3] => memchr::memchr3(*m1, *m2, *m3, buf),
                _ => unreachable!(),
            }
        } else {
            buf.iter()
                .position(|byte| self.latin1_bitset.contains(*byte))
        }
    }
}

/// Packed bitset for Latin1 code points.
struct Latin1BitSet {
    bitset: [u64; 4],
}

impl Latin1BitSet {
    pub fn new() -> Latin1BitSet {
        Latin1BitSet { bitset: [0; 4] }
    }

    #[inline]
    pub fn insert(&mut self, code_point: u8) {
        self.bitset[(code_point >> 6) as usize] |= 1 << (code_point & 63);
    }

    #[inline]
    pub fn contains(&self, code_point: u8) -> bool {
        self.bitset[(code_point >> 6) as usize] & (1 << (code_point & 63)) != 0
    }
}
