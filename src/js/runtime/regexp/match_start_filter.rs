use std::fmt;

use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};

use crate::{
    common::unicode::{MAX_LATIN1_CODE_POINT, is_latin1, to_string_or_unicode_escape_sequence},
    parser::regexp::{
        Alternative, Assertion, CharacterClass, Disjunction, RegExp, RegExpFlags, Term,
    },
    runtime::regexp::{
        code_point_set::{CodePointSetBuilder, EMPTY_SET},
        compiler::RegExpFlagsStack,
    },
};

/// Maximum number of Latin1 members that are stored individually for scanning with memchr.
const MAX_MEMCHR_ARGS: usize = 3;

/// Sentinel meaning there is no memchr set (e.g. due to too many Latin1 members of the set)
const NO_MEMCHR_SET: u8 = u8::MAX;

/// If the code point set contains more than this many Latin1 members then scanning would stop too
/// frequently.
const MAX_LATIN1_DENSITY: usize = 192;

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
    pub fn new(regexp_match_start: &MatchStartAnalysis) -> Self {
        match regexp_match_start {
            MatchStartAnalysis::Unknown => Self::new_empty_set(MatchStartKind::Unknown),
            MatchStartAnalysis::InputStart => Self::new_empty_set(MatchStartKind::InputStart),
            MatchStartAnalysis::Line => Self::new_empty_set(MatchStartKind::Line),
            MatchStartAnalysis::CodePoints(set) => Self::new_code_points(set),
        }
    }

    fn new_empty_set(kind: MatchStartKind) -> Self {
        Self {
            kind,
            latin1_memchr_set_len: 0,
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
        if num_latin1_code_points > MAX_LATIN1_DENSITY {
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
        if is_latin1(code_point) {
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

/// Where a RegExp match must start in the input. This may be conservative instead of exact.
pub enum MatchStartAnalysis {
    /// RegExp could match at any position in the input.
    Unknown,
    /// RegExp can only match the start of the input.
    InputStart,
    /// RegExp can only match the start of a line.
    Line,
    /// RegExp can only match starting at a specific set of code points.
    CodePoints(CodePointInversionList<'static>),
}

impl fmt::Display for MatchStartAnalysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MatchStartAnalysis::Unknown => f.write_str("Unknown"),
            MatchStartAnalysis::InputStart => f.write_str("Input Start"),
            MatchStartAnalysis::Line => f.write_str("Line"),
            MatchStartAnalysis::CodePoints(code_points) => {
                write!(f, "Code Points(")?;

                for (i, range) in code_points.iter_ranges().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    if range.start() == range.end() {
                        let start_str = to_string_or_unicode_escape_sequence(*range.start());
                        write!(f, "\"{}\"", start_str)?;
                    } else {
                        let start_str = to_string_or_unicode_escape_sequence(*range.start());
                        let end_str = to_string_or_unicode_escape_sequence(*range.end());
                        write!(f, "\"{}\"-\"{}\"", start_str, end_str)?;
                    }
                }

                write!(f, ")")
            }
        }
    }
}

/// Intermediate information about the start of a RegExp, disjunction, alternative, or term during
/// match start analysis.
struct StartInfo {
    /// Only matches if the first code point is in this set. None if the set cannot be statically
    /// computed.
    ///
    /// Note that this set may be an over-approximation.
    first_code_points: Option<CodePointInversionList<'static>>,
    /// Whether this path may be optional, i.e. may match the empty string
    is_optional: bool,
    /// Whether all paths through the RegExp can be considered to have a start anchor (`^`), which
    /// is either the start of the input or the start of a line in multiline mode.
    ///
    /// Note that this may be an under-approximation.
    anchor: Option<StartAnchor>,
}

/// The type of start anchor - either start of the input or start of a line in multiline mode.
///
/// Note that an input start anchor can correctly be treated as a line start anchor.
#[derive(PartialEq)]
enum StartAnchor {
    Input,
    Line,
}

pub struct MatchStartAnalyzer {
    flags: RegExpFlagsStack,
}

impl MatchStartAnalyzer {
    fn new(flags: RegExpFlags) -> Self {
        MatchStartAnalyzer { flags: RegExpFlagsStack::new(flags) }
    }

    pub fn analyze(regexp: &RegExp) -> MatchStartAnalysis {
        let mut analyzer = MatchStartAnalyzer::new(regexp.flags);
        analyzer.analyze_regexp(regexp)
    }

    fn analyze_regexp(&mut self, regexp: &RegExp) -> MatchStartAnalysis {
        let analysis = self.analyze_disjunction_start(&regexp.disjunction);

        // If an anchor is present on all paths it has precedence over code point sets
        match analysis.anchor {
            Some(StartAnchor::Input) => return MatchStartAnalysis::InputStart,
            Some(StartAnchor::Line) => return MatchStartAnalysis::Line,
            None => {}
        }

        // If entire RegExp can match the empty string then start position cannot be determined
        if analysis.is_optional {
            return MatchStartAnalysis::Unknown;
        }

        // If first code point set cannot be determined then start position cannot be determined
        let Some(first_code_points) = analysis.first_code_points else {
            return MatchStartAnalysis::Unknown;
        };

        MatchStartAnalysis::CodePoints(first_code_points)
    }

    fn analyze_disjunction_start(&mut self, disjunction: &Disjunction) -> StartInfo {
        let mut set = CodePointInversionListBuilder::new();
        let mut has_set = true;
        let mut is_optional = disjunction.alternatives.is_empty();
        let mut anchor = if disjunction.alternatives.is_empty() {
            None
        } else {
            Some(StartAnchor::Input)
        };

        for alternative in disjunction.alternatives.iter() {
            let alternative_info = self.analyze_alternative_start(alternative);

            // Combine code point sets for all alternatives
            if let Some(alternative_set) = &alternative_info.first_code_points {
                set.add_set(alternative_set);
            } else {
                has_set = false;
            }

            // If any alternative is optional the entire disjunction is optional
            if alternative_info.is_optional {
                is_optional = true;
            }

            // Combine start anchor analysis for all alternatives
            anchor = match (anchor, alternative_info.anchor) {
                // All alternatives must be anchored for entire disjunction to be anchored
                (_, None) | (None, _) => None,
                // Any line anchored alternative makes the entire disjunction line anchored, even
                // if other alternatives are anchored to the start of the input.
                (Some(anchor), Some(alternative_anchor)) => {
                    if anchor == StartAnchor::Line || alternative_anchor == StartAnchor::Line {
                        Some(StartAnchor::Line)
                    } else {
                        Some(StartAnchor::Input)
                    }
                }
            };
        }

        let first_code_points = if has_set { Some(set.build()) } else { None };

        StartInfo { first_code_points, is_optional, anchor }
    }

    fn analyze_alternative_start(&mut self, alternative: &Alternative) -> StartInfo {
        let mut is_optional = true;
        let mut anchor = None;
        let mut set = CodePointInversionListBuilder::new();
        let mut has_set = true;

        // Whether we can guarantee that no code points have been consumed yet in this
        // alternative. May be an under-approximation.
        let mut no_code_points_consumed = true;

        for term in alternative.terms.iter() {
            let term_info = self.analyze_term_start(term);

            if let Some(term_set) = &term_info.first_code_points {
                set.add_set(term_set);
            } else {
                has_set = false;
            }

            // Alternative is anchored if we can guarantee that a term is anchored before any
            // code points have been consumed.
            if let Some(term_anchor) = term_info.anchor
                && no_code_points_consumed
                && anchor.is_none()
            {
                anchor = Some(term_anchor);
            }

            // Collect code point sets until non-optional term is found
            if !term_info.is_optional {
                is_optional = false;
                break;
            }

            // Any non-empty set may have consumed code points. If the set could not be constructed
            // then pessimistically assume that code points may have been consumed.
            if term_info
                .first_code_points
                .is_none_or(|set| !set.is_empty())
            {
                no_code_points_consumed = false;
            }
        }

        let first_code_points = if has_set { Some(set.build()) } else { None };

        StartInfo { first_code_points, is_optional, anchor }
    }

    fn analyze_term_start(&mut self, term: &Term) -> StartInfo {
        match term {
            // Create set for first code point of literal
            Term::Literal(literal) => {
                let first_code_point = literal.iter_code_points().next().unwrap();

                let set =
                    CodePointSetBuilder::code_point_to_set(first_code_point, self.flags.current());

                StartInfo {
                    first_code_points: Some(set),
                    is_optional: false,
                    anchor: None,
                }
            }
            // Create set for the character class
            Term::CharacterClass(class) => self.analyze_character_class_start(class),
            // Any code point may match so set cannot be computed
            Term::Wildcard => {
                StartInfo { first_code_points: None, is_optional: false, anchor: None }
            }
            // An optional quantifier may match no input,
            Term::Quantifier(quantifier) => {
                let term_info = self.analyze_term_start(&quantifier.term);
                let is_optional = quantifier.min == 0 || term_info.is_optional;
                let anchor = if quantifier.min > 0 {
                    term_info.anchor
                } else {
                    None
                };

                StartInfo {
                    first_code_points: term_info.first_code_points,
                    is_optional,
                    anchor,
                }
            }
            // Assertions and lookarounds do not consume any code points
            Term::Assertion(Assertion::Start) => {
                // Start assertion is either input or line anchored depending on current flags
                let anchor = if self.flags.current().is_multiline() {
                    Some(StartAnchor::Line)
                } else {
                    Some(StartAnchor::Input)
                };

                StartInfo {
                    first_code_points: Some(EMPTY_SET.clone()),
                    is_optional: true,
                    anchor,
                }
            }
            Term::Assertion(_) | Term::Lookaround(_) => StartInfo {
                first_code_points: Some(EMPTY_SET.clone()),
                is_optional: true,
                anchor: None,
            },
            // Descend into capture groups
            Term::CaptureGroup(group) => self.analyze_disjunction_start(&group.disjunction),
            // Descend into anonymous groups, updating the current flags if necessary
            Term::AnonymousGroup(group) => {
                let updated_flags = self.flags.push_group_flags(group);

                let result = self.analyze_disjunction_start(&group.disjunction);

                if updated_flags {
                    self.flags.pop_group_flags();
                }

                result
            }
            // Backreferences match at runtime so we do not attempt to statically compute them.
            // For example a backreference may match a group within an earlier lookaround.
            Term::Backreference(_) => {
                StartInfo { first_code_points: None, is_optional: true, anchor: None }
            }
        }
    }

    /// Return the set of code points to match for a the first code point in a character class.
    /// Includes the case closure if in case-insensitive mode.
    fn analyze_character_class_start(&self, character_class: &CharacterClass) -> StartInfo {
        let flags = self.flags.current();

        let (mut set, strings) =
            CodePointSetBuilder::character_class_to_set(character_class, flags);

        // Invert the set, unless in unicode sets mode which eagerly inverts the set on creation
        if character_class.is_inverted && !flags.has_unicode_sets_flag() {
            let mut builder = CodePointInversionListBuilder::new();
            builder.add_set(&set);
            builder.complement();
            set = builder.build();
        }

        // Add the first code point of all strings to the set. Any empty string makes the entire
        // match optional.
        let mut is_optional = set.is_empty() && strings.is_empty();
        if !strings.is_empty() {
            let mut set_builder = CodePointInversionListBuilder::new();
            set_builder.add_set(&set);

            for string in strings {
                match string.as_str().iter_code_points().next() {
                    Some(first_code_point) => {
                        let set = CodePointSetBuilder::code_point_to_set(first_code_point, flags);
                        set_builder.add_set(&set);
                    }
                    None => is_optional = true,
                }
            }

            set = set_builder.build();
        }

        StartInfo { first_code_points: Some(set), is_optional, anchor: None }
    }
}
