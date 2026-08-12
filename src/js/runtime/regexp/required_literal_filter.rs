use std::{cmp::Ordering, fmt, ops::Range};

use memchr::memmem;

use crate::{
    common::unicode::{
        CodePoint, is_ascii, is_ascii_alphabetic, is_latin1, to_string_or_unicode_escape_sequence,
        two_byte_slice_as_bytes,
    },
    parser::{
        ast::AstStr,
        regexp::{Alternative, Disjunction, RegExp, RegExpFlags, Term},
    },
    runtime::regexp::{code_point_set::CodePointSetBuilder, compiler::RegExpFlagsStack},
};

/// Maximum number of code points in a required literal filter. Longer literals are truncated.
const MAX_LITERAL_LEN: usize = 16;

/// Literals shorter than this aren't worth it and rely on the MatchStartFilter scan.
const MIN_LITERAL_LEN: usize = 3;

/// A sequence of Latin1 code points that must appear for a RegExp to match. Stored on a
/// CompiledRegExp and used to efficiently scan for possible match start positions in the input.
#[repr(C)]
pub struct RequiredLiteralFilter {
    /// Number of bytes aka Latin1 code points in the literal. Zero if there is no literal.
    len: u8,
    /// Bytes aka Latin1 code points of the literal.
    bytes: [u8; MAX_LITERAL_LEN],
    /// The number of code points between the start of a match and the start of the literal.
    offset: Width,
}

impl RequiredLiteralFilter {
    fn new_at(offset: Width) -> Self {
        Self { len: 0, bytes: [0; MAX_LITERAL_LEN], offset }
    }

    fn new_none() -> Self {
        Self::new_at(Width::new_unbounded())
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn min_offset(&self) -> usize {
        self.offset.min as usize
    }

    #[inline]
    pub fn max_offset(&self) -> usize {
        self.offset.max as usize
    }

    #[inline]
    pub fn has_bounded_offset(&self) -> bool {
        !self.offset.is_unbounded()
    }

    /// Whether the literal appears at a single known offset within the match.
    #[inline]
    pub fn has_exact_offset(&self) -> bool {
        !self.offset.is_unbounded() && self.offset.min == self.offset.max
    }

    /// The range of positions that must be searched to determine whether the literal is present
    /// for a match starting at `pos`, None if the literal's offset is unbounded in the match.
    ///
    /// Must specify the maximum number of code units per code point in the buffer so that we can
    /// conservatively widen the search range to account for multi-unit code points.
    pub fn search_window(
        &self,
        pos: usize,
        buf_len: usize,
        max_code_units_per_code_point: usize,
    ) -> Option<Range<usize>> {
        if self.offset.is_unbounded() {
            return None;
        }

        let window_len = (self.max_offset() + self.len as usize) * max_code_units_per_code_point;

        let start = (pos + self.min_offset()).min(buf_len);
        let end = (pos + window_len).min(buf_len).max(start);

        Some(start..end)
    }

    #[inline]
    fn bytes(&self) -> &[u8] {
        &self.bytes[..self.len as usize]
    }

    /// Widen the literal to two byte code units so that it can be searched for in a two byte string
    fn widen_two_byte_literal<'a>(&self, literal: &'a mut [u8]) -> &'a [u8] {
        let literal = &mut literal[..(self.len as usize) * 2];
        for (code_unit, byte) in literal.chunks_exact_mut(2).zip(self.bytes()) {
            code_unit.copy_from_slice(&(*byte as u16).to_ne_bytes());
        }

        literal
    }

    /// Whether a one byte (Latin1) buffer contains the required literal.
    #[inline]
    pub fn matches_one_byte(&self, buf: &[u8]) -> bool {
        self.is_empty() || memmem::find(buf, self.bytes()).is_some()
    }

    /// Whether a two byte buffer contains the required literal.
    #[inline]
    pub fn matches_two_byte(&self, buf: &[u16]) -> bool {
        if self.is_empty() {
            return true;
        }

        let mut wide_literal = [0; MAX_LITERAL_LEN * 2];
        let wide_literal = self.widen_two_byte_literal(&mut wide_literal);

        find_two_byte_with(&memmem::Finder::new(wide_literal), buf).is_some()
    }

    /// Whether the literal appears at exactly the given position in a one byte (Latin1) buffer.
    #[inline]
    pub fn matches_one_byte_at(&self, buf: &[u8], pos: usize) -> bool {
        match buf.get(pos..pos + self.len as usize) {
            Some(slice) => slice == self.bytes(),
            None => false,
        }
    }

    /// Whether the literal appears at exactly the given position in a two byte buffer.
    #[inline]
    pub fn matches_two_byte_at(&self, buf: &[u16], pos: usize) -> bool {
        match buf.get(pos..pos + self.len as usize) {
            Some(slice) => slice
                .iter()
                .zip(self.bytes())
                .all(|(code_unit, byte)| *code_unit == *byte as u16),
            None => false,
        }
    }
}

impl fmt::Display for RequiredLiteralFilter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\"")?;
        for byte in self.bytes() {
            f.write_str(&to_string_or_unicode_escape_sequence(*byte as CodePoint))?;
        }
        f.write_str("\"")?;

        write!(f, " at {}-", self.offset.min)?;
        if self.offset.is_unbounded() {
            f.write_str("any")
        } else {
            write!(f, "{}", self.offset.max)
        }
    }
}

const UNBOUNDED_WIDTH: u32 = u32::MAX;

/// Inclusive range for the number of code points that a pattern may consume. Max may be
/// `UNBOUNDED_WIDTH` if there is no upper bound on the number of code points consumed.
#[repr(C)]
#[derive(Clone, Copy)]
struct Width {
    min: u32,
    max: u32,
}

impl Width {
    fn new_exact(width: u32) -> Width {
        Width { min: width, max: width }
    }

    fn new_unbounded() -> Width {
        Width { min: 0, max: UNBOUNDED_WIDTH }
    }

    fn is_unbounded(&self) -> bool {
        self.max == UNBOUNDED_WIDTH
    }

    /// Combine the widths of two adjacent patterns.
    fn concat(self, other: Width) -> Width {
        Width {
            min: self.min.saturating_add(other.min),
            max: self.max.saturating_add(other.max),
        }
    }

    /// Combine the widths of two patterns matched as alternatives of each other.
    fn union(self, other: Width) -> Width {
        Width { min: self.min.min(other.min), max: self.max.max(other.max) }
    }

    /// The width of a pattern repeated a minimum and optional maximum number of times.
    fn repeat(self, min_repetitions: u64, max_repetitions: Option<u64>) -> Width {
        let min = Self::repeat_until_unbounded(self.min, min_repetitions);
        let max = max_repetitions
            .map_or(UNBOUNDED_WIDTH, |count| Self::repeat_until_unbounded(self.max, count));

        Width { min, max }
    }

    /// Repeat a width with a maximum of `UNBOUNDED_WIDTH`.
    fn repeat_until_unbounded(width: u32, count: u64) -> u32 {
        (width as u64)
            .saturating_mul(count)
            .min(UNBOUNDED_WIDTH as u64) as u32
    }
}

/// The result of walking a subpattern for the literal it requires.
struct LiteralAnalysis {
    /// The longest required literal found in this subpattern, if any.
    literal: Option<RequiredLiteralFilter>,
    /// Width of the entire subpattern itself (not just the required literal).
    width: Width,
}

impl LiteralAnalysis {
    fn new(literal: Option<RequiredLiteralFilter>, width: Width) -> LiteralAnalysis {
        LiteralAnalysis { literal, width }
    }

    fn new_width_only(width: Width) -> LiteralAnalysis {
        LiteralAnalysis { literal: None, width }
    }
}

pub struct RequiredLiteralAnalyzer {
    flags: RegExpFlagsStack,
}

impl RequiredLiteralAnalyzer {
    fn new(flags: RegExpFlags) -> Self {
        RequiredLiteralAnalyzer { flags: RegExpFlagsStack::new(flags) }
    }

    /// Find the longest literal that must be present in the input for the RegExp to match.
    ///
    /// Note that this analysis is conservative and may be an under-approximation.
    pub fn analyze(regexp: &RegExp) -> RequiredLiteralFilter {
        let mut analyzer = RequiredLiteralAnalyzer::new(regexp.flags);
        let analysis = analyzer.analyze_disjunction(&regexp.disjunction);

        analysis
            .literal
            .unwrap_or_else(RequiredLiteralFilter::new_none)
    }

    fn analyze_disjunction(&mut self, disjunction: &Disjunction) -> LiteralAnalysis {
        // We do not analyze multiple alternatives for a shared literal, so a required literal can
        // only be found when there is exactly one alternative.
        if let [alternative] = &*disjunction.alternatives {
            return self.analyze_alternative(alternative);
        }

        // Otherwise only the width of the disjunction is needed, which is the union of the widths
        // of all alternatives. Disjunction is guaranteed to have at least one alternative.
        let (first_alternative, rest) = disjunction.alternatives.split_first().unwrap();

        let mut width = self.analyze_alternative(first_alternative).width;
        for alternative in rest {
            width = width.union(self.analyze_alternative(alternative).width);
        }

        LiteralAnalysis::new_width_only(width)
    }

    fn analyze_alternative(&mut self, alternative: &Alternative) -> LiteralAnalysis {
        let mut best_literal: Option<RequiredLiteralFilter> = None;

        // Width of all terms visited so far in the alternative
        let mut current_width = Width::new_exact(0);

        for term in alternative.terms.iter() {
            let analysis = self.analyze_term(term);

            // Literal from the term must be offset by the width of all previous terms
            if let Some(mut literal) = analysis.literal {
                literal.offset = literal.offset.concat(current_width);
                Self::update_best_literal(&mut best_literal, literal);
            }

            current_width = current_width.concat(analysis.width);
        }

        LiteralAnalysis::new(best_literal, current_width)
    }

    fn analyze_term(&mut self, term: &Term) -> LiteralAnalysis {
        match term {
            // A literal term finds the best required literal within it
            Term::Literal(literal) => self.analyze_literal(literal),
            Term::Wildcard => LiteralAnalysis::new_width_only(Width::new_exact(1)),
            // A character class matches a single code point, unless it may contain strings in which
            // case strings of any length can be matched.
            Term::CharacterClass(character_class) => {
                let width = if character_class.may_contain_strings {
                    Width::new_unbounded()
                } else {
                    Width::new_exact(1)
                };

                LiteralAnalysis::new_width_only(width)
            }
            // Quantifiers require the literal of their inner term if there is at least one required
            // repetition.
            Term::Quantifier(quantifier) => {
                let analysis = self.analyze_term(&quantifier.term);

                let literal = if quantifier.min >= 1 {
                    analysis.literal
                } else {
                    None
                };

                let width = analysis.width.repeat(quantifier.min, quantifier.max);

                LiteralAnalysis::new(literal, width)
            }
            // Assertions and lookarounds have no width
            Term::Assertion(_) | Term::Lookaround(_) => {
                LiteralAnalysis::new_width_only(Width::new_exact(0))
            }
            // Descend into groups, updating the flags if necessary
            Term::CaptureGroup(group) => self.analyze_disjunction(&group.disjunction),
            Term::AnonymousGroup(group) => {
                let updated_flags = self.flags.push_group_flags(group);

                let analysis = self.analyze_disjunction(&group.disjunction);

                if updated_flags {
                    self.flags.pop_group_flags();
                }

                analysis
            }
            // Backreference is unbounded since we cannot statically determine width
            Term::Backreference(_) => LiteralAnalysis::new_width_only(Width::new_unbounded()),
        }
    }

    /// Whether a code point can be searched for verbatim in the input.
    fn is_searchable_code_point(&self, code_point: CodePoint) -> bool {
        // Only one-byte (Latin1) code points are stored in the filter
        if !is_latin1(code_point) {
            return false;
        }

        // In case insensitive mode only code points without case variants can be searched directly
        let flags = self.flags.current();
        if !flags.is_case_insensitive() {
            return true;
        }

        // Fast path for ASCII code points, as only alphabetic code points have case variants
        if is_ascii(code_point) {
            !is_ascii_alphabetic(code_point)
        } else {
            CodePointSetBuilder::code_point_to_set(code_point, flags).size() == 1
        }
    }

    /// Prefer longer literals and bounded offsets for stronger filtering.
    fn update_best_literal(
        best_literal: &mut Option<RequiredLiteralFilter>,
        literal: RequiredLiteralFilter,
    ) {
        if (literal.len as usize) < MIN_LITERAL_LEN {
            return;
        }

        let is_better = match best_literal {
            None => true,
            Some(best_literal) => match literal.len.cmp(&best_literal.len) {
                Ordering::Greater => true,
                Ordering::Less => false,
                Ordering::Equal => {
                    best_literal.offset.is_unbounded() && !literal.offset.is_unbounded()
                }
            },
        };

        if is_better {
            *best_literal = Some(literal);
        }
    }

    fn analyze_literal(&self, literal: AstStr) -> LiteralAnalysis {
        let mut best_literal = None;
        let mut count = 0;

        // Accumulator building searchable code points into a literal
        let mut literal_acc = RequiredLiteralFilter::new_at(Width::new_exact(0));

        for code_point in literal.iter_code_points() {
            count += 1;

            // Stop accumulating if literal is already at the maximum length
            if (literal_acc.len as usize) == MAX_LITERAL_LEN {
                continue;
            }

            if self.is_searchable_code_point(code_point) {
                literal_acc.bytes[literal_acc.len as usize] = code_point as u8;
                literal_acc.len += 1;
            } else {
                // Literal is interrupted - complete it, start a new one, and update best seen
                let completed_literal = std::mem::replace(
                    &mut literal_acc,
                    RequiredLiteralFilter::new_at(Width::new_exact(count)),
                );
                Self::update_best_literal(&mut best_literal, completed_literal);
            }
        }

        // Complete the final literal
        Self::update_best_literal(&mut best_literal, literal_acc);

        LiteralAnalysis::new(best_literal, Width::new_exact(count))
    }
}

/// Utility for repeatedly searching for a required literal in a buffer.
pub struct RequiredLiteralSearcher {
    /// Finder for the literal's bytes, matching target buffer encoding.
    finder: memmem::Finder<'static>,
    /// Cache containing the most recently found occurrence of the literal. Positions are searched
    /// in increasing order so this can be used to avoid searching multiple times if the most recent
    /// search still applies to the next search position.
    found_pos: Option<usize>,
}

impl RequiredLiteralSearcher {
    /// Create a searcher for repeatedly finding a literal in a one byte (Latin1) buffer.
    pub fn new_one_byte(filter: &RequiredLiteralFilter) -> Self {
        Self::new(filter.bytes())
    }

    /// Create a searcher for repeatedly finding a literal in a two byte buffer.
    pub fn new_two_byte(filter: &RequiredLiteralFilter) -> Self {
        let mut wide_literal = [0; MAX_LITERAL_LEN * 2];
        let wide_literal = filter.widen_two_byte_literal(&mut wide_literal);

        Self::new(wide_literal)
    }

    fn new(literal_bytes: &[u8]) -> Self {
        Self {
            finder: memmem::Finder::new(literal_bytes).into_owned(),
            found_pos: None,
        }
    }

    #[inline]
    pub fn find_one_byte(&mut self, buf: &[u8], start_pos: usize) -> Option<usize> {
        self.find_with(buf.len(), start_pos, |finder| finder.find(&buf[start_pos..]))
    }

    #[inline]
    pub fn find_two_byte(&mut self, buf: &[u16], start_pos: usize) -> Option<usize> {
        self.find_with(buf.len(), start_pos, |finder| find_two_byte_with(finder, &buf[start_pos..]))
    }

    /// Find the literal in a buffer starting at the given position. Return the offset of the first
    /// occurrence, or None if not found.
    #[inline]
    fn find_with(
        &mut self,
        buf_len: usize,
        start_pos: usize,
        search: impl FnOnce(&memmem::Finder) -> Option<usize>,
    ) -> Option<usize> {
        // Use the cached position if it is still applicable to the requested start position
        if let Some(found_pos) = self.found_pos
            && start_pos <= found_pos
        {
            return Some(found_pos);
        }

        if start_pos >= buf_len {
            return None;
        }

        let found_pos = start_pos + search(&self.finder)?;
        self.found_pos = Some(found_pos);

        Some(found_pos)
    }
}

/// Find a widened literal in a two byte string buffer, returning the offset in code units of the
/// first occurrence.
fn find_two_byte_with(finder: &memmem::Finder, buf: &[u16]) -> Option<usize> {
    let bytes = two_byte_slice_as_bytes(buf);

    let mut start = 0;
    while let Some(offset) = finder.find(&bytes[start..]) {
        let offset = start + offset;

        // Match must occur at an even byte offset in order to match a full code unit
        if offset % 2 == 0 {
            return Some(offset / 2);
        }

        start = offset + 1;
    }

    None
}
