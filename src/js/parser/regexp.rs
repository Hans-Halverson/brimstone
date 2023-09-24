use bitflags::bitflags;

use crate::js::common::{unicode_property::UnicodeProperty, wtf_8::Wtf8String};

use super::ast::P;

pub struct RegExp {
    pub disjunction: Disjunction,
    pub flags: RegExpFlags,
    // All capture groups with their names if one was provided
    pub capture_groups: Vec<Option<String>>,
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct RegExpFlags: u8 {
        /// Whether to generate indices for substring matches: `d`
        const HAS_INDICES = 1 << 0;
        /// Global search. Find all matches in the string instead of just the first one: `g`
        const GLOBAL = 1 << 1;
        /// Whether to ignore case when matching: `i`
        const IGNORE_CASE = 1 << 2;
        /// Whether to allow `^` and `$` to match newlines: `m`
        const MULTILINE = 1 << 3;
        /// Whether to allow '.' to match newlines: `s`
        const DOT_ALL = 1 << 4;
        /// Unicode aware mode which enables various unicode features like escapes. Treat inputs as
        /// a sequence of UTF-16 encoded code points, as opposed to default mode which treats every
        /// 16-bit sequence as a literal (non-encoded) code point value: `u`
        const UNICODE_AWARE = 1 << 5;
        /// Unicode sets mode which enables additional unicode features like set operations: `v`
        const UNICODE_SETS = 1 << 6;
        /// Sticky search. Remembers its position and only matches at the current position in the
        /// target string: `y`
        const STICKY = 1 << 7;
    }
}

impl RegExpFlags {
    /// The `d` flag is set
    pub fn has_indices(&self) -> bool {
        self.contains(Self::HAS_INDICES)
    }

    /// The `g` flag is set
    pub fn is_global(&self) -> bool {
        self.contains(Self::GLOBAL)
    }

    /// The `i` flag is set
    pub fn is_case_insensitive(&self) -> bool {
        self.contains(Self::IGNORE_CASE)
    }

    /// The `m` flag is set
    pub fn is_multiline(&self) -> bool {
        self.contains(Self::MULTILINE)
    }

    /// The `s` flag is set
    pub fn is_dot_all(&self) -> bool {
        self.contains(Self::DOT_ALL)
    }

    /// The `u` flag is set
    pub fn has_simple_unicode_flag(&self) -> bool {
        self.contains(Self::UNICODE_AWARE)
    }

    /// The `v` flag is set
    pub fn has_unicode_sets_flag(&self) -> bool {
        self.contains(Self::UNICODE_SETS)
    }

    /// Either the `u` or `v` flag is set
    pub fn has_any_unicode_flag(&self) -> bool {
        self.contains(Self::UNICODE_AWARE)
    }

    /// The `y` flag is set
    pub fn is_sticky(&self) -> bool {
        self.contains(Self::STICKY)
    }
}

pub struct Disjunction {
    /// List of alternatives separated by `|`. This list is non-empty, empty disjunctions are
    /// represented by a single empty alternative.
    pub alternatives: Vec<Alternative>,
}

pub struct Alternative {
    /// Sequence of terms that make up this alternative. May be empty in which case it always matches.
    pub terms: Vec<Term>,
}

pub enum Term {
    /// A literal string of characters with escape codes decoded into code points. Must be nonempty.
    Literal(Wtf8String),
    /// The wildcard which matches any single character: `.`
    Wildcard,
    /// A repition of a pattern: `a*`, `a+`, `a?`, `a{x,y}`, etc.
    Quantifier(Quantifier),
    /// Assert a property e.g. `^` or `\b`
    Assertion(Assertion),
    /// Assert a condition before or after the current position without consuming any characters:
    /// (?=pattern), (?!pattern), (?<=pattern), or (?<!pattern
    Lookaround(Lookaround),
    /// Group that does capture: (pattern) or (?<name>pattern)
    CaptureGroup(CaptureGroup),
    /// Group that does not capture: (?:pattern)
    AnonymousGroup(AnonymousGroup),
    /// Matches any characters in a set e.g. [a-b]
    CharacterClass(CharacterClass),
    /// References to a capture group: `\1` or `\k<name>`
    Backreference(P<Backreference>),
}

pub struct Quantifier {
    pub term: P<Term>,
    /// The minimum number of times the term must match
    pub min: u32,
    /// The maximum number of times the term can match, inclusive. If None then there is no maximum.
    pub max: Option<u32>,
    /// Whether the quantifier is greedy or not. Greedy by default, postfixed with `?` to be lazy.
    pub is_greedy: bool,
}

pub enum Assertion {
    /// Asserts that the current position is the start of the string: `^`
    Start,
    /// Asserts that the current position is the end of the string: `$`
    End,
    /// Asserts that the current position is at a word boundary: `\b`
    WordBoundary,
    /// Asserts that the current position is not at a word boundary: `\B`
    NotWordBoundary,
}

/// Capture group indices are 1-indexed to match their syntax in RegExp literals.
pub type CaptureGroupIndex = u32;

pub struct CaptureGroup {
    // Optional capture group name
    pub name: Option<String>,
    // Index of the capture group in the RegExp
    pub index: CaptureGroupIndex,
    pub disjunction: Disjunction,
}

pub struct AnonymousGroup {
    pub disjunction: Disjunction,
}

pub enum ClassRange {
    /// A single code point: `a`
    Single(u32),
    /// A range of code points: `a-z`
    Range(u32, u32),
    /// All digits: `\d`
    Digit,
    /// All non-digits: `\D`
    NotDigit,
    /// All word characters (letters, digits, and underscore): `\w`
    Word,
    /// All non-word characters: `\W`
    NotWord,
    /// All whitespace characters including both spaces and newlines: `\s`
    Whitespace,
    /// All non-whitespace characters: `\S`
    NotWhitespace,
    /// All code points matching a unicode property `\p{UnicodeProperty}`
    UnicodeProperty(UnicodeProperty),
    /// All code points that do not match a unicode property `\P{UnicodeProperty}`
    NotUnicodeProperty(UnicodeProperty),
}

pub struct CharacterClass {
    /// Whether to only match characters not listed in this class
    pub is_inverted: bool,
    /// Union of class ranges
    pub ranges: Vec<ClassRange>,
}

pub struct Lookaround {
    /// Whether this is lookahead or lookbehind
    pub is_ahead: bool,
    /// Whether this is positive or negative lookaround
    pub is_positive: bool,
    pub disjunction: Disjunction,
}

pub struct Backreference {
    /// Index of the capture group to reference
    pub index: CaptureGroupIndex,
}
