use bitflags::bitflags;

use super::ast::P;

pub struct RegExp {
    pub disjunction: Disjunction,
    pub flags: RegExpFlags,
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
        /// Sticky search. Remembers its position and only matches at the current position in the
        /// target string: `y`
        const STICKY = 1 << 6;
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
    /// A literal string of characters with escape codes decoded into code points
    Literal(String),
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
    Backreference(Backreference),
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

pub struct CaptureGroup {
    /// Optional name for the capture group
    pub name: Option<P<String>>,
    pub disjunction: Disjunction,
}

pub struct AnonymousGroup {
    pub disjunction: Disjunction,
}

pub enum ClassRange {
    /// A single character: `a`
    Single(char),
    /// A range of characters: `a-z`
    Range(char, char),
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
    /// Optional name for the capture group, otherwise uses index
    pub name: Option<P<String>>,
    /// All backrefrences have a unique index even if they also have a name
    pub index: u32,
}
