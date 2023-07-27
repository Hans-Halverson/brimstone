use super::ast::{Identifier, P};

pub struct RegExp {
    pub disjunction: Disjunction,
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
    pub name: Option<P<Identifier>>,
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
    pub name: Option<P<Identifier>>,
    /// All backrefrences have a unique index even if they also have a name
    pub index: u32,
}
