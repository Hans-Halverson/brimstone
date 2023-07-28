use std::rc::Rc;

use crate::js::common::unicode::{decode_utf8_codepoint, is_ascii, is_decimal_digit};

use super::{
    loc::{Loc, Pos},
    regexp::{Alternative, Disjunction, Quantifier, RegExp, RegExpFlags, Term},
    source::Source,
    LocalizedParseError, ParseError, ParseResult,
};

/// 22.2.1 Patterns
/// Parser of the full RegExp grammar and static semantics
pub struct RegExpParser<'a> {
    /// Buffer of bytes for pattern that is being parsed
    buf: &'a str,
    /// Location of the current byte in the buffer
    pos: Pos,
    /// Current byte or EOF_CHAR
    current: char,
    /// Flags for this regexp
    flags: RegExpFlags,
    /// Loc of the RegExpLiteral AST node
    loc: Loc,
    /// Source of the RegExprLiteral we are parsing
    source: Rc<Source>,
}

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: char = '\u{ffff}';

impl<'a> RegExpParser<'a> {
    fn new(loc: Loc, source: Rc<Source>, pattern: &'a str) -> Self {
        let buf = pattern;
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        RegExpParser {
            buf,
            pos: 0,
            current,
            loc,
            source,
            flags: RegExpFlags::empty(),
        }
    }

    #[inline]
    fn char_at(&self, index: usize) -> char {
        self.buf.as_bytes()[index].into()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.char_at(self.pos);
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    #[inline]
    fn advance(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    fn eat(&mut self, char: char) -> bool {
        if self.current == char {
            self.advance();
            true
        } else {
            false
        }
    }

    #[inline]
    fn is_end(&self) -> bool {
        self.current == EOF_CHAR
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        // Positions are indexes into a RegExp's pattern section, so their absolute location is
        // the start of the RegExp plus the index plus one extra for the leading `/`.
        let start = self.loc.start + start_pos + 1;
        let end = self.loc.start + self.pos + 1;
        Loc { start, end }
    }

    fn localized_parse_error(&self, loc: Loc, error: ParseError) -> LocalizedParseError {
        let source = self.source.clone();
        LocalizedParseError { error, source_loc: Some((loc, source)) }
    }

    fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        Err(self.localized_parse_error(loc, error))
    }

    fn error_unexpected_token<T>(&self, loc: Loc) -> ParseResult<T> {
        self.error(loc, ParseError::UnexpectedRegExpToken)
    }

    pub fn parse_regexp(
        loc: Loc,
        source: Rc<Source>,
        pattern: &'a str,
        flags: &'a str,
    ) -> ParseResult<RegExp> {
        let mut parser = Self::new(loc, source, pattern);
        parser.parse_flags(flags)?;

        let disjunction = parser.parse_disjunction()?;

        Ok(RegExp { disjunction, flags: parser.flags })
    }

    fn parse_flags(&mut self, flags_buf: &str) -> ParseResult<()> {
        macro_rules! add_flag {
            ($i:expr, $flag:expr) => {
                if !self.flags.contains($flag) {
                    self.flags |= $flag;
                } else {
                    return error(
                        self.loc,
                        self.source.clone(),
                        $i,
                        ParseError::DuplicateRegExpFlag,
                    );
                }
            };
        }

        #[inline]
        fn error(
            loc: Loc,
            source: Rc<Source>,
            offset: usize,
            error: ParseError,
        ) -> ParseResult<()> {
            let loc = Loc { start: loc.start + offset, end: loc.start + offset + 1 };
            Err(LocalizedParseError { error, source_loc: Some((loc, source.clone())) })
        }

        for (i, byte) in flags_buf.as_bytes().iter().enumerate() {
            match byte {
                b'd' => add_flag!(i, RegExpFlags::HAS_INDICES),
                b'g' => add_flag!(i, RegExpFlags::GLOBAL),
                b'i' => add_flag!(i, RegExpFlags::IGNORE_CASE),
                b'm' => add_flag!(i, RegExpFlags::MULTILINE),
                b's' => add_flag!(i, RegExpFlags::DOT_ALL),
                b'u' => add_flag!(i, RegExpFlags::UNICODE_AWARE),
                b'y' => add_flag!(i, RegExpFlags::STICKY),
                _ => return error(self.loc, self.source.clone(), i, ParseError::InvalidRegExpFlag),
            }
        }

        Ok(())
    }

    fn parse_disjunction(&mut self) -> ParseResult<Disjunction> {
        let mut alternatives = vec![];

        // There is always at least one alternative, even if the alternative is empty
        loop {
            alternatives.push(self.parse_alternative()?);

            if !self.eat('|') {
                break;
            }

            // A trailing `|` means there is a final empty alternative
            if self.is_end() {
                alternatives.push(Alternative { terms: vec![] });
                break;
            }
        }

        Ok(Disjunction { alternatives })
    }

    fn parse_alternative(&mut self) -> ParseResult<Alternative> {
        let mut terms = vec![];

        while !self.is_end() {
            // Punctuation that does not mark the start of a term
            if let '*' | '+' | '?' | '{' | '}' | '|' = self.current {
                break;
            }

            let term = self.parse_term()?;

            match (&term, terms.last_mut()) {
                // Coalesce adjacent string literals
                (Term::Literal(new_string), Some(Term::Literal(prev_string))) => {
                    prev_string.push_str(&new_string);
                }
                _ => terms.push(term),
            }
        }

        Ok(Alternative { terms })
    }

    fn parse_term(&mut self) -> ParseResult<Term> {
        // Parse a single atomic term
        let atom = match self.current {
            '.' => {
                self.advance();
                Term::Wildcard
            }
            other => {
                if is_ascii(other) {
                    let ascii_char = self.current;
                    self.advance();
                    Term::Literal(ascii_char.to_string())
                } else {
                    let code_point = self.parse_utf8_codepoint()?;
                    Term::Literal(code_point.to_string())
                }
            }
        };

        // Term may be postfixed with a quantifier
        self.parse_quantifier(atom)
    }

    fn parse_quantifier(&mut self, term: Term) -> ParseResult<Term> {
        let bounds_opt = match self.current {
            '*' => {
                self.advance();
                Some((0, None))
            }
            '+' => {
                self.advance();
                Some((1, None))
            }
            '?' => {
                self.advance();
                Some((0, Some(1)))
            }
            '{' => {
                self.advance();

                let lower_bound = self.parse_decimal_digits()?;
                let upper_bound = if self.eat(',') {
                    if self.current == '}' {
                        None
                    } else {
                        Some(self.parse_decimal_digits()?)
                    }
                } else {
                    Some(lower_bound)
                };

                self.eat('}');

                Some((lower_bound, upper_bound))
            }
            _ => None,
        };

        if let Some((min, max)) = bounds_opt {
            // Every quantifier can be postfixed with a `?` to make it lazy
            let is_greedy = !self.eat('?');
            Ok(Term::Quantifier(Quantifier { term: Box::new(term), min, max, is_greedy }))
        } else {
            Ok(term)
        }
    }

    #[inline]
    fn parse_utf8_codepoint(&mut self) -> ParseResult<char> {
        let buf = &self.buf.as_bytes()[self.pos..];
        match decode_utf8_codepoint(buf) {
            Ok((code_point, byte_length)) => {
                self.advance_n(byte_length);
                Ok(code_point)
            }
            Err(byte_length) => {
                self.advance_n(byte_length);
                let loc = self.mark_loc(self.pos - byte_length);
                self.error(loc, ParseError::InvalidUnicode)
            }
        }
    }

    fn parse_decimal_digits(&mut self) -> ParseResult<u32> {
        // Sequence of decimal digits must be nonempty
        if !is_decimal_digit(self.current) {
            self.advance();
            let loc = self.mark_loc(self.pos - 1);

            return self.error_unexpected_token(loc);
        }

        let mut value = 0;

        while is_decimal_digit(self.current) {
            value *= 10;
            value += (self.current as u32) - ('0' as u32);
            self.advance();
        }

        Ok(value)
    }
}
