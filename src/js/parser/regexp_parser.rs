use crate::js::common::unicode::{
    get_hex_value, is_ascii_alphabetic, is_decimal_digit, is_id_continue_unicode, is_id_part,
    is_id_start,
};

use super::{
    ast::p,
    lexer_stream::{LexerStream, SavedLexerStreamState},
    loc::Pos,
    regexp::{
        Alternative, AnonymousGroup, Assertion, Backreference, CaptureGroup, CharacterClass,
        ClassRange, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags, Term,
    },
    ParseError, ParseResult,
};

/// 22.2.1 Patterns
/// Parser of the full RegExp grammar and static semantics
pub struct RegExpParser<T: LexerStream> {
    /// The stream of code points to parse
    lexer_stream: T,
    /// Flags for this regexp
    flags: RegExpFlags,
}

impl<T: LexerStream> RegExpParser<T> {
    fn new(lexer_stream: T, flags: RegExpFlags) -> Self {
        RegExpParser { lexer_stream, flags }
    }

    #[inline]
    fn pos(&self) -> Pos {
        self.lexer_stream.pos()
    }

    #[inline]
    fn current(&self) -> char {
        self.lexer_stream.current()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.lexer_stream.advance_n(n);
    }

    #[inline]
    fn peek_n(&self, n: usize) -> char {
        self.lexer_stream.peek_n(n)
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<char> {
        self.lexer_stream.parse_unicode_codepoint()
    }

    #[inline]
    fn error<E>(&self, start_pos: Pos, error: ParseError) -> ParseResult<E> {
        self.lexer_stream.error(start_pos, error)
    }

    #[inline]
    fn save(&self) -> SavedLexerStreamState {
        self.lexer_stream.save()
    }

    #[inline]
    fn restore(&mut self, save_state: &SavedLexerStreamState) {
        self.lexer_stream.restore(save_state);
    }

    #[inline]
    fn is_end(&self) -> bool {
        self.lexer_stream.is_end()
    }

    #[inline]
    fn advance(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    fn advance2(&mut self) {
        self.advance_n(2);
    }

    #[inline]
    fn advance3(&mut self) {
        self.advance_n(3);
    }

    fn peek(&mut self) -> char {
        self.peek_n(1)
    }

    fn peek2(&mut self) -> char {
        self.peek_n(2)
    }

    fn expect(&mut self, byte: char) -> ParseResult<()> {
        if self.current() != byte {
            return self.error_unexpected_token(self.pos());
        }

        self.advance();
        Ok(())
    }

    #[inline]
    fn eat(&mut self, char: char) -> bool {
        if self.current() == char {
            self.advance();
            true
        } else {
            false
        }
    }

    #[inline]
    fn is_unicode_aware(&self) -> bool {
        self.flags.contains(RegExpFlags::UNICODE_AWARE)
    }

    fn error_unexpected_token<E>(&self, start_pos: Pos) -> ParseResult<E> {
        self.error(start_pos, ParseError::UnexpectedRegExpToken)
    }

    pub fn parse_regexp(lexer_stream: T, flags: RegExpFlags) -> ParseResult<RegExp> {
        let mut parser = Self::new(lexer_stream, flags);

        let disjunction = parser.parse_disjunction()?;

        Ok(RegExp { disjunction, flags })
    }

    // On failure return the error along with the offset into the input buffer
    pub fn parse_flags(mut lexer_stream: T) -> ParseResult<RegExpFlags> {
        let mut flags = RegExpFlags::empty();

        macro_rules! add_flag {
            ($flag:expr) => {{
                if !flags.contains($flag) {
                    flags |= $flag;
                    lexer_stream.advance_n(1);
                } else {
                    return lexer_stream.error(lexer_stream.pos(), ParseError::DuplicateRegExpFlag);
                }
            }};
        }

        while !lexer_stream.is_end() {
            match lexer_stream.current() {
                'd' => add_flag!(RegExpFlags::HAS_INDICES),
                'g' => add_flag!(RegExpFlags::GLOBAL),
                'i' => add_flag!(RegExpFlags::IGNORE_CASE),
                'm' => add_flag!(RegExpFlags::MULTILINE),
                's' => add_flag!(RegExpFlags::DOT_ALL),
                'u' => add_flag!(RegExpFlags::UNICODE_AWARE),
                'y' => add_flag!(RegExpFlags::STICKY),
                _ => return lexer_stream.error(lexer_stream.pos(), ParseError::InvalidRegExpFlag),
            }
        }

        Ok(flags)
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
            match self.current() {
                '*' | '+' | '?' | '{' => {
                    return self.error(self.pos(), ParseError::UnexpectedRegExpQuantifier);
                }
                '}' | ']' => return self.error_unexpected_token(self.pos()),
                // Valid ends to an alternative
                ')' | '|' => break,
                // Otherwise must be the start of a term
                _ => {}
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
        let atom = match self.current() {
            '.' => {
                self.advance();
                Term::Wildcard
            }
            '(' => self.parse_group()?,
            '[' => self.parse_character_class()?,
            // Might be an escape code
            '\\' => {
                match self.peek() {
                    // Standard character class shorthands
                    'w' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: false,
                            ranges: vec![ClassRange::Word],
                        })
                    }
                    'W' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: true,
                            ranges: vec![ClassRange::NotWord],
                        })
                    }
                    'd' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: false,
                            ranges: vec![ClassRange::Digit],
                        })
                    }
                    'D' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: true,
                            ranges: vec![ClassRange::NotDigit],
                        })
                    }
                    's' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: false,
                            ranges: vec![ClassRange::Whitespace],
                        })
                    }
                    'S' => {
                        self.advance2();
                        Term::CharacterClass(CharacterClass {
                            is_inverted: true,
                            ranges: vec![ClassRange::NotWhitespace],
                        })
                    }
                    // Word boundary assertions
                    'b' => {
                        self.advance2();
                        Term::Assertion(Assertion::WordBoundary)
                    }
                    'B' => {
                        self.advance2();
                        Term::Assertion(Assertion::NotWordBoundary)
                    }
                    // Indexed backreferences
                    '1'..='9' => {
                        // Skip the `\` but start at the first digit
                        self.advance();
                        let index = self.parse_decimal_digits()?;
                        Term::Backreference(Backreference::Index(index))
                    }
                    // Named backreferences
                    'k' => {
                        self.advance2();

                        self.expect('<')?;
                        let name = self.parse_identifier()?;
                        self.expect('>')?;

                        Term::Backreference(Backreference::Name(p(name)))
                    }
                    // Otherwise must be a regular regexp escape sequence
                    _ => {
                        let code_point = self.parse_regexp_escape_sequence()?;
                        Term::Literal(code_point.to_string())
                    }
                }
            }
            // Otherwise this must be a literal term
            _ => {
                let code_point = self.parse_unicode_codepoint()?;
                Term::Literal(code_point.to_string())
            }
        };

        // Term may be postfixed with a quantifier
        self.parse_quantifier(atom)
    }

    fn parse_quantifier(&mut self, term: Term) -> ParseResult<Term> {
        let bounds_opt = match self.current() {
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
                    if self.current() == '}' {
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
            Ok(Term::Quantifier(Quantifier { term: p(term), min, max, is_greedy }))
        } else {
            Ok(term)
        }
    }

    fn parse_decimal_digits(&mut self) -> ParseResult<u32> {
        // Sequence of decimal digits must be nonempty
        if !is_decimal_digit(self.current()) {
            return self.error_unexpected_token(self.pos());
        }

        let mut value = 0;

        while is_decimal_digit(self.current()) {
            value *= 10;
            value += (self.current() as u32) - ('0' as u32);
            self.advance();
        }

        Ok(value)
    }

    fn parse_group(&mut self) -> ParseResult<Term> {
        self.advance();

        if self.eat('?') {
            match self.current() {
                ':' => {
                    self.advance();
                    let disjunction = self.parse_disjunction()?;
                    self.expect(')')?;

                    Ok(Term::AnonymousGroup(AnonymousGroup { disjunction }))
                }
                '=' => {
                    self.advance();
                    let disjunction = self.parse_disjunction()?;
                    self.expect(')')?;

                    Ok(Term::Lookaround(Lookaround {
                        disjunction,
                        is_ahead: true,
                        is_positive: true,
                    }))
                }
                '!' => {
                    self.advance();
                    let disjunction = self.parse_disjunction()?;
                    self.expect(')')?;

                    Ok(Term::Lookaround(Lookaround {
                        disjunction,
                        is_ahead: true,
                        is_positive: false,
                    }))
                }
                '<' => {
                    self.advance();

                    match self.current() {
                        '=' => {
                            self.advance();
                            let disjunction = self.parse_disjunction()?;
                            self.expect(')')?;

                            Ok(Term::Lookaround(Lookaround {
                                disjunction,
                                is_ahead: false,
                                is_positive: true,
                            }))
                        }
                        '!' => {
                            self.advance();
                            let disjunction = self.parse_disjunction()?;
                            self.expect(')')?;

                            Ok(Term::Lookaround(Lookaround {
                                disjunction,
                                is_ahead: false,
                                is_positive: false,
                            }))
                        }
                        _ => {
                            let name = self.parse_identifier()?;
                            self.expect('>')?;
                            let disjunction = self.parse_disjunction()?;
                            self.expect(')')?;

                            Ok(Term::CaptureGroup(CaptureGroup {
                                name: Some(p(name)),
                                disjunction,
                            }))
                        }
                    }
                }
                _ => self.error_unexpected_token(self.pos()),
            }
        } else {
            let disjunction = self.parse_disjunction()?;
            self.expect(')')?;

            Ok(Term::CaptureGroup(CaptureGroup { name: None, disjunction }))
        }
    }

    fn parse_character_class(&mut self) -> ParseResult<Term> {
        self.advance();

        let is_inverted = self.eat('^');

        let mut ranges = vec![];
        while !self.eat(']') {
            let atom = self.parse_class_atom()?;

            if self.eat('-') {
                // Trailing `-` at the end of the character class
                if self.eat(']') {
                    ranges.push(atom);
                    ranges.push(ClassRange::Single('-'));
                    break;
                }

                // Otherwise this must be a range
                let end_atom = self.parse_class_atom()?;

                let start = self.class_atom_to_range_bound(atom)?;
                let end = self.class_atom_to_range_bound(end_atom)?;

                ranges.push(ClassRange::Range(start, end));
            } else {
                ranges.push(atom);
            }
        }

        Ok(Term::CharacterClass(CharacterClass { is_inverted, ranges }))
    }

    fn class_atom_to_range_bound(&mut self, atom: ClassRange) -> ParseResult<char> {
        match atom {
            ClassRange::Single(code_point) => Ok(code_point),
            ClassRange::Word
            | ClassRange::NotWord
            | ClassRange::Digit
            | ClassRange::NotDigit
            | ClassRange::Whitespace
            | ClassRange::NotWhitespace => {
                self.error(self.pos(), ParseError::RegExpCharacterClassInRange)
            }
            ClassRange::Range(..) => unreachable!("Ranges are not returned by parse_class_atom"),
        }
    }

    fn parse_class_atom(&mut self) -> ParseResult<ClassRange> {
        // Could be the start of an escape sequence
        if self.current() == '\\' {
            return match self.peek() {
                // Standard character class shorthands
                'w' => {
                    self.advance2();
                    Ok(ClassRange::Word)
                }
                'W' => {
                    self.advance2();
                    Ok(ClassRange::NotWord)
                }
                'd' => {
                    self.advance2();
                    Ok(ClassRange::Digit)
                }
                'D' => {
                    self.advance2();
                    Ok(ClassRange::NotDigit)
                }
                's' => {
                    self.advance2();
                    Ok(ClassRange::Whitespace)
                }
                'S' => {
                    self.advance2();
                    Ok(ClassRange::NotWhitespace)
                }
                // Backspace escape
                'b' => {
                    self.advance2();
                    Ok(ClassRange::Single('\u{0008}'))
                }
                // Minus is only escaped in unicode aware mode
                '-' if self.is_unicode_aware() => {
                    self.advance2();
                    Ok(ClassRange::Single('-'))
                }
                // After checking class-specific escape sequences, must be a regular regexp
                // escape sequence.
                _ => Ok(ClassRange::Single(self.parse_regexp_escape_sequence()?)),
            };
        }

        let code_point = self.parse_unicode_codepoint()?;

        Ok(ClassRange::Single(code_point))
    }

    fn parse_regexp_escape_sequence(&mut self) -> ParseResult<char> {
        match self.peek() {
            // Unicode escape sequence
            'u' => self.parse_regex_unicode_escape_sequence(),
            // Standard control characters
            'f' => {
                self.advance2();
                Ok('\u{000c}')
            }
            'n' => {
                self.advance2();
                Ok('\n')
            }
            'r' => {
                self.advance2();
                Ok('\r')
            }
            't' => {
                self.advance2();
                Ok('\t')
            }
            'v' => {
                self.advance2();
                Ok('\u{000b}')
            }
            // Any escaped ASCII letter - converted to letter value mod 32
            'c' if is_ascii_alphabetic(self.peek2()) => {
                let code_point = self.peek2() as u32 % 32;
                self.advance3();

                // Safe since code point is guaranteed to be in range [0, 32)
                Ok(unsafe { char::from_u32_unchecked(code_point) })
            }
            // ASCII hex escape sequence
            'x' => {
                let start_pos = self.pos();
                self.advance2();
                let code_point = self.parse_hex2_digits(start_pos)?;

                // Safe since code point is guaranteed to be in the range [0, 255)
                Ok(unsafe { char::from_u32_unchecked(code_point) })
            }
            // The null byte
            '0' if !is_decimal_digit(self.peek2()) => {
                self.advance2();
                Ok('\0')
            }
            _ => {
                let start_pos = self.pos();
                self.advance();

                // In unicode-aware mode only regexp special characters can be escaped
                if self.is_unicode_aware() {
                    if let '/' | '\\' | '^' | '$' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']'
                    | '{' | '}' | '|' = self.current()
                    {
                        let code_point = self.current();
                        self.advance();
                        Ok(code_point)
                    } else {
                        self.error(start_pos, ParseError::MalformedEscapeSeqence)
                    }
                } else {
                    // Otherwise all non id_continue characters can be escaped
                    let code_point = self.parse_unicode_codepoint()?;
                    if !is_id_continue_unicode(code_point) {
                        Ok(code_point)
                    } else {
                        self.error(start_pos, ParseError::MalformedEscapeSeqence)
                    }
                }
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let mut string_builder = String::new();

        // First character must be an id start, which can be an escape sequence
        if self.current() == '\\' {
            let code_point = self.parse_regex_unicode_escape_sequence()?;
            if !is_id_start(code_point) {
                return self.error_unexpected_token(self.pos());
            }

            string_builder.push(code_point);
        } else {
            // Otherwise must be a unicode codepoint
            let code_point = self.parse_unicode_codepoint()?;
            if is_id_start(code_point) {
                string_builder.push(code_point);
            } else {
                return self.error_unexpected_token(self.pos());
            }
        }

        // All following characters must be id parts
        loop {
            // Can be an escape sequence
            if self.current() == '\\' {
                let code_point = self.parse_regex_unicode_escape_sequence()?;
                if !is_id_part(code_point) {
                    return self.error_unexpected_token(self.pos());
                }

                string_builder.push(code_point);
            } else {
                // Otherwise must be a unicode codepoint
                let save_state = self.save();
                let code_point = self.parse_unicode_codepoint()?;
                if is_id_part(code_point) {
                    string_builder.push(code_point);
                } else {
                    // Restore to before codepoint if not part of the id
                    self.restore(&save_state);
                    break;
                }
            }
        }

        Ok(string_builder)
    }

    fn parse_regex_unicode_escape_sequence(&mut self) -> ParseResult<char> {
        let start_pos = self.pos();

        // Unicode escape sequences always start with `\u`
        self.advance();
        self.expect('u')?;

        // In unicode aware mode the escape sequence \u{digits} is allowed
        if self.is_unicode_aware() && self.eat('{') {
            // Cannot be empty
            if self.current() == '}' {
                self.advance();
                return self.error(start_pos, ParseError::MalformedEscapeSeqence);
            }

            // collect all hex digits until closing brace
            let mut value = 0;
            while self.current() != '}' {
                if let Some(hex_value) = get_hex_value(self.current()) {
                    self.advance();
                    value <<= 4;
                    value += hex_value;
                } else {
                    return self.error(start_pos, ParseError::MalformedEscapeSeqence);
                }

                // Check if number is out of range
                if value > 0x10FFFF {
                    return self.error(start_pos, ParseError::MalformedEscapeSeqence);
                }
            }

            self.expect('}')?;

            // TODO: Handle invalid utf-8 here and in else case
            Ok(char::from_u32(value).unwrap())
        } else {
            let code_point = self.parse_hex4_digits(start_pos)?;
            Ok(char::from_u32(code_point).unwrap())
        }
    }

    fn parse_hex2_digits(&mut self, start_pos: Pos) -> ParseResult<u32> {
        let mut value = 0;
        for _ in 0..2 {
            if let Some(hex_value) = get_hex_value(self.current()) {
                self.advance();
                value <<= 4;
                value += hex_value;
            } else {
                return self.error(start_pos, ParseError::MalformedEscapeSeqence);
            }
        }

        Ok(value)
    }

    fn parse_hex4_digits(&mut self, start_pos: Pos) -> ParseResult<u32> {
        let mut value = 0;
        for _ in 0..4 {
            if let Some(hex_value) = get_hex_value(self.current()) {
                self.advance();
                value <<= 4;
                value += hex_value;
            } else {
                return self.error(start_pos, ParseError::MalformedEscapeSeqence);
            }
        }

        Ok(value)
    }
}
