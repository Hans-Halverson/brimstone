use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
};

use brimstone_macros::match_u32;

use crate::{
    js::common::{
        alloc,
        options::Options,
        unicode::{
            as_id_part, as_id_start, code_point_from_surrogate_pair, get_hex_value,
            is_ascii_alphabetic, is_decimal_digit, is_high_surrogate_code_point,
            is_high_surrogate_code_unit, is_id_continue_unicode, is_low_surrogate_code_point,
            is_low_surrogate_code_unit,
        },
        unicode_property::{
            BinaryUnicodeProperty, GeneralCategoryProperty, ScriptProperty, UnicodeProperty,
        },
        wtf_8::Wtf8String,
    },
    p,
};

use super::{
    ast::{AstAlloc, AstPtr, AstStr, AstString, AstVec},
    lexer_stream::{LexerStream, SavedLexerStreamState},
    loc::Pos,
    regexp::{
        Alternative, AnonymousGroup, Assertion, Backreference, CaptureGroup, CaptureGroupIndex,
        CharacterClass, ClassExpressionType, ClassRange, Disjunction, Lookaround, Quantifier,
        RegExp, RegExpFlags, StringDisjunction, Term,
    },
    ParseError, ParseResult,
};

/// Parser of the full RegExp grammar and static semantics
///
/// Patterns (https://tc39.es/ecma262/#sec-patterns)
pub struct RegExpParser<'a, T: LexerStream> {
    /// The stream of code points to parse
    lexer_stream: T,
    /// Flags for this regexp
    flags: RegExpFlags,
    /// Number of capture groups seen so far
    num_capture_groups: usize,
    /// All capture groups seen so far, with name if a name was specified
    capture_groups: AstVec<'a, Option<AstString<'a>>>,
    /// Map of capture group names that have been encountered so far to the index of their last
    /// occurrence in the RegExp.
    capture_group_names: HashMap<AstStr<'a>, CaptureGroupIndex>,
    /// Set of all capture group names that are currently in scope
    current_capture_group_names: HashSet<AstStr<'a>>,
    /// List of the capture group names that are in the current scope on the implicit scope stack
    current_capture_group_name_scope: Vec<AstStr<'a>>,
    /// Whether the RegExp has any duplicate named capture groups.
    has_duplicate_named_capture_groups: bool,
    /// Whether we should be parsing named capture groups or not.
    parse_named_capture_groups: bool,
    /// Whether the parse is parsing in Annex B mode.
    in_annex_b_mode: bool,
    /// All named backreferences encountered. Saves the name, source position, and a reference to the
    /// backreference node itself.
    named_backreferences: Vec<(AstStr<'a>, Pos, AstPtr<Backreference>)>,
    /// All indexed backreferences encountered. Saves the index and source position.
    indexed_backreferences: Vec<(CaptureGroupIndex, Pos)>,
    /// Number of parenthesized groups the parser is currently inside
    group_depth: usize,
    /// Allocator used for allocating AST nodes
    alloc: AstAlloc<'a>,
}

impl<'a, T: LexerStream> RegExpParser<'a, T> {
    fn new(
        lexer_stream: T,
        flags: RegExpFlags,
        alloc: AstAlloc<'a>,
        parse_named_capture_groups: bool,
        in_annex_b_mode: bool,
    ) -> Self {
        RegExpParser {
            lexer_stream,
            flags,
            num_capture_groups: 0,
            capture_groups: alloc::vec![in alloc],
            capture_group_names: HashMap::new(),
            current_capture_group_names: HashSet::new(),
            current_capture_group_name_scope: vec![],
            has_duplicate_named_capture_groups: false,
            parse_named_capture_groups,
            in_annex_b_mode,
            named_backreferences: vec![],
            indexed_backreferences: vec![],
            group_depth: 0,
            alloc,
        }
    }

    #[inline]
    fn pos(&self) -> Pos {
        self.lexer_stream.pos()
    }

    #[inline]
    fn current(&self) -> u32 {
        self.lexer_stream.current()
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.lexer_stream.advance_n(n);
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u32 {
        self.lexer_stream.peek_n(n)
    }

    #[inline]
    fn parse_unicode_codepoint(&mut self) -> ParseResult<u32> {
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

    fn peek(&mut self) -> u32 {
        self.peek_n(1)
    }

    fn peek2(&mut self) -> u32 {
        self.peek_n(2)
    }

    fn expect(&mut self, char: char) -> ParseResult<()> {
        if self.current() != char as u32 {
            return self.error_unexpected_token(self.pos());
        }

        self.advance();
        Ok(())
    }

    #[inline]
    fn eat(&mut self, char: char) -> bool {
        if self.current() == char as u32 {
            self.advance();
            true
        } else {
            false
        }
    }

    fn alloc_str(&self, string: &str) -> AstString<'a> {
        AstString::from_str_in(string, self.alloc)
    }

    fn alloc_vec<U>(&self) -> AstVec<'a, U> {
        AstVec::new_in(self.alloc)
    }

    fn alloc_vec_with_element<U>(&self, element: U) -> AstVec<'a, U> {
        let mut vec = AstVec::new_in(self.alloc);
        vec.push(element);
        vec
    }

    #[inline]
    fn is_unicode_aware(&self) -> bool {
        self.flags.has_any_unicode_flag()
    }

    fn error_unexpected_token<E>(&self, start_pos: Pos) -> ParseResult<E> {
        self.error(start_pos, ParseError::UnexpectedRegExpToken)
    }

    fn next_capture_group_index(&mut self, error_pos: Pos) -> ParseResult<u32> {
        // Capture group indices are 1-indexed
        let index = self.num_capture_groups + 1;

        // Deviation from spec - only allows up to 2^31 - 1 capture groups instead of 2^32 - 1
        if index >= (u32::MAX as usize / 2) {
            return self.error(error_pos, ParseError::TooManyCaptureGroups);
        }

        self.num_capture_groups += 1;

        Ok(index as u32)
    }

    pub fn parse_regexp(
        create_lexer_stream: &dyn Fn() -> T,
        flags: RegExpFlags,
        options: &Options,
        alloc: AstAlloc<'a>,
    ) -> ParseResult<RegExp<'a>> {
        // First try to parse without named capture groups. Only reparse if a named capture group
        // was encountered.
        let lexer_stream = create_lexer_stream();
        let mut parser;

        let disjunction = if !options.annex_b {
            // Always parse with named capture groups if Annex B is not enabled
            parser = Self::new(
                lexer_stream,
                flags,
                alloc,
                /* parse_named_capture_groups */ true,
                options.annex_b,
            );
            parser.parse_disjunction()?
        } else {
            // If Annex B is enabled then first try to parse without named capture groups. Only
            // reparse if a named capture group was encountered.
            parser = Self::new(
                lexer_stream,
                flags,
                alloc,
                /* parse_named_capture_groups */ false,
                options.annex_b,
            );
            match parser.parse_disjunction() {
                Ok(disjunction) => disjunction,
                // If we encountered a named capture group then reparse with named capture groups
                Err(error) if matches!(error.error, ParseError::NamedCaptureGroupEncountered) => {
                    let lexer_stream = create_lexer_stream();
                    parser = Self::new(
                        lexer_stream,
                        flags,
                        alloc,
                        /* parse_named_capture_groups */ true,
                        options.annex_b,
                    );
                    parser.parse_disjunction()?
                }
                Err(error) => return Err(error),
            }
        };

        let regexp = RegExp {
            disjunction,
            flags,
            capture_groups: parser.capture_groups.clone(),
            has_duplicate_named_capture_groups: parser.has_duplicate_named_capture_groups,
        };

        parser.resolve_backreferences()?;

        Ok(regexp)
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
            match_u32!(match lexer_stream.current() {
                'd' => add_flag!(RegExpFlags::HAS_INDICES),
                'g' => add_flag!(RegExpFlags::GLOBAL),
                'i' => add_flag!(RegExpFlags::IGNORE_CASE),
                'm' => add_flag!(RegExpFlags::MULTILINE),
                's' => add_flag!(RegExpFlags::DOT_ALL),
                'u' => add_flag!(RegExpFlags::UNICODE_AWARE),
                'v' => add_flag!(RegExpFlags::UNICODE_SETS),
                'y' => add_flag!(RegExpFlags::STICKY),
                _ => return lexer_stream.error(lexer_stream.pos(), ParseError::InvalidRegExpFlag),
            })
        }

        // RegExp can only have one of the `u` or `v` flags
        if flags.has_simple_unicode_flag() && flags.has_unicode_sets_flag() {
            return lexer_stream.error(lexer_stream.pos(), ParseError::MultipleUnicodeFlags);
        }

        Ok(flags)
    }

    fn parse_disjunction(&mut self) -> ParseResult<Disjunction<'a>> {
        let mut alternatives = self.alloc_vec();

        // There is always at least one alternative, even if the alternative is empty
        loop {
            // Set up new empty capture group name scope. Old scope is saved on the stack, forming
            // an implicit scope stack.
            let mut previous_scope = vec![];
            std::mem::swap(&mut self.current_capture_group_name_scope, &mut previous_scope);

            // Parse the alternative inside the scope
            alternatives.push(self.parse_alternative()?);

            // Tear down capture group scope by removing all names added in the current scope then
            // restoring the previous scope.
            for name in &self.current_capture_group_name_scope {
                self.current_capture_group_names.remove(name);
            }
            self.current_capture_group_name_scope = previous_scope;

            if !self.eat('|') {
                break;
            }

            // A trailing `|` means there is a final empty alternative
            if self.is_end() {
                alternatives.push(Alternative { terms: self.alloc_vec() });
                break;
            }
        }

        Ok(Disjunction { alternatives })
    }

    fn parse_alternative(&mut self) -> ParseResult<Alternative<'a>> {
        let mut terms = self.alloc_vec();

        while !self.is_end() {
            // Punctuation that does not mark the start of a term
            match_u32!(match self.current() {
                '*' | '+' | '?' => {
                    return self.error(self.pos(), ParseError::UnexpectedRegExpQuantifier);
                }
                // ']', '{', and '}' are only valid pattern characters in Annex B mode
                '{' if !self.in_annex_b_mode => {
                    return self.error(self.pos(), ParseError::UnexpectedRegExpQuantifier);
                }
                '}' | ']' if !self.in_annex_b_mode =>
                    return self.error_unexpected_token(self.pos()),
                // Valid ends to an alternative
                '|' => break,
                ')' => {
                    // Only a valid end if we are inside a group
                    if self.group_depth > 0 {
                        break;
                    } else {
                        return self.error_unexpected_token(self.pos());
                    }
                }
                // Otherwise must be the start of a term
                _ => {}
            });

            let term = self.parse_term()?;

            match (&term, terms.last_mut()) {
                // Coalesce adjacent string literals
                (Term::Literal(new_string), Some(Term::Literal(prev_string))) => {
                    prev_string.push_wtf8_str(new_string);
                }
                _ => terms.push(term),
            }
        }

        Ok(Alternative { terms })
    }

    fn parse_term(&mut self) -> ParseResult<Term<'a>> {
        // Parse a single atomic term
        let atom: Term<'a> = match_u32!(match self.current() {
            '.' => {
                self.advance();
                Term::Wildcard
            }
            '(' => {
                self.group_depth += 1;
                let group = self.parse_group()?;
                self.group_depth -= 1;

                group
            }
            '[' => {
                if self.flags.has_unicode_sets_flag() {
                    let (character_class, _) = self.parse_unicode_sets_character_class()?;
                    Term::CharacterClass(character_class)
                } else {
                    self.parse_standard_character_class()?
                }
            }
            '^' => {
                self.advance();
                Term::Assertion(Assertion::Start)
            }
            '$' => {
                self.advance();
                Term::Assertion(Assertion::End)
            }
            // Might be an escape code
            '\\' => {
                let start_pos = self.pos();

                match_u32!(match self.peek() {
                    // Standard character class shorthands
                    'w' => {
                        self.advance2();
                        Term::CharacterClass(self.character_class_from_shorthand(ClassRange::Word))
                    }
                    'W' => {
                        self.advance2();
                        Term::CharacterClass(
                            self.character_class_from_shorthand(ClassRange::NotWord),
                        )
                    }
                    'd' => {
                        self.advance2();
                        Term::CharacterClass(self.character_class_from_shorthand(ClassRange::Digit))
                    }
                    'D' => {
                        self.advance2();
                        Term::CharacterClass(
                            self.character_class_from_shorthand(ClassRange::NotDigit),
                        )
                    }
                    's' => {
                        self.advance2();
                        Term::CharacterClass(
                            self.character_class_from_shorthand(ClassRange::Whitespace),
                        )
                    }
                    'S' => {
                        self.advance2();
                        Term::CharacterClass(
                            self.character_class_from_shorthand(ClassRange::NotWhitespace),
                        )
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

                        // Ensure that index is in range
                        if let Some(index) = index.and_then(|index| u32::try_from(index).ok()) {
                            // Save indexed backreference to be analyzed after parsing
                            self.indexed_backreferences.push((index, start_pos));

                            Term::Backreference(p!(self, Backreference { index }))
                        } else {
                            return self.error(start_pos, ParseError::InvalidBackreferenceIndex);
                        }
                    }
                    // Named backreferences. Can only parse when we are in parse named capture
                    // groups mode.
                    'k' if self.parse_named_capture_groups => {
                        self.advance2();

                        self.expect('<')?;
                        let name = self.parse_identifier()?;
                        self.expect('>')?;

                        // Initialize backreference with a fake index that will be resolved later
                        let backreference = p!(self, Backreference { index: 0 });

                        // Save named backreference to be analyzed and resolved after parsing
                        self.named_backreferences.push((
                            name.as_arena_str(),
                            start_pos,
                            AstPtr::from_ref(backreference.as_ref()),
                        ));

                        Term::Backreference(backreference)
                    }
                    // Unicode properties
                    'p' if self.is_unicode_aware() => {
                        self.advance2();
                        self.expect('{')?;

                        let property = self.parse_unicode_property()?;

                        self.expect('}')?;

                        Term::CharacterClass(
                            self.character_class_from_shorthand(ClassRange::UnicodeProperty(
                                property,
                            )),
                        )
                    }
                    'P' if self.is_unicode_aware() => {
                        self.advance2();
                        self.expect('{')?;

                        let property = self.parse_unicode_property()?;

                        self.expect('}')?;

                        Term::CharacterClass(self.character_class_from_shorthand(
                            ClassRange::NotUnicodeProperty(property),
                        ))
                    }
                    // Otherwise must be a regular regexp escape sequence
                    _ => {
                        let code_point = self.parse_regexp_escape_sequence()?;
                        Term::Literal(Wtf8String::from_code_point_in(code_point, self.alloc))
                    }
                })
            }
            // Otherwise this must be a literal term
            _ => {
                let code_point = self.parse_unicode_codepoint()?;
                Term::Literal(Wtf8String::from_code_point_in(code_point, self.alloc))
            }
        });

        // Term may be postfixed with a quantifier
        self.parse_quantifier(atom)
    }

    fn asdf() {}

    fn character_class_from_shorthand(&self, shorthand: ClassRange<'a>) -> CharacterClass<'a> {
        let mut operands = alloc::vec![in self.alloc];
        operands.push(shorthand);

        CharacterClass {
            expression_type: ClassExpressionType::Union,
            is_inverted: false,
            operands,
        }
    }

    fn parse_quantifier(&mut self, term: Term<'a>) -> ParseResult<Term<'a>> {
        let quantifier_pos = self.pos();

        let bounds_opt = match_u32!(match self.current() {
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
                // In Annex B mode if we fail to parse a '{...' quantifier then we should recover
                // and reparse it as the next term.
                let (lower_bound, upper_bound) = if self.in_annex_b_mode {
                    let save_state = self.save();
                    if let Ok(bounds) = self.parse_braced_quantifier() {
                        bounds
                    } else {
                        self.restore(&save_state);
                        return Ok(term);
                    }
                } else {
                    self.parse_braced_quantifier()?
                };

                // Check that quantifier is valid, meaning the lower bound is not greater
                // than the upper bound.
                if let Some(upper_bound) = upper_bound {
                    if lower_bound > upper_bound {
                        return self.error(quantifier_pos, ParseError::InvalidQuantifierBounds);
                    }
                }

                Some((lower_bound, upper_bound))
            }
            _ => None,
        });

        if let Some((min, max)) = bounds_opt {
            // Every quantifier can be postfixed with a `?` to make it lazy
            let is_greedy = !self.eat('?');

            // Check if term is a a non-quantifiable assertion. Only lookaheads are allowed as
            // quantifiable assertions in Annex B (but all engines appear to support quantifiable
            // lookaheads in non-Annex B mode).
            match term {
                Term::Lookaround(Lookaround { is_ahead: true, .. }) if !self.is_unicode_aware() => {
                }
                Term::Assertion(_) | Term::Lookaround(_) => {
                    return self.error(quantifier_pos, ParseError::NonQuantifiableAssertion);
                }
                _ => {}
            }

            Ok(Term::Quantifier(Quantifier { term: p!(self, term), min, max, is_greedy }))
        } else {
            Ok(term)
        }
    }

    /// Try to parse a braced quantifier (e.g. `{3}``, or `{1, 3}`). On failure return the error
    /// and the original term itself, as the original term is need to recover in Annex B mode.
    fn parse_braced_quantifier(&mut self) -> ParseResult<(u64, Option<u64>)> {
        self.advance();

        // Parse quantifier's lower bound
        let lower_bound_pos = self.pos();
        let lower_bound = self.parse_decimal_digits()?;

        // If lower bound is out of range then error immediately since we can't generate
        // conforming bytecode.
        let Some(lower_bound) = lower_bound else {
            return self.error(lower_bound_pos, ParseError::QuantifierBoundTooLarge);
        };

        let upper_bound = if self.eat(',') {
            if self.current() == '}' as u32 {
                None
            } else {
                // Parse the upper bound, treating as none if the upper bound is out of
                // range.
                self.parse_decimal_digits()?
            }
        } else {
            Some(lower_bound)
        };

        self.expect('}')?;

        Ok((lower_bound, upper_bound))
    }

    /// Parse a sequence of decimal digits into a number. Error if there is no sequence of decimal
    /// digits. Returns `None` if the number is too large to be represented as a `u64`.
    fn parse_decimal_digits(&mut self) -> ParseResult<Option<u64>> {
        // Sequence of decimal digits must be nonempty
        if !is_decimal_digit(self.current()) {
            return self.error_unexpected_token(self.pos());
        }

        let mut value: u64 = 0;

        // Keep track of whether we have overflowed but keep consuming all digits
        let mut has_overflowed = false;

        while is_decimal_digit(self.current()) {
            // Check for overflow when multiplying by 10 or adding digit to accumulator
            if let Some(new_value) = value.checked_mul(10) {
                value = new_value;
            } else {
                has_overflowed = true;
            }

            if let Some(new_value) = value.checked_add((self.current() as u64) - ('0' as u64)) {
                value = new_value;
            } else {
                has_overflowed = true;
            }

            self.advance();
        }

        if has_overflowed {
            return Ok(None);
        }

        Ok(Some(value))
    }

    fn parse_group(&mut self) -> ParseResult<Term<'a>> {
        self.advance();

        let left_paren_pos = self.pos();

        if self.eat('?') {
            match_u32!(match self.current() {
                ':' => {
                    self.advance();
                    let disjunction = self.parse_disjunction()?;
                    self.expect(')')?;

                    Ok(Term::AnonymousGroup(AnonymousGroup {
                        disjunction,
                        positive_modifiers: RegExpFlags::empty(),
                        negative_modifiers: RegExpFlags::empty(),
                    }))
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

                    match_u32!(match self.current() {
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
                            // First parse the name
                            let name_start_pos = self.pos();
                            let name = self.parse_identifier()?;
                            self.expect('>')?;

                            // Generate the next capture group index and add name to list of all
                            // capture groups.
                            let index = self.next_capture_group_index(left_paren_pos)?;
                            self.capture_groups.push(Some(name.clone()));

                            // Add index to `capture_group_names`, overwriting earlier index
                            if self
                                .capture_group_names
                                .insert(name.as_arena_str(), index)
                                .is_some()
                            {
                                self.has_duplicate_named_capture_groups = true;
                            }

                            // If name is currently in scope then error
                            if self
                                .current_capture_group_names
                                .contains(name.as_arena_str())
                            {
                                return self
                                    .error(name_start_pos, ParseError::DuplicateCaptureGroupName);
                            }

                            // Add to set of all capture groups in scope, and to the current
                            // scope.
                            self.current_capture_group_names.insert(name.as_arena_str());
                            self.current_capture_group_name_scope
                                .push(name.as_arena_str());

                            let disjunction = self.parse_disjunction()?;
                            self.expect(')')?;

                            // In Annex B the first parse assumes there are no named capture groups.
                            // Throw a marker error to signal that a named capture group was found.
                            if !self.parse_named_capture_groups {
                                return self
                                    .error(self.pos(), ParseError::NamedCaptureGroupEncountered);
                            }

                            Ok(Term::CaptureGroup(CaptureGroup {
                                name: Some(name),
                                index,
                                disjunction,
                            }))
                        }
                    })
                }
                // Start of regexp modifiers for an anonymous group
                'i' | 's' | 'm' | '-' => {
                    let modifier_start_pos = self.pos();
                    let positive_modifiers = self.parse_modifiers()?;

                    let negative_modifiers = if self.eat('-') {
                        let negative_modifiers = self.parse_modifiers()?;

                        // Both positive and negative modifiers can't be empty (i.e. `(?-:` prefix)
                        if positive_modifiers.is_empty() && negative_modifiers.is_empty() {
                            return self
                                .error(modifier_start_pos, ParseError::EmptyRegExpModifiers);
                        }

                        negative_modifiers
                    } else {
                        RegExpFlags::empty()
                    };

                    // Check if any modifier is contained in both positive and negative modifiers
                    if !((positive_modifiers & negative_modifiers).is_empty()) {
                        return self.error(modifier_start_pos, ParseError::DuplicateRegExpModifier);
                    }

                    self.expect(':')?;
                    let disjunction = self.parse_disjunction()?;
                    self.expect(')')?;

                    Ok(Term::AnonymousGroup(AnonymousGroup {
                        disjunction,
                        positive_modifiers,
                        negative_modifiers,
                    }))
                }
                _ => self.error_unexpected_token(self.pos()),
            })
        } else {
            // Add to list of all capture groups without name
            let index = self.next_capture_group_index(left_paren_pos)?;
            self.capture_groups.push(None);

            let disjunction = self.parse_disjunction()?;
            self.expect(')')?;

            Ok(Term::CaptureGroup(CaptureGroup { name: None, index, disjunction }))
        }
    }

    fn parse_modifiers(&mut self) -> ParseResult<RegExpFlags> {
        let mut modifiers = RegExpFlags::empty();

        loop {
            match_u32!(match self.current() {
                'i' => {
                    if modifiers.is_case_insensitive() {
                        return self.error(self.pos(), ParseError::DuplicateRegExpModifier);
                    }

                    self.advance();
                    modifiers.insert(RegExpFlags::IGNORE_CASE);
                }
                'm' => {
                    if modifiers.is_multiline() {
                        return self.error(self.pos(), ParseError::DuplicateRegExpModifier);
                    }

                    self.advance();
                    modifiers.insert(RegExpFlags::MULTILINE);
                }
                's' => {
                    if modifiers.is_dot_all() {
                        return self.error(self.pos(), ParseError::DuplicateRegExpModifier);
                    }

                    self.advance();
                    modifiers.insert(RegExpFlags::DOT_ALL);
                }
                _ => break,
            })
        }

        Ok(modifiers)
    }

    /// Parse a character class when not in `v` mode.
    fn parse_standard_character_class(&mut self) -> ParseResult<Term<'a>> {
        self.advance();

        let is_inverted = self.eat('^');

        let mut ranges = self.alloc_vec();
        while !self.eat(']') {
            let atom_start_pos = self.pos();
            let atom = self.parse_class_atom()?;

            if self.eat('-') {
                // Trailing `-` at the end of the character class
                if self.eat(']') {
                    ranges.push(atom);
                    ranges.push(ClassRange::Single('-' as u32));
                    break;
                }

                // Otherwise this must be a range
                let end_atom = self.parse_class_atom()?;

                let start = self.class_atom_to_range_bound(atom)?;
                let end = self.class_atom_to_range_bound(end_atom)?;

                if end < start {
                    return self.error(atom_start_pos, ParseError::InvalidCharacterClassRange);
                }

                ranges.push(ClassRange::Range(start, end));
            } else {
                ranges.push(atom);
            }
        }

        Ok(Term::CharacterClass(CharacterClass {
            expression_type: ClassExpressionType::Union,
            is_inverted,
            operands: ranges,
        }))
    }

    fn class_atom_to_range_bound(&mut self, atom: ClassRange) -> ParseResult<u32> {
        match atom {
            ClassRange::Single(code_point) => Ok(code_point),
            ClassRange::Word
            | ClassRange::NotWord
            | ClassRange::Digit
            | ClassRange::NotDigit
            | ClassRange::Whitespace
            | ClassRange::NotWhitespace
            | ClassRange::UnicodeProperty(_)
            | ClassRange::NotUnicodeProperty(_)
            | ClassRange::NestedClass(_)
            | ClassRange::StringDisjunction(_) => {
                self.error(self.pos(), ParseError::RegExpCharacterClassInRange)
            }
            ClassRange::Range(..) => unreachable!("Ranges are not returned by parse_class_atom"),
        }
    }

    fn parse_class_atom(&mut self) -> ParseResult<ClassRange<'a>> {
        // Could be the start of an escape sequence
        if self.current() == '\\' as u32 {
            return match_u32!(match self.peek() {
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
                    Ok(ClassRange::Single('\u{0008}' as u32))
                }
                // Minus is only escaped in unicode aware mode
                '-' if self.is_unicode_aware() => {
                    self.advance2();
                    Ok(ClassRange::Single('-' as u32))
                }
                // Unicode properties
                'p' if self.is_unicode_aware() => {
                    self.advance2();
                    self.expect('{')?;

                    let property = self.parse_unicode_property()?;

                    self.expect('}')?;

                    Ok(ClassRange::UnicodeProperty(property))
                }
                'P' if self.is_unicode_aware() => {
                    self.advance2();
                    self.expect('{')?;

                    let property = self.parse_unicode_property()?;

                    self.expect('}')?;

                    Ok(ClassRange::NotUnicodeProperty(property))
                }
                // After checking class-specific escape sequences, must be a regular regexp
                // escape sequence.
                _ => Ok(ClassRange::Single(self.parse_regexp_escape_sequence()?)),
            });
        }

        let code_point = self.parse_unicode_codepoint()?;

        Ok(ClassRange::Single(code_point))
    }

    /// Parse a character class when in `v` mode.
    ///
    /// Return whether the character class may contain strings.
    fn parse_unicode_sets_character_class(&mut self) -> ParseResult<(CharacterClass<'a>, bool)> {
        self.advance();

        let is_inverted = self.eat('^');

        // Character class may be empty
        if self.eat(']') {
            return Ok((
                CharacterClass {
                    expression_type: ClassExpressionType::Union,
                    is_inverted,
                    operands: self.alloc_vec(),
                },
                false,
            ));
        }

        let first_operand_pos = self.pos();
        let (first_operand, first_operand_may_contain_strings) = self.parse_class_set_operand()?;
        let mut operands = self.alloc_vec_with_element(first_operand);

        let expression_type;
        let mut may_contain_strings;

        if self.is_at_class_intersection_operator() {
            // Intersection character class
            expression_type = ClassExpressionType::Intersection;

            // All operands must contain strings for the entire intersection to contain strings
            may_contain_strings = true;

            while self.is_at_class_intersection_operator() {
                self.advance2();

                let (operand, operand_may_contain_strings) = self.parse_class_set_operand()?;

                // If any operand does not contain strings, the entire intersection also cannot
                if !operand_may_contain_strings {
                    may_contain_strings = false;
                }

                operands.push(operand);
            }
        } else if self.is_at_class_difference_operator() {
            // Difference character class
            expression_type = ClassExpressionType::Difference;

            // The entire difference may contains strings if the first operand does
            may_contain_strings = first_operand_may_contain_strings;

            while self.is_at_class_difference_operator() {
                self.advance2();

                let (operand, _) = self.parse_class_set_operand()?;
                operands.push(operand);
            }
        } else if self.current() == ']' as u32 {
            // Single operand union
            expression_type = ClassExpressionType::Union;
            may_contain_strings = first_operand_may_contain_strings;
        } else {
            // Non-empty union character class
            expression_type = ClassExpressionType::Union;

            // The entire union may contain strings if any operand does
            may_contain_strings = false;

            // Check if the first operand is actually the left side of a range
            if self.current() == '-' as u32 {
                let start_operand = operands.pop().unwrap();
                let range = self
                    .parse_unicode_sets_character_class_range(start_operand, first_operand_pos)?;
                operands.push(range);
            }

            // Parse adjacent operands to the union class
            while self.current() != ']' as u32 {
                let operand_start_pos = self.pos();
                let (operand, operand_has_strings) = self.parse_class_set_operand()?;

                if operand_has_strings {
                    may_contain_strings = true;
                }

                // Check if the operand is actually the left side of a range
                if self.current() == '-' as u32 {
                    let range =
                        self.parse_unicode_sets_character_class_range(operand, operand_start_pos)?;
                    operands.push(range);
                } else {
                    operands.push(operand);
                }
            }
        }

        self.expect(']')?;

        // Inverted character classes cannot contain strings
        if may_contain_strings && is_inverted {
            return self.error(first_operand_pos, ParseError::InvertedCharacterClassContainStrings);
        }

        Ok((CharacterClass { expression_type, is_inverted, operands }, may_contain_strings))
    }

    /// Parse a single ClassSetOperand, returning whether it MayContainStrings.
    fn parse_class_set_operand(&mut self) -> ParseResult<(ClassRange<'a>, bool)> {
        if self.current() == '[' as u32 {
            let (nested_class, may_contain_strings) = self.parse_unicode_sets_character_class()?;
            return Ok((ClassRange::NestedClass(nested_class), may_contain_strings));
        }

        if let Some(code_point) = self.parse_class_set_character_reserved_syntax()? {
            return Ok((ClassRange::Single(code_point), false));
        }

        // String disjunction \q{...}
        if self.current() == '\\' as u32 && self.peek() == 'q' as u32 {
            let (string_disjunction, may_contain_strings) = self.parse_string_disjunction()?;
            return Ok((ClassRange::StringDisjunction(string_disjunction), may_contain_strings));
        }

        // Otherwise defer to standard character class atom parsing
        let atom = self.parse_class_atom()?;
        Ok((atom, false))
    }

    /// Parses the following parts of ClassSetCharacter (https://tc39.es/ecma262/#prod-ClassSetCharacter)
    ///   \ ClassSetReservedPunctuator
    ///   [lookahead ∉ ClassSetReservedDoublePunctuator]
    ///   not ClassSetSyntaxCharacter
    fn parse_class_set_character_reserved_syntax(&mut self) -> ParseResult<Option<u32>> {
        // First handle `v`-mode specific syntax
        match_u32!(match self.current() {
            '\\' => match_u32!(match self.peek() {
                // Members of ClassSetReservedPunctuator which can be escaped
                code_point @ ('&' | '-' | '!' | '#' | '%' | ',' | ':' | ';' | '<' | '=' | '>'
                | '@' | '`' | '~') => {
                    self.advance2();
                    return Ok(Some(code_point));
                }
                _ => {}
            }),
            // Members of ClassSetSyntaxCharacter are not allowed without escaping
            '(' | ')' | '[' | ']' | '{' | '}' | '/' | '-' | '|' => {
                return self.error_unexpected_token(self.pos());
            }
            // Do not allow reserved double punctuators
            _ if self.is_at_class_set_reserved_double_punctuator() => {
                // Point error to second character of the double punctuator
                self.advance();
                return self.error_unexpected_token(self.pos());
            }
            _ => {}
        });

        Ok(None)
    }

    fn parse_class_set_character(&mut self) -> ParseResult<u32> {
        if let Some(code_point) = self.parse_class_set_character_reserved_syntax()? {
            return Ok(code_point);
        }

        if self.current() == '\\' as u32 {
            // '\b' is class specific and not included in common regexp escape sequences
            if self.peek() == 'b' as u32 {
                self.advance2();
                return Ok('\u{0008}' as u32);
            }

            return self.parse_regexp_escape_sequence();
        }

        self.parse_unicode_codepoint()
    }

    fn parse_unicode_sets_character_class_range(
        &mut self,
        start_operand: ClassRange<'a>,
        operand_start_pos: Pos,
    ) -> ParseResult<ClassRange<'a>> {
        // Skip the `-` character
        self.advance();

        let (end_operand, _) = self.parse_class_set_operand()?;

        let start = self.class_atom_to_range_bound(start_operand)?;
        let end = self.class_atom_to_range_bound(end_operand)?;

        if end < start {
            return self.error(operand_start_pos, ParseError::InvalidCharacterClassRange);
        }

        Ok(ClassRange::Range(start, end))
    }

    fn is_at_class_intersection_operator(&mut self) -> bool {
        self.current() == '&' as u32 && self.peek() == '&' as u32 && self.peek2() != '&' as u32
    }

    fn is_at_class_difference_operator(&mut self) -> bool {
        self.current() == '-' as u32 && self.peek() == '-' as u32
    }

    fn is_at_class_set_reserved_double_punctuator(&mut self) -> bool {
        match_u32!(match self.current() {
            punctuator @ ('&' | '!' | '#' | '$' | '%' | '*' | '+' | ',' | '.' | ':' | ';' | '<'
            | '=' | '>' | '?' | '@' | '^' | '`' | '~') => {
                self.peek() == punctuator
            }
            _ => false,
        })
    }

    fn parse_regexp_escape_sequence(&mut self) -> ParseResult<u32> {
        match_u32!(match self.peek() {
            // Unicode escape sequence
            'u' => self.parse_regex_unicode_escape_sequence(self.is_unicode_aware()),
            // Standard control characters
            'f' => {
                self.advance2();
                Ok('\u{000c}' as u32)
            }
            'n' => {
                self.advance2();
                Ok('\n' as u32)
            }
            'r' => {
                self.advance2();
                Ok('\r' as u32)
            }
            't' => {
                self.advance2();
                Ok('\t' as u32)
            }
            'v' => {
                self.advance2();
                Ok('\u{000b}' as u32)
            }
            // Any escaped ASCII letter - converted to letter value mod 32
            'c' if is_ascii_alphabetic(self.peek2()) => {
                let code_point = self.peek2() % 32;
                self.advance3();

                // Safe since code point is guaranteed to be in range [0, 32)
                Ok(code_point)
            }
            // ASCII hex escape sequence
            'x' => {
                let start_pos = self.pos();
                self.advance2();
                let code_point = self.parse_hex2_digits(start_pos)?;

                // Safe since code point is guaranteed to be in the range [0, 255)
                Ok(code_point)
            }
            // The null byte
            '0' if !is_decimal_digit(self.peek2()) => {
                self.advance2();
                Ok(0)
            }
            _ => {
                let start_pos = self.pos();
                self.advance();

                // In unicode-aware mode only regexp special characters can be escaped
                if self.is_unicode_aware() {
                    match_u32!(match self.current() {
                        '/' | '\\' | '^' | '$' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']'
                        | '{' | '}' | '|' => {
                            let code_point = self.current();
                            self.advance();
                            Ok(code_point)
                        }
                        _ => self.error(start_pos, ParseError::MalformedEscapeSeqence),
                    })
                } else {
                    let save_state = self.save();

                    // In non-unicode mode all non id_continue characters can be escaped
                    let code_point = self.parse_unicode_codepoint()?;
                    if !is_id_continue_unicode(code_point) {
                        Ok(code_point)
                    } else {
                        // Otherwise back up and treat the slash as a literal character, not the
                        // start of an escape sequence.
                        self.restore(&save_state);
                        Ok('\\' as u32)
                    }
                }
            }
        })
    }

    fn parse_unicode_property(&mut self) -> ParseResult<UnicodeProperty> {
        let start_pos = self.pos();
        let property_name = self.parse_unicode_property_name();

        if self.eat('=') {
            let property_value = self.parse_unicode_property_name();

            if matches!(property_name.as_str(), "General_Category" | "gc") {
                if let Some(general_category) = GeneralCategoryProperty::parse(&property_value) {
                    return Ok(UnicodeProperty::GeneralCategory(general_category));
                }
            }

            if matches!(property_name.as_str(), "Script" | "sc") {
                if let Some(script_property) = ScriptProperty::parse(&property_value, false) {
                    return Ok(UnicodeProperty::Script(script_property));
                }
            }

            if matches!(property_name.as_str(), "Script_Extensions" | "scx") {
                if let Some(script_property) = ScriptProperty::parse(&property_value, true) {
                    return Ok(UnicodeProperty::Script(script_property));
                }
            }

            return self.error(start_pos, ParseError::InvalidUnicodeProperty);
        }

        // Otherwise must be a binary unicode property or general category property
        if let Some(binary_property) = BinaryUnicodeProperty::parse(&property_name) {
            Ok(UnicodeProperty::Binary(binary_property))
        } else if let Some(general_category) = GeneralCategoryProperty::parse(&property_name) {
            Ok(UnicodeProperty::GeneralCategory(general_category))
        } else {
            self.error(start_pos, ParseError::InvalidUnicodeProperty)
        }
    }

    fn parse_unicode_property_name(&mut self) -> String {
        let mut string_builder = String::new();

        while is_ascii_alphabetic(self.current())
            || is_decimal_digit(self.current())
            || self.current() == '_' as u32
        {
            string_builder.push(self.current() as u8 as char);
            self.advance();
        }

        string_builder
    }

    /// Parses string disjunction syntax, starting at the opening `\`.
    ///
    /// Return the string disjunction, and whether the disjunction MayContainStrings.
    fn parse_string_disjunction(&mut self) -> ParseResult<(StringDisjunction<'a>, bool)> {
        self.advance2();
        self.expect('{')?;

        let mut alternatives = self.alloc_vec();

        // Keep track of whether this disjunction contains strings, as opposed to contains only
        // single code points.
        //
        // Note that this means the empty string is considered a string. So initially check for an
        // entirely empty disjunction.
        let mut may_contain_strings = self.current() == '}' as u32;

        while self.current() != '}' as u32 {
            let (alternative, len) = self.parse_string_disjunction_alternative()?;

            if !alternative.is_empty() {
                alternatives.push(alternative);
            }

            if len != 1 {
                may_contain_strings = true;
            }

            if !self.eat('|') {
                break;
            }

            // Check for a trailing `|` which means there is a final empty alternative
            if self.current() == '}' as u32 {
                may_contain_strings = true;
                break;
            }
        }

        self.expect('}')?;

        Ok((StringDisjunction { alternatives }, may_contain_strings))
    }

    /// Parse a single alternative in a class string disjunction. Return both the string and its
    /// length in code points.
    fn parse_string_disjunction_alternative(&mut self) -> ParseResult<(AstString<'a>, u64)> {
        let mut string = self.alloc_str("");
        let mut num_code_points = 0;

        while self.current() != '|' as u32 && self.current() != '}' as u32 {
            let code_point = self.parse_class_set_character()?;
            string.push(code_point);
            num_code_points += 1;
        }

        Ok((string, num_code_points))
    }

    fn parse_identifier(&mut self) -> ParseResult<AstString<'a>> {
        let mut string_builder = self.alloc_str("");

        // First character must be an id start, which can be an escape sequence
        let code_point = if self.current() == '\\' as u32 {
            self.parse_regex_unicode_escape_sequence(true)?
        } else {
            // Otherwise must be a unicode codepoint
            let code_point = self.parse_unicode_codepoint()?;

            // If in non-unicode mode then parse as surrogate pair
            if !self.flags.has_any_unicode_flag() && is_high_surrogate_code_point(code_point) {
                let next_code_point = self.parse_unicode_codepoint()?;
                if is_low_surrogate_code_point(next_code_point) {
                    code_point_from_surrogate_pair(code_point as u16, next_code_point as u16)
                } else {
                    code_point
                }
            } else {
                code_point
            }
        };

        if let Some(char) = as_id_start(code_point) {
            string_builder.push_char(char);
        } else {
            return self.error_unexpected_token(self.pos());
        }

        // All following characters must be id parts
        loop {
            // Can be an escape sequence
            if self.current() == '\\' as u32 {
                let code_point = self.parse_regex_unicode_escape_sequence(true)?;

                if let Some(char) = as_id_part(code_point) {
                    string_builder.push_char(char);
                } else {
                    return self.error_unexpected_token(self.pos());
                }
            } else {
                // Otherwise must be a unicode codepoint
                let save_state = self.save();
                let mut code_point = self.parse_unicode_codepoint()?;

                // If in non-unicode mode then parse as surrogate pair
                if !self.flags.has_any_unicode_flag() && is_high_surrogate_code_point(code_point) {
                    let next_code_point = self.parse_unicode_codepoint()?;
                    if is_low_surrogate_code_point(next_code_point) {
                        code_point = code_point_from_surrogate_pair(
                            code_point as u16,
                            next_code_point as u16,
                        );
                    }
                }

                if let Some(char) = as_id_part(code_point) {
                    string_builder.push_char(char);
                } else {
                    // Restore to before codepoint if not part of the id
                    self.restore(&save_state);
                    break;
                }
            }
        }

        Ok(string_builder)
    }

    fn parse_regex_unicode_escape_sequence(&mut self, is_unicode_aware: bool) -> ParseResult<u32> {
        let start_pos = self.pos();

        // Unicode escape sequences always start with `\u`
        self.advance();

        let after_slash_state = self.save();

        self.expect('u')?;

        // In unicode aware mode the escape sequence \u{digits} is allowed
        if self.eat('{') {
            // If not in unicode aware mode this was an escaped 'u' and following characters should
            // be parsed separately.
            if !is_unicode_aware {
                self.restore(&after_slash_state);
                return Ok('u' as u32);
            }

            // Cannot be empty
            if self.eat('}') {
                return self.error(start_pos, ParseError::MalformedEscapeSeqence);
            }

            // collect all hex digits until closing brace
            let mut value = 0;
            while self.current() != '}' as u32 {
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

            return Ok(value);
        }

        let code_unit = self.parse_hex4_digits(start_pos)?;

        // All code units are handled separately in unicode unaware mode
        if !is_unicode_aware {
            return Ok(code_unit as u32);
        }

        // Check if this could be the start of a surrogate pair
        if is_high_surrogate_code_unit(code_unit)
            && self.current() == '\\' as u32
            && self.peek() == 'u' as u32
        {
            let save_state = self.save();

            // Speculatively parse the next code unit, checking if it forms a surrogate pair. If not
            // then restore to before the next code unit.
            self.advance2();
            let next_code_unit = self.parse_hex4_digits(start_pos)?;
            if is_low_surrogate_code_unit(next_code_unit) {
                return Ok(code_point_from_surrogate_pair(code_unit, next_code_unit));
            }

            self.restore(&save_state);
        }

        Ok(code_unit as u32)
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

    fn parse_hex4_digits(&mut self, start_pos: Pos) -> ParseResult<u16> {
        let mut value = 0;
        for _ in 0..4 {
            if let Some(hex_value) = get_hex_value(self.current()) {
                self.advance();
                value <<= 4;
                value += hex_value as u16;
            } else {
                return self.error(start_pos, ParseError::MalformedEscapeSeqence);
            }
        }

        Ok(value)
    }

    /// Analyze and resolve all backreferences in the pattern regexp. Backreferences may have been
    /// parsed before the associated capture group was found, so error on unresolved backreferences
    /// and fill in correct indices for named backreferences.
    fn resolve_backreferences(&mut self) -> ParseResult<()> {
        // Keep track of the first error that appears in the input
        let mut first_error = None;

        let mut update_first_error = |pos, error| {
            if let Some((first_error_pos, _)) = &first_error {
                if pos < *first_error_pos {
                    first_error = Some((pos, error));
                }
            } else {
                first_error = Some((pos, error));
            }
        };

        // Check for any indexed backreferences that are out of range
        for (index, error_pos) in &self.indexed_backreferences {
            if (*index) as usize > self.capture_groups.len() {
                update_first_error(*error_pos, ParseError::InvalidBackreferenceIndex);
            }
        }

        // Resolve named backreferences, erroring if the name cannot be resolved
        for (name, error_pos, backreference_ptr) in &mut self.named_backreferences {
            if let Some(index) = self.capture_group_names.get(name) {
                backreference_ptr.as_mut().index = *index;
            } else {
                update_first_error(*error_pos, ParseError::InvalidBackreferenceName);
            }
        }

        match first_error {
            None => Ok(()),
            Some((pos, error)) => self.error(pos, error),
        }
    }
}
