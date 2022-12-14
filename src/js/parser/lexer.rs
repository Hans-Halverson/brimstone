use std::rc::Rc;
use std::str::FromStr;

use super::loc::{Loc, Pos};
use super::parser::{LocalizedParseError, ParseError, ParseResult};
use super::source::Source;
use super::token::Token;

pub struct Lexer<'a> {
    pub source: &'a Rc<Source>,
    buf: &'a str,
    current: char,
    pos: Pos,
    is_new_line_before_current: bool,
}

/// A save point for the lexer, can be used to restore the lexer to a particular position.
pub struct SavedLexerState {
    current: char,
    pos: Pos,
}

type LexResult = ParseResult<(Token, Loc)>;

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: char = '\u{ffff}';

/// Can this character appear as the first character of an identifier.
fn is_id_start_char_ascii(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '_' | '$' => true,
        _ => false,
    }
}

/// Can this character appear in an identifier (after the first character).
fn is_id_part_char_ascii(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$' => true,
        _ => false,
    }
}

fn is_decimal_digit(char: char) -> bool {
    '0' <= char && char <= '9'
}

fn get_binary_value(char: char) -> Option<u32> {
    match char {
        '0' => Some(0),
        '1' => Some(1),
        _ => None,
    }
}

fn get_octal_value(char: char) -> Option<u32> {
    match char {
        '0'..='7' => Some(char as u32 - '0' as u32),
        _ => None,
    }
}

fn get_hex_value(char: char) -> Option<u32> {
    match char {
        '0'..='9' => Some(char as u32 - '0' as u32),
        'a'..='f' => Some(char as u32 - 'a' as u32 + 10),
        'A'..='F' => Some(char as u32 - 'A' as u32 + 10),
        _ => None,
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Rc<Source>) -> Lexer<'a> {
        let buf = &source.contents;
        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        Lexer {
            source: &source,
            buf: &source.contents,
            current,
            pos: 0,
            is_new_line_before_current: false,
        }
    }

    pub fn save(&self) -> SavedLexerState {
        SavedLexerState {
            current: self.current,
            pos: self.pos,
        }
    }

    pub fn restore(&mut self, save_state: &SavedLexerState) {
        self.current = save_state.current;
        self.pos = save_state.pos;
    }

    pub fn is_new_line_before_current(&self) -> bool {
        self.is_new_line_before_current
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        if self.pos < self.buf.len() {
            self.current = self.buf.as_bytes()[self.pos].into();
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    fn advance(&mut self) {
        self.advance_n(1);
    }

    fn advance2(&mut self) {
        self.advance_n(2);
    }

    fn advance3(&mut self) {
        self.advance_n(3);
    }

    fn advance4(&mut self) {
        self.advance_n(4);
    }

    #[inline]
    fn peek_n(&self, n: usize) -> char {
        let next_pos = self.pos + n;
        if next_pos < self.buf.len() {
            self.buf.as_bytes()[next_pos].into()
        } else {
            EOF_CHAR
        }
    }

    fn peek(&mut self) -> char {
        self.peek_n(1)
    }

    fn peek2(&mut self) -> char {
        self.peek_n(2)
    }

    fn peek3(&mut self) -> char {
        self.peek_n(3)
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc {
            start: start_pos,
            end: self.pos,
        }
    }

    fn emit(&self, token: Token, start_pos: Pos) -> LexResult {
        Ok((token, self.mark_loc(start_pos)))
    }

    fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        let source = (*self.source).clone();
        Err(LocalizedParseError {
            error,
            source_loc: Some((loc, source)),
        })
    }

    pub fn next(&mut self) -> LexResult {
        self.is_new_line_before_current = false;
        self.lex_token()
    }

    fn lex_token(&mut self) -> LexResult {
        // Skip whitespace
        loop {
            match self.current {
              | ' '
              | '\t'
              // Vertical tab
              | '\u{000b}'
              // Form feed
              | '\u{000c}' => self.advance(),
              | '\n'
              | '\r' => {
                self.is_new_line_before_current = true;
                self.advance();
              }
              | _ => break,
            }
        }

        let start_pos = self.pos;

        match self.current {
            '+' => match self.peek() {
                '+' => {
                    self.advance2();
                    self.emit(Token::Increment, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::AddEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Plus, start_pos)
                }
            },
            '-' => match self.peek() {
                '-' => {
                    self.advance2();
                    self.emit(Token::Decrement, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::SubtractEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Minus, start_pos)
                }
            },
            '*' => match self.peek() {
                '*' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::ExponentEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::Exponent, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::MultiplyEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Multiply, start_pos)
                }
            },
            '/' => match self.peek() {
                '/' => {
                    self.advance2();
                    self.skip_line_comment();
                    self.lex_token()
                }
                '*' => {
                    self.advance2();
                    let result_opt = self.skip_block_comment();

                    // Propagate result error up if one exists
                    if let Some(result) = result_opt {
                        result?;
                    }

                    self.lex_token()
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::DivideEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Divide, start_pos)
                }
            },
            '%' => match self.peek() {
                '=' => {
                    self.advance2();
                    self.emit(Token::RemainderEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Remainder, start_pos)
                }
            },
            '&' => match self.peek() {
                '&' => {
                    self.advance2();
                    self.emit(Token::LogicalAnd, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::AndEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseAnd, start_pos)
                }
            },
            '|' => match self.peek() {
                '|' => {
                    self.advance2();
                    self.emit(Token::LogicalOr, start_pos)
                }
                '=' => {
                    self.advance2();
                    self.emit(Token::OrEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseOr, start_pos)
                }
            },
            '?' => match self.peek() {
                '?' => {
                    self.advance2();
                    self.emit(Token::NullishCoalesce, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Question, start_pos)
                }
            },
            '^' => match self.peek() {
                '=' => {
                    self.advance2();
                    self.emit(Token::XorEq, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::BitwiseXor, start_pos)
                }
            },
            '>' => match self.peek() {
                '>' => match self.peek2() {
                    '>' => match self.peek3() {
                        '=' => {
                            self.advance4();
                            self.emit(Token::ShiftRightLogicalEq, start_pos)
                        }
                        _ => {
                            self.advance3();
                            self.emit(Token::ShiftRightLogical, start_pos)
                        }
                    },
                    '=' => {
                        self.advance3();
                        self.emit(Token::ShiftRightArithmeticEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::ShiftRightArithmetic, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::GreaterThanOrEqual, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::GreaterThan, start_pos)
                }
            },
            '<' => match self.peek() {
                '<' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::ShiftLeftEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::ShiftLeft, start_pos)
                    }
                },
                '=' => {
                    self.advance2();
                    self.emit(Token::LessThanOrEqual, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::LessThan, start_pos)
                }
            },
            '~' => {
                self.advance();
                self.emit(Token::BitwiseNot, start_pos)
            }
            '=' => match self.peek() {
                '=' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::EqEqEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::EqEq, start_pos)
                    }
                },
                '>' => {
                    self.advance2();
                    self.emit(Token::Arrow, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Equals, start_pos)
                }
            },
            '!' => match self.peek() {
                '=' => match self.peek2() {
                    '=' => {
                        self.advance3();
                        self.emit(Token::NotEqEq, start_pos)
                    }
                    _ => {
                        self.advance2();
                        self.emit(Token::NotEq, start_pos)
                    }
                },
                _ => {
                    self.advance();
                    self.emit(Token::LogicalNot, start_pos)
                }
            },
            '(' => {
                self.advance();
                self.emit(Token::LeftParen, start_pos)
            }
            ')' => {
                self.advance();
                self.emit(Token::RightParen, start_pos)
            }
            '{' => {
                self.advance();
                self.emit(Token::LeftBrace, start_pos)
            }
            '}' => {
                self.advance();
                self.emit(Token::RightBrace, start_pos)
            }
            '[' => {
                self.advance();
                self.emit(Token::LeftBracket, start_pos)
            }
            ']' => {
                self.advance();
                self.emit(Token::RightBracket, start_pos)
            }
            ';' => {
                self.advance();
                self.emit(Token::Semicolon, start_pos)
            }
            ',' => {
                self.advance();
                self.emit(Token::Comma, start_pos)
            }
            '.' => {
                if is_decimal_digit(self.peek()) {
                    self.lex_decimal_literal()
                } else {
                    self.advance();
                    self.emit(Token::Period, start_pos)
                }
            }
            ':' => {
                self.advance();
                self.emit(Token::Colon, start_pos)
            }
            '#' => {
                self.advance();
                self.emit(Token::Hash, start_pos)
            }
            '0' => match self.peek() {
                'b' | 'B' => self.lex_binary_literal(),
                'o' | 'O' => self.lex_octal_literal(),
                'x' | 'X' => self.lex_hex_literal(),
                _ => self.lex_decimal_literal(),
            },
            '1'..='9' => self.lex_decimal_literal(),
            '"' | '\'' => self.lex_string_literal(),
            EOF_CHAR => self.emit(Token::Eof, start_pos),
            char if is_id_start_char_ascii(char) => self.lex_identifier_ascii(start_pos),
            // Escape sequence at the start of an identifier
            '\\' => self.lex_identifier_non_ascii(start_pos),
            other => {
                self.advance();
                let loc = self.mark_loc(start_pos);
                self.error(
                    loc,
                    ParseError::UnknownToken(((other as u8) as char).into()),
                )
            }
        }
    }

    fn skip_line_comment(&mut self) {
        loop {
            match self.current {
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                    return;
                }
                EOF_CHAR => return,
                _ => self.advance(),
            }
        }
    }

    fn skip_block_comment(&mut self) -> Option<LexResult> {
        loop {
            match self.current {
                '*' => match self.peek() {
                    '/' => {
                        self.advance2();
                        break;
                    }
                    EOF_CHAR => {
                        let loc = self.mark_loc(self.pos + 1);
                        return Some(
                            self.error(loc, ParseError::ExpectedToken(Token::Eof, Token::Divide)),
                        );
                    }
                    _ => self.advance(),
                },
                '\n' | '\r' => {
                    self.advance();
                    self.is_new_line_before_current = true;
                }
                EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return Some(
                        self.error(loc, ParseError::ExpectedToken(Token::Eof, Token::Multiply)),
                    );
                }
                _ => self.advance(),
            }
        }

        None
    }

    fn skip_decimal_digits(&mut self) {
        while is_decimal_digit(self.current) {
            self.advance();
        }
    }

    fn lex_decimal_literal(&mut self) -> LexResult {
        let start_pos = self.pos;

        // Read optional digits before the decimal point
        self.skip_decimal_digits();

        // Read optional decimal point with its optional following digits
        if self.current == '.' {
            self.advance();
            self.skip_decimal_digits();
        }

        // Read optional exponent
        if self.current == 'e' || self.current == 'E' {
            self.advance();

            // Exponent has optional sign
            if self.current == '-' || self.current == '+' {
                self.advance();
            }

            if !is_decimal_digit(self.current) {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedNumericLiteral);
            }

            self.skip_decimal_digits();
        }

        // Parse float using rust stdlib
        let end_pos = self.pos;
        let value = f64::from_str(&self.buf[start_pos..end_pos]).unwrap();

        self.emit(Token::NumberLiteral(value), start_pos)
    }

    fn lex_binary_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_binary_value(self.current) {
            value <<= 1;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_octal_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_octal_value(self.current) {
            value <<= 3;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_hex_literal(&mut self) -> LexResult {
        let start_pos = self.pos;
        self.advance2();

        let mut value = 0;
        let mut has_digit = false;

        while let Some(digit) = get_hex_value(self.current) {
            value <<= 4;
            value += digit;

            has_digit = true;
            self.advance();
        }

        if !has_digit {
            let loc = self.mark_loc(start_pos);
            self.error(loc, ParseError::MalformedNumericLiteral)
        } else {
            self.emit(Token::NumberLiteral(f64::from(value)), start_pos)
        }
    }

    fn lex_string_literal(&mut self) -> LexResult {
        let quote_char = self.current;
        let start_pos = self.pos;
        self.advance();

        let mut value = String::new();

        while self.current != quote_char {
            match self.current {
                // Escape sequences
                '\\' => match self.peek() {
                    // Single character escapes
                    'n' => {
                        value.push('\n');
                        self.advance2()
                    }
                    '\\' => {
                        value.push('\\');
                        self.advance2()
                    }
                    '\'' => {
                        value.push('\'');
                        self.advance2()
                    }
                    '"' => {
                        value.push('"');
                        self.advance2()
                    }
                    't' => {
                        value.push('\t');
                        self.advance2()
                    }
                    'r' => {
                        value.push('\r');
                        self.advance2()
                    }
                    'b' => {
                        value.push('\x08');
                        self.advance2()
                    }
                    'v' => {
                        value.push('\x0B');
                        self.advance2()
                    }
                    'f' => {
                        value.push('\x0C');
                        self.advance2()
                    }
                    '0' if self.peek2() < '0' || self.peek2() > '9' => {
                        value.push('\x00');
                        self.advance2()
                    }
                    // Hex escape sequence
                    'x' => {
                        self.advance2();

                        if let Some(x1) = get_hex_value(self.current) {
                            if let Some(x2) = get_hex_value(self.peek()) {
                                let escaped_char = std::char::from_u32(x1 * 16 + x2).unwrap();
                                value.push(escaped_char);
                                self.advance2();
                            } else {
                                let loc = self.mark_loc(self.pos);
                                self.advance();
                                return self.error(loc, ParseError::MalformedEscapeSeqence);
                            }
                        } else {
                            let loc = self.mark_loc(self.pos);
                            return self.error(loc, ParseError::MalformedEscapeSeqence);
                        }
                    }
                    // Unicode escape sequence
                    'u' => {
                        let escape_start_pos = self.pos;
                        self.advance2();
                        let code_point = self.lex_unicode_escape_sequence(escape_start_pos)?;
                        value.push(code_point)
                    }
                    // Line continuations, either LF, CR, or CRLF
                    '\n' => {
                        value.push('\n');
                        self.advance2()
                    }
                    '\r' => {
                        value.push('\r');
                        self.advance2();

                        if self.current == '\n' {
                            value.push('\n');
                            self.advance()
                        }
                    }
                    // Not an escape sequence, use '/' directly
                    _ => {
                        value.push('/');
                        self.advance()
                    }
                },
                // Unterminated string literal
                '\n' | EOF_CHAR => {
                    let loc = self.mark_loc(self.pos);
                    return self.error(loc, ParseError::UnterminatedStringLiteral);
                }
                quote if quote == quote_char => break,
                other => {
                    value.push(other);
                    self.advance()
                }
            }
        }

        self.advance();

        return self.emit(Token::StringLiteral(value), start_pos);
    }

    // Lex a single unicode escape sequence, called after the '\u' prefix has already been processed
    fn lex_unicode_escape_sequence(&mut self, start_pos: Pos) -> ParseResult<char> {
        // Escape sequence has form \u{HEX_DIGITS}, with at most 6 hex digits
        if self.current == '{' {
            self.advance();

            if self.current == '}' {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            let mut value = 0;
            for _ in 0..6 {
                if let Some(hex_value) = get_hex_value(self.current) {
                    self.advance();
                    value <<= 4;
                    value += hex_value;
                } else if self.current == '}' {
                    break;
                } else {
                    let loc = self.mark_loc(start_pos);
                    return self.error(loc, ParseError::MalformedEscapeSeqence);
                }
            }

            if self.current != '}' {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }

            self.advance();

            // Check that value is not out of range (greater than \u10FFFF)
            if let Some(code_point) = std::char::from_u32(value) {
                return Ok(code_point);
            } else {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }
        }

        // Otherwise this is \uXXXX so expect exactly four hex digits
        let mut value = 0;
        for _ in 0..4 {
            if let Some(hex_value) = get_hex_value(self.current) {
                self.advance();
                value <<= 4;
                value += hex_value;
            } else {
                let loc = self.mark_loc(start_pos);
                return self.error(loc, ParseError::MalformedEscapeSeqence);
            }
        }

        let code_point = unsafe { std::char::from_u32_unchecked(value) };
        Ok(code_point)
    }

    // Fast path for lexing a purely ASCII identifier
    fn lex_identifier_ascii(&mut self, start_pos: Pos) -> LexResult {
        self.advance();

        loop {
            if is_id_part_char_ascii(self.current) {
                self.advance();
                continue;
            } else if self.current == '\\' {
                // Start of an escape sequence, so bail to slow path
                return self.lex_identifier_non_ascii(start_pos);
            } else {
                break;
            }
        }

        match &self.buf[start_pos..self.pos] {
            "var" => self.emit(Token::Var, start_pos),
            "let" => self.emit(Token::Let, start_pos),
            "const" => self.emit(Token::Const, start_pos),
            "function" => self.emit(Token::Function, start_pos),
            "async" => self.emit(Token::Async, start_pos),
            "this" => self.emit(Token::This, start_pos),
            "if" => self.emit(Token::If, start_pos),
            "else" => self.emit(Token::Else, start_pos),
            "switch" => self.emit(Token::Switch, start_pos),
            "case" => self.emit(Token::Case, start_pos),
            "default" => self.emit(Token::Default, start_pos),
            "for" => self.emit(Token::For, start_pos),
            "of" => self.emit(Token::Of, start_pos),
            "while" => self.emit(Token::While, start_pos),
            "do" => self.emit(Token::Do, start_pos),
            "with" => self.emit(Token::With, start_pos),
            "return" => self.emit(Token::Return, start_pos),
            "break" => self.emit(Token::Break, start_pos),
            "continue" => self.emit(Token::Continue, start_pos),
            "try" => self.emit(Token::Try, start_pos),
            "catch" => self.emit(Token::Catch, start_pos),
            "finally" => self.emit(Token::Finally, start_pos),
            "throw" => self.emit(Token::Throw, start_pos),
            "null" => self.emit(Token::Null, start_pos),
            "true" => self.emit(Token::True, start_pos),
            "false" => self.emit(Token::False, start_pos),
            "in" => self.emit(Token::In, start_pos),
            "instanceof" => self.emit(Token::InstanceOf, start_pos),
            "new" => self.emit(Token::New, start_pos),
            "typeof" => self.emit(Token::Typeof, start_pos),
            "void" => self.emit(Token::Void, start_pos),
            "delete" => self.emit(Token::Delete, start_pos),
            "debugger" => self.emit(Token::Debugger, start_pos),
            "static" => self.emit(Token::Static, start_pos),
            "from" => self.emit(Token::From, start_pos),
            "as" => self.emit(Token::As, start_pos),
            "class" => self.emit(Token::Class, start_pos),
            "extends" => self.emit(Token::Extends, start_pos),
            "super" => self.emit(Token::Super, start_pos),
            "get" => self.emit(Token::Get, start_pos),
            "set" => self.emit(Token::Set, start_pos),
            id => self.emit(Token::Identifier(String::from(id)), start_pos),
        }
    }

    // Slow path for lexing an identifier with at least one unicode character or escape sequence
    fn lex_identifier_non_ascii(&mut self, start_pos: Pos) -> LexResult {
        // Copy over the ASCII part into the string builder
        let mut string_builder = String::from(&self.buf[start_pos..self.pos]);

        loop {
            if is_id_part_char_ascii(self.current) {
                string_builder.push(self.current);
                self.advance();
            } else if self.current == '\\' {
                let escape_start_pos = self.pos;
                self.advance();

                if self.current == 'u' {
                    self.advance();

                    let code_point = self.lex_unicode_escape_sequence(escape_start_pos)?;
                    string_builder.push(code_point);
                } else {
                    let loc = self.mark_loc(escape_start_pos);
                    return self.error(loc, ParseError::MalformedEscapeSeqence);
                }
            } else {
                break;
            }
        }

        self.emit(Token::Identifier(string_builder), start_pos)
    }
}
