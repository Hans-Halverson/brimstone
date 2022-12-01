use std::rc::Rc;

use super::loc::{Loc, Pos};
use super::parser::{LocalizedParseError, ParseError, ParseResult};
use super::source::Source;
use super::token::Token;

pub struct Lexer<'a> {
    pub source: &'a Rc<Source>,
    buf: &'a str,
    current: char,
    pos: Pos,
}

type LexResult = ParseResult<(Token, Loc)>;

/// Character that marks an EOF. Not a valid unicode character.
const EOF_CHAR: char = '\u{ffff}';

/// Can this character appear as the first character of an identifier.
fn is_id_start_char(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '_' | '$' => true,
        _ => false,
    }
}

/// Can this character appear in an identifier (after the first character).
fn is_id_part_char(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$' => true,
        _ => false,
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
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        if self.pos < self.buf.len() {
            self.current = self.buf.as_bytes()[self.pos].into();
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    fn advance2(&mut self) {
        self.pos += 2;
        if self.pos < self.buf.len() {
            self.current = self.buf.as_bytes()[self.pos].into();
        } else {
            self.current = EOF_CHAR;
            self.pos = self.buf.len();
        }
    }

    fn peek(&self) -> char {
        let next_pos = self.pos + 1;
        if next_pos < self.buf.len() {
            self.buf.as_bytes()[next_pos].into()
        } else {
            EOF_CHAR
        }
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

    fn error(&self, loc: Loc, error: ParseError) -> LexResult {
        let source = (*self.source).clone();
        Err(LocalizedParseError {
            error,
            source_loc: Some((loc, source)),
        })
    }

    pub fn next(&mut self) -> LexResult {
        // Skip whitespace
        loop {
            match self.current {
              | ' '
              | '\n'
              | '\t'
              | '\r'
              // Vertical tab
              | '\u{000b}'
              // Form feed
              | '\u{000c}' => self.advance(),
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
                _ => {
                    self.advance();
                    self.emit(Token::Minus, start_pos)
                }
            },
            '*' => match self.peek() {
                '*' => {
                    self.advance2();
                    self.emit(Token::Exponent, start_pos)
                }
                _ => {
                    self.advance();
                    self.emit(Token::Multiply, start_pos)
                }
            },
            '/' => {
                self.advance();
                self.emit(Token::Divide, start_pos)
            }
            '%' => {
                self.advance();
                self.emit(Token::Remainder, start_pos)
            }
            '=' => {
                self.advance();
                self.emit(Token::Equals, start_pos)
            }
            ';' => {
                self.advance();
                self.emit(Token::Semicolon, start_pos)
            }
            ',' => {
                self.advance();
                self.emit(Token::Comma, start_pos)
            }
            EOF_CHAR => self.emit(Token::Eof, start_pos),
            char if is_id_start_char(char) => self.lex_identifier(start_pos),
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

    pub fn lex_identifier(&mut self, start_pos: Pos) -> LexResult {
        self.advance();

        while is_id_part_char(self.current) {
            self.advance();
        }

        match &self.buf[start_pos..self.pos] {
            "var" => self.emit(Token::Var, start_pos),
            "let" => self.emit(Token::Let, start_pos),
            "const" => self.emit(Token::Const, start_pos),
            id => self.emit(Token::Identifier(id.to_owned()), start_pos),
        }
    }
}
