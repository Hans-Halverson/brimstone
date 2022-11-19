use std::fs::File;
use std::io::{BufReader, Read};

use super::loc::{Loc, Pos};
use super::parser::{ParseError, ParseResult};
use super::token::Token;

pub struct Lexer {
    buf: String,
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

impl Lexer {
    pub fn new(file_name: &str) -> ParseResult<Lexer> {
        let file = File::open(file_name)?;
        let mut reader = BufReader::new(file);

        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;

        let current = if buf.len() == 0 {
            EOF_CHAR
        } else {
            buf.as_bytes()[0].into()
        };

        Ok(Lexer {
            buf,
            current,
            pos: 0,
        })
    }

    fn advance(&mut self) {
        self.pos += 1;
        if self.pos < self.buf.len() {
            self.current = self.buf.as_bytes()[self.pos].into();
        } else {
            self.current = EOF_CHAR;
            self.pos -= 1;
        }
    }

    fn emit(&self, token: Token, start_pos: Pos) -> LexResult {
        let loc = Loc {
            start: start_pos,
            end: self.pos,
        };
        Ok((token, loc))
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
            '+' => {
                self.advance();
                self.emit(Token::Plus, start_pos)
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
                Err(ParseError::UnknownToken(((other as u8) as char).into()))
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
