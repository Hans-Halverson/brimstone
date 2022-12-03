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
                self.advance();
                self.emit(Token::Period, start_pos)
            }
            ':' => {
                self.advance();
                self.emit(Token::Colon, start_pos)
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
            "in" => self.emit(Token::In, start_pos),
            "instanceof" => self.emit(Token::InstanceOf, start_pos),
            "new" => self.emit(Token::New, start_pos),
            "typeof" => self.emit(Token::Typeof, start_pos),
            "void" => self.emit(Token::Void, start_pos),
            "delete" => self.emit(Token::Delete, start_pos),
            id => self.emit(Token::Identifier(id.to_owned()), start_pos),
        }
    }
}
