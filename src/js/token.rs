use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Plus,
    Equals,
    Semicolon,
    Comma,
    Eof,
    Var,
    Let,
    Const,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Token::Identifier(name) => name,
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Equals => "=",
            Token::Eof => "<EOF>",
            Token::Var => "var",
            Token::Let => "let",
            Token::Const => "Const",
        };

        f.write_str(str)
    }
}
