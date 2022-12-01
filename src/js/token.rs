use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Exponent,
    Increment,
    Decrement,
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
            Token::Minus => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::Remainder => "%",
            Token::Exponent => "**",
            Token::Increment => "++",
            Token::Decrement => "--",
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
