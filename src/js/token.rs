use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Eof,
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Exponent,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRightArithmetic,
    ShiftRightLogical,
    Increment,
    Decrement,
    LogicalNot,
    BitwiseNot,
    Equals,
    GreaterThan,
    LessThan,
    // Punctuation
    Semicolon,
    Comma,
    // Keywords
    Var,
    Let,
    Const,
    In,
    InstanceOf,
    Typeof,
    Void,
    Delete,
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
            Token::BitwiseAnd => "&",
            Token::BitwiseOr => "|",
            Token::BitwiseXor => "^",
            Token::ShiftLeft => "<<",
            Token::ShiftRightArithmetic => ">>",
            Token::ShiftRightLogical => ">>>",
            Token::Increment => "++",
            Token::Decrement => "--",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::LogicalNot => "~",
            Token::BitwiseNot => ",",
            Token::Equals => "=",
            Token::GreaterThan => ">",
            Token::LessThan => "<",
            Token::Eof => "<EOF>",
            Token::Var => "var",
            Token::Let => "let",
            Token::Const => "const",
            Token::In => "in",
            Token::InstanceOf => "instanceof",
            Token::Typeof => "typeof",
            Token::Void => "void",
            Token::Delete => "delete",
        };

        f.write_str(str)
    }
}
