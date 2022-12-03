use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    Eof,
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Exponent,
    LogicalAnd,
    LogicalOr,
    NullishCoalesce,
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
    // Equality and relational
    Equals,
    EqEq,
    EqEqEq,
    NotEq,
    NotEqEq,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    // Operator assignment
    AddEq,
    SubtractEq,
    MultiplyEq,
    DivideEq,
    RemainderEq,
    ExponentEq,
    AndEq,
    OrEq,
    XorEq,
    ShiftLeftEq,
    ShiftRightArithmeticEq,
    ShiftRightLogicalEq,
    // Punctuation
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Semicolon,
    Comma,
    Period,
    Question,
    Colon,
    // Keywords
    Var,
    Let,
    Const,
    This,
    Null,
    True,
    False,
    In,
    InstanceOf,
    New,
    Typeof,
    Void,
    Delete,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Token::Identifier(name) => name,
            Token::StringLiteral(lit) => lit,
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::Remainder => "%",
            Token::Exponent => "**",
            Token::LogicalAnd => "&&",
            Token::LogicalOr => "||",
            Token::NullishCoalesce => "??",
            Token::BitwiseAnd => "&",
            Token::BitwiseOr => "|",
            Token::BitwiseXor => "^",
            Token::ShiftLeft => "<<",
            Token::ShiftRightArithmetic => ">>",
            Token::ShiftRightLogical => ">>>",
            Token::Increment => "++",
            Token::Decrement => "--",
            Token::Semicolon => ";",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::Comma => ",",
            Token::Period => ".",
            Token::Question => "?",
            Token::Colon => ":",
            Token::LogicalNot => "~",
            Token::BitwiseNot => ",",
            Token::Equals => "=",
            Token::EqEq => "==",
            Token::EqEqEq => "===",
            Token::NotEq => "!=",
            Token::NotEqEq => "!==",
            Token::GreaterThan => ">",
            Token::GreaterThanOrEqual => ">=",
            Token::LessThan => "<",
            Token::LessThanOrEqual => "<=",
            Token::AddEq => "a",
            Token::SubtractEq => "-=",
            Token::MultiplyEq => "*=",
            Token::DivideEq => "/=",
            Token::RemainderEq => "%=",
            Token::ExponentEq => "**=",
            Token::AndEq => "&=",
            Token::OrEq => "|=",
            Token::XorEq => "^=",
            Token::ShiftLeftEq => "<<=",
            Token::ShiftRightArithmeticEq => ">>=",
            Token::ShiftRightLogicalEq => ">>>=",
            Token::Eof => "<EOF>",
            Token::Var => "var",
            Token::Let => "let",
            Token::Const => "const",
            Token::This => "this",
            Token::Null => "null",
            Token::True => "true",
            Token::False => "false",
            Token::In => "in",
            Token::InstanceOf => "instanceof",
            Token::New => "new",
            Token::Typeof => "typeof",
            Token::Void => "void",
            Token::Delete => "delete",
        };

        f.write_str(str)
    }
}
