use std::fmt;

use num_bigint::BigInt;

use super::loc::Loc;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumberLiteral(f64),
    StringLiteral(String),
    BigIntLiteral(BigInt),
    RegExpLiteral {
        raw: String,
        pattern: String,
        flags: String,
    },
    TemplatePart {
        raw: String,
        // Either a successful string, or a malformed escape sequence error at the given loc
        cooked: Result<String, Loc>,
        is_head: bool,
        is_tail: bool,
    },
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
    LogicalAndEq,
    LogicalOrEq,
    NullishCoalesceEq,
    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,
    Comma,
    Period,
    Question,
    QuestionDot,
    Colon,
    Arrow,
    Spread,
    Hash,
    // Keywords
    Var,
    Let,
    Const,
    Function,
    Async,
    This,
    If,
    Else,
    Switch,
    Case,
    Default,
    For,
    Of,
    While,
    Do,
    With,
    Return,
    Break,
    Continue,
    Try,
    Catch,
    Finally,
    Throw,
    Null,
    True,
    False,
    In,
    InstanceOf,
    New,
    Typeof,
    Void,
    Delete,
    Debugger,
    Static,
    From,
    As,
    Class,
    Extends,
    Super,
    Get,
    Set,
    Import,
    Export,
    Await,
    Yield,
    Target,
    Meta,
    Enum,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Token::Identifier(name) => name,
            Token::NumberLiteral(lit) => return f.write_str(&lit.to_string()),
            Token::StringLiteral(lit) => lit,
            Token::BigIntLiteral(lit) => return write!(f, "{}n", lit.to_string()),
            Token::RegExpLiteral { raw, .. } => return write!(f, "{}", raw),
            Token::TemplatePart { raw, is_head: true, is_tail: true, .. } => {
                return write!(f, "`{}`", raw)
            }
            Token::TemplatePart { raw, is_head: true, is_tail: false, .. } => {
                return write!(f, "`{}${{`", raw)
            }
            Token::TemplatePart { raw, is_head: false, is_tail: true, .. } => {
                return write!(f, "}}{}`", raw)
            }
            Token::TemplatePart { raw, is_head: false, is_tail: false, .. } => {
                return write!(f, "}}{}${{`", raw)
            }
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
            Token::Arrow => "=>",
            Token::Spread => "...",
            Token::Hash => "#",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::Comma => ",",
            Token::Period => ".",
            Token::Question => "?",
            Token::QuestionDot => "?.",
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
            Token::LogicalAndEq => "&&=",
            Token::LogicalOrEq => "||=",
            Token::NullishCoalesceEq => "??=",
            Token::Eof => "<EOF>",
            Token::Var => "var",
            Token::Let => "let",
            Token::Const => "const",
            Token::Function => "function",
            Token::Async => "async",
            Token::This => "this",
            Token::If => "if",
            Token::Else => "else",
            Token::Switch => "switch",
            Token::Case => "case",
            Token::Default => "default",
            Token::For => "for",
            Token::Of => "of",
            Token::While => "while",
            Token::Do => "do",
            Token::With => "with",
            Token::Return => "return",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Try => "try",
            Token::Catch => "catch",
            Token::Finally => "finally",
            Token::Throw => "throw",
            Token::Null => "null",
            Token::True => "true",
            Token::False => "false",
            Token::In => "in",
            Token::InstanceOf => "instanceof",
            Token::New => "new",
            Token::Typeof => "typeof",
            Token::Void => "void",
            Token::Delete => "delete",
            Token::Debugger => "debugger",
            Token::Static => "static",
            Token::From => "from",
            Token::As => "as",
            Token::Class => "class",
            Token::Extends => "extends",
            Token::Super => "super",
            Token::Get => "get",
            Token::Set => "set",
            Token::Import => "import",
            Token::Export => "export",
            Token::Await => "await",
            Token::Yield => "yield",
            Token::Target => "target",
            Token::Meta => "meta",
            Token::Enum => "enum",
        };

        f.write_str(str)
    }
}
