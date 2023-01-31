use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use super::{
    loc::{find_line_col_for_pos, Loc},
    scope::NameKind,
    source::Source,
    token::Token,
};

#[derive(Debug)]
pub enum ParseError {
    Io(io::Error),
    UnknownToken(String),
    UnexpectedToken(Token),
    ExpectedToken(Token, Token),
    InvalidUnicode,
    UnterminatedStringLiteral,
    MalformedEscapeSeqence,
    MalformedNumericLiteral,
    BigIntLeadingZero,
    TrailingNumericSeparator,
    AdjacentNumericSeparators,
    RestTrailingComma,
    ThrowArgumentOnNewLine,
    ArrowOnNewLine,
    AmbiguousLetBracket,
    InvalidAssignmentLeftHandSide,
    InvalidForLeftHandSide,
    InvalidUpdateExpressionArgument,
    IdentifierIsReservedWord,
    ExpectedNewTarget,
    ForEachInitInvalidVarDecl,
    NameRedeclaration(String, NameKind),
    DuplicateLabel,
    LabelNotFound,
    WithInStrictMode,
    DeleteIdentifierInStrictMode,
    AssignEvalInStrictMode,
    AssignArgumentsInStrictMode,
    UseStrictFunctionNonSimpleParameterList,
    InvalidDuplicateParameters(InvalidDuplicateParametersReason),
    InvalidLabeledFunction(bool),
    ReturnOutsideFunction,
    ContinueOutsideIterable,
    UnlabeledBreakOutsideBreakable,
    MultipleConstructors,
    NonSimpleConstructor,
    ClassStaticPrototype,
    InvalidPatternInitializer,
    DuplicatePrivateName(String),
    PrivateNameOutsideClass,
    PrivateNameNotDefined(String),
    PrivateNameConstructor,
    NewTargetOutsideFunction,
    SuperPropertyOutsideMethod,
    SuperCallOutsideDerivedConstructor,
    DuplicateProtoProperty,
    ConstWithoutInitializer,
    LetNameInLexicalDeclaration,
    GetterWrongNumberOfParams,
    SetterWrongNumberOfParams,
}

#[derive(Debug)]
pub enum InvalidDuplicateParametersReason {
    StrictMode,
    ArrowFunction,
    Method,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Io(io_error) => {
                f.write_str("Error: ")?;
                return io_error.fmt(f);
            }
            ParseError::UnknownToken(token) => write!(f, "Unknown token {}", token),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token {}", token),
            ParseError::ExpectedToken(actual, expected) => {
                write!(f, "Unexpected token {}, expected {}", actual, expected)
            }
            ParseError::InvalidUnicode => write!(f, "Invalid utf-8 sequence"),
            ParseError::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            ParseError::MalformedEscapeSeqence => write!(f, "Malformed escape sequence"),
            ParseError::MalformedNumericLiteral => write!(f, "Malformed numeric literal"),
            ParseError::BigIntLeadingZero => write!(f, "BigInt cannot have a leading zero"),
            ParseError::TrailingNumericSeparator => write!(
                f,
                "Underscore can appear only between digits, not after the last digit in a number"
            ),
            ParseError::AdjacentNumericSeparators => {
                write!(f, "Number cannot contain multiple adjacent underscores")
            }
            ParseError::RestTrailingComma => {
                write!(f, "Rest element may not have a trailing comma")
            }
            ParseError::ThrowArgumentOnNewLine => {
                write!(f, "No line break is allowed between 'throw' and its expression")
            }
            ParseError::ArrowOnNewLine => {
                write!(f, "No line break is allowed between arrow arguments and '=>'")
            }
            ParseError::AmbiguousLetBracket => {
                write!(f, "Expression cannot start with ambiguous `let [`")
            }
            ParseError::InvalidAssignmentLeftHandSide => {
                write!(f, "Invalid left hand side of assignment")
            }
            ParseError::InvalidForLeftHandSide => {
                write!(f, "Invalid left hand side of for statement")
            }
            ParseError::InvalidUpdateExpressionArgument => {
                write!(f, "Invalid increment/decrement operand")
            }
            ParseError::IdentifierIsReservedWord => {
                write!(f, "Identifier is a reserved word")
            }
            ParseError::ExpectedNewTarget => {
                write!(f, "Expected new.target")
            }
            ParseError::ForEachInitInvalidVarDecl => {
                write!(f, "Variable declarations in the left hand side of a for each loop must contain a single declaration with no initializer")
            }
            ParseError::NameRedeclaration(name, kind) => {
                let kind_string = match kind {
                    NameKind::Var => "var",
                    NameKind::Const => "const",
                    NameKind::Let => "let",
                    NameKind::Function => "function",
                    NameKind::FunctionParameter => "function parameter",
                    NameKind::Class => "class",
                    NameKind::CatchParameter => "catch parameter",
                };
                write!(f, "Redeclaration of {} {}", kind_string, name)
            }
            ParseError::DuplicateLabel => write!(f, "Duplicate label"),
            ParseError::LabelNotFound => write!(f, "Label not found"),
            ParseError::WithInStrictMode => {
                write!(f, "Strict mode code may not contain 'with' statements")
            }
            ParseError::DeleteIdentifierInStrictMode => {
                write!(f, "Cannot delete variables in strict mode code")
            }
            ParseError::AssignEvalInStrictMode => {
                write!(f, "Cannot assign to 'eval' in strict mode code")
            }
            ParseError::AssignArgumentsInStrictMode => {
                write!(f, "Cannot assign to 'arguments' in strict mode code")
            }
            ParseError::UseStrictFunctionNonSimpleParameterList => {
                write!(f, "'use strict' only allowed in functions with simple parameter lists")
            }
            ParseError::InvalidDuplicateParameters(reason) => {
                let reason_string = match reason {
                    InvalidDuplicateParametersReason::StrictMode => "strict mode functions",
                    InvalidDuplicateParametersReason::ArrowFunction => "arrow functions",
                    InvalidDuplicateParametersReason::Method => "methods",
                };
                write!(f, "Duplicate parameters not allowed in {}", reason_string)
            }
            ParseError::InvalidLabeledFunction(true) => write!(f, "Functions cannot be labeled"),
            ParseError::InvalidLabeledFunction(false) => {
                write!(f, "Functions can only be labeled inside blocks")
            }
            ParseError::ReturnOutsideFunction => write!(f, "Return must be inside function"),
            ParseError::ContinueOutsideIterable => write!(f, "Continue must be inside loop"),
            ParseError::UnlabeledBreakOutsideBreakable => {
                write!(f, "Unlabeled break must be inside loop or switch")
            }
            ParseError::MultipleConstructors => {
                write!(f, "Class can only have a single constructor")
            }
            ParseError::NonSimpleConstructor => {
                write!(f, "Constructors must be simple methods")
            }
            ParseError::ClassStaticPrototype => {
                write!(f, "Classes cannot have a static prototype field or method")
            }
            ParseError::InvalidPatternInitializer => {
                write!(f, "Object property initializers do not use `=`")
            }
            ParseError::DuplicatePrivateName(name) => {
                write!(f, "Redeclaration of private name #{}", name)
            }
            ParseError::PrivateNameOutsideClass => {
                write!(f, "Private name outside class")
            }
            ParseError::PrivateNameNotDefined(name) => {
                write!(f, "Reference to undeclared private name #{}", name)
            }
            ParseError::PrivateNameConstructor => {
                write!(f, "Private name not allowed to be #constructor")
            }
            ParseError::NewTargetOutsideFunction => {
                write!(f, "new.target only allowed in functions")
            }
            ParseError::SuperPropertyOutsideMethod => {
                write!(f, "Super property accesses only allowed in methods")
            }
            ParseError::SuperCallOutsideDerivedConstructor => {
                write!(f, "Super calls only allowed in derived constructors")
            }
            ParseError::DuplicateProtoProperty => {
                write!(f, "Duplicate __proto__ properties are not allowed in object literals")
            }
            ParseError::ConstWithoutInitializer => {
                write!(f, "Const declarations must have an initializer")
            }
            ParseError::LetNameInLexicalDeclaration => {
                write!(f, "Lexical declarations can't define a 'let' binding")
            }
            ParseError::GetterWrongNumberOfParams => {
                write!(f, "Getter functions must have no parameters")
            }
            ParseError::SetterWrongNumberOfParams => {
                write!(f, "Setter functions must exactly one parameter")
            }
        }
    }
}

pub struct LocalizedParseError {
    pub error: ParseError,
    pub source_loc: Option<(Loc, Rc<Source>)>,
}

impl LocalizedParseError {
    fn new_without_loc(error: ParseError) -> LocalizedParseError {
        LocalizedParseError { error, source_loc: None }
    }
}

impl Error for LocalizedParseError {}

impl fmt::Display for LocalizedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.source_loc {
            None => write!(f, "SyntaxError: {}", self.error),
            Some((loc, source)) => {
                let offsets = source.line_offsets();
                let (line, col) = find_line_col_for_pos(loc.start, offsets);
                write!(f, "SyntaxError: {}:{}:{} {}", source.file_path, line, col, self.error)
            }
        }
    }
}

impl fmt::Debug for LocalizedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <LocalizedParseError as fmt::Display>::fmt(self, f)
    }
}

impl From<io::Error> for LocalizedParseError {
    fn from(error: io::Error) -> LocalizedParseError {
        LocalizedParseError::new_without_loc(ParseError::Io(error))
    }
}

pub struct LocalizedParseErrors {
    pub errors: Vec<LocalizedParseError>,
}

impl LocalizedParseErrors {
    pub fn new(errors: Vec<LocalizedParseError>) -> Self {
        LocalizedParseErrors { errors }
    }
}

impl Error for LocalizedParseErrors {}

impl fmt::Display for LocalizedParseErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_localized_parse_errors(&self.errors))
    }
}

impl fmt::Debug for LocalizedParseErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <LocalizedParseErrors as fmt::Display>::fmt(self, f)
    }
}

pub type ParseResult<T> = Result<T, LocalizedParseError>;

pub fn format_localized_parse_errors(errors: &[LocalizedParseError]) -> String {
    // Separate errors into those with and without locs
    let mut errors_without_loc = vec![];
    let mut errors_with_loc = vec![];
    for error in errors {
        match &error.source_loc {
            None => {
                errors_without_loc.push(error);
            }
            Some((loc, source)) => {
                let offsets = source.line_offsets();
                let (line, col) = find_line_col_for_pos(loc.start, offsets);
                errors_with_loc.push((error, source, line, col))
            }
        }
    }

    // Sort errors with locs
    errors_with_loc.sort_by(|a, b| {
        a.1.file_path
            .cmp(&b.1.file_path)
            .then_with(|| a.2.cmp(&b.2))
            .then_with(|| a.3.cmp(&b.3))
    });

    let mut error_messages = vec![];
    for error in errors_without_loc {
        error_messages.push(format!("{}", error))
    }

    for (error, _, _, _) in errors_with_loc {
        error_messages.push(format!("{}", error))
    }

    error_messages.join("\n\n")
}
