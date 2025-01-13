use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use crate::js::common::wtf_8::Wtf8String;
use crate::js::parser::scope_tree::ANONYMOUS_DEFAULT_EXPORT_NAME;

use super::scope_tree::BindingKind;
use super::{
    loc::{find_line_col_for_pos, Loc},
    source::Source,
    token::Token,
};

#[derive(Debug)]
pub enum ParseError {
    Io(io::Error),
    #[allow(clippy::box_collection)]
    UnknownToken(Box<String>),
    UnexpectedToken(Box<Token>),
    ExpectedToken(Box<(Token, Token)>),
    SourceTooLarge(bool),
    InvalidUnicode,
    UnterminatedStringLiteral,
    UnterminatedRegExpLiteral,
    MalformedEscapeSeqence,
    MalformedNumericLiteral,
    BigIntLeadingZero,
    InvalidNumericLiteralNextChar,
    TrailingNumericSeparator,
    AdjacentNumericSeparators,
    RestTrailingComma,
    ThrowArgumentOnNewLine,
    ArrowOnNewLine,
    AmbiguousLetBracket,
    InvalidAssignmentLeftHandSide,
    InvalidForLeftHandSide,
    InvalidUpdateExpressionArgument,
    InvalidForAwait,
    IdentifierIsReservedWord,
    ExpectedNewTarget,
    ExpectedImportMeta,
    ImportMetaOutsideModule,
    ExponentLHSUnary,
    TaggedTemplateInChain,
    NullishCoalesceMixedWithLogical,
    HashNotFollowedByIdentifier,
    ForEachInitInvalidVarDecl,
    NameRedeclaration(Box<(String, BindingKind)>),
    DuplicateLabel,
    LabelNotFound,
    WithInStrictMode,
    DeleteIdentifierInStrictMode,
    DeletePrivateProperty,
    LegacyOctalLiteralInStrictMode,
    LegacyOctalEscapeSequenceInStrictMode,
    LegacyNonOctalEscapeSequenceInStrictMode,
    AssignEvalInStrictMode,
    AssignArgumentsInStrictMode,
    UseStrictFunctionNonSimpleParameterList,
    InvalidDuplicateParameters(InvalidDuplicateParametersReason),
    InvalidLabeledFunction(bool),
    AsyncOrGeneratorLabeledFunction(bool),
    ReturnOutsideFunction,
    ContinueOutsideIterable,
    UnlabeledBreakOutsideBreakable,
    SwitchMultipleDefaults,
    MultipleConstructors,
    NonSimpleConstructor,
    ClassStaticPrototype,
    InvalidPatternInitializer,
    #[allow(clippy::box_collection)]
    DuplicatePrivateName(Box<String>),
    PrivateNameOutsideClass,
    #[allow(clippy::box_collection)]
    PrivateNameNotDefined(Box<String>),
    PrivateNameConstructor,
    ArgumentsInClassInitializer,
    NewTargetOutsideFunction,
    SuperPropertyOutsideMethod,
    SuperCallOutsideDerivedConstructor,
    DuplicateProtoProperty,
    ConstWithoutInitializer,
    LetNameInLexicalDeclaration,
    GetterWrongNumberOfParams,
    SetterWrongNumberOfParams,
    AwaitInStaticInitializer,
    AwaitInParameters,
    YieldInParameters,
    ExportNameNotWellFormed,
    DirectExportNameIsString,
    UnresolvedExport,
    DuplicateExport(Box<Wtf8String>),
    ImportAttributeInvalidKey,
    ImportAttributeInvalidValue,
    DuplicateImportAttribute,
    // RegExp parsing errors
    UnexpectedRegExpToken,
    InvalidRegExpFlag,
    DuplicateRegExpFlag,
    MultipleUnicodeFlags,
    UnexpectedRegExpQuantifier,
    UnexpectedRegExpEnd,
    InvalidCharacterClassRange,
    RegExpCharacterClassInRange,
    TooManyCaptureGroups,
    DuplicateCaptureGroupName,
    InvalidBackreferenceIndex,
    InvalidBackreferenceName,
    InvalidQuantifierBounds,
    NonQuantifiableAssertion,
    InvalidUnicodeProperty,
    DuplicateRegExpModifier,
    EmptyRegExpModifiers,
    // RegExp parsing marker error to signal a named capture group was encountered
    NamedCaptureGroupEncountered,
}

#[derive(Debug)]
pub enum InvalidDuplicateParametersReason {
    StrictMode,
    ArrowFunction,
    Method,
    NonSimpleParameters,
}

impl ParseError {
    pub fn new_unknown_token(token: String) -> ParseError {
        ParseError::UnknownToken(Box::new(token))
    }

    pub fn new_unexpected_token(token: Token) -> ParseError {
        ParseError::UnexpectedToken(Box::new(token))
    }

    pub fn new_expected_token(actual: Token, expected: Token) -> ParseError {
        ParseError::ExpectedToken(Box::new((actual, expected)))
    }

    pub fn new_name_redeclaration(name: String, kind: BindingKind) -> ParseError {
        ParseError::NameRedeclaration(Box::new((name, kind)))
    }

    pub fn new_duplicate_private_name(name: String) -> ParseError {
        ParseError::DuplicatePrivateName(Box::new(name))
    }

    pub fn new_private_name_not_defined(name: String) -> ParseError {
        ParseError::PrivateNameNotDefined(Box::new(name))
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Io(io_error) => {
                f.write_str("Error: ")?;
                io_error.fmt(f)
            }
            ParseError::UnknownToken(token) => write!(f, "Unknown token {}", token),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token {}", token),
            ParseError::ExpectedToken(payload) => {
                let (actual, expected) = payload.as_ref();
                write!(f, "Unexpected token {}, expected {}", actual, expected)
            }
            ParseError::SourceTooLarge(is_file) => {
                let source = if *is_file { "File" } else { "String" };
                write!(f, "{} is too large, max size is 2^32 bytes", source)
            }
            ParseError::InvalidUnicode => write!(f, "Invalid utf-8 sequence"),
            ParseError::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            ParseError::UnterminatedRegExpLiteral => {
                write!(f, "Unterminated regular expression literal")
            }
            ParseError::MalformedEscapeSeqence => write!(f, "Malformed escape sequence"),
            ParseError::MalformedNumericLiteral => write!(f, "Malformed numeric literal"),
            ParseError::BigIntLeadingZero => write!(f, "BigInt cannot have a leading zero"),
            ParseError::InvalidNumericLiteralNextChar => {
                write!(f, "This character cannot appear immediately after a numeric literal")
            }
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
            ParseError::InvalidForAwait => {
                write!(f, "Await can only appear on for-of loops")
            }
            ParseError::IdentifierIsReservedWord => {
                write!(f, "Identifier is a reserved word")
            }
            ParseError::ExpectedNewTarget => {
                write!(f, "Expected new.target")
            }
            ParseError::ExpectedImportMeta => {
                write!(f, "Expected import.meta")
            }
            ParseError::ImportMetaOutsideModule => {
                write!(f, "import.meta is only allowed in modules")
            }
            ParseError::ExponentLHSUnary => {
                write!(
                    f,
                    "Unparenthesized unary expression can't appear on the left hand side of '**'"
                )
            }
            ParseError::TaggedTemplateInChain => {
                write!(f, "Tagged template cannot be used in optional chain")
            }
            ParseError::NullishCoalesceMixedWithLogical => {
                write!(f, "Parentheses are required when mixing '??' with '&&' or '||' expressions")
            }
            ParseError::HashNotFollowedByIdentifier => {
                write!(f, "Expected '#' to be immediately followed by an identifier")
            }
            ParseError::ForEachInitInvalidVarDecl => {
                write!(f, "Variable declarations in the left hand side of a for each loop must contain a single declaration with no initializer")
            }
            ParseError::NameRedeclaration(payload) => {
                let (name, kind) = payload.as_ref();
                if name == ANONYMOUS_DEFAULT_EXPORT_NAME {
                    write!(f, "Default export was already declared in this module")
                } else {
                    let kind_string = match kind {
                        BindingKind::Var => "var",
                        BindingKind::Const { .. } => "const",
                        BindingKind::Let { .. } => "let",
                        BindingKind::Function { .. } => "function",
                        BindingKind::FunctionParameter { .. } => "function parameter",
                        BindingKind::Class { .. } => "class",
                        BindingKind::CatchParameter { .. } => "catch parameter",
                        BindingKind::Import { .. } => "import",
                        BindingKind::ImplicitThis { .. } => "`this`",
                        BindingKind::ImplicitArguments => "`arguments`",
                        BindingKind::ImplicitNewTarget => "`new.target`",
                        BindingKind::DerivedConstructor => "constructor",
                        BindingKind::HomeObject => "home object",
                        BindingKind::PrivateName => "private name",
                        BindingKind::DefaultExportExpression => "default export",
                    };
                    write!(f, "Redeclaration of {} {}", kind_string, name)
                }
            }
            ParseError::DuplicateLabel => write!(f, "Duplicate label"),
            ParseError::LabelNotFound => write!(f, "Label not found"),
            ParseError::WithInStrictMode => {
                write!(f, "Strict mode code may not contain 'with' statements")
            }
            ParseError::DeleteIdentifierInStrictMode => {
                write!(f, "Cannot delete variables in strict mode")
            }
            ParseError::DeletePrivateProperty => {
                write!(f, "Cannot delete private properties")
            }
            ParseError::LegacyOctalLiteralInStrictMode => {
                write!(f, "Cannot use '0'-prefixed octal literals in strict mode")
            }
            ParseError::LegacyOctalEscapeSequenceInStrictMode => {
                write!(f, "Octal escape sequences are not allowed in strict mode")
            }
            ParseError::LegacyNonOctalEscapeSequenceInStrictMode => {
                write!(f, "\\8 and \\9 escape sequences are not allowed in strict mode")
            }
            ParseError::AssignEvalInStrictMode => {
                write!(f, "Cannot assign to 'eval' in strict mode")
            }
            ParseError::AssignArgumentsInStrictMode => {
                write!(f, "Cannot assign to 'arguments' in strict mode")
            }
            ParseError::UseStrictFunctionNonSimpleParameterList => {
                write!(f, "'use strict' only allowed in functions with simple parameter lists")
            }
            ParseError::InvalidDuplicateParameters(reason) => {
                let reason_string = match reason {
                    InvalidDuplicateParametersReason::StrictMode => "strict mode functions",
                    InvalidDuplicateParametersReason::ArrowFunction => "arrow functions",
                    InvalidDuplicateParametersReason::Method => "methods",
                    InvalidDuplicateParametersReason::NonSimpleParameters => {
                        "functions with non-simple parameter lists"
                    }
                };
                write!(f, "Duplicate parameters not allowed in {}", reason_string)
            }
            ParseError::InvalidLabeledFunction(true) => write!(f, "Functions cannot be labeled"),
            ParseError::InvalidLabeledFunction(false) => {
                write!(f, "Functions can only be labeled inside blocks")
            }
            ParseError::AsyncOrGeneratorLabeledFunction(true) => {
                write!(f, "Async functions cannot be labeled")
            }
            ParseError::AsyncOrGeneratorLabeledFunction(false) => {
                write!(f, "Generator functions cannot be labeled")
            }
            ParseError::ReturnOutsideFunction => write!(f, "Return must be inside function"),
            ParseError::ContinueOutsideIterable => write!(f, "Continue must be inside loop"),
            ParseError::UnlabeledBreakOutsideBreakable => {
                write!(f, "Unlabeled break must be inside loop or switch")
            }
            ParseError::SwitchMultipleDefaults => {
                write!(f, "More than one default case in switch")
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
            ParseError::ArgumentsInClassInitializer => {
                write!(f, "'arguments' is not allowed in class field initializer or static initialization block")
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
            ParseError::AwaitInStaticInitializer => {
                write!(f, "Await expression not allowed in static initializer")
            }
            ParseError::AwaitInParameters => {
                write!(f, "Await expression not allowed in function parameters")
            }
            ParseError::YieldInParameters => {
                write!(f, "Yield expression not allowed in function parameters")
            }
            ParseError::ExportNameNotWellFormed => {
                write!(f, "Exported name is not well formed")
            }
            ParseError::DirectExportNameIsString => {
                write!(f, "String export names are only allowed when there is a `from` clause")
            }
            ParseError::UnresolvedExport => {
                write!(f, "Exported name is not defined in module")
            }
            ParseError::DuplicateExport(name) => {
                write!(f, "Export with name \"{}\" already exists in module", name)
            }
            ParseError::ImportAttributeInvalidKey => {
                write!(f, "Import attribute key must be an identifier or string")
            }
            ParseError::ImportAttributeInvalidValue => {
                write!(f, "Import attribute value must be a string")
            }
            ParseError::DuplicateImportAttribute => {
                write!(f, "Import attribute with that key already exists")
            }
            ParseError::UnexpectedRegExpToken => {
                write!(f, "Unexpected token")
            }
            ParseError::InvalidRegExpFlag => {
                write!(f, "Invalid regular expression flag")
            }
            ParseError::DuplicateRegExpFlag => {
                write!(f, "Duplicate regular expression flag")
            }
            ParseError::MultipleUnicodeFlags => {
                write!(f, "RegExp cannot have both `u` and `v` flags")
            }
            ParseError::UnexpectedRegExpQuantifier => {
                write!(f, "Unexpected regular expression quantifier")
            }
            ParseError::UnexpectedRegExpEnd => {
                write!(f, "Unexpected end of regular expression")
            }
            ParseError::RegExpCharacterClassInRange => {
                write!(f, "Character class cannot be a bound in a character range")
            }
            ParseError::InvalidCharacterClassRange => {
                write!(f, "Invalid character class range in regular expression")
            }
            ParseError::TooManyCaptureGroups => {
                write!(f, "Too many capture groups in regular expression")
            }
            ParseError::DuplicateCaptureGroupName => {
                write!(f, "Duplicate capture group name in regular expression")
            }
            ParseError::InvalidBackreferenceIndex => {
                write!(f, "No capture group with index found")
            }
            ParseError::InvalidBackreferenceName => {
                write!(f, "No capture group with name found")
            }
            ParseError::InvalidQuantifierBounds => {
                write!(f, "Invalid quantifier bounds in regular expression")
            }
            ParseError::NonQuantifiableAssertion => {
                write!(f, "Quantifier on non-quantifiable assertion in regular expression")
            }
            ParseError::InvalidUnicodeProperty => {
                write!(f, "Invalid unicode property in regular expression")
            }
            ParseError::DuplicateRegExpModifier => {
                write!(f, "Duplicate regular expression modifier")
            }
            ParseError::EmptyRegExpModifiers => {
                write!(f, "No regular expression modifiers found")
            }
            ParseError::NamedCaptureGroupEncountered => {
                panic!("Marker error should have been caught")
            }
        }
    }
}

pub struct LocalizedParseError {
    pub error: ParseError,
    pub source_loc: Option<(Loc, Rc<Source>)>,
}

impl LocalizedParseError {
    pub fn new_without_loc(error: ParseError) -> LocalizedParseError {
        LocalizedParseError { error, source_loc: None }
    }

    /// Format as a string to display to the user without the "SyntaxError:" prefix
    pub fn to_string_without_name(&self) -> String {
        match &self.source_loc {
            None => self.error.to_string(),
            Some((loc, source)) => {
                let offsets = source.line_offsets();
                let (line, col) = find_line_col_for_pos(loc.start, offsets);
                format!("{}:{}:{} {}", source.display_name(), line, col, self.error)
            }
        }
    }
}

impl Error for LocalizedParseError {}

impl fmt::Display for LocalizedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SyntaxError: {}", self.to_string_without_name())
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
        a.1.file_path()
            .cmp(b.1.file_path())
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
