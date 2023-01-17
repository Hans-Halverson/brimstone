use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use super::ast::*;
use super::lexer::{Lexer, SavedLexerState};
use super::loc::{find_line_col_for_pos, Loc, Pos, EMPTY_LOC};
use super::source::Source;
use super::token::Token;

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
    ThrowArgumentOnNewLine,
    AmbiguousLetBracket,
    InvalidForLeftHandSide,
    DuplicateLabel,
    LabelNotFound,
    WithInStrictMode,
    InvalidLabeledFunction(bool),
    ReturnOutsideFunction,
    ContinueOutsideIterable,
    UnlabeledBreakOutsideBreakable,
    MultipleConstructors,
    NonSimpleConstructor,
    ClassStaticPrototype,
    DuplicatePrivateName(String),
    PrivateNameOutsideClass,
    PrivateNameNotDefined(String),
    PrivateNameConstructor,
}

// Arbitrary error used to fail try parse
const FAIL_TRY_PARSED_ERROR: ParseError = ParseError::MalformedNumericLiteral;

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
            ParseError::InvalidUnicode => {
                write!(f, "Invalid utf-8 sequence")
            }
            ParseError::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            ParseError::MalformedEscapeSeqence => write!(f, "Malformed escape sequence"),
            ParseError::MalformedNumericLiteral => write!(f, "Malformed numeric literal"),
            ParseError::ThrowArgumentOnNewLine => {
                write!(f, "No line break is allowed between 'throw' and its expression")
            }
            ParseError::AmbiguousLetBracket => {
                write!(f, "Expression cannot start with ambiguous `let [`")
            }
            ParseError::InvalidForLeftHandSide => {
                write!(f, "Invalid left hand side of for statement")
            }
            ParseError::DuplicateLabel => write!(f, "Duplicate label"),
            ParseError::LabelNotFound => write!(f, "Label not found"),
            ParseError::WithInStrictMode => {
                write!(f, "Strict mode code may not contain 'with' statements")
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

/// Expression operator precedence. A lower number binds tighter than a larger number.
#[derive(Clone, Copy)]
enum Precedence {
    PostfixUpdate = 0,
    Unary = 1, // Includes prefix update
    Exponentiation = 2,
    Multiplication = 3,
    Addition = 4,
    Shift = 5,
    Relational = 6, // Includes in and instanceof
    Equality = 7,
    BitwiseAnd = 8,
    BitwiseXor = 9,
    BitwiseOr = 10,
    LogicalAnd = 11,
    LogicalOr = 12, // Includes nullish coalescing
    Conditional = 13,
}

impl Precedence {
    fn is_weaker_than(self, other: Precedence) -> bool {
        (self as i32) > (other as i32)
    }
}

#[derive(Clone, Copy, PartialEq)]
enum PropertyContext {
    Class,
    Object,
    Pattern,
}

struct PropertyNameResult {
    key: P<Expression>,
    is_computed: bool,
    is_shorthand: bool,
    is_private: bool,
}

fn p<T>(node: T) -> P<T> {
    Box::new(node)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
    // Whether the parser is currently parsing in strict mode
    in_strict_mode: bool,
}

/// A save point for the parser, can be used to restore the parser to a particular position.
struct ParserSaveState {
    saved_lexer_state: SavedLexerState,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
    in_strict_mode: bool,
}

impl<'a> Parser<'a> {
    // Must prime parser by calling advance before using.
    fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            token: Token::Eof,
            loc: EMPTY_LOC,
            prev_loc: EMPTY_LOC,
            in_strict_mode: false,
        }
    }

    pub fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        let source = (*self.lexer.source).clone();
        Err(LocalizedParseError { error, source_loc: Some((loc, source)) })
    }

    fn save(&self) -> ParserSaveState {
        ParserSaveState {
            saved_lexer_state: self.lexer.save(),
            token: self.token.clone(),
            loc: self.loc,
            prev_loc: self.prev_loc,
            in_strict_mode: self.in_strict_mode,
        }
    }

    fn restore(&mut self, save_state: ParserSaveState) {
        self.lexer.restore(&save_state.saved_lexer_state);
        self.token = save_state.token;
        self.loc = save_state.loc;
        self.prev_loc = save_state.prev_loc;
        self.in_strict_mode = save_state.in_strict_mode;
    }

    /// Try parsing, restoring to state before this function was called if an error occurs.
    fn try_parse<T>(&mut self, try_fn: fn(&mut Self) -> ParseResult<T>) -> ParseResult<T> {
        let save_state = self.save();
        let result = try_fn(self);
        if let Err(_) = result {
            self.restore(save_state);
        }
        result
    }

    fn advance(&mut self) -> ParseResult<()> {
        let (token, loc) = self.lexer.next()?;
        self.prev_loc = self.loc;
        self.token = token;
        self.loc = loc;

        Ok(())
    }

    fn expect(&mut self, token: Token) -> ParseResult<()> {
        if self.token != token {
            return self.error(self.loc, ParseError::ExpectedToken(self.token.clone(), token));
        }

        self.advance()?;
        Ok(())
    }

    fn error_unexpected_token<T>(&self, loc: Loc, token: &Token) -> ParseResult<T> {
        self.error(loc, ParseError::UnexpectedToken(token.clone()))
    }

    fn error_expected_token<T>(
        &self,
        loc: Loc,
        actual: &Token,
        expected: &Token,
    ) -> ParseResult<T> {
        self.error(loc, ParseError::ExpectedToken(actual.clone(), expected.clone()))
    }

    #[inline]
    fn current_start_pos(&self) -> Pos {
        self.loc.start
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc { start: start_pos, end: self.prev_loc.end }
    }

    // Expect a semicolon, or insert one via automatic semicolon insertion if possible. Error if
    // a semicolon was not present and one could not be inserted.
    fn expect_semicolon(&mut self) -> ParseResult<()> {
        match &self.token {
            Token::Semicolon => {
                self.advance()?;
                Ok(())
            }
            Token::RightBrace | Token::Eof => Ok(()),
            other => {
                if self.lexer.is_new_line_before_current() {
                    Ok(())
                } else {
                    self.error(self.loc, ParseError::ExpectedToken(other.clone(), Token::Semicolon))
                }
            }
        }
    }

    // Consume a semicolon if present and return true, otherwise return if a semicolon could be
    // inserted via automatic semicolon insertion.
    fn maybe_expect_semicolon(&mut self) -> ParseResult<bool> {
        match &self.token {
            Token::Semicolon => {
                self.advance()?;
                Ok(true)
            }
            Token::RightBrace | Token::Eof => Ok(true),
            _ => Ok(self.lexer.is_new_line_before_current()),
        }
    }

    fn parse_script(&mut self) -> ParseResult<Program> {
        let has_use_strict_directive = self.parse_use_strict_directive()?;
        if has_use_strict_directive {
            self.in_strict_mode = true;
        }

        let mut toplevels = vec![];
        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        Ok(Program::new(loc, toplevels, has_use_strict_directive))
    }

    fn parse_use_strict_directive(&mut self) -> ParseResult<bool> {
        // Try to parse "use strict" directive
        match &self.token {
            Token::StringLiteral(str) if str == "use strict" => {
                let saved_state = self.save();

                self.advance()?;

                // If "use strict" is followed by a semicolon we should use strict mode. Otherwise
                // this is the start of an expression so restore to beginning.
                if self.token == Token::Semicolon {
                    self.advance()?;
                    Ok(true)
                } else {
                    self.restore(saved_state);
                    Ok(false)
                }
            }
            _ => Ok(false),
        }
    }

    fn parse_toplevel(&mut self) -> ParseResult<Toplevel> {
        let stmt = self.parse_statement_list_item()?;
        Ok(Toplevel::Statement(stmt))
    }

    fn parse_statement_list_item(&mut self) -> ParseResult<Statement> {
        match self.token {
            Token::Let | Token::Const => {
                Ok(Statement::VarDecl(self.parse_variable_declaration(false)?))
            }
            Token::Class => Ok(Statement::ClassDecl(self.parse_class(true)?)),
            _ => {
                if self.is_function_start()? {
                    return Ok(Statement::FuncDecl(self.parse_function(true)?));
                }

                self.parse_statement()
            }
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.token {
            Token::Var => Ok(Statement::VarDecl(self.parse_variable_declaration(false)?)),
            Token::LeftBrace => Ok(Statement::Block(self.parse_block()?)),
            Token::If => self.parse_if_statement(),
            Token::Switch => self.parse_switch_statement(),
            Token::For => self.parse_any_for_statement(),
            Token::While => self.parse_while_statement(),
            Token::Do => self.parse_do_while_statement(),
            Token::With => self.parse_with_statement(),
            Token::Try => self.parse_try_statement(),
            Token::Throw => self.parse_throw_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Break => self.parse_break_statement(),
            Token::Continue => self.parse_continue_statement(),
            Token::Semicolon => {
                let loc = self.loc;
                self.advance()?;

                Ok(Statement::Empty(loc))
            }
            Token::Debugger => {
                let start_pos = self.current_start_pos();
                self.advance()?;
                self.expect_semicolon()?;

                Ok(Statement::Debugger(self.mark_loc(start_pos)))
            }
            _ => {
                if self.is_function_start()? || self.token == Token::Class {
                    return self.error_unexpected_token(self.loc, &self.token);
                } else if self.token == Token::Let {
                    // The form 'let [` is ambiguous as it could be the start of a let declaration
                    // with an array pattern, or a computed member access expression statement.
                    let let_loc = self.loc;
                    let save_state = self.save();
                    self.advance()?;
                    if self.token == Token::LeftBracket {
                        return self.error(let_loc, ParseError::AmbiguousLetBracket);
                    }

                    self.restore(save_state);
                }

                let start_pos = self.current_start_pos();
                let expr = self.parse_expression()?;

                // Parse labeled statement
                if self.token == Token::Colon {
                    if let Expression::Id(label) = *expr {
                        self.advance()?;

                        // Functions can be labeled items
                        let body = if self.is_function_start()? {
                            Statement::FuncDecl(self.parse_function(true)?)
                        } else {
                            self.parse_statement()?
                        };

                        let loc = self.mark_loc(start_pos);

                        return Ok(Statement::Labeled(LabeledStatement {
                            loc,
                            label: Label::new(p(label)),
                            body: p(body),
                        }));
                    }
                }

                // Otherwise must be an expression statement
                self.expect_semicolon()?;
                let loc = self.mark_loc(start_pos);

                Ok(Statement::Expr(ExpressionStatement { loc, expr }))
            }
        }
    }

    fn is_function_start(&mut self) -> ParseResult<bool> {
        match self.token {
            Token::Function => Ok(true),
            Token::Async => {
                let save_state = self.save();
                self.advance()?;
                let is_function = self.token == Token::Function;
                self.restore(save_state);

                Ok(is_function)
            }
            _ => Ok(false),
        }
    }

    fn parse_variable_declaration(
        &mut self,
        is_for_init: bool,
    ) -> ParseResult<VariableDeclaration> {
        let start_pos = self.current_start_pos();
        let kind = match &self.token {
            Token::Var => VarKind::Var,
            Token::Let => VarKind::Let,
            Token::Const => VarKind::Const,
            _ => unreachable!(),
        };
        self.advance()?;

        // Gather comma separated declarators
        let mut declarations = vec![];
        loop {
            let start_pos = self.current_start_pos();
            let id = self.parse_pattern()?;

            let init = match self.token {
                Token::Equals => {
                    self.advance()?;
                    Some(self.parse_assignment_expression()?)
                }
                _ => None,
            };

            let loc = self.mark_loc(start_pos);

            declarations.push(VariableDeclarator { loc, id: p(id), init });

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        if !is_for_init {
            self.expect_semicolon()?;
        }

        let loc = self.mark_loc(start_pos);

        Ok(VariableDeclaration { loc, kind, declarations })
    }

    fn parse_function(&mut self, is_decl: bool) -> ParseResult<Function> {
        let start_pos = self.current_start_pos();

        // Function can be prefixed by async keyword
        let is_async = self.token == Token::Async;
        if is_async {
            self.advance()?
        }

        self.expect(Token::Function)?;

        // Function keyword can be suffixed by generator `*`
        let is_generator = self.token == Token::Multiply;
        if is_generator {
            self.advance()?
        }

        // Id is optional only for function expresssions
        let id = if self.token != Token::LeftParen || is_decl {
            Some(p(self.parse_binding_identifier()?))
        } else {
            None
        };

        let params = self.parse_function_params()?;
        let (block, has_use_strict_directive, is_strict_mode) = self.parse_function_block_body()?;
        let body = p(FunctionBody::Block(block));
        let loc = self.mark_loc(start_pos);

        Ok(Function::new(
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
            is_strict_mode,
            has_use_strict_directive,
        ))
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<Pattern>> {
        // Read all function params between the parentheses
        let mut params = vec![];
        self.expect(Token::LeftParen)?;

        while self.token != Token::RightParen {
            params.push(self.parse_pattern_including_assignment_pattern()?);

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightParen)?;

        Ok(params)
    }

    fn parse_function_block_body(&mut self) -> ParseResult<(Block, bool, bool)> {
        let start_pos = self.current_start_pos();
        self.expect(Token::LeftBrace)?;

        let has_use_strict_directive = self.parse_use_strict_directive()?;

        // Enter strict mode if applicable, saving strict mode context from before this function
        let old_in_strict_mode = self.in_strict_mode;
        if has_use_strict_directive {
            self.in_strict_mode = true;
        }

        let mut body = vec![];
        while self.token != Token::RightBrace {
            body.push(self.parse_statement_list_item()?)
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        // Restore to strict mode context from before this function
        let is_strict_mode = self.in_strict_mode;
        self.in_strict_mode = old_in_strict_mode;

        Ok((Block::new(loc, body), has_use_strict_directive, is_strict_mode))
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut body = vec![];
        while self.token != Token::RightBrace {
            body.push(self.parse_statement_list_item()?)
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(Block::new(loc, body))
    }

    fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let conseq = p(self.parse_statement()?);

        let altern = if self.token == Token::Else {
            self.advance()?;
            Some(p(self.parse_statement()?))
        } else {
            None
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::If(IfStatement { loc, test, conseq, altern }))
    }

    fn parse_switch_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let discriminant = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let mut cases = vec![];
        self.expect(Token::LeftBrace)?;

        while self.token != Token::RightBrace {
            match self.token {
                Token::Case | Token::Default => {
                    let case_start_pos = self.current_start_pos();
                    let is_case = self.token == Token::Case;
                    self.advance()?;

                    let test = if is_case {
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect(Token::Colon)?;

                    // Parse statement list, which will be terminated by the start of another case
                    // or the end of the switch.
                    let mut body = vec![];
                    while self.token != Token::Case
                        && self.token != Token::Default
                        && self.token != Token::RightBrace
                    {
                        body.push(self.parse_statement_list_item()?)
                    }

                    let loc = self.mark_loc(case_start_pos);
                    cases.push(SwitchCase { loc, test, body })
                }
                _ => return self.error_expected_token(self.loc, &self.token, &Token::Catch),
            }
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Switch(SwitchStatement::new(loc, discriminant, cases)))
    }

    fn parse_any_for_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;

        // Init statement, if it exists
        match self.token {
            // Both for and for each loops can start with a variable declaration
            // TODO: Restrict variable declaration inits in for each statements
            Token::Var | Token::Let | Token::Const => {
                let var_decl = self.parse_variable_declaration(true)?;
                match self.token {
                    Token::In | Token::Of => {
                        let init = p(ForEachInit::VarDecl(var_decl));
                        self.parse_for_each_statement(init, start_pos)
                    }
                    _ => {
                        let init = Some(p(ForInit::VarDecl(var_decl)));
                        self.expect(Token::Semicolon)?;
                        self.parse_for_statement(init, start_pos)
                    }
                }
            }
            // Empty init, but we know this is a regular for loop
            Token::Semicolon => {
                self.advance()?;
                self.parse_for_statement(None, start_pos)
            }
            _ => {
                let expr_start_pos = self.current_start_pos();
                let expr = self.parse_expression()?;
                match (self.token.clone(), *expr) {
                    // If this is a for each loop the parsed expression must actually be a pattern
                    (Token::In, expr) | (Token::Of, expr) => {
                        let pattern =
                            self.reparse_expression_as_for_left_hand_side(expr, expr_start_pos)?;
                        let left = p(ForEachInit::Pattern(pattern));
                        self.parse_for_each_statement(left, start_pos)
                    }
                    // An in expression is actually `for (expr in right)`
                    (
                        _,
                        Expression::Binary(BinaryExpression {
                            operator: BinaryOperator::In,
                            left,
                            right,
                            ..
                        }),
                    ) => {
                        self.expect(Token::RightParen)?;
                        let pattern =
                            self.reparse_expression_as_for_left_hand_side(*left, expr_start_pos)?;
                        let left = p(ForEachInit::Pattern(pattern));
                        let body = p(self.parse_statement()?);
                        let loc = self.mark_loc(start_pos);

                        Ok(Statement::ForEach(ForEachStatement {
                            loc,
                            kind: ForEachKind::In,
                            left,
                            right,
                            body,
                            is_await: false,
                        }))
                    }
                    // Otherwise this is a regular for loop and the expression is used directly
                    (_, expr) => {
                        let init = Some(p(ForInit::Expression(expr)));
                        self.expect(Token::Semicolon)?;
                        self.parse_for_statement(init, start_pos)
                    }
                }
            }
        }
    }

    fn reparse_expression_as_for_left_hand_side(
        &self,
        expr: Expression,
        start_pos: Pos,
    ) -> ParseResult<Pattern> {
        match self.reparse_expression_as_pattern(expr) {
            Some(pattern) => Ok(pattern),
            None => {
                let loc = self.mark_loc(start_pos);
                self.error(loc, ParseError::InvalidForLeftHandSide)
            }
        }
    }

    fn parse_for_statement(
        &mut self,
        init: Option<P<ForInit>>,
        start_pos: Pos,
    ) -> ParseResult<Statement> {
        let test = match self.token {
            Token::Semicolon => None,
            _ => Some(self.parse_expression()?),
        };
        self.expect(Token::Semicolon)?;

        let update = match self.token {
            Token::RightParen => None,
            _ => Some(self.parse_expression()?),
        };

        self.expect(Token::RightParen)?;
        let body = p(self.parse_statement()?);
        let loc = self.mark_loc(start_pos);

        Ok(Statement::For(ForStatement { loc, init, test, update, body }))
    }

    fn parse_for_each_statement(
        &mut self,
        left: P<ForEachInit>,
        start_pos: Pos,
    ) -> ParseResult<Statement> {
        let kind = match self.token {
            Token::In => ForEachKind::In,
            Token::Of => ForEachKind::Of,
            _ => unreachable!(),
        };

        self.advance()?;

        let right = match kind {
            ForEachKind::In => self.parse_expression()?,
            ForEachKind::Of => self.parse_assignment_expression()?,
        };

        self.expect(Token::RightParen)?;
        let body = p(self.parse_statement()?);
        let loc = self.mark_loc(start_pos);

        Ok(Statement::ForEach(ForEachStatement {
            loc,
            kind,
            left,
            right,
            body,
            is_await: false,
        }))
    }

    fn parse_while_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let body = p(self.parse_statement()?);

        let loc = self.mark_loc(start_pos);

        Ok(Statement::While(WhileStatement { loc, test, body }))
    }

    fn parse_do_while_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let body = p(self.parse_statement()?);

        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        // A semicolon is always automatically inserted after a do while statement
        if self.token == Token::Semicolon {
            self.advance()?;
        }

        let loc = self.mark_loc(start_pos);

        Ok(Statement::DoWhile(DoWhileStatement { loc, test, body }))
    }

    fn parse_with_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let object = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let body = p(self.parse_statement()?);

        let loc = self.mark_loc(start_pos);

        Ok(Statement::With(WithStatement { loc, object, body }))
    }

    fn parse_try_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let block = p(self.parse_block()?);

        // Optional handler block
        let handler = if self.token == Token::Catch {
            let catch_start_pos = self.current_start_pos();
            self.advance()?;

            let param = if self.token == Token::LeftBrace {
                None
            } else {
                // Handler optionally has a single pattern as the parameter
                self.expect(Token::LeftParen)?;
                let param = self.parse_pattern()?;
                self.expect(Token::RightParen)?;
                Some(p(param))
            };

            let body = p(self.parse_block()?);
            let loc = self.mark_loc(catch_start_pos);

            Some(p(CatchClause { loc, param, body }))
        } else {
            None
        };

        let finalizer = if self.token == Token::Finally {
            self.advance()?;
            Some(p(self.parse_block()?))
        } else {
            None
        };

        // Must have at least one handler or finalizer
        if handler.is_none() && finalizer.is_none() {
            return self.error_expected_token(self.loc, &self.token, &Token::Catch);
        }

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Try(TryStatement { loc, block, handler, finalizer }))
    }

    fn parse_throw_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        if self.lexer.is_new_line_before_current() {
            return self.error(self.loc, ParseError::ThrowArgumentOnNewLine);
        }

        let argument = self.parse_expression()?;
        self.expect_semicolon()?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Throw(ThrowStatement { loc, argument }))
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = if self.maybe_expect_semicolon()? {
            None
        } else {
            let argument = self.parse_expression()?;
            self.expect_semicolon()?;
            Some(argument)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Return(ReturnStatement { loc, argument }))
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.maybe_expect_semicolon()? {
            None
        } else {
            let label = Label::new(p(self.parse_label_identifier()?));
            self.expect_semicolon()?;
            Some(label)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Break(BreakStatement { loc, label }))
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.maybe_expect_semicolon()? {
            None
        } else {
            let label = Label::new(p(self.parse_label_identifier()?));
            self.expect_semicolon()?;
            Some(label)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Continue(ContinueStatement { loc, label }))
    }

    /// 13.16 Expression
    fn parse_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_assignment_expression()?;

        if self.token == Token::Comma {
            let mut expressions = vec![*expr];
            while self.token == Token::Comma {
                self.advance()?;
                expressions.push(*self.parse_assignment_expression()?);
            }

            let loc = self.mark_loc(start_pos);

            Ok(p(Expression::Sequence(SequenceExpression { loc, expressions })))
        } else {
            Ok(expr)
        }
    }

    /// 13.15 AssignmentExpression
    fn parse_assignment_expression(&mut self) -> ParseResult<P<Expression>> {
        // First try parsing as a non-arrow assignment
        match self.try_parse(Parser::parse_non_arrow_assignment_expression) {
            Ok(expr) => Ok(expr),
            // Then try parsing as an arrow function if that doesn't succeed
            Err(err) => match self.try_parse(Parser::parse_arrow_function) {
                Ok(expr) => Ok(expr),
                // Error as if parsing a non-arrow assignment if neither match
                Err(_) => Err(err),
            },
        }
    }

    fn parse_non_arrow_assignment_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_conditional_expression()?;

        let assignment_op = match self.token {
            Token::Equals => Some(AssignmentOperator::Equals),
            Token::AddEq => Some(AssignmentOperator::Add),
            Token::SubtractEq => Some(AssignmentOperator::Subtract),
            Token::MultiplyEq => Some(AssignmentOperator::Multiply),
            Token::DivideEq => Some(AssignmentOperator::Divide),
            Token::RemainderEq => Some(AssignmentOperator::Remainder),
            Token::ExponentEq => Some(AssignmentOperator::Exponent),
            Token::AndEq => Some(AssignmentOperator::And),
            Token::OrEq => Some(AssignmentOperator::Or),
            Token::XorEq => Some(AssignmentOperator::Xor),
            Token::ShiftLeftEq => Some(AssignmentOperator::ShiftLeft),
            Token::ShiftRightArithmeticEq => Some(AssignmentOperator::ShiftRightArithmetic),
            Token::ShiftRightLogicalEq => Some(AssignmentOperator::ShiftRightLogical),
            _ => None,
        };

        let result = match assignment_op {
            None => Ok(expr),
            Some(operator) => {
                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::Assign(AssignmentExpression { loc, left: expr, right, operator })))
            }
        };

        // Force parsing as an async function if we see an arrow
        if self.token == Token::Arrow {
            return self.error(self.loc, FAIL_TRY_PARSED_ERROR);
        }

        result
    }

    fn parse_arrow_function(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();

        let is_async = self.token == Token::Async;
        if is_async {
            let async_loc = self.loc;
            self.advance()?;

            // Special case for when async is actually the single parameter: async => body
            if self.token == Token::Arrow {
                self.advance()?;
                let params = vec![Pattern::Id(Identifier {
                    loc: async_loc,
                    name: "async".to_owned(),
                })];
                let (body, has_use_strict_directive, is_strict_mode) =
                    self.parse_arrow_function_body()?;
                let loc = self.mark_loc(start_pos);

                return Ok(p(Expression::ArrowFunction(Function::new(
                    loc,
                    /* id */ None,
                    params,
                    body,
                    /* is_async */ false,
                    /* is_generator */ false,
                    is_strict_mode,
                    has_use_strict_directive,
                ))));
            }
        }

        // Arrow function params can be either parenthesized function params or a single id
        let params = match self.token {
            Token::LeftParen => self.parse_function_params()?,
            _ => {
                let id = self.parse_binding_identifier()?;
                vec![Pattern::Id(id)]
            }
        };

        self.expect(Token::Arrow)?;
        let (body, has_use_strict_directive, is_strict_mode) = self.parse_arrow_function_body()?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::ArrowFunction(Function::new(
            loc,
            /* id */ None,
            params,
            body,
            is_async,
            /* is_generator */ false,
            is_strict_mode,
            has_use_strict_directive,
        ))))
    }

    fn parse_arrow_function_body(&mut self) -> ParseResult<(P<FunctionBody>, bool, bool)> {
        if self.token == Token::LeftBrace {
            let (block, has_use_strict_directive, is_strict_mode) =
                self.parse_function_block_body()?;
            Ok((p(FunctionBody::Block(block)), has_use_strict_directive, is_strict_mode))
        } else {
            Ok((
                p(FunctionBody::Expression(*self.parse_assignment_expression()?)),
                false,
                self.in_strict_mode,
            ))
        }
    }

    /// 13.14 ConditionalExpression
    fn parse_conditional_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_expression_with_precedence(Precedence::Conditional)?;

        if self.token == Token::Question {
            self.advance()?;
            let conseq = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let altern = self.parse_assignment_expression()?;
            let loc = self.mark_loc(start_pos);

            Ok(p(Expression::Conditional(ConditionalExpression {
                loc,
                test: expr,
                conseq,
                altern,
            })))
        } else {
            Ok(expr)
        }
    }

    /// Precedence parsing for all binary operations and below.
    /// Corresponds to 13.13 ShortCircuitExpression
    fn parse_expression_with_precedence(
        &mut self,
        precedence: Precedence,
    ) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();

        // Private names must be the start of an `in` expression
        if self.token == Token::Hash {
            let private_name = p(Expression::Id(self.parse_private_name()?));

            if self.token == Token::In && precedence.is_weaker_than(Precedence::Relational) {
                return self.parse_binary_expression(
                    private_name,
                    start_pos,
                    BinaryOperator::InPrivate,
                    Precedence::Relational,
                );
            }

            return self.error_unexpected_token(self.loc, &self.token);
        }

        let mut current_expr = self.parse_expression_prefix()?;
        loop {
            let current_expr_ref = current_expr.as_ref() as *const Expression;
            let next_expr = self.parse_expression_infix(current_expr, precedence, start_pos)?;
            if std::ptr::eq(current_expr_ref, next_expr.as_ref()) {
                return Ok(next_expr);
            }

            current_expr = next_expr;
        }
    }

    fn parse_expression_prefix(&mut self) -> ParseResult<P<Expression>> {
        match &self.token {
            Token::Plus => self.parse_unary_expression(UnaryOperator::Plus),
            Token::Minus => self.parse_unary_expression(UnaryOperator::Minus),
            Token::LogicalNot => self.parse_unary_expression(UnaryOperator::LogicalNot),
            Token::BitwiseNot => self.parse_unary_expression(UnaryOperator::BitwiseNot),
            Token::Typeof => self.parse_unary_expression(UnaryOperator::TypeOf),
            Token::Void => self.parse_unary_expression(UnaryOperator::Void),
            Token::Delete => self.parse_unary_expression(UnaryOperator::Delete),
            Token::Increment => self.parse_update_expression_prefix(UpdateOperator::Increment),
            Token::Decrement => self.parse_update_expression_prefix(UpdateOperator::Decrement),
            _ => self.parse_left_hand_side_expression(),
        }
    }

    fn parse_expression_infix(
        &mut self,
        left: P<Expression>,
        precedence: Precedence,
        start_pos: Pos,
    ) -> ParseResult<P<Expression>> {
        match &self.token {
            // Binary operations
            Token::Plus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Add,
                    Precedence::Addition,
                ),
            Token::Minus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Subtract,
                    Precedence::Addition,
                ),
            Token::Multiply if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Multiply,
                    Precedence::Multiplication,
                ),
            Token::Divide if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Divide,
                    Precedence::Multiplication,
                ),
            Token::Remainder if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Remainder,
                    Precedence::Multiplication,
                ),
            Token::Exponent if precedence.is_weaker_than(Precedence::Exponentiation) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Exponent,
                    // Right associative, so lower precedence
                    Precedence::Multiplication,
                ),
            Token::BitwiseAnd if precedence.is_weaker_than(Precedence::BitwiseAnd) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::And,
                    Precedence::BitwiseAnd,
                ),
            Token::BitwiseOr if precedence.is_weaker_than(Precedence::BitwiseOr) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Or,
                    Precedence::BitwiseOr,
                ),
            Token::BitwiseXor if precedence.is_weaker_than(Precedence::BitwiseXor) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::Xor,
                    Precedence::BitwiseXor,
                ),
            Token::ShiftLeft if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::ShiftLeft,
                    Precedence::Shift,
                ),
            Token::ShiftRightArithmetic if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::ShiftRightArithmetic,
                    Precedence::Shift,
                ),
            Token::ShiftRightLogical if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::ShiftRightLogical,
                    Precedence::Shift,
                ),
            Token::EqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::EqEq,
                    Precedence::Equality,
                ),
            Token::NotEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::NotEq,
                    Precedence::Equality,
                ),
            Token::EqEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::EqEqEq,
                    Precedence::Equality,
                ),
            Token::NotEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::NotEqEq,
                    Precedence::Equality,
                ),
            Token::LessThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::LessThan,
                    Precedence::Relational,
                ),
            Token::LessThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::LessThanOrEqual,
                    Precedence::Relational,
                ),
            Token::GreaterThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::GreaterThan,
                    Precedence::Relational,
                ),
            Token::GreaterThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::GreaterThanOrEqual,
                    Precedence::Relational,
                ),
            Token::In if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::In,
                    Precedence::Relational,
                ),
            Token::InstanceOf if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    BinaryOperator::InstanceOf,
                    Precedence::Relational,
                ),

            // Logical operations
            Token::LogicalAnd if precedence.is_weaker_than(Precedence::LogicalAnd) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    LogicalOperator::And,
                    Precedence::LogicalAnd,
                ),
            Token::LogicalOr if precedence.is_weaker_than(Precedence::LogicalOr) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    LogicalOperator::Or,
                    Precedence::LogicalOr,
                ),
            Token::NullishCoalesce if precedence.is_weaker_than(Precedence::LogicalOr) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    LogicalOperator::NullishCoalesce,
                    Precedence::LogicalOr,
                ),

            // Update expressions
            Token::Increment
                if precedence.is_weaker_than(Precedence::PostfixUpdate)
                    && !self.lexer.is_new_line_before_current() =>
            {
                self.parse_update_expression_postfix(left, start_pos, UpdateOperator::Increment)
            }
            Token::Decrement
                if precedence.is_weaker_than(Precedence::PostfixUpdate)
                    && !self.lexer.is_new_line_before_current() =>
            {
                self.parse_update_expression_postfix(left, start_pos, UpdateOperator::Decrement)
            }

            // No infix expression
            _ => Ok(left),
        }
    }

    fn parse_binary_expression(
        &mut self,
        left: P<Expression>,
        start_pos: Pos,
        operator: BinaryOperator,
        precedence: Precedence,
    ) -> ParseResult<P<Expression>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Binary(BinaryExpression { loc, left, right, operator })))
    }

    fn parse_logical_expression(
        &mut self,
        left: P<Expression>,
        start_pos: Pos,
        operator: LogicalOperator,
        precedence: Precedence,
    ) -> ParseResult<P<Expression>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Logical(LogicalExpression { loc, left, right, operator })))
    }

    fn parse_update_expression_prefix(
        &mut self,
        operator: UpdateOperator,
    ) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Update(UpdateExpression {
            loc,
            operator,
            argument,
            is_prefix: true,
        })))
    }

    fn parse_update_expression_postfix(
        &mut self,
        argument: P<Expression>,
        start_pos: Pos,
        operator: UpdateOperator,
    ) -> ParseResult<P<Expression>> {
        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Update(UpdateExpression {
            loc,
            operator,
            argument,
            is_prefix: false,
        })))
    }

    fn parse_unary_expression(&mut self, operator: UnaryOperator) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Unary(UnaryExpression { loc, operator, argument })))
    }

    /// 13.3 LeftHandSideExpression
    fn parse_left_hand_side_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = match &self.token {
            Token::New => self.parse_new_expression()?,
            Token::Super => self.parse_super_expression()?,
            _ => self.parse_primary_expression()?,
        };

        self.parse_call_expression(expr, start_pos, true)
    }

    fn parse_call_expression(
        &mut self,
        expr: P<Expression>,
        start_pos: Pos,
        allow_call: bool,
    ) -> ParseResult<P<Expression>> {
        let expr = self.parse_member_expression(expr, start_pos, allow_call)?;
        match self.token {
            Token::LeftParen if allow_call => {
                let arguments = self.parse_call_arguments()?;
                let loc = self.mark_loc(start_pos);

                let call_expr = p(Expression::Call(CallExpression {
                    loc,
                    callee: expr,
                    arguments,
                    is_optional: false,
                }));

                self.parse_call_expression(call_expr, start_pos, allow_call)
            }
            _ => Ok(expr),
        }
    }

    fn parse_member_expression(
        &mut self,
        expr: P<Expression>,
        start_pos: Pos,
        allow_call: bool,
    ) -> ParseResult<P<Expression>> {
        match &self.token {
            Token::Period => {
                self.advance()?;

                let is_private = self.token == Token::Hash;
                if is_private {
                    self.advance()?;
                }

                let property = match self.parse_identifier_name()? {
                    Some(id) => id,
                    None => return self.error_unexpected_token(self.loc, &self.token),
                };

                let loc = self.mark_loc(start_pos);

                let member_expr = p(Expression::Member(MemberExpression {
                    loc,
                    object: expr,
                    property: p(Expression::Id(property)),
                    is_computed: false,
                    is_optional: false,
                    is_private,
                }));

                self.parse_call_expression(member_expr, start_pos, allow_call)
            }
            Token::LeftBracket => {
                self.advance()?;
                let property = self.parse_expression()?;
                self.expect(Token::RightBracket)?;
                let loc = self.mark_loc(start_pos);

                let member_expr = p(Expression::Member(MemberExpression {
                    loc,
                    object: expr,
                    property,
                    is_computed: true,
                    is_optional: false,
                    is_private: false,
                }));

                self.parse_call_expression(member_expr, start_pos, allow_call)
            }
            _ => Ok(expr),
        }
    }

    fn parse_new_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let callee_start_pos = self.current_start_pos();
        let callee = match self.token {
            Token::New => self.parse_new_expression()?,
            _ => self.parse_primary_expression()?,
        };

        // Disallow call since parenthesized arguments should be attached to this new instead
        let callee = self.parse_member_expression(callee, callee_start_pos, false)?;

        let arguments = if self.token == Token::LeftParen {
            self.parse_call_arguments()?
        } else {
            vec![]
        };

        let loc = self.mark_loc(start_pos);
        Ok(p(Expression::New(NewExpression { loc, callee, arguments })))
    }

    fn parse_super_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let super_loc = self.loc;
        self.advance()?;

        match self.token {
            Token::Period => {
                self.advance()?;
                let id = match self.parse_identifier_name()? {
                    Some(id) => id,
                    None => return self.error_unexpected_token(self.loc, &self.token),
                };

                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::SuperMember(SuperMemberExpression {
                    loc,
                    super_: super_loc,
                    property: p(Expression::Id(id)),
                    is_computed: false,
                })))
            }
            Token::LeftBracket => {
                self.advance()?;
                let property = self.parse_expression()?;
                self.expect(Token::RightBracket)?;
                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::SuperMember(SuperMemberExpression {
                    loc,
                    super_: super_loc,
                    property,
                    is_computed: true,
                })))
            }
            Token::LeftParen => {
                let arguments = self.parse_call_arguments()?;
                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::SuperCall(SuperCallExpression {
                    loc,
                    super_: super_loc,
                    arguments,
                })))
            }
            _ => self.error_expected_token(self.loc, &self.token, &Token::LeftParen),
        }
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<CallArgument>> {
        self.expect(Token::LeftParen)?;

        let mut arguments = vec![];
        while self.token != Token::RightParen {
            if self.token == Token::Spread {
                arguments.push(CallArgument::Spread(self.parse_spread_element()?))
            } else {
                arguments.push(CallArgument::Expression(*self.parse_assignment_expression()?));
            }

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightParen)?;

        Ok(arguments)
    }

    /// 13.2 PrimaryExpression
    fn parse_primary_expression(&mut self) -> ParseResult<P<Expression>> {
        match &self.token {
            Token::Null => {
                let loc = self.loc;
                self.advance()?;
                Ok(p(Expression::Null(loc)))
            }
            Token::True | Token::False => {
                let value = self.token == Token::True;
                let loc = self.loc;
                self.advance()?;
                Ok(p(Expression::Boolean(BooleanLiteral { loc, value })))
            }
            Token::NumberLiteral(value) => {
                let loc = self.loc;
                let value = value.clone();
                self.advance()?;
                Ok(p(Expression::Number(NumberLiteral { loc, value })))
            }
            Token::StringLiteral(value) => {
                let loc = self.loc;
                let value = value.clone();
                self.advance()?;
                Ok(p(Expression::String(StringLiteral { loc, value })))
            }
            Token::This => {
                let loc = self.loc;
                self.advance()?;
                Ok(p(Expression::This(loc)))
            }
            Token::LeftParen => {
                self.advance()?;
                let expr = self.parse_expression()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }
            Token::LeftBrace => self.parse_object_expression(),
            Token::LeftBracket => self.parse_array_expression(),
            Token::Class => Ok(p(Expression::Class(self.parse_class(false)?))),
            _ => {
                if self.is_function_start()? {
                    return Ok(p(Expression::Function(self.parse_function(false)?)));
                }

                // Check for the start of an async arrow function
                if self.token == Token::Async {
                    let async_loc = self.loc;
                    self.advance()?;

                    // `async [newline] id` is an `async` identifier with ASI followed by another
                    // identifier instead of the start of an async arrow function.
                    if self.lexer.is_new_line_before_current() {
                        return Ok(p(Expression::Id(Identifier {
                            loc: async_loc,
                            name: String::from("async"),
                        })));
                    }

                    // If followed by an identifier this is `async id`
                    let id_token = self.token.clone();
                    let id_loc = self.loc;
                    if let Ok(_) = self.parse_binding_identifier() {
                        // Start of an async arrow function. This can only occur if we are trying
                        // to parse a non-arrow function first, so fail the try parse.
                        if self.token == Token::Arrow {
                            return self.error(async_loc, FAIL_TRY_PARSED_ERROR);
                        } else {
                            // Otherwise this is a regular parse error at `id`
                            return self.error_unexpected_token(id_loc, &id_token);
                        }
                    } else {
                        // If not followed by an identifier this is just the identifier `async`
                        return Ok(p(Expression::Id(Identifier {
                            loc: async_loc,
                            name: String::from("async"),
                        })));
                    }
                }

                Ok(p(Expression::Id(self.parse_identifier_reference()?)))
            }
        }
    }

    fn parse_identifier_reference(&mut self) -> ParseResult<Identifier> {
        self.parse_identifier()
    }

    fn parse_binding_identifier(&mut self) -> ParseResult<Identifier> {
        self.parse_identifier()
    }

    fn parse_label_identifier(&mut self) -> ParseResult<Identifier> {
        self.parse_identifier()
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        match &self.token {
            Token::Identifier(name) => {
                // Names that are contextually disallowed as identifiers when in strict mode
                if self.in_strict_mode {
                    match name.as_str() {
                        "implements" | "interface" | "package" | "private" | "protected"
                        | "public" => {
                            return self.error_unexpected_token(self.loc, &self.token);
                        }
                        _ => {}
                    }
                }

                let loc = self.loc;
                let name = name.clone();
                self.advance()?;
                Ok(Identifier { loc, name })
            }
            // Tokens that are always allowed as identifiers
            Token::Async | Token::Of | Token::From | Token::As | Token::Get | Token::Set => {
                let loc = self.loc;
                self.advance()?;
                Ok(Identifier { loc, name: self.token.to_string() })
            }
            // Tokens that are contextually allowed as identifiers, when not in strict mode
            Token::Let | Token::Static if !self.in_strict_mode => {
                let loc = self.loc;
                self.advance()?;
                Ok(Identifier { loc, name: self.token.to_string() })
            }
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    // Parse any identifier, including reserved words
    fn parse_identifier_name(&mut self) -> ParseResult<Option<Identifier>> {
        match &self.token {
            Token::Identifier(name) => {
                let loc = self.loc;
                let name = name.clone();
                self.advance()?;
                Ok(Some(Identifier { loc, name }))
            }
            // All keywords can be uses as an identifier name
            Token::Var
            | Token::Let
            | Token::Const
            | Token::Function
            | Token::Async
            | Token::This
            | Token::If
            | Token::Else
            | Token::Switch
            | Token::Case
            | Token::Default
            | Token::For
            | Token::Of
            | Token::While
            | Token::Do
            | Token::With
            | Token::Return
            | Token::Break
            | Token::Continue
            | Token::Try
            | Token::Catch
            | Token::Finally
            | Token::Throw
            | Token::Null
            | Token::True
            | Token::False
            | Token::In
            | Token::InstanceOf
            | Token::New
            | Token::Typeof
            | Token::Void
            | Token::Delete
            | Token::Debugger
            | Token::Static
            | Token::From
            | Token::As
            | Token::Class
            | Token::Extends
            | Token::Super
            | Token::Get
            | Token::Set
            | Token::Import
            | Token::Export
            | Token::Enum => {
                let loc = self.loc;
                let name = self.token.to_string();
                self.advance()?;
                Ok(Some(Identifier { loc, name }))
            }
            _ => Ok(None),
        }
    }

    fn parse_spread_element(&mut self) -> ParseResult<SpreadElement> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_assignment_expression()?;
        let loc = self.mark_loc(start_pos);

        Ok(SpreadElement { loc, argument })
    }

    fn parse_array_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut elements = vec![];
        while self.token != Token::RightBracket {
            match self.token {
                Token::Comma => {
                    self.advance()?;
                    elements.push(ArrayElement::Hole);
                    continue;
                }
                Token::Spread => {
                    elements.push(ArrayElement::Spread(self.parse_spread_element()?));
                }
                _ => {
                    elements.push(ArrayElement::Expression(*self.parse_assignment_expression()?));
                }
            }

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightBracket)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Array(ArrayExpression { loc, elements })))
    }

    fn parse_object_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut properties = vec![];
        while self.token != Token::RightBrace {
            if self.token == Token::Spread {
                let spread = self.parse_spread_element()?;
                let spread_property = Property {
                    loc: spread.loc,
                    key: spread.argument,
                    value: None,
                    is_computed: false,
                    is_method: false,
                    kind: PropertyKind::Spread,
                };
                properties.push(spread_property);
            } else {
                let (property, _) = self.parse_property(PropertyContext::Object)?;
                properties.push(property);
            }

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Object(ObjectExpression { loc, properties })))
    }

    fn parse_property(&mut self, prop_context: PropertyContext) -> ParseResult<(Property, bool)> {
        let start_pos = self.current_start_pos();

        // Handle getters and setters
        match self.token {
            Token::Get | Token::Set => {
                let id_loc = self.loc;
                let id_token = self.token.clone();
                let kind = if self.token == Token::Get {
                    PropertyKind::Get
                } else {
                    PropertyKind::Set
                };

                self.advance()?;

                // Handle `get` or `set` as name of method
                if self.token == Token::LeftParen {
                    let name =
                        p(Expression::Id(Identifier { loc: id_loc, name: id_token.to_string() }));
                    return self.parse_method_property(
                        name,
                        start_pos,
                        PropertyKind::Init,
                        /* is_async */ false,
                        /* is_generator */ false,
                        /* is_computed */ false,
                        /* is_private */ false,
                    );
                }

                // Handle `get` or `set` as shorthand or init property
                let is_init_property = self.is_property_initializer(prop_context);
                if is_init_property || self.is_property_end(prop_context) {
                    let name =
                        p(Expression::Id(Identifier { loc: id_loc, name: id_token.to_string() }));
                    return self.parse_init_property(
                        name,
                        start_pos,
                        prop_context,
                        /* is_computed */ false,
                        /* is_shorthand */ !is_init_property,
                        /* is_private */ false,
                    );
                }

                // Otherwise this is a getter or setter
                let property_name = self.parse_property_name(prop_context)?;
                return self.parse_method_property(
                    property_name.key,
                    start_pos,
                    kind,
                    /* is_async */ false,
                    /* is_generator */ false,
                    property_name.is_computed,
                    property_name.is_private,
                );
            }
            _ => (),
        }

        // Async method (or method with name async)
        if self.token == Token::Async {
            let async_loc = self.loc;
            self.advance()?;

            // Handle `async` as name of method: `async() {}`
            if self.token == Token::LeftParen {
                let name =
                    p(Expression::Id(Identifier { loc: async_loc, name: "async".to_owned() }));
                return self.parse_method_property(
                    name,
                    start_pos,
                    PropertyKind::Init,
                    /* is_async */ false,
                    /* is_generator */ false,
                    /* is_computed */ false,
                    /* is_private */ false,
                );
            }

            // Handle `async` as shorthand or init property
            let is_init_property = self.is_property_initializer(prop_context);
            if is_init_property || self.is_property_end(prop_context) {
                let name =
                    p(Expression::Id(Identifier { loc: async_loc, name: "async".to_owned() }));
                return self.parse_init_property(
                    name,
                    start_pos,
                    prop_context,
                    /* is_computed */ false,
                    /* is_shorthand */ !is_init_property,
                    /* is_private */ false,
                );
            }

            // Async method may also be a generator
            let is_generator = self.token == Token::Multiply;
            if is_generator {
                self.advance()?;
            }

            let property_name = self.parse_property_name(prop_context)?;
            return self.parse_method_property(
                property_name.key,
                start_pos,
                PropertyKind::Init,
                /* is_async */ true,
                is_generator,
                property_name.is_computed,
                property_name.is_private,
            );
        }

        // Generator method
        if self.token == Token::Multiply {
            self.advance()?;

            let property_name = self.parse_property_name(prop_context)?;
            return self.parse_method_property(
                property_name.key,
                start_pos,
                PropertyKind::Init,
                /* is_async */ false,
                /* is_generator */ true,
                property_name.is_computed,
                property_name.is_private,
            );
        }

        // Regular init and method properties
        let property_name = self.parse_property_name(prop_context)?;

        match self.token {
            Token::LeftParen => self.parse_method_property(
                property_name.key,
                start_pos,
                PropertyKind::Init,
                /* is_async */ false,
                /* is_generator */ false,
                property_name.is_computed,
                property_name.is_private,
            ),
            _ => self.parse_init_property(
                property_name.key,
                start_pos,
                prop_context,
                property_name.is_computed,
                property_name.is_shorthand,
                property_name.is_private,
            ),
        }
    }

    fn is_property_end(&mut self, prop_context: PropertyContext) -> bool {
        match prop_context {
            PropertyContext::Class => {
                self.token == Token::Semicolon
                    || self.token == Token::RightBrace
                    || self.lexer.is_new_line_before_current()
            }
            PropertyContext::Object => {
                self.token == Token::Comma || self.token == Token::RightBrace
            }
            PropertyContext::Pattern => {
                self.token == Token::Comma
                    || self.token == Token::RightBrace
                    || self.token == Token::Equals
            }
        }
    }

    fn is_property_initializer(&mut self, prop_context: PropertyContext) -> bool {
        let expected_token = self.get_property_initializer(prop_context);
        self.token == expected_token
    }

    fn get_property_initializer(&mut self, prop_context: PropertyContext) -> Token {
        match prop_context {
            PropertyContext::Class | PropertyContext::Pattern => Token::Equals,
            PropertyContext::Object => Token::Colon,
        }
    }

    fn parse_property_name(
        &mut self,
        prop_context: PropertyContext,
    ) -> ParseResult<PropertyNameResult> {
        let mut is_computed = false;
        let mut is_shorthand = false;
        let mut is_identifier = false;
        let mut is_private = false;

        let key = match self.token {
            Token::LeftBracket => {
                self.advance()?;
                let expr = self.parse_assignment_expression()?;
                self.expect(Token::RightBracket)?;
                is_computed = true;
                expr
            }
            Token::NumberLiteral(_) | Token::StringLiteral(_) => self.parse_primary_expression()?,
            // Private properties are only allowed in classes
            Token::Hash if prop_context == PropertyContext::Class => {
                is_private = true;
                p(Expression::Id(self.parse_private_name()?))
            }
            _ => match self.parse_identifier_name()? {
                Some(key) => {
                    is_identifier = true;
                    p(Expression::Id(key))
                }
                None => {
                    return self.error_unexpected_token(self.loc, &self.token);
                }
            },
        };

        // All non-private key types can be shorthand for classes, but only identifier keys can be
        // shorthand elsewhere.
        if is_identifier || (prop_context == PropertyContext::Class) {
            is_shorthand = self.is_property_end(prop_context);
        }

        Ok(PropertyNameResult { key, is_computed, is_shorthand, is_private })
    }

    fn parse_init_property(
        &mut self,
        key: P<Expression>,
        start_pos: Pos,
        prop_context: PropertyContext,
        is_computed: bool,
        is_shorthand: bool,
        is_private: bool,
    ) -> ParseResult<(Property, bool)> {
        let value = if is_shorthand {
            None
        } else if self.is_property_initializer(prop_context) {
            self.advance()?;
            Some(self.parse_assignment_expression()?)
        } else {
            let expected_token = self.get_property_initializer(prop_context);
            return self.error_expected_token(self.loc, &self.token, &expected_token);
        };

        let loc = self.mark_loc(start_pos);

        let property = Property {
            loc,
            key,
            value,
            is_computed,
            is_method: false,
            kind: PropertyKind::Init,
        };

        Ok((property, is_private))
    }

    fn parse_method_property(
        &mut self,
        key: P<Expression>,
        start_pos: Pos,
        kind: PropertyKind,
        is_async: bool,
        is_generator: bool,
        is_computed: bool,
        is_private: bool,
    ) -> ParseResult<(Property, bool)> {
        let params = self.parse_function_params()?;
        let (block, has_use_strict_directive, is_strict_mode) = self.parse_function_block_body()?;
        let body = p(FunctionBody::Block(block));
        let loc = self.mark_loc(start_pos);

        // TODO: Error if getter or setter has wrong number of params

        let property = Property {
            loc,
            key,
            is_computed,
            is_method: true,
            kind,
            value: Some(p(Expression::Function(Function::new(
                loc,
                /* id */ None,
                params,
                body,
                is_async,
                is_generator,
                is_strict_mode,
                has_use_strict_directive,
            )))),
        };

        Ok((property, is_private))
    }

    fn parse_class(&mut self, is_decl: bool) -> ParseResult<Class> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Enter strict mode for entire class, saving strict mode context from beforehand
        let old_in_strict_mode = self.in_strict_mode;
        self.in_strict_mode = true;

        // Id is optional only for class expresssions
        let id = if is_decl || (self.token != Token::LeftBrace && self.token != Token::Extends) {
            Some(p(self.parse_binding_identifier()?))
        } else {
            None
        };

        let super_class = if self.token == Token::Extends {
            self.advance()?;
            Some(self.parse_left_hand_side_expression()?)
        } else {
            None
        };

        let mut body = vec![];

        self.expect(Token::LeftBrace)?;
        while self.token != Token::RightBrace {
            // Empty semicolon statements are allowed in class body
            if self.token == Token::Semicolon {
                self.advance()?;
                continue;
            }

            body.push(self.parse_class_element()?);
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        // Restore to strict mode context from beforehand
        self.in_strict_mode = old_in_strict_mode;

        Ok(Class::new(loc, id, super_class, body))
    }

    fn parse_private_name(&mut self) -> ParseResult<Identifier> {
        self.expect(Token::Hash)?;

        match self.parse_identifier_name()? {
            Some(id) => Ok(id),
            None => return self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_class_element(&mut self) -> ParseResult<ClassElement> {
        let start_pos = self.current_start_pos();

        // Every class element can start with a `static` modifier
        let is_static = self.token == Token::Static;
        if is_static {
            let static_loc = self.loc;
            self.advance()?;

            // Check for static initializer
            if self.token == Token::LeftBrace {
                let block = self.parse_block()?;

                let loc = self.mark_loc(start_pos);

                // Static initializers implemented as method with block body. All fields except for
                // the kind and the function's block body are ignored, so put in placeholders.
                return Ok(ClassElement::Method(ClassMethod {
                    loc,
                    key: p(Expression::Null(loc)),
                    value: p(Function::new(
                        loc,
                        None,
                        vec![],
                        p(FunctionBody::Block(block)),
                        /* is_async */ false,
                        /* is_generator */ false,
                        /* is_strict_mode */ true,
                        /* has_use_strict_directive */ false,
                    )),
                    kind: ClassMethodKind::StaticInitializer,
                    is_computed: false,
                    is_static: false,
                    is_private: false,
                }));
            }

            // Handle `static` as name of method: `static() {}`
            if self.token == Token::LeftParen {
                let name =
                    p(Expression::Id(Identifier { loc: static_loc, name: "static".to_owned() }));

                let (property, is_private) = self.parse_method_property(
                    name,
                    start_pos,
                    PropertyKind::Init,
                    /* is_async */ false,
                    /* is_generator */ false,
                    /* is_computed */ false,
                    /* is_private */ false,
                )?;
                let loc = self.mark_loc(start_pos);

                return Ok(ClassElement::Method(self.reparse_property_as_class_method(
                    loc, property, /* is_static */ false, is_private,
                )));
            }

            // Handle `static` as shorthand or init property
            let is_init_property = self.is_property_initializer(PropertyContext::Class);
            if is_init_property || self.is_property_end(PropertyContext::Class) {
                let name =
                    p(Expression::Id(Identifier { loc: static_loc, name: "static".to_owned() }));

                let (property, is_private) = self.parse_init_property(
                    name,
                    start_pos,
                    PropertyContext::Class,
                    /* is_computed */ false,
                    /* is_shorthand */ !is_init_property,
                    /* is_private */ false,
                )?;
                let loc = self.mark_loc(start_pos);

                return Ok(ClassElement::Property(self.reparse_property_as_class_property(
                    loc, property, /* is_static */ false, is_private,
                )));
            }
        }

        // Parse an object property because syntax is almost identical to class property
        let (property, is_private) = self.parse_property(PropertyContext::Class)?;
        let loc = self.mark_loc(start_pos);

        // Translate from object property to class property or method
        if property.is_method {
            Ok(ClassElement::Method(
                self.reparse_property_as_class_method(loc, property, is_static, is_private),
            ))
        } else {
            Ok(ClassElement::Property(
                self.reparse_property_as_class_property(loc, property, is_static, is_private),
            ))
        }
    }

    fn reparse_property_as_class_method(
        &mut self,
        loc: Loc,
        property: Property,
        is_static: bool,
        is_private: bool,
    ) -> ClassMethod {
        let Property { key, value, is_computed, kind, .. } = property;

        let func_value = if let Expression::Function(func) = *value.unwrap() {
            p(func)
        } else {
            unreachable!("method properties must have function expression")
        };

        let kind = match kind {
            PropertyKind::Get => ClassMethodKind::Get,
            PropertyKind::Set => ClassMethodKind::Set,
            PropertyKind::Init if is_static => ClassMethodKind::Method,
            PropertyKind::Init => {
                // Any function name may be a constructor
                let is_constructor_key = match key.as_ref() {
                    Expression::Id(id) if id.name == "constructor" => true,
                    Expression::String(str) if str.value == "constructor" => true,
                    _ => false,
                };

                if is_constructor_key && !is_static && !is_computed {
                    ClassMethodKind::Constructor
                } else {
                    ClassMethodKind::Method
                }
            }
            PropertyKind::Spread => unreachable!("spread element cannot appear in class"),
        };

        ClassMethod {
            loc,
            key,
            value: func_value,
            kind,
            is_computed,
            is_static,
            is_private,
        }
    }

    fn reparse_property_as_class_property(
        &mut self,
        loc: Loc,
        property: Property,
        is_static: bool,
        is_private: bool,
    ) -> ClassProperty {
        let Property { key, value, is_computed, .. } = property;

        ClassProperty { loc, key, value, is_computed, is_static, is_private }
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match &self.token {
            Token::LeftBracket => self.parse_array_pattern(),
            Token::LeftBrace => self.parse_object_pattern(),
            _ => Ok(Pattern::Id(self.parse_binding_identifier()?)),
        }
    }

    fn parse_pattern_including_assignment_pattern(&mut self) -> ParseResult<Pattern> {
        let start_pos = self.current_start_pos();
        let patt = self.parse_pattern()?;
        self.parse_assignment_pattern(patt, start_pos)
    }

    /// Parse an assignment pattern if one exists, otherwise return left hand side
    fn parse_assignment_pattern(&mut self, left: Pattern, start_pos: Pos) -> ParseResult<Pattern> {
        // If followed by an equals sign, this is an assignment pattern
        match self.token {
            Token::Equals => {
                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                Ok(Pattern::Assign(AssignmentPattern { loc, left: p(left), right }))
            }
            _ => Ok(left),
        }
    }

    fn parse_array_pattern(&mut self) -> ParseResult<Pattern> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut elements = vec![];
        while self.token != Token::RightBracket {
            if self.token == Token::Comma {
                self.advance()?;
                elements.push(None);
            } else {
                elements.push(Some(self.parse_pattern_including_assignment_pattern()?));
                if self.token == Token::Comma {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        self.expect(Token::RightBracket)?;
        let loc = self.mark_loc(start_pos);

        Ok(Pattern::Array(ArrayPattern { loc, elements }))
    }

    fn parse_object_pattern(&mut self) -> ParseResult<Pattern> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut properties = vec![];
        while self.token != Token::RightBrace {
            properties.push(self.parse_object_pattern_property()?);

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(Pattern::Object(ObjectPattern { loc, properties }))
    }

    fn parse_object_pattern_property(&mut self) -> ParseResult<ObjectPatternProperty> {
        let start_pos = self.current_start_pos();

        let property_name = self.parse_property_name(PropertyContext::Pattern)?;

        // Shorthand property
        if property_name.is_shorthand {
            let value = if let Expression::Id(id) = *property_name.key {
                Pattern::Id(id)
            } else {
                unreachable!()
            };

            // Shorthand property may be followed by assignment pattern
            let value = p(self.parse_assignment_pattern(value, start_pos)?);
            let loc = self.mark_loc(start_pos);

            return Ok(ObjectPatternProperty { loc, key: None, value, is_computed: false });
        }

        // Regular properties
        self.expect(Token::Colon)?;
        let value = p(self.parse_pattern_including_assignment_pattern()?);
        let loc = self.mark_loc(start_pos);

        Ok(ObjectPatternProperty {
            loc,
            key: Some(property_name.key),
            value,
            is_computed: property_name.is_computed,
        })
    }

    fn reparse_expression_as_pattern(&self, expr: Expression) -> Option<Pattern> {
        match expr {
            Expression::Id(id) => Some(Pattern::Id(id)),
            _ => None,
        }
    }
}

pub fn parse_script(source: &Rc<Source>) -> ParseResult<Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(parser.parse_script()?)
}
