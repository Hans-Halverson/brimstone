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
    UnterminatedStringLiteral,
    MalformedEscapeSeqence,
    MalformedNumericLiteral,
    InvalidForLeftHandSide,
}

// Arbitrary error used to fail try parse
const FAIL_TRY_PARSED_ERROR: ParseError = ParseError::MalformedNumericLiteral;

pub struct LocalizedParseError {
    pub error: ParseError,
    pub source_loc: Option<(Loc, Rc<Source>)>,
}

impl LocalizedParseError {
    fn new_without_loc(error: ParseError) -> LocalizedParseError {
        LocalizedParseError {
            error,
            source_loc: None,
        }
    }
}

impl Error for LocalizedParseError {}

impl fmt::Display for LocalizedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match &self.error {
            ParseError::Io(io_error) => return io_error.fmt(f),
            ParseError::UnknownToken(token) => format!("Unknown token {}", token),
            ParseError::UnexpectedToken(token) => format!("Unexpected token {}", token),
            ParseError::ExpectedToken(actual, expected) => {
                format!("Unexpected token {}, expected {}", actual, expected)
            }
            ParseError::UnterminatedStringLiteral => format!("Unterminated string literal"),
            ParseError::MalformedEscapeSeqence => format!("Malformed escape sequence"),
            ParseError::MalformedNumericLiteral => format!("Malformed numeric literal"),
            ParseError::InvalidForLeftHandSide => {
                format!("Invalid left hand side of for statement")
            }
        };

        match &self.source_loc {
            None => write!(f, "{}", message),
            Some((loc, source)) => {
                let offsets = source.line_offsets();
                let (line, col) = find_line_col_for_pos(loc.start, offsets);
                write!(f, "{}:{}:{} {}", source.file_path, line, col, message)
            }
        }
    }
}

impl fmt::Debug for LocalizedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <LocalizedParseError as fmt::Display>::fmt(self, f)
    }
}

pub type ParseResult<T> = Result<T, LocalizedParseError>;

impl From<io::Error> for LocalizedParseError {
    fn from(error: io::Error) -> LocalizedParseError {
        LocalizedParseError::new_without_loc(ParseError::Io(error))
    }
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

fn p<T>(node: T) -> P<T> {
    Box::new(node)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
}

/// A save point for the parser, can be used to restore the parser to a particular position.
struct ParserSaveState {
    saved_lexer_state: SavedLexerState,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
}

impl<'a> Parser<'a> {
    // Must prime parser by calling advance before using.
    fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            token: Token::Eof,
            loc: EMPTY_LOC,
            prev_loc: EMPTY_LOC,
        }
    }

    pub fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        let source = (*self.lexer.source).clone();
        Err(LocalizedParseError {
            error,
            source_loc: Some((loc, source)),
        })
    }

    fn save(&self) -> ParserSaveState {
        ParserSaveState {
            saved_lexer_state: self.lexer.save(),
            token: self.token.clone(),
            loc: self.loc,
            prev_loc: self.prev_loc,
        }
    }

    fn restore(&mut self, save_state: ParserSaveState) {
        self.lexer.restore(&save_state.saved_lexer_state);
        self.token = save_state.token;
        self.loc = save_state.loc;
        self.prev_loc = save_state.prev_loc;
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
            return self.error(
                self.loc,
                ParseError::ExpectedToken(self.token.clone(), token),
            );
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
        self.error(
            loc,
            ParseError::ExpectedToken(actual.clone(), expected.clone()),
        )
    }

    #[inline]
    fn current_start_pos(&self) -> Pos {
        self.loc.start
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc {
            start: start_pos,
            end: self.prev_loc.end,
        }
    }

    fn parse_program(&mut self) -> ParseResult<Program> {
        let mut toplevels = vec![];

        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        Ok(Program { loc, toplevels })
    }

    fn parse_toplevel(&mut self) -> ParseResult<Toplevel> {
        let stmt = self.parse_statement()?;
        Ok(Toplevel::Statement(stmt))
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.token {
            Token::Var | Token::Let | Token::Const => {
                Ok(Statement::VarDecl(self.parse_variable_declaration(false)?))
            }
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
                self.expect(Token::Semicolon)?;

                Ok(Statement::Debugger(self.mark_loc(start_pos)))
            }
            _ => {
                if self.is_function_start()? {
                    return Ok(Statement::FuncDecl(self.parse_function(true)?));
                }

                let start_pos = self.current_start_pos();
                let expr = self.parse_expression()?;

                // Parse labeled statement
                if self.token == Token::Colon {
                    if let Expression::Id(label) = *expr {
                        self.advance()?;
                        let body = self.parse_statement()?;
                        let loc = self.mark_loc(start_pos);

                        return Ok(Statement::Labeled(LabeledStatement {
                            loc,
                            label: p(label),
                            body: p(body),
                        }));
                    }
                }

                // Otherwise must be an expression statement
                self.expect(Token::Semicolon)?;
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

            declarations.push(VariableDeclarator {
                loc,
                id: p(id),
                init,
            });

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        if !is_for_init {
            self.expect(Token::Semicolon)?;
        }

        let loc = self.mark_loc(start_pos);

        Ok(VariableDeclaration {
            loc,
            kind,
            declarations,
        })
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
            Some(p(self.parse_identifier()?))
        } else {
            None
        };

        let params = self.parse_function_params()?;
        let body = p(FunctionBody::Block(self.parse_block()?));
        let loc = self.mark_loc(start_pos);

        Ok(Function {
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
        })
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<Pattern>> {
        // Read all function params between the parentheses
        let mut params = vec![];
        self.expect(Token::LeftParen)?;

        while self.token != Token::RightParen {
            params.push(self.parse_pattern()?);

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightParen)?;

        Ok(params)
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut body = vec![];
        while self.token != Token::RightBrace {
            body.push(self.parse_statement()?)
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(Block { loc, body })
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

        Ok(Statement::If(IfStatement {
            loc,
            test,
            conseq,
            altern,
        }))
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
                        body.push(self.parse_statement()?)
                    }

                    let loc = self.mark_loc(case_start_pos);
                    cases.push(SwitchCase { loc, test, body })
                }
                _ => return self.error_expected_token(self.loc, &self.token, &Token::Catch),
            }
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Switch(SwitchStatement {
            loc,
            discriminant,
            cases,
        }))
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

        Ok(Statement::For(ForStatement {
            loc,
            init,
            test,
            update,
            body,
        }))
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
        self.expect(Token::Semicolon)?;

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

        Ok(Statement::Try(TryStatement {
            loc,
            block,
            handler,
            finalizer,
        }))
    }

    fn parse_throw_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Throw(ThrowStatement { loc, argument }))
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = if self.token == Token::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Return(ReturnStatement { loc, argument }))
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.token == Token::Semicolon {
            None
        } else {
            Some(p(self.parse_identifier()?))
        };

        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Break(BreakStatement { loc, label }))
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.token == Token::Semicolon {
            None
        } else {
            Some(p(self.parse_identifier()?))
        };

        self.expect(Token::Semicolon)?;
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

            Ok(p(Expression::Sequence(SequenceExpression {
                loc,
                expressions,
            })))
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

                Ok(p(Expression::Assign(AssignmentExpression {
                    loc,
                    left: expr,
                    right,
                    operator,
                })))
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
                let body = self.parse_arrow_function_body()?;
                let loc = self.mark_loc(start_pos);

                return Ok(p(Expression::ArrowFunction(Function {
                    loc,
                    id: None,
                    params,
                    body,
                    is_async: false,
                    is_generator: false,
                })));
            }
        }

        // Arrow function params can be either parenthesized function params or a single id
        let params = match self.token {
            Token::LeftParen => self.parse_function_params()?,
            Token::Identifier(_) => {
                let id = self.parse_identifier()?;
                vec![Pattern::Id(id)]
            }
            _ => return self.error_unexpected_token(self.loc, &self.token),
        };

        self.expect(Token::Arrow)?;
        let body = self.parse_arrow_function_body()?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::ArrowFunction(Function {
            loc,
            id: None,
            params,
            body,
            is_async,
            is_generator: false,
        })))
    }

    fn parse_arrow_function_body(&mut self) -> ParseResult<P<FunctionBody>> {
        if self.token == Token::LeftBrace {
            Ok(p(FunctionBody::Block(self.parse_block()?)))
        } else {
            Ok(p(FunctionBody::Expression(
                *self.parse_assignment_expression()?,
            )))
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
            Token::Increment if precedence.is_weaker_than(Precedence::PostfixUpdate) => {
                self.parse_update_expression_postfix(left, start_pos, UpdateOperator::Increment)
            }
            Token::Decrement if precedence.is_weaker_than(Precedence::PostfixUpdate) => {
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

        Ok(p(Expression::Binary(BinaryExpression {
            loc,
            left,
            right,
            operator,
        })))
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

        Ok(p(Expression::Logical(LogicalExpression {
            loc,
            left,
            right,
            operator,
        })))
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

        Ok(p(Expression::Unary(UnaryExpression {
            loc,
            operator,
            argument,
        })))
    }

    /// 13.3 LeftHandSideExpression
    fn parse_left_hand_side_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = match &self.token {
            Token::New => self.parse_new_expression()?,
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
                let property = self.parse_identifier()?;
                let loc = self.mark_loc(start_pos);

                let member_expr = p(Expression::Member(MemberExpression {
                    loc,
                    object: expr,
                    property: p(Expression::Id(property)),
                    is_computed: false,
                    is_optional: false,
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
        Ok(p(Expression::New(NewExpression {
            loc,
            callee,
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<Expression>> {
        self.expect(Token::LeftParen)?;

        let mut arguments = vec![];
        while self.token != Token::RightParen {
            arguments.push(*self.parse_assignment_expression()?);

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
            Token::Identifier(_) => Ok(p(Expression::Id(self.parse_identifier()?))),
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
            _ => {
                if self.is_function_start()? {
                    return Ok(p(Expression::Function(self.parse_function(false)?)));
                }

                self.error_unexpected_token(self.loc, &self.token)
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        match &self.token {
            Token::Identifier(name) => {
                let loc = self.loc;
                let name = name.clone();
                self.advance()?;
                Ok(Identifier { loc, name })
            }
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    fn parse_array_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut elements = vec![];
        while self.token != Token::RightBracket {
            if self.token == Token::Comma {
                self.advance()?;
                elements.push(None);
            } else {
                elements.push(Some(*self.parse_assignment_expression()?));
                if self.token == Token::Comma {
                    self.advance()?;
                } else {
                    break;
                }
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
            properties.push(self.parse_property()?);

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Object(ObjectExpression { loc, properties })))
    }

    fn parse_property(&mut self) -> ParseResult<Property> {
        let start_pos = self.current_start_pos();

        // Handle getters and setters
        match self.token {
            Token::Identifier(ref id) if id.eq("get") || id.eq("set") => {
                let id_loc = self.loc;
                let kind = if id.eq("get") {
                    PropertyKind::Get
                } else {
                    PropertyKind::Set
                };
                let name = id.to_owned();

                self.advance()?;

                return match self.token {
                    // Handle `get` or `set` as name of method
                    Token::LeftParen => {
                        let name = p(Expression::Id(Identifier { loc: id_loc, name }));
                        self.parse_method_property(
                            name,
                            start_pos,
                            PropertyKind::Init,
                            false,
                            false,
                            false,
                        )
                    }
                    // Handle `get` or `set` as shorthand or init property
                    Token::Comma | Token::RightBrace | Token::Colon => {
                        let name = p(Expression::Id(Identifier { loc: id_loc, name }));
                        self.parse_init_property(name, start_pos, false, self.token != Token::Colon)
                    }
                    // Otherwise this is a getter or setter
                    _ => match self.parse_property_name()? {
                        (Some(name), is_computed, _) => self.parse_method_property(
                            name,
                            start_pos,
                            kind,
                            false,
                            false,
                            is_computed,
                        ),
                        _ => self.error_unexpected_token(self.loc, &self.token),
                    },
                };
            }
            _ => (),
        }

        match self.parse_property_name()? {
            // Regular init and method properties
            (Some(name), is_computed, is_shorthand) => match self.token {
                Token::LeftParen => self.parse_method_property(
                    name,
                    start_pos,
                    PropertyKind::Init,
                    false,
                    false,
                    is_computed,
                ),
                _ => self.parse_init_property(name, start_pos, is_computed, is_shorthand),
            },
            _ => match self.token {
                // Generator method
                Token::Multiply => {
                    self.advance()?;

                    match self.parse_property_name()? {
                        (Some(name), is_computed, _) => self.parse_method_property(
                            name,
                            start_pos,
                            PropertyKind::Init,
                            false,
                            true,
                            is_computed,
                        ),
                        _ => self.error_unexpected_token(self.loc, &self.token),
                    }
                }
                // Async method (or method with name async)
                Token::Async => {
                    let async_loc = self.loc;
                    self.advance()?;

                    match self.token {
                        // Handle `async` as name of method: `async() {}`
                        Token::LeftParen => {
                            let name = p(Expression::Id(Identifier {
                                loc: async_loc,
                                name: "async".to_owned(),
                            }));
                            return self.parse_method_property(
                                name,
                                start_pos,
                                PropertyKind::Init,
                                false,
                                false,
                                false,
                            );
                        }
                        // Handle `async` as shorthand or init property
                        Token::Comma | Token::RightBrace | Token::Colon => {
                            let name = p(Expression::Id(Identifier {
                                loc: async_loc,
                                name: "async".to_owned(),
                            }));
                            return self.parse_init_property(
                                name,
                                start_pos,
                                false,
                                self.token != Token::Colon,
                            );
                        }
                        _ => (),
                    }

                    // Async method may also be a generator
                    let is_generator = self.token == Token::Multiply;
                    if is_generator {
                        self.advance()?;
                    }

                    match self.parse_property_name()? {
                        (Some(name), is_computed, _) => self.parse_method_property(
                            name,
                            start_pos,
                            PropertyKind::Init,
                            true,
                            is_generator,
                            is_computed,
                        ),
                        _ => self.error_unexpected_token(self.loc, &self.token),
                    }
                }
                ref other => self.error_unexpected_token(self.loc, other),
            },
        }
    }

    fn parse_property_name(&mut self) -> ParseResult<(Option<P<Expression>>, bool, bool)> {
        let mut is_computed = false;
        let mut is_shorthand = false;

        let expr = match self.token {
            Token::LeftBracket => {
                self.advance()?;
                let expr = self.parse_assignment_expression()?;
                self.expect(Token::RightBracket)?;
                is_computed = true;
                Some(expr)
            }
            Token::NumberLiteral(_) | Token::StringLiteral(_) => {
                Some(self.parse_primary_expression()?)
            }
            Token::Identifier(_) => {
                let key = self.parse_identifier()?;

                if self.token == Token::Comma || self.token == Token::RightBrace {
                    is_shorthand = true;
                }

                Some(p(Expression::Id(key)))
            }
            _ => None,
        };

        Ok((expr, is_computed, is_shorthand))
    }

    fn parse_init_property(
        &mut self,
        key: P<Expression>,
        start_pos: Pos,
        is_computed: bool,
        is_shorthand: bool,
    ) -> ParseResult<Property> {
        let value = match self.token {
            _ if is_shorthand => None,
            Token::Colon => {
                self.advance()?;
                Some(self.parse_assignment_expression()?)
            }
            ref other => return self.error_expected_token(self.loc, other, &Token::Colon),
        };

        let loc = self.mark_loc(start_pos);

        Ok(Property {
            loc,
            key,
            value,
            is_computed,
            is_method: false,
            kind: PropertyKind::Init,
        })
    }

    fn parse_method_property(
        &mut self,
        key: P<Expression>,
        start_pos: Pos,
        kind: PropertyKind,
        is_async: bool,
        is_generator: bool,
        is_computed: bool,
    ) -> ParseResult<Property> {
        let params = self.parse_function_params()?;
        let body = p(FunctionBody::Block(self.parse_block()?));
        let loc = self.mark_loc(start_pos);

        // TODO: Error if getter or setter has wrong number of params

        Ok(Property {
            loc,
            key,
            is_computed,
            is_method: true,
            kind,
            value: Some(p(Expression::Function(Function {
                loc,
                id: None,
                params,
                body,
                is_async,
                is_generator,
            }))),
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match &self.token {
            Token::Identifier(_) => Ok(Pattern::Id(self.parse_identifier()?)),
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    fn reparse_expression_as_pattern(&self, expr: Expression) -> Option<Pattern> {
        match expr {
            Expression::Id(id) => Some(Pattern::Id(id)),
            _ => None,
        }
    }
}

pub fn parse_file(source: &Rc<Source>) -> ParseResult<Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(parser.parse_program()?)
}
