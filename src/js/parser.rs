use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use super::ast::{self, P};
use super::lexer::Lexer;
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
}

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

    fn parse_program(&mut self) -> ParseResult<ast::Program> {
        let mut toplevels = vec![];

        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        Ok(ast::Program { loc, toplevels })
    }

    fn parse_toplevel(&mut self) -> ParseResult<ast::Toplevel> {
        let stmt = self.parse_statement()?;
        Ok(ast::Toplevel::Statement(stmt))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        match &self.token {
            Token::Var | Token::Let | Token::Const => {
                Ok(ast::Statement::VarDecl(self.parse_variable_declaration()?))
            }
            Token::LeftBrace => Ok(ast::Statement::Block(self.parse_block()?)),
            Token::If => self.parse_if_statement(),
            Token::Switch => self.parse_switch_statement(),
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

                Ok(ast::Statement::Empty(loc))
            }
            Token::Debugger => {
                let start_pos = self.current_start_pos();
                self.advance()?;
                self.expect(Token::Semicolon)?;

                Ok(ast::Statement::Debugger(self.mark_loc(start_pos)))
            }
            _ => {
                let start_pos = self.current_start_pos();
                let expr = self.parse_expression()?;

                // Parse labeled statement
                if self.token == Token::Colon {
                    if let ast::Expression::Id(label) = *expr {
                        self.advance()?;
                        let body = self.parse_statement()?;
                        let loc = self.mark_loc(start_pos);

                        return Ok(ast::Statement::Labeled(ast::LabeledStatement {
                            loc,
                            label: p(label),
                            body: p(body),
                        }));
                    }
                }

                // Otherwise must be an expression statement
                self.expect(Token::Semicolon)?;
                let loc = self.mark_loc(start_pos);

                Ok(ast::Statement::Expr(ast::ExpressionStatement { loc, expr }))
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> ParseResult<ast::VariableDeclaration> {
        let start_pos = self.current_start_pos();
        let kind = match &self.token {
            Token::Var => ast::VarKind::Var,
            Token::Let => ast::VarKind::Let,
            Token::Const => ast::VarKind::Const,
            _ => unreachable!(),
        };
        self.advance()?;

        // Gather comma separated declarators
        let mut declarations = vec![];
        loop {
            let start_pos = self.current_start_pos();
            let id = self.parse_pattern()?;
            self.expect(Token::Equals)?;
            let init = self.parse_assignment_expression()?;
            let loc = self.mark_loc(start_pos);

            declarations.push(ast::VariableDeclarator {
                loc,
                id: p(id),
                init: Some(init),
            });

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        let loc = self.mark_loc(start_pos);

        Ok(ast::VariableDeclaration {
            loc,
            kind,
            declarations,
        })
    }

    fn parse_block(&mut self) -> ParseResult<ast::Block> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut body = vec![];
        while self.token != Token::RightBrace {
            body.push(self.parse_statement()?)
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Block { loc, body })
    }

    fn parse_if_statement(&mut self) -> ParseResult<ast::Statement> {
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

        Ok(ast::Statement::If(ast::IfStatement {
            loc,
            test,
            conseq,
            altern,
        }))
    }

    fn parse_switch_statement(&mut self) -> ParseResult<ast::Statement> {
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
                    cases.push(ast::SwitchCase { loc, test, body })
                }
                _ => return self.error_expected_token(self.loc, &self.token, &Token::Catch),
            }
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::Switch(ast::SwitchStatement {
            loc,
            discriminant,
            cases,
        }))
    }

    fn parse_while_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let body = p(self.parse_statement()?);

        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::While(ast::WhileStatement {
            loc,
            test,
            body,
        }))
    }

    fn parse_do_while_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let body = p(self.parse_statement()?);

        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::RightParen)?;
        self.expect(Token::Semicolon)?;

        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::DoWhile(ast::DoWhileStatement {
            loc,
            test,
            body,
        }))
    }

    fn parse_with_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let object = self.parse_expression()?;
        self.expect(Token::RightParen)?;

        let body = p(self.parse_statement()?);

        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::With(ast::WithStatement {
            loc,
            object,
            body,
        }))
    }

    fn parse_try_statement(&mut self) -> ParseResult<ast::Statement> {
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

            Some(p(ast::CatchClause { loc, param, body }))
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

        Ok(ast::Statement::Try(ast::TryStatement {
            loc,
            block,
            handler,
            finalizer,
        }))
    }

    fn parse_throw_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::Throw(ast::ThrowStatement { loc, argument }))
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = if self.token == Token::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::Return(ast::ReturnStatement {
            loc,
            argument,
        }))
    }

    fn parse_break_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.token == Token::Semicolon {
            None
        } else {
            Some(p(self.parse_identifier()?))
        };

        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::Break(ast::BreakStatement { loc, label }))
    }

    fn parse_continue_statement(&mut self) -> ParseResult<ast::Statement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.token == Token::Semicolon {
            None
        } else {
            Some(p(self.parse_identifier()?))
        };

        self.expect(Token::Semicolon)?;
        let loc = self.mark_loc(start_pos);

        Ok(ast::Statement::Continue(ast::ContinueStatement {
            loc,
            label,
        }))
    }

    /// 13.16 Expression
    fn parse_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_assignment_expression()?;

        if self.token == Token::Comma {
            let mut expressions = vec![*expr];
            while self.token == Token::Comma {
                self.advance()?;
                expressions.push(*self.parse_assignment_expression()?);
            }

            let loc = self.mark_loc(start_pos);

            Ok(p(ast::Expression::Sequence(ast::SequenceExpression {
                loc,
                expressions,
            })))
        } else {
            Ok(expr)
        }
    }

    /// 13.15 AssignmentExpression
    fn parse_assignment_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_conditional_expression()?;

        let assignment_op = match self.token {
            Token::Equals => Some(ast::AssignmentOperator::Equals),
            Token::AddEq => Some(ast::AssignmentOperator::Add),
            Token::SubtractEq => Some(ast::AssignmentOperator::Subtract),
            Token::MultiplyEq => Some(ast::AssignmentOperator::Multiply),
            Token::DivideEq => Some(ast::AssignmentOperator::Divide),
            Token::RemainderEq => Some(ast::AssignmentOperator::Remainder),
            Token::ExponentEq => Some(ast::AssignmentOperator::Exponent),
            Token::AndEq => Some(ast::AssignmentOperator::And),
            Token::OrEq => Some(ast::AssignmentOperator::Or),
            Token::XorEq => Some(ast::AssignmentOperator::Xor),
            Token::ShiftLeftEq => Some(ast::AssignmentOperator::ShiftLeft),
            Token::ShiftRightArithmeticEq => Some(ast::AssignmentOperator::ShiftRightArithmetic),
            Token::ShiftRightLogicalEq => Some(ast::AssignmentOperator::ShiftRightLogical),
            _ => None,
        };

        match assignment_op {
            None => Ok(expr),
            Some(operator) => {
                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                Ok(p(ast::Expression::Assign(ast::AssignmentExpression {
                    loc,
                    left: expr,
                    right,
                    operator,
                })))
            }
        }
    }

    /// 13.14 ConditionalExpression
    fn parse_conditional_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_expression_with_precedence(Precedence::Conditional)?;

        if self.token == Token::Question {
            self.advance()?;
            let conseq = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let altern = self.parse_assignment_expression()?;
            let loc = self.mark_loc(start_pos);

            Ok(p(ast::Expression::Conditional(
                ast::ConditionalExpression {
                    loc,
                    test: expr,
                    conseq,
                    altern,
                },
            )))
        } else {
            Ok(expr)
        }
    }

    /// Precedence parsing for all binary operations and below.
    /// Corresponds to 13.13 ShortCircuitExpression
    fn parse_expression_with_precedence(
        &mut self,
        precedence: Precedence,
    ) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        let mut current_expr = self.parse_expression_prefix()?;
        loop {
            let current_expr_ref = current_expr.as_ref() as *const ast::Expression;
            let next_expr = self.parse_expression_infix(current_expr, precedence, start_pos)?;
            if std::ptr::eq(current_expr_ref, next_expr.as_ref()) {
                return Ok(next_expr);
            }

            current_expr = next_expr;
        }
    }

    fn parse_expression_prefix(&mut self) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            Token::Plus => self.parse_unary_expression(ast::UnaryOperator::Plus),
            Token::Minus => self.parse_unary_expression(ast::UnaryOperator::Minus),
            Token::LogicalNot => self.parse_unary_expression(ast::UnaryOperator::LogicalNot),
            Token::BitwiseNot => self.parse_unary_expression(ast::UnaryOperator::BitwiseNot),
            Token::Typeof => self.parse_unary_expression(ast::UnaryOperator::TypeOf),
            Token::Void => self.parse_unary_expression(ast::UnaryOperator::Void),
            Token::Delete => self.parse_unary_expression(ast::UnaryOperator::Delete),
            Token::Increment => self.parse_update_expression_prefix(ast::UpdateOperator::Increment),
            Token::Decrement => self.parse_update_expression_prefix(ast::UpdateOperator::Decrement),
            _ => self.parse_left_hand_side_expression(),
        }
    }

    fn parse_expression_infix(
        &mut self,
        left: P<ast::Expression>,
        precedence: Precedence,
        start_pos: Pos,
    ) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            // Binary operations
            Token::Plus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Add,
                    Precedence::Addition,
                ),
            Token::Minus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Subtract,
                    Precedence::Addition,
                ),
            Token::Multiply if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Multiply,
                    Precedence::Multiplication,
                ),
            Token::Divide if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Divide,
                    Precedence::Multiplication,
                ),
            Token::Remainder if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Remainder,
                    Precedence::Multiplication,
                ),
            Token::Exponent if precedence.is_weaker_than(Precedence::Exponentiation) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Exponent,
                    // Right associative, so lower precedence
                    Precedence::Multiplication,
                ),
            Token::BitwiseAnd if precedence.is_weaker_than(Precedence::BitwiseAnd) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::And,
                    Precedence::BitwiseAnd,
                ),
            Token::BitwiseOr if precedence.is_weaker_than(Precedence::BitwiseOr) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Or,
                    Precedence::BitwiseOr,
                ),
            Token::BitwiseXor if precedence.is_weaker_than(Precedence::BitwiseXor) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::Xor,
                    Precedence::BitwiseXor,
                ),
            Token::ShiftLeft if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::ShiftLeft,
                    Precedence::Shift,
                ),
            Token::ShiftRightArithmetic if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::ShiftRightArithmetic,
                    Precedence::Shift,
                ),
            Token::ShiftRightLogical if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::ShiftRightLogical,
                    Precedence::Shift,
                ),
            Token::EqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::EqEq,
                    Precedence::Equality,
                ),
            Token::NotEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::NotEq,
                    Precedence::Equality,
                ),
            Token::EqEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::EqEqEq,
                    Precedence::Equality,
                ),
            Token::NotEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::NotEqEq,
                    Precedence::Equality,
                ),
            Token::LessThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::LessThan,
                    Precedence::Relational,
                ),
            Token::LessThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::LessThanOrEqual,
                    Precedence::Relational,
                ),
            Token::GreaterThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::GreaterThan,
                    Precedence::Relational,
                ),
            Token::GreaterThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::GreaterThanOrEqual,
                    Precedence::Relational,
                ),
            Token::In if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::In,
                    Precedence::Relational,
                ),
            Token::InstanceOf if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_pos,
                    ast::BinaryOperator::InstanceOf,
                    Precedence::Relational,
                ),

            // Logical operations
            Token::LogicalAnd if precedence.is_weaker_than(Precedence::LogicalAnd) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    ast::LogicalOperator::And,
                    Precedence::LogicalAnd,
                ),
            Token::LogicalOr if precedence.is_weaker_than(Precedence::LogicalOr) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    ast::LogicalOperator::Or,
                    Precedence::LogicalOr,
                ),
            Token::NullishCoalesce if precedence.is_weaker_than(Precedence::LogicalOr) => self
                .parse_logical_expression(
                    left,
                    start_pos,
                    ast::LogicalOperator::NullishCoalesce,
                    Precedence::LogicalOr,
                ),

            // Update expressions
            Token::Increment if precedence.is_weaker_than(Precedence::PostfixUpdate) => self
                .parse_update_expression_postfix(left, start_pos, ast::UpdateOperator::Increment),
            Token::Decrement if precedence.is_weaker_than(Precedence::PostfixUpdate) => self
                .parse_update_expression_postfix(left, start_pos, ast::UpdateOperator::Decrement),

            // No infix expression
            _ => Ok(left),
        }
    }

    fn parse_binary_expression(
        &mut self,
        left: P<ast::Expression>,
        start_pos: Pos,
        operator: ast::BinaryOperator,
        precedence: Precedence,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(ast::Expression::Binary(ast::BinaryExpression {
            loc,
            left,
            right,
            operator,
        })))
    }

    fn parse_logical_expression(
        &mut self,
        left: P<ast::Expression>,
        start_pos: Pos,
        operator: ast::LogicalOperator,
        precedence: Precedence,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(ast::Expression::Logical(ast::LogicalExpression {
            loc,
            left,
            right,
            operator,
        })))
    }

    fn parse_update_expression_prefix(
        &mut self,
        operator: ast::UpdateOperator,
    ) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(ast::Expression::Update(ast::UpdateExpression {
            loc,
            operator,
            argument,
            is_prefix: true,
        })))
    }

    fn parse_update_expression_postfix(
        &mut self,
        argument: P<ast::Expression>,
        start_pos: Pos,
        operator: ast::UpdateOperator,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(p(ast::Expression::Update(ast::UpdateExpression {
            loc,
            operator,
            argument,
            is_prefix: false,
        })))
    }

    fn parse_unary_expression(
        &mut self,
        operator: ast::UnaryOperator,
    ) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(ast::Expression::Unary(ast::UnaryExpression {
            loc,
            operator,
            argument,
        })))
    }

    /// 13.3 LeftHandSideExpression
    fn parse_left_hand_side_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        let start_pos = self.current_start_pos();
        let expr = match &self.token {
            Token::New => self.parse_new_expression()?,
            _ => self.parse_primary_expression()?,
        };

        self.parse_call_expression(expr, start_pos, true)
    }

    fn parse_call_expression(
        &mut self,
        expr: P<ast::Expression>,
        start_pos: Pos,
        allow_call: bool,
    ) -> ParseResult<P<ast::Expression>> {
        let expr = self.parse_member_expression(expr, start_pos, allow_call)?;
        match self.token {
            Token::LeftParen if allow_call => {
                let arguments = self.parse_call_arguments()?;
                let loc = self.mark_loc(start_pos);

                let call_expr = p(ast::Expression::Call(ast::CallExpression {
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
        expr: P<ast::Expression>,
        start_pos: Pos,
        allow_call: bool,
    ) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            Token::Period => {
                self.advance()?;
                let property = self.parse_identifier()?;
                let loc = self.mark_loc(start_pos);

                let member_expr = p(ast::Expression::Member(ast::MemberExpression {
                    loc,
                    object: expr,
                    property: p(ast::Expression::Id(property)),
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

                let member_expr = p(ast::Expression::Member(ast::MemberExpression {
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

    fn parse_new_expression(&mut self) -> ParseResult<P<ast::Expression>> {
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
        Ok(p(ast::Expression::New(ast::NewExpression {
            loc,
            callee,
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<ast::Expression>> {
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
    fn parse_primary_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            Token::Identifier(_) => Ok(p(ast::Expression::Id(self.parse_identifier()?))),
            Token::Null => {
                let loc = self.loc;
                self.advance()?;
                Ok(p(ast::Expression::Null(loc)))
            }
            Token::True | Token::False => {
                let value = self.token == Token::True;
                let loc = self.loc;
                self.advance()?;
                Ok(p(ast::Expression::Boolean(ast::BooleanLiteral {
                    loc,
                    value,
                })))
            }
            Token::NumberLiteral(value) => {
                let loc = self.loc;
                let value = value.clone();
                self.advance()?;
                Ok(p(ast::Expression::Number(ast::NumberLiteral {
                    loc,
                    value,
                })))
            }
            Token::StringLiteral(value) => {
                let loc = self.loc;
                let value = value.clone();
                self.advance()?;
                Ok(p(ast::Expression::String(ast::StringLiteral {
                    loc,
                    value,
                })))
            }
            Token::This => {
                let loc = self.loc;
                self.advance()?;
                Ok(p(ast::Expression::This(loc)))
            }
            Token::LeftBracket => self.parse_array_expression(),
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<ast::Identifier> {
        match &self.token {
            Token::Identifier(name) => {
                let loc = self.loc;
                let name = name.clone();
                self.advance()?;
                Ok(ast::Identifier { loc, name })
            }
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    fn parse_array_expression(&mut self) -> ParseResult<P<ast::Expression>> {
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

        Ok(p(ast::Expression::Array(ast::ArrayExpression {
            loc,
            elements,
        })))
    }

    fn parse_pattern(&mut self) -> ParseResult<ast::Pattern> {
        match &self.token {
            Token::Identifier(_) => Ok(ast::Pattern::Id(self.parse_identifier()?)),
            other => self.error_unexpected_token(self.loc, other),
        }
    }
}

pub fn parse_file(source: &Rc<Source>) -> ParseResult<ast::Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(parser.parse_program()?)
}
