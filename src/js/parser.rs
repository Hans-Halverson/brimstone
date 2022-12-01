use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use super::ast::{self, P};
use super::lexer::Lexer;
use super::loc::{find_line_col_for_pos, Loc, EMPTY_LOC};
use super::source::Source;
use super::token::Token;

#[derive(Debug)]
pub enum ParseError {
    Io(io::Error),
    UnknownToken(String),
    UnexpectedToken(Token),
    ExpectedToken(Token, Token),
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
    Call = 0, // Includes member access, new with argments
    New = 1,  // Without arguments
    PostfixUpdate = 2,
    Unary = 3, // Includes prefix update
    Exponentiation = 4,
    Multiplication = 5,
    Addition = 6,
    Shift = 7,
    Relational = 8, // Includes in and instanceof
    Equality = 9,
    BitwiseAnd = 10,
    BitwiseXor = 11,
    BitwiseOr = 12,
    LogicalAnd = 13,
    LogicalOr = 14,  // Includes nullish coalescing
    Assignment = 15, // Includes conditional, arrow, yield
    Sequence = 16,
    None = 17,
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

    fn mark_loc(&self, start_loc: Loc) -> Loc {
        Loc {
            start: start_loc.start,
            end: self.prev_loc.end,
        }
    }

    fn parse_program(&mut self) -> ParseResult<ast::Program> {
        let mut toplevels = vec![];

        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let start_loc = Loc { start: 0, end: 0 };
        let loc = self.mark_loc(start_loc);

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
            // Anything else must be an expression statement
            _ => {
                let start_loc = self.loc;
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                let loc = self.mark_loc(start_loc);

                Ok(ast::Statement::Expr(ast::ExpressionStatement { loc, expr }))
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> ParseResult<ast::VariableDeclaration> {
        let start_loc = self.loc;
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
            let start_loc = self.loc;
            let id = self.parse_pattern()?;
            self.expect(Token::Equals)?;
            let init = self.parse_expression()?;
            let loc = self.mark_loc(start_loc);

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

        let loc = self.mark_loc(start_loc);

        Ok(ast::VariableDeclaration {
            loc,
            kind,
            declarations,
        })
    }

    fn parse_expression(&mut self) -> ParseResult<P<ast::Expression>> {
        self.parse_exression_with_precedence(Precedence::None)
    }

    fn parse_exression_with_precedence(
        &mut self,
        precedence: Precedence,
    ) -> ParseResult<P<ast::Expression>> {
        let start_loc = self.loc;
        let mut current_expr = self.parse_expression_prefix()?;
        loop {
            let current_expr_ref = current_expr.as_ref() as *const ast::Expression;
            let next_expr = self.parse_expression_infix(current_expr, precedence, start_loc)?;
            if std::ptr::eq(current_expr_ref, next_expr.as_ref()) {
                return Ok(next_expr);
            }

            current_expr = next_expr;
        }
    }

    fn parse_expression_prefix(&mut self) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            Token::Identifier(name) => {
                let start_loc = self.loc;
                let name = name.clone();
                self.advance()?;
                let loc = self.mark_loc(start_loc);
                Ok(p(ast::Expression::Id(ast::Identifier { loc, name })))
            }
            Token::Increment => self.parse_update_expression_prefix(ast::UpdateOperator::Increment),
            Token::Decrement => self.parse_update_expression_prefix(ast::UpdateOperator::Decrement),
            Token::Plus => self.parse_unary_expression(ast::UnaryOperator::Plus),
            Token::Minus => self.parse_unary_expression(ast::UnaryOperator::Minus),
            Token::LogicalNot => self.parse_unary_expression(ast::UnaryOperator::LogicalNot),
            Token::BitwiseNot => self.parse_unary_expression(ast::UnaryOperator::BitwiseNot),
            Token::Typeof => self.parse_unary_expression(ast::UnaryOperator::TypeOf),
            Token::Void => self.parse_unary_expression(ast::UnaryOperator::Void),
            Token::Delete => self.parse_unary_expression(ast::UnaryOperator::Delete),
            other => self.error_unexpected_token(self.loc, other),
        }
    }

    fn parse_expression_infix(
        &mut self,
        left: P<ast::Expression>,
        precedence: Precedence,
        start_loc: Loc,
    ) -> ParseResult<P<ast::Expression>> {
        match &self.token {
            Token::Plus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Add,
                    Precedence::Addition,
                ),
            Token::Minus if precedence.is_weaker_than(Precedence::Addition) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Subtract,
                    Precedence::Addition,
                ),
            Token::Multiply if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Multiply,
                    Precedence::Multiplication,
                ),
            Token::Divide if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Divide,
                    Precedence::Multiplication,
                ),
            Token::Remainder if precedence.is_weaker_than(Precedence::Multiplication) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Remainder,
                    Precedence::Multiplication,
                ),
            Token::Exponent if precedence.is_weaker_than(Precedence::Exponentiation) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Exponent,
                    // Right associative, so lower precedence
                    Precedence::Multiplication,
                ),
            Token::BitwiseAnd if precedence.is_weaker_than(Precedence::BitwiseAnd) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::And,
                    Precedence::BitwiseAnd,
                ),
            Token::BitwiseOr if precedence.is_weaker_than(Precedence::BitwiseOr) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Or,
                    Precedence::BitwiseOr,
                ),
            Token::BitwiseXor if precedence.is_weaker_than(Precedence::BitwiseXor) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::Xor,
                    Precedence::BitwiseXor,
                ),
            Token::ShiftLeft if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::ShiftLeft,
                    Precedence::Shift,
                ),
            Token::ShiftRightArithmetic if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::ShiftRightArithmetic,
                    Precedence::Shift,
                ),
            Token::ShiftRightLogical if precedence.is_weaker_than(Precedence::Shift) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::ShiftRightLogical,
                    Precedence::Shift,
                ),
            Token::EqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::EqEq,
                    Precedence::Equality,
                ),
            Token::NotEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::NotEq,
                    Precedence::Equality,
                ),
            Token::EqEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::EqEqEq,
                    Precedence::Equality,
                ),
            Token::NotEqEq if precedence.is_weaker_than(Precedence::Equality) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::NotEqEq,
                    Precedence::Equality,
                ),
            Token::LessThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::LessThan,
                    Precedence::Relational,
                ),
            Token::LessThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::LessThanOrEqual,
                    Precedence::Relational,
                ),
            Token::GreaterThan if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::GreaterThan,
                    Precedence::Relational,
                ),
            Token::GreaterThanOrEqual if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::GreaterThanOrEqual,
                    Precedence::Relational,
                ),
            Token::In if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::In,
                    Precedence::Relational,
                ),
            Token::InstanceOf if precedence.is_weaker_than(Precedence::Relational) => self
                .parse_binary_expression(
                    left,
                    start_loc,
                    ast::BinaryOperator::InstanceOf,
                    Precedence::Relational,
                ),
            Token::Increment if precedence.is_weaker_than(Precedence::PostfixUpdate) => self
                .parse_update_expression_postfix(left, start_loc, ast::UpdateOperator::Increment),
            Token::Decrement if precedence.is_weaker_than(Precedence::PostfixUpdate) => self
                .parse_update_expression_postfix(left, start_loc, ast::UpdateOperator::Decrement),
            Token::Equals if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Equals)
            }
            Token::AddEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Add)
            }
            Token::SubtractEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Subtract)
            }
            Token::MultiplyEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Multiply)
            }
            Token::DivideEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Divide)
            }
            Token::RemainderEq if precedence.is_weaker_than(Precedence::Assignment) => self
                .parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Remainder),
            Token::ExponentEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Exponent)
            }
            Token::AndEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::And)
            }
            Token::OrEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Or)
            }
            Token::XorEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(left, start_loc, ast::AssignmentOperator::Xor)
            }
            Token::ShiftLeftEq if precedence.is_weaker_than(Precedence::Assignment) => self
                .parse_assignment_expression(left, start_loc, ast::AssignmentOperator::ShiftLeft),
            Token::ShiftRightArithmeticEq if precedence.is_weaker_than(Precedence::Assignment) => {
                self.parse_assignment_expression(
                    left,
                    start_loc,
                    ast::AssignmentOperator::ShiftRightArithmetic,
                )
            }
            Token::ShiftRightLogicalEq if precedence.is_weaker_than(Precedence::Assignment) => self
                .parse_assignment_expression(
                    left,
                    start_loc,
                    ast::AssignmentOperator::ShiftRightLogical,
                ),
            _ => Ok(left),
        }
    }

    fn parse_binary_expression(
        &mut self,
        left: P<ast::Expression>,
        start_loc: Loc,
        operator: ast::BinaryOperator,
        precedence: Precedence,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        let right = self.parse_exression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_loc);

        Ok(p(ast::Expression::Binary(ast::BinaryExpression {
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
        let start_loc = self.loc;
        self.advance()?;
        let argument = self.parse_exression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_loc);

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
        start_loc: Loc,
        operator: ast::UpdateOperator,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        let loc = self.mark_loc(start_loc);

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
        let start_loc = self.loc;
        self.advance()?;
        let argument = self.parse_exression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_loc);

        Ok(p(ast::Expression::Unary(ast::UnaryExpression {
            loc,
            operator,
            argument,
        })))
    }

    fn parse_assignment_expression(
        &mut self,
        left: P<ast::Expression>,
        start_loc: Loc,
        operator: ast::AssignmentOperator,
    ) -> ParseResult<P<ast::Expression>> {
        self.advance()?;
        // Right associative, so lower precedence
        let right = self.parse_exression_with_precedence(Precedence::Sequence)?;
        let loc = self.mark_loc(start_loc);

        Ok(p(ast::Expression::Assign(ast::AssignmentExpression {
            loc,
            left,
            right,
            operator,
        })))
    }

    fn parse_pattern(&mut self) -> ParseResult<ast::Pattern> {
        let start_loc = self.loc;
        match &self.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance()?;
                let loc = self.mark_loc(start_loc);
                Ok(ast::Pattern::Id(ast::Identifier { loc, name }))
            }
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
