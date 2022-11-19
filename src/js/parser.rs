use std::error::Error;
use std::{fmt, io};

use super::ast;
use super::lexer::Lexer;
use super::loc::{Loc, EMPTY_LOC};
use super::token::Token;

#[derive(Debug)]
pub enum ParseError {
    Io(io::Error),
    UnknownToken(String),
    UnexpectedToken(Token),
    ExpectedToken(Token, Token),
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Io(io_error) => io_error.fmt(f),
            ParseError::UnknownToken(token) => write!(f, "Unknown token {}", token),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token {}", token),
            ParseError::ExpectedToken(actual, expected) => {
                write!(f, "Unexpected token {}, expected {}", actual, expected)
            }
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> ParseError {
        ParseError::Io(err)
    }
}

fn error_unexpected_token<T>(token: &Token) -> ParseResult<T> {
    Err(ParseError::UnexpectedToken(token.clone()))
}

fn p<T>(node: T) -> ast::P<T> {
    Box::new(node)
}

struct Parser {
    lexer: Lexer,
    token: Token,
    loc: Loc,
}

impl Parser {
    // Must prime parser by calling advance before using.
    fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            token: Token::Eof,
            loc: EMPTY_LOC,
        }
    }

    fn advance(&mut self) -> ParseResult<()> {
        let (token, loc) = self.lexer.next()?;
        self.token = token;
        self.loc = loc;

        Ok(())
    }

    fn expect(&mut self, token: Token) -> ParseResult<()> {
        if self.token != token {
            return Err(ParseError::ExpectedToken(self.token.clone(), token));
        }

        self.advance()?;
        Ok(())
    }

    fn mark_loc(&self, start_loc: Loc) -> Loc {
        Loc {
            start: start_loc.start,
            end: self.loc.end,
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
            other => error_unexpected_token(other),
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
                init: Some(p(init)),
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

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        let start_loc = self.loc;
        match &self.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance()?;
                let loc = self.mark_loc(start_loc);
                Ok(ast::Expression::Id(ast::Identifier { loc, name }))
            }
            other => error_unexpected_token(other),
        }
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
            other => error_unexpected_token(other),
        }
    }
}

pub fn parse_file(file_name: &str) -> ParseResult<ast::Program> {
    // Create and prime parser
    let lexer = Lexer::new(file_name)?;
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(parser.parse_program()?)
}
