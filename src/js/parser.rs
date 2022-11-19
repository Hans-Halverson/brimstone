use std::error::Error;
use std::rc::Rc;
use std::{fmt, io};

use super::ast;
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

fn p<T>(node: T) -> ast::P<T> {
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
            other => self.error_unexpected_token(self.loc, other),
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
            other => self.error_unexpected_token(self.loc, other),
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
