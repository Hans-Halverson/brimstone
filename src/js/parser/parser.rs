use std::rc::Rc;

use crate::js::common::wtf_8::Wtf8String;

use super::ast::*;
use super::lexer::{Lexer, SavedLexerState};
use super::lexer_stream::Utf8LexerStream;
use super::loc::{Loc, Pos, EMPTY_LOC};
use super::parse_error::{LocalizedParseError, ParseError, ParseResult};
use super::regexp_parser::RegExpParser;
use super::source::Source;
use super::token::Token;

// Arbitrary error used to fail try parse
const FAIL_TRY_PARSED_ERROR: ParseError = ParseError::MalformedNumericLiteral;

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
    LogicalOr = 12,
    // Lower precedence than both logical operators so that we can check for unparenthesized child
    // logical operators after parsing a nullish coalesce expression. Exact precedence does not matter
    // in comparison to logical operators as they are not allowed to be directly next to each other.
    NullishCoalesce = 13,
    Conditional = 14,
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

struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
    // Whether the parser is currently parsing in strict mode
    in_strict_mode: bool,
    // Whether the parser is currently in a context where an await expression is allowed
    allow_await: bool,
    // Whether the parser is currently in a context where a yield expression is allowed
    allow_yield: bool,
    // Whether the parser is currently in a context where an in expression is allowed
    allow_in: bool,
}

/// A save point for the parser, can be used to restore the parser to a particular position.
struct ParserSaveState {
    saved_lexer_state: SavedLexerState,
    token: Token,
    loc: Loc,
    prev_loc: Loc,
    in_strict_mode: bool,
    allow_await: bool,
    allow_yield: bool,
    allow_in: bool,
}

fn swap_and_save<T: Copy>(reference: &mut T, new_value: T) -> T {
    let old_value = *reference;
    *reference = new_value;
    old_value
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
            allow_await: false,
            allow_yield: false,
            allow_in: true,
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
            allow_await: self.allow_await,
            allow_yield: self.allow_yield,
            allow_in: self.allow_in,
        }
    }

    fn restore(&mut self, save_state: ParserSaveState) {
        self.lexer.restore(&save_state.saved_lexer_state);
        self.token = save_state.token;
        self.loc = save_state.loc;
        self.prev_loc = save_state.prev_loc;
        self.set_in_strict_mode(save_state.in_strict_mode);
        self.allow_await = save_state.allow_await;
        self.allow_yield = save_state.allow_yield;
        self.allow_in = save_state.allow_in;
    }

    fn set_in_strict_mode(&mut self, in_strict_mode: bool) {
        self.in_strict_mode = in_strict_mode;
        self.lexer.in_strict_mode = in_strict_mode;
    }

    fn prevent_hashbang_comment(&mut self) {
        self.lexer.allow_hashbang_comment = false;
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

    fn advance_template_part(&mut self) -> ParseResult<()> {
        let (token, loc) = self.lexer.next_template_part()?;
        self.prev_loc = self.loc;
        self.token = token;
        self.loc = loc;

        Ok(())
    }

    fn advance_regexp_literal(&mut self) -> ParseResult<()> {
        let (token, loc) = self.lexer.next_regexp_literal()?;
        self.prev_loc = self.loc;
        self.token = token;
        self.loc = loc;

        Ok(())
    }

    fn peek(&mut self) -> ParseResult<Token> {
        let lexer_state = self.lexer.save();
        let (token, _) = self.lexer.next()?;
        self.lexer.restore(&lexer_state);

        Ok(token)
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

    fn parse_script(&mut self, initial_state: ParserSaveState) -> ParseResult<Program> {
        let has_use_strict_directive = self.parse_directive_prologue()?;

        // Restore to initial state, then reparse directives with strict mode properly set
        self.restore(initial_state);

        if has_use_strict_directive {
            self.set_in_strict_mode(true);
        }

        // Re-prime the parser
        self.advance()?;

        let mut toplevels = vec![];
        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        Ok(Program::new(
            loc,
            toplevels,
            ProgramKind::Script,
            self.lexer.source.clone(),
            self.in_strict_mode,
            has_use_strict_directive,
        ))
    }

    fn parse_directive_prologue(&mut self) -> ParseResult<bool> {
        // Parse a sequence of string literals followed by semicolons. If any is the literal
        // "use strict" then we are in strict mode. Be sure to restore to original state
        // afterwards, as earlier literals may need to be reparsed in strict mode.
        let mut has_use_strict_directive = false;

        while let Token::StringLiteral(str) = &self.token {
            // Use strict directive cannot have any escape characters in it, so check length
            let is_unscaped_use_strict = str == "use strict" && self.loc.end - self.loc.start == 12;

            self.advance()?;

            if !self.maybe_expect_semicolon()? {
                break;
            }

            if is_unscaped_use_strict {
                has_use_strict_directive = true;
            }
        }

        Ok(has_use_strict_directive)
    }

    fn parse_module(&mut self) -> ParseResult<Program> {
        // Modules are always in strict mode
        self.set_in_strict_mode(true);

        // Allow top level await
        self.allow_await = true;

        let mut toplevels = vec![];
        while self.token != Token::Eof {
            match self.token {
                Token::Import => toplevels.push(self.parse_import_declaration()?),
                Token::Export => toplevels.push(self.parse_export_declaration()?),
                _ => toplevels.push(self.parse_toplevel()?),
            }
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        Ok(Program::new(
            loc,
            toplevels,
            ProgramKind::Module,
            self.lexer.source.clone(),
            self.in_strict_mode,
            /* has_use_strict_directive */ false,
        ))
    }

    fn parse_toplevel(&mut self) -> ParseResult<Toplevel> {
        let stmt = self.parse_statement_list_item()?;
        Ok(Toplevel::Statement(stmt))
    }

    fn parse_statement_list_item(&mut self) -> ParseResult<Statement> {
        match self.token {
            Token::Const => Ok(Statement::VarDecl(self.parse_variable_declaration(false)?)),
            Token::Let => {
                if self.is_let_declaration_start()? {
                    Ok(Statement::VarDecl(self.parse_variable_declaration(false)?))
                } else {
                    self.parse_statement()
                }
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
                    if self.peek()? == Token::LeftBracket {
                        return self.error(let_loc, ParseError::AmbiguousLetBracket);
                    }
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
            Token::Async => Ok(self.peek()? == Token::Function),
            _ => Ok(false),
        }
    }

    /// Called when the current token is "let", return true if this is the start of a let
    /// declaration. Otherwise this is the start of an expression or expression statement.
    fn is_let_declaration_start(&mut self) -> ParseResult<bool> {
        if self.in_strict_mode {
            Ok(true)
        } else {
            let next_token = self.peek()?;

            // All tokens that can appear at the start of a pattern
            let is_decl_start = next_token == Token::LeftBrace
                || next_token == Token::LeftBracket
                // Await and yield are allowed as identifiers here to prevent ASI in cases such as:
                //   let
                //   await 0;
                || Self::is_identifier(
                    &next_token,
                    /* in_strict_mode */ false,
                    /* allow_await */ false,
                    /* allow_yield */ false,
                );

            Ok(is_decl_start)
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

        // For declarations name is required and await/yield inherited from surrounding context
        let mut id = None;
        if is_decl {
            id = Some(p(self.parse_binding_identifier()?))
        }

        // Enter async/generator context for parsing the function arguments and body
        let did_allow_await = swap_and_save(&mut self.allow_await, is_async);
        let did_allow_yield = swap_and_save(&mut self.allow_yield, is_generator);

        // For expressions name is optional and is within this function's await/yield context
        if !is_decl && self.token != Token::LeftParen {
            id = Some(p(self.parse_binding_identifier()?))
        }

        let params = self.parse_function_params()?;
        let (block, has_use_strict_directive, is_strict_mode) = self.parse_function_block_body()?;
        let body = p(FunctionBody::Block(block));
        let loc = self.mark_loc(start_pos);

        let func = Function::new(
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
            is_strict_mode,
            has_use_strict_directive,
        );

        self.allow_await = did_allow_await;
        self.allow_yield = did_allow_yield;

        Ok(func)
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<FunctionParam>> {
        // Read all function params between the parentheses
        let mut params = vec![];
        self.expect(Token::LeftParen)?;

        while self.token != Token::RightParen {
            if self.token == Token::Spread {
                let rest_element = self.parse_rest_element()?;
                params.push(FunctionParam::Rest(rest_element));

                // Trailing commas are not allowed after rest elements, nor are any other params
                if self.token == Token::Comma {
                    return self.error(self.loc, ParseError::RestTrailingComma);
                } else {
                    break;
                }
            }

            let pattern = self.parse_pattern_including_assignment_pattern()?;
            params.push(FunctionParam::Pattern(pattern));

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightParen)?;

        Ok(params)
    }

    fn parse_function_params_without_parens(&mut self) -> ParseResult<Vec<FunctionParam>> {
        // Read all function params until EOF
        let mut params = vec![];

        while self.token != Token::Eof {
            if self.token == Token::Spread {
                let rest_element = self.parse_rest_element()?;
                params.push(FunctionParam::Rest(rest_element));

                // Trailing commas are not allowed after rest elements, nor are any other params
                if self.token == Token::Comma {
                    return self.error(self.loc, ParseError::RestTrailingComma);
                } else {
                    break;
                }
            }

            let pattern = self.parse_pattern_including_assignment_pattern()?;
            params.push(FunctionParam::Pattern(pattern));

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_function_block_body(&mut self) -> ParseResult<(Block, bool, bool)> {
        let start_pos = self.current_start_pos();

        // Save state before the first potential directive token is lexed
        let before_directive_state = self.save();

        self.expect(Token::LeftBrace)?;

        let has_use_strict_directive = self.parse_directive_prologue()?;

        // Restore to state before first directive token, then reparse directives with strict mode set
        self.restore(before_directive_state);

        // Enter strict mode if applicable, saving strict mode context from before this function
        let old_in_strict_mode = self.in_strict_mode;
        if has_use_strict_directive {
            self.set_in_strict_mode(true);
        }

        // Advance past left brace again
        self.advance()?;

        let mut body = vec![];
        while self.token != Token::RightBrace {
            body.push(self.parse_statement_list_item()?)
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        // Restore to strict mode context from before this function
        let is_strict_mode = self.in_strict_mode;
        self.set_in_strict_mode(old_in_strict_mode);

        Ok((Block::new(loc, body), has_use_strict_directive, is_strict_mode))
    }

    fn parse_function_body_statements(
        &mut self,
        initial_state: ParserSaveState,
    ) -> ParseResult<Vec<Statement>> {
        let has_use_strict_directive = self.parse_directive_prologue()?;

        // Restore to initial state, then reparse directives with strict mode properly set
        self.restore(initial_state);

        // Enter strict mode if applicable, saving strict mode context from before this function
        let old_in_strict_mode = self.in_strict_mode;
        if has_use_strict_directive {
            self.set_in_strict_mode(true);
        }

        // Re-prime the parser
        self.advance()?;

        let mut body = vec![];
        while self.token != Token::Eof {
            body.push(self.parse_statement_list_item()?)
        }

        // Restore to strict mode context from before this function
        self.set_in_strict_mode(old_in_strict_mode);

        Ok(body)
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

        // Optional await keyword signifies a for-await-of
        let await_loc = self.loc;
        let is_await = self.allow_await && self.token == Token::Await;
        if is_await {
            self.advance()?;
        }

        self.expect(Token::LeftParen)?;

        // Both for and for each loops can start with a variable declaration
        let is_var_decl = self.token == Token::Var
            || self.token == Token::Const
            || (self.token == Token::Let && self.is_let_declaration_start()?);

        // Init statement, if it exists
        let for_stmt = if is_var_decl {
            // Restrict "in" when parsing for init
            let old_allow_in = swap_and_save(&mut self.allow_in, false);
            let var_decl = self.parse_variable_declaration(true)?;
            self.allow_in = old_allow_in;

            match self.token {
                Token::In | Token::Of => {
                    // Var decl must consist of a single declaration with no initializer to
                    // match the `var ForBinding` and `ForDeclaration` productions.
                    if var_decl.declarations.len() != 1 || var_decl.declarations[0].init.is_some() {
                        return self.error(var_decl.loc, ParseError::ForEachInitInvalidVarDecl);
                    }

                    let init = p(ForEachInit::VarDecl(var_decl));
                    self.parse_for_each_statement(init, start_pos, is_await)?
                }
                _ => {
                    let init = Some(p(ForInit::VarDecl(var_decl)));
                    self.expect(Token::Semicolon)?;
                    self.parse_for_statement(init, start_pos)?
                }
            }
        } else if self.token == Token::Semicolon {
            // Empty init, but we know this is a regular for loop
            self.advance()?;
            self.parse_for_statement(None, start_pos)?
        } else {
            // Otherwise init is an expression
            let expr_start_pos = self.current_start_pos();

            // Restrict "in" when parsing for init
            let old_allow_in = swap_and_save(&mut self.allow_in, false);
            let expr = self.parse_expression()?;
            self.allow_in = old_allow_in;

            match (self.token.clone(), *expr) {
                // If this is a for each loop the parsed expression must actually be a pattern
                (Token::In, expr) | (Token::Of, expr) => {
                    let pattern =
                        self.reparse_expression_as_for_left_hand_side(expr, expr_start_pos)?;
                    let left = p(ForEachInit::Pattern(pattern));
                    self.parse_for_each_statement(left, start_pos, is_await)?
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

                    Statement::ForEach(ForEachStatement {
                        loc,
                        kind: ForEachKind::In,
                        left,
                        right,
                        body,
                        is_await: false,
                    })
                }
                // Otherwise this is a regular for loop and the expression is used directly
                (_, expr) => {
                    let init = Some(p(ForInit::Expression(expr)));
                    self.expect(Token::Semicolon)?;
                    self.parse_for_statement(init, start_pos)?
                }
            }
        };

        if is_await {
            match &for_stmt {
                Statement::ForEach(ForEachStatement { kind: ForEachKind::Of, .. }) => {}
                _ => return self.error(await_loc, ParseError::InvalidForAwait),
            }
        }

        Ok(for_stmt)
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
        is_await: bool,
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

        Ok(Statement::ForEach(ForEachStatement { loc, kind, left, right, body, is_await }))
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
                // Preserve arrow function errors
                err @ Err(LocalizedParseError { error: ParseError::ArrowOnNewLine, .. }) => err,
                // Error as if parsing a non-arrow assignment if neither match
                Err(_) => Err(err),
            },
        }
    }

    fn parse_non_arrow_assignment_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();

        if self.allow_yield && self.token == Token::Yield {
            return Ok(p(Expression::Yield(self.parse_yield_expression()?)));
        }

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
            Token::LogicalAndEq => Some(AssignmentOperator::LogicalAnd),
            Token::LogicalOrEq => Some(AssignmentOperator::LogicalOr),
            Token::NullishCoalesceEq => Some(AssignmentOperator::NullishCoalesce),
            _ => None,
        };

        let result = match assignment_op {
            None => Ok(expr),
            Some(operator) => {
                let left = if operator == AssignmentOperator::Equals {
                    p(self.reparse_expression_as_assignment_left_hand_side(*expr, start_pos)?)
                } else {
                    p(self.reparse_expression_as_operator_assignment_left_hand_side(
                        *expr, start_pos,
                    )?)
                };

                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::Assign(AssignmentExpression {
                    loc,
                    left,
                    right,
                    operator,
                    is_parenthesized: false,
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
                let async_id = Identifier { loc: async_loc, name: "async".to_owned() };
                let params = vec![FunctionParam::Pattern(Pattern::Id(async_id))];
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
                vec![FunctionParam::Pattern(Pattern::Id(id))]
            }
        };

        if self.token == Token::Arrow {
            if self.lexer.is_new_line_before_current() {
                return self.error(self.loc, ParseError::ArrowOnNewLine);
            }

            self.advance()?;
        } else {
            self.expect(Token::Arrow)?;
        }

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

    fn parse_yield_expression(&mut self) -> ParseResult<YieldExpression> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Check for the end of an assignment expression, either due to ASI or encountering a token
        // that signals the end of an assignment expression.
        if self.maybe_expect_semicolon()? {
            let loc = self.mark_loc(start_pos);
            return Ok(YieldExpression { loc, argument: None, is_delegate: false });
        }

        let is_assignment_expression_end = match self.token {
            Token::RightParen
            | Token::RightBracket
            | Token::Comma
            | Token::Colon
            | Token::In
            | Token::Of => true,
            _ => false,
        };
        if is_assignment_expression_end {
            let loc = self.mark_loc(start_pos);
            return Ok(YieldExpression { loc, argument: None, is_delegate: false });
        }

        let is_delegate = self.token == Token::Multiply;
        if is_delegate {
            self.advance()?;
        }

        let argument = Some(self.parse_assignment_expression()?);
        let loc = self.mark_loc(start_pos);

        return Ok(YieldExpression { loc, argument, is_delegate });
    }

    /// 13.14 ConditionalExpression
    fn parse_conditional_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_expression_with_precedence(Precedence::Conditional)?;

        if self.token == Token::Question {
            self.advance()?;

            // In expressions can always appear in consequent
            let old_allow_in = swap_and_save(&mut self.allow_in, true);
            let conseq = self.parse_assignment_expression()?;
            self.allow_in = old_allow_in;

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

            if self.token == Token::In
                && precedence.is_weaker_than(Precedence::Relational)
                && self.allow_in
            {
                return self.parse_binary_expression(
                    private_name,
                    start_pos,
                    BinaryOperator::InPrivate,
                    Precedence::Relational,
                );
            }

            return self.error_unexpected_token(self.loc, &self.token);
        }

        // In expressions can always appear in (unary > LHS > primary) expressions in recursive
        // calls below this point. For example in grouped expressions, functions/classes, objects
        // initializers, array initializers, call arguments, etc.
        let old_allow_in = swap_and_save(&mut self.allow_in, true);
        let mut current_expr = self.parse_expression_prefix()?;
        self.allow_in = old_allow_in;

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
            Token::Await if self.allow_await => self.parse_await_expression(),
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
            Token::In if precedence.is_weaker_than(Precedence::Relational) && self.allow_in => self
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
            Token::NullishCoalesce if precedence.is_weaker_than(Precedence::NullishCoalesce) => {
                self.parse_nullish_coalesce_expression(left, start_pos)
            }
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

    fn parse_nullish_coalesce_expression(
        &mut self,
        left: P<Expression>,
        start_pos: Pos,
    ) -> ParseResult<P<Expression>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(Precedence::NullishCoalesce)?;
        let loc = self.mark_loc(start_pos);

        // Check if mixed with an unparenthesized logical left or right expression. Can check for
        // parens by comparing parent expression start/end pos to inner expression start/end pos.
        if let Expression::Logical(left_expr) = left.as_ref() {
            if left_expr.operator != LogicalOperator::NullishCoalesce
                && left_expr.loc.start == loc.start
            {
                return self.error(loc, ParseError::NullishCoalesceMixedWithLogical);
            }
        } else if let Expression::Logical(right_expr) = right.as_ref() {
            if right_expr.operator != LogicalOperator::NullishCoalesce
                && right_expr.loc.end == loc.end
            {
                return self.error(loc, ParseError::NullishCoalesceMixedWithLogical);
            }
        }

        Ok(p(Expression::Logical(LogicalExpression {
            loc,
            left,
            right,
            operator: LogicalOperator::NullishCoalesce,
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

        if !self.is_valid_assignment_target(argument.as_ref()) {
            return self.error(loc, ParseError::InvalidUpdateExpressionArgument);
        }

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

        if !self.is_valid_assignment_target(argument.as_ref()) {
            return self.error(loc, ParseError::InvalidUpdateExpressionArgument);
        }

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

        // Unary expressions cannot be followed directly by an exponentiation expression
        if self.token == Token::Exponent {
            return self.error(loc, ParseError::ExponentLHSUnary);
        }

        Ok(p(Expression::Unary(UnaryExpression { loc, operator, argument })))
    }

    fn parse_await_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Await(AwaitExpression { loc, argument })))
    }

    /// 13.3 LeftHandSideExpression
    fn parse_left_hand_side_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        let expr = match &self.token {
            Token::New => self.parse_new_expression()?,
            Token::Super => self.parse_super_expression()?,
            Token::Import => self.parse_import_expression()?,
            _ => self.parse_primary_expression()?,
        };

        self.parse_call_expression(
            expr, start_pos, /* allow_call */ true, /* in_optional_chain */ false,
        )
    }

    fn parse_call_expression(
        &mut self,
        expr: P<Expression>,
        start_pos: Pos,
        allow_call: bool,
        in_optional_chain: bool,
    ) -> ParseResult<P<Expression>> {
        let expr = self.parse_member_expression(expr, start_pos, allow_call, in_optional_chain)?;
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

                self.parse_call_expression(call_expr, start_pos, allow_call, in_optional_chain)
            }
            _ => Ok(expr),
        }
    }

    fn parse_member_expression(
        &mut self,
        expr: P<Expression>,
        start_pos: Pos,
        allow_call: bool,
        in_optional_chain: bool,
    ) -> ParseResult<P<Expression>> {
        match &self.token {
            Token::Period => {
                self.advance()?;

                let member_expr = self.parse_property_member_expression(expr, start_pos, false)?;

                self.parse_call_expression(member_expr, start_pos, allow_call, in_optional_chain)
            }
            Token::LeftBracket => {
                let member_expr = self.parse_computed_member_expression(expr, start_pos, false)?;

                self.parse_call_expression(member_expr, start_pos, allow_call, in_optional_chain)
            }
            Token::TemplatePart { raw, cooked, is_tail, is_head: _ } => {
                if in_optional_chain {
                    return self.error(self.loc, ParseError::TaggedTemplateInChain);
                }

                // Malformed escape sequence errors are swalled in tagged template, and cooked
                // string is marked as None.
                let cooked = match cooked {
                    Ok(cooked) => Some(cooked.clone()),
                    Err(_) => None,
                };

                let quasi = p(self.parse_template_literal(
                    raw.clone(),
                    cooked,
                    *is_tail,
                    /* is_tagged */ true,
                )?);
                let loc = self.mark_loc(start_pos);

                let tagged_template_expr =
                    p(Expression::TaggedTemplate(TaggedTemplateExpression {
                        loc,
                        tag: expr,
                        quasi,
                    }));

                self.parse_call_expression(
                    tagged_template_expr,
                    start_pos,
                    allow_call,
                    in_optional_chain,
                )
            }
            // Start of an optional chain
            Token::QuestionDot => {
                self.advance()?;

                match self.token {
                    // Optional call
                    Token::LeftParen => {
                        let arguments = self.parse_call_arguments()?;
                        let loc = self.mark_loc(start_pos);

                        let call_expr = p(Expression::Call(CallExpression {
                            loc,
                            callee: expr,
                            arguments,
                            is_optional: true,
                        }));

                        let full_expr =
                            self.parse_call_expression(call_expr, start_pos, allow_call, true)?;

                        // Wrap in chain expression if this is the first optional part
                        if !in_optional_chain {
                            let loc = self.mark_loc(start_pos);
                            Ok(p(Expression::Chain(ChainExpression { loc, expression: full_expr })))
                        } else {
                            Ok(full_expr)
                        }
                    }
                    // Optional computed member access
                    Token::LeftBracket => {
                        let member_expr =
                            self.parse_computed_member_expression(expr, start_pos, true)?;

                        let full_expr =
                            self.parse_call_expression(member_expr, start_pos, allow_call, true)?;

                        // Wrap in chain expression if this is the first optional part
                        if !in_optional_chain {
                            let loc = self.mark_loc(start_pos);
                            Ok(p(Expression::Chain(ChainExpression { loc, expression: full_expr })))
                        } else {
                            Ok(full_expr)
                        }
                    }
                    // Optional simple property access
                    _ => {
                        let member_expr =
                            self.parse_property_member_expression(expr, start_pos, true)?;

                        let full_expr =
                            self.parse_call_expression(member_expr, start_pos, allow_call, true)?;

                        // Wrap in chain expression if this is the first optional part
                        if !in_optional_chain {
                            let loc = self.mark_loc(start_pos);
                            Ok(p(Expression::Chain(ChainExpression { loc, expression: full_expr })))
                        } else {
                            Ok(full_expr)
                        }
                    }
                }
            }
            _ => Ok(expr),
        }
    }

    fn parse_property_member_expression(
        &mut self,
        object: P<Expression>,
        start_pos: Pos,
        is_optional: bool,
    ) -> ParseResult<P<Expression>> {
        let is_private = self.token == Token::Hash;
        if is_private {
            let hash_loc = self.loc;
            self.advance()?;

            // Hash must be directly followed by the identifier, without any characters between
            if self.loc.start != hash_loc.end {
                return self.error(hash_loc, ParseError::HashNotFollowedByIdentifier);
            }
        }

        let property = match self.parse_identifier_name()? {
            Some(id) => id,
            None => return self.error_unexpected_token(self.loc, &self.token),
        };

        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Member(MemberExpression {
            loc,
            object,
            property: p(Expression::Id(property)),
            is_computed: false,
            is_optional,
            is_private,
        })))
    }

    fn parse_computed_member_expression(
        &mut self,
        object: P<Expression>,
        start_pos: Pos,
        is_optional: bool,
    ) -> ParseResult<P<Expression>> {
        self.advance()?;
        let property = self.parse_expression()?;
        self.expect(Token::RightBracket)?;
        let loc = self.mark_loc(start_pos);

        Ok(p(Expression::Member(MemberExpression {
            loc,
            object,
            property,
            is_computed: true,
            is_optional,
            is_private: false,
        })))
    }

    fn parse_new_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();

        self.advance()?;

        let callee_start_pos = self.current_start_pos();
        let callee = match self.token {
            Token::New => self.parse_new_expression()?,
            // Parse new.target meta property
            Token::Period => {
                self.advance()?;
                return if self.token == Token::Target {
                    self.advance()?;
                    let loc = self.mark_loc(start_pos);

                    Ok(p(Expression::MetaProperty(MetaProperty {
                        loc,
                        kind: MetaPropertyKind::NewTarget,
                    })))
                } else {
                    self.error(self.loc, ParseError::ExpectedNewTarget)
                };
            }
            _ => self.parse_primary_expression()?,
        };

        // Disallow call since parenthesized arguments should be attached to this new instead
        let callee = self.parse_member_expression(callee, callee_start_pos, false, false)?;

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

    fn parse_import_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();

        self.advance()?;

        match &self.token {
            // Parse import.meta meta property
            Token::Period => {
                self.advance()?;
                return if self.token == Token::Meta {
                    self.advance()?;
                    let loc = self.mark_loc(start_pos);

                    Ok(p(Expression::MetaProperty(MetaProperty {
                        loc,
                        kind: MetaPropertyKind::ImportMeta,
                    })))
                } else {
                    self.error(self.loc, ParseError::ExpectedImportMeta)
                };
            }
            _ => {
                self.expect(Token::LeftParen)?;
                let source = self.parse_assignment_expression()?;
                self.expect(Token::RightParen)?;

                let loc = self.mark_loc(start_pos);

                Ok(p(Expression::Import(ImportExpression { loc, source })))
            }
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
            Token::BigIntLiteral(value) => {
                let loc = self.loc;
                let value = value.clone();
                self.advance()?;
                Ok(p(Expression::BigInt(BigIntLiteral { loc, value })))
            }
            // RegExp may be started by "/=" which is treated as a single token
            Token::Divide | Token::DivideEq => {
                Ok(p(Expression::RegExp(self.parse_regexp_literal()?)))
            }
            Token::This => {
                let loc = self.loc;
                self.advance()?;
                Ok(p(Expression::This(loc)))
            }
            Token::LeftParen => {
                self.advance()?;
                let mut expr = self.parse_expression()?;
                self.expect(Token::RightParen)?;

                // Mark expression which need to know if they are parenthesized, in order for them
                // to be potentially reparsed into patterns.
                match expr.as_mut() {
                    Expression::Assign(expr) => expr.is_parenthesized = true,
                    Expression::Object(expr) => expr.is_parenthesized = true,
                    Expression::Array(expr) => expr.is_parenthesized = true,
                    _ => {}
                }

                Ok(expr)
            }
            Token::LeftBrace => self.parse_object_expression(),
            Token::LeftBracket => self.parse_array_expression(),
            Token::Class => Ok(p(Expression::Class(self.parse_class(false)?))),
            Token::TemplatePart { raw, cooked, is_tail, is_head: _ } => {
                // Non-tagged template literals error on malformed escape sequences
                let cooked = match cooked {
                    Ok(cooked) => Some(cooked.clone()),
                    Err(loc) => return self.error(*loc, ParseError::MalformedEscapeSeqence),
                };

                let template_literal = self.parse_template_literal(
                    raw.clone(),
                    cooked,
                    *is_tail,
                    /* is_tagged */ false,
                )?;
                Ok(p(Expression::Template(template_literal)))
            }
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

    fn parse_imported_identifier(&mut self) -> ParseResult<Identifier> {
        self.parse_identifier()
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        if Self::is_identifier(&self.token, self.in_strict_mode, self.allow_await, self.allow_yield)
        {
            if let Token::Identifier(name) = &self.token {
                let loc = self.loc;
                let name = name.clone();
                self.advance()?;
                Ok(Identifier { loc, name })
            } else {
                let loc = self.loc;
                let name = self.token.to_string();
                self.advance()?;
                Ok(Identifier { loc, name })
            }
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    /// Whether the current token represents an identifier in the given context
    #[inline]
    fn is_identifier(
        token: &Token,
        in_strict_mode: bool,
        allow_await: bool,
        allow_yield: bool,
    ) -> bool {
        match token {
            // Identifier's value cannot be a reserved word (e.g. due to escape characters)
            Token::Identifier(name) => !Self::is_reserved_word(name, in_strict_mode, allow_yield),
            // Tokens that are always allowed as identifiers
            Token::Async
            | Token::Of
            | Token::From
            | Token::As
            | Token::Get
            | Token::Set
            | Token::Target
            | Token::Meta => true,
            // Tokens that are contextually allowed as identifiers, when not in strict mode
            Token::Let | Token::Static if !in_strict_mode => true,
            // Contextually allowed as identifier when not in strict mode and not allowing yield
            // expressions.
            Token::Yield if !in_strict_mode && !allow_yield => true,
            // Contextually allowed as identifier when not allowing await expressions
            Token::Await if !allow_await => true,
            _ => false,
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
            | Token::Await
            | Token::Yield
            | Token::Target
            | Token::Meta
            | Token::Enum => {
                let loc = self.loc;
                let name = self.token.to_string();
                self.advance()?;
                Ok(Some(Identifier { loc, name }))
            }
            _ => Ok(None),
        }
    }

    fn is_reserved_word_in_current_context(&self, str: &str) -> bool {
        Self::is_reserved_word(str, self.in_strict_mode, self.allow_yield)
    }

    fn is_reserved_word(str: &str, in_strict_mode: bool, allow_yield: bool) -> bool {
        match str {
            // Names that are always reserved
            "await" | "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger"
            | "default" | "delete" | "do" | "else" | "enum" | "export" | "extends" | "false"
            | "finally" | "for" | "function" | "if" | "import" | "in" | "instanceof" | "new"
            | "null" | "return" | "super" | "switch" | "this" | "throw" | "true" | "try"
            | "typeof" | "var" | "void" | "while" | "with" => true,
            // Names that are only reserved in strict mode
            "let" | "static" | "implements" | "interface" | "package" | "private" | "protected"
            | "public"
                if in_strict_mode =>
            {
                true
            }
            "yield" => in_strict_mode || allow_yield,
            _ => false,
        }
    }

    fn parse_regexp_literal(&mut self) -> ParseResult<RegExpLiteral> {
        let start_pos = self.current_start_pos();

        self.advance_regexp_literal()?;

        if let Token::RegExpLiteral { raw, pattern, flags } = &self.token {
            let raw = raw.clone();
            let pattern = pattern.clone();
            let flags_string = flags.clone();

            self.advance()?;
            let loc = self.mark_loc(start_pos);
            let source = self.lexer.source.clone();

            // Start position of flags is offset by two `/` characters and the entire pattern
            let flags_start_pos = start_pos + 2 + pattern.len();
            let lexer_stream =
                Utf8LexerStream::new(flags_start_pos, source.clone(), flags_string.as_bytes());
            let flags = RegExpParser::parse_flags(lexer_stream)?;

            // Start position of pattern is offset by one to account for the leading `/`
            let pattern_start_pos = start_pos + 1;
            let lexer_stream = Utf8LexerStream::new(pattern_start_pos, source, pattern.as_bytes());
            let regexp = RegExpParser::parse_regexp(lexer_stream, flags)?;

            Ok(RegExpLiteral { loc, raw, pattern, flags: flags_string, regexp })
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn parse_template_literal(
        &mut self,
        raw: Wtf8String,
        cooked: Option<Wtf8String>,
        is_single_quasi: bool,
        is_tagged: bool,
    ) -> ParseResult<TemplateLiteral> {
        let start_pos = self.current_start_pos();

        let head_quasi = TemplateElement { loc: self.loc, raw: raw.clone(), cooked };

        self.advance()?;

        let mut quasis = vec![head_quasi];
        let mut expressions = vec![];

        if !is_single_quasi {
            loop {
                expressions.push(*self.parse_expression()?);

                if self.token != Token::RightBrace {
                    return self.error_expected_token(self.loc, &self.token, &Token::RightBrace);
                }

                self.advance_template_part()?;

                if let Token::TemplatePart { raw, cooked, is_tail, is_head: false } = &self.token {
                    // In non-tagged templates malformed escape sequences throw an error, otherwise
                    // malformed sequences are swallowed and cooked string is marked as None.
                    let cooked = match cooked {
                        Ok(cooked) => Some(cooked.clone()),
                        Err(loc) => {
                            if is_tagged {
                                None
                            } else {
                                return self.error(*loc, ParseError::MalformedEscapeSeqence);
                            }
                        }
                    };

                    quasis.push(TemplateElement {
                        loc: self.loc,
                        raw: raw.clone(),
                        cooked: cooked.clone(),
                    });

                    let is_tail = *is_tail;
                    self.advance()?;

                    if is_tail {
                        break;
                    }
                } else {
                    unreachable!("advance_template_part always returns a template part")
                }
            }
        }

        let loc = self.mark_loc(start_pos);

        Ok(TemplateLiteral { loc, quasis, expressions })
    }

    fn parse_spread_element(&mut self) -> ParseResult<SpreadElement> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_assignment_expression()?;
        let loc = self.mark_loc(start_pos);

        let has_trailing_comma = self.token == Token::Comma;

        Ok(SpreadElement { loc, argument, has_trailing_comma })
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

        Ok(p(Expression::Array(ArrayExpression { loc, elements, is_parenthesized: false })))
    }

    fn parse_object_expression(&mut self) -> ParseResult<P<Expression>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut properties = vec![];
        while self.token != Token::RightBrace {
            if self.token == Token::Spread {
                let spread = self.parse_spread_element()?;
                let has_spread_comma = self.token == Token::Comma;
                let spread_property = Property {
                    loc: spread.loc,
                    key: spread.argument,
                    value: None,
                    is_computed: false,
                    is_method: false,
                    kind: PropertyKind::Spread(has_spread_comma),
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

        Ok(p(Expression::Object(ObjectExpression {
            loc,
            properties,
            is_parenthesized: false,
        })))
    }

    fn parse_property(&mut self, prop_context: PropertyContext) -> ParseResult<(Property, bool)> {
        let start_pos = self.current_start_pos();

        // Handle getters and setters
        match self.token {
            Token::Get | Token::Set => {
                let id_loc = self.loc;
                let id_token = self.token.clone();
                let kind = if let Token::Get = self.token {
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
                let is_init_property = self.is_property_initializer(prop_context)
                    || self.is_pattern_initializer_in_object(prop_context);
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
                    p(Expression::Id(Identifier { loc: async_loc, name: String::from("async") }));
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
            let is_init_property = self.is_property_initializer(prop_context)
                || self.is_pattern_initializer_in_object(prop_context);
            if is_init_property || self.is_property_end(prop_context) {
                let name =
                    p(Expression::Id(Identifier { loc: async_loc, name: String::from("async") }));
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

    fn is_pattern_initializer_in_object(&mut self, prop_context: PropertyContext) -> bool {
        prop_context == PropertyContext::Object
            && self.is_property_initializer(PropertyContext::Pattern)
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
            Token::NumberLiteral(_) | Token::StringLiteral(_) | Token::BigIntLiteral(_) => {
                self.parse_primary_expression()?
            }
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
            is_shorthand = self.is_property_end(prop_context)
                || (prop_context == PropertyContext::Object
                    && self.is_property_end(PropertyContext::Pattern));
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
            if prop_context != PropertyContext::Class {
                let key_id = key.to_id();
                if self.is_reserved_word_in_current_context(&key_id.name) {
                    return self.error(key_id.loc, ParseError::IdentifierIsReservedWord);
                }
            }

            // Object initializer properties may reparsed as patterns in some contexts. If a property
            // initializer is seen here this may be a shorthand initializer pattern.
            if self.is_pattern_initializer_in_object(prop_context) {
                self.advance()?;

                let initializer = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                let property = Property {
                    loc,
                    key,
                    value: None,
                    is_computed,
                    is_method: false,
                    kind: PropertyKind::PatternInitializer(initializer),
                };

                return Ok((property, false));
            }

            None
        } else if self.is_property_initializer(prop_context) {
            self.advance()?;
            let value = self.parse_assignment_expression()?;

            Some(value)
        } else {
            let expected_token = self.get_property_initializer(prop_context);
            return self.error_expected_token(self.loc, &self.token, &expected_token);
        };

        // In classes, properties must be followed by a semicolon
        if prop_context == PropertyContext::Class {
            self.expect_semicolon()?;
        }

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
        // Enter async/generator context for parsing the function arguments and body
        let did_allow_await = swap_and_save(&mut self.allow_await, is_async);
        let did_allow_yield = swap_and_save(&mut self.allow_yield, is_generator);

        let params = self.parse_function_params()?;
        let (block, has_use_strict_directive, is_strict_mode) = self.parse_function_block_body()?;
        let body = p(FunctionBody::Block(block));
        let loc = self.mark_loc(start_pos);

        // Check for correct number of parameters
        match kind {
            PropertyKind::Get => {
                if !params.is_empty() {
                    return self.error(loc, ParseError::GetterWrongNumberOfParams);
                }
            }
            PropertyKind::Set => {
                if params.len() != 1 {
                    return self.error(loc, ParseError::SetterWrongNumberOfParams);
                }
            }
            _ => {}
        }

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

        self.allow_await = did_allow_await;
        self.allow_yield = did_allow_yield;

        Ok((property, is_private))
    }

    fn parse_class(&mut self, is_name_required: bool) -> ParseResult<Class> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Enter strict mode for entire class, saving strict mode context from beforehand
        let old_in_strict_mode = self.in_strict_mode;
        self.set_in_strict_mode(true);

        // Id is optional only for class expresssions
        let id = if is_name_required
            || (self.token != Token::LeftBrace && self.token != Token::Extends)
        {
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
        self.set_in_strict_mode(old_in_strict_mode);

        Ok(Class::new(loc, id, super_class, body))
    }

    fn parse_private_name(&mut self) -> ParseResult<Identifier> {
        let hash_loc = self.loc;
        self.expect(Token::Hash)?;

        // Hash must be directly followed by the identifier, without any characters between
        if self.loc.start != hash_loc.end {
            return self.error(hash_loc, ParseError::HashNotFollowedByIdentifier);
        }

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
                    p(Expression::Id(Identifier { loc: static_loc, name: String::from("static") }));

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
                    p(Expression::Id(Identifier { loc: static_loc, name: String::from("static") }));

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
            PropertyKind::Spread(_) => unreachable!("spread element cannot appear in class"),
            PropertyKind::PatternInitializer(_) => {
                unreachable!("pattern initializer cannot appear in class")
            }
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

        // In expressions are always allowed within array patterns. Needed in case an array pattern
        // with in expression initializer appears in for init.
        let old_allow_in = swap_and_save(&mut self.allow_in, true);

        let mut elements = vec![];
        while self.token != Token::RightBracket {
            match self.token {
                Token::Comma => {
                    self.advance()?;
                    elements.push(ArrayPatternElement::Hole);
                }
                Token::Spread => {
                    let rest_element = self.parse_rest_element()?;
                    elements.push(ArrayPatternElement::Rest(rest_element));

                    // Trailing commas are not allowed after rest elements, nor are any other elements
                    if self.token == Token::Comma {
                        return self.error(self.loc, ParseError::RestTrailingComma);
                    } else {
                        break;
                    }
                }
                _ => {
                    let pattern = self.parse_pattern_including_assignment_pattern()?;
                    elements.push(ArrayPatternElement::Pattern(pattern));
                    if self.token == Token::Comma {
                        self.advance()?;
                    } else {
                        break;
                    }
                }
            }
        }

        self.expect(Token::RightBracket)?;
        let loc = self.mark_loc(start_pos);

        self.allow_in = old_allow_in;

        Ok(Pattern::Array(ArrayPattern { loc, elements }))
    }

    fn parse_rest_element(&mut self) -> ParseResult<RestElement> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = p(self.parse_pattern()?);
        let loc = self.mark_loc(start_pos);

        Ok(RestElement { loc, argument })
    }

    fn parse_object_pattern(&mut self) -> ParseResult<Pattern> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // In expressions are always allowed within object patterns. Needed in case an object pattern
        // with in expression initializer appears in for init.
        let old_allow_in = swap_and_save(&mut self.allow_in, true);

        let mut properties = vec![];
        while self.token != Token::RightBrace {
            if self.token == Token::Spread {
                properties.push(self.parse_object_rest_property()?);

                // Trailing commas are not allowed after rest elements, nor are any other properties
                if self.token == Token::Comma {
                    return self.error(self.loc, ParseError::RestTrailingComma);
                } else {
                    break;
                }
            }

            properties.push(self.parse_object_pattern_property()?);

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        self.allow_in = old_allow_in;

        Ok(Pattern::Object(ObjectPattern { loc, properties }))
    }

    fn parse_object_pattern_property(&mut self) -> ParseResult<ObjectPatternProperty> {
        let start_pos = self.current_start_pos();

        let property_name = self.parse_property_name(PropertyContext::Pattern)?;

        // Shorthand property
        if property_name.is_shorthand {
            let value = if let Expression::Id(id) = *property_name.key {
                if self.is_reserved_word_in_current_context(&id.name) {
                    return self.error(id.loc, ParseError::IdentifierIsReservedWord);
                }

                Pattern::Id(id)
            } else {
                unreachable!()
            };

            // Shorthand property may be followed by assignment pattern
            let value = p(self.parse_assignment_pattern(value, start_pos)?);
            let loc = self.mark_loc(start_pos);

            return Ok(ObjectPatternProperty {
                loc,
                key: None,
                value,
                is_computed: false,
                is_rest: false,
            });
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
            is_rest: false,
        })
    }

    fn parse_object_rest_property(&mut self) -> ParseResult<ObjectPatternProperty> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Only identifiers are allowed as rest property arguments
        let id_argument = self.parse_binding_identifier()?;
        let loc = self.mark_loc(start_pos);

        Ok(ObjectPatternProperty {
            loc,
            key: None,
            value: p(Pattern::Id(id_argument)),
            is_computed: false,
            is_rest: true,
        })
    }

    fn parse_import_declaration(&mut self) -> ParseResult<Toplevel> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // May be the start of a toplevel import expression
        if self.token == Token::LeftParen {
            self.advance()?;
            let source = self.parse_assignment_expression()?;
            self.expect(Token::RightParen)?;

            let loc = self.mark_loc(start_pos);
            let import_expr = Expression::Import(ImportExpression { loc, source });

            // Finish parsing rest of expression started by this import expression
            let full_expr = self.parse_call_expression(
                p(import_expr),
                start_pos,
                /* allow_call */ true,
                /* in_optional_chain */ false,
            )?;

            self.expect_semicolon()?;
            let loc = self.mark_loc(start_pos);

            let expr_stmt = Statement::Expr(ExpressionStatement { loc, expr: full_expr });

            return Ok(Toplevel::Statement(expr_stmt));
        }

        // No specifiers
        if let Token::StringLiteral(value) = &self.token {
            let source = p(StringLiteral { loc: self.loc, value: value.clone() });
            self.advance()?;
            self.expect_semicolon()?;

            let loc = self.mark_loc(start_pos);

            return Ok(Toplevel::Import(ImportDeclaration { loc, specifiers: vec![], source }));
        }

        // Check for default specifier, which must be at start
        let mut specifiers = vec![];

        if let Token::Identifier(_) = self.token {
            // Starts with default specifier
            let local = p(self.parse_imported_identifier()?);
            let default_specifier = ImportDefaultSpecifier { loc: local.loc, local };
            specifiers.push(ImportSpecifier::Default(default_specifier));

            // May be optionally followed by named or namespace specifier
            if self.token == Token::Comma {
                self.advance()?;
                self.parse_import_named_or_namespace_specifier(&mut specifiers)?;
            }
        } else {
            // Otherwise is only a named or namespace specifier
            self.parse_import_named_or_namespace_specifier(&mut specifiers)?;
        }

        let source = p(self.parse_source()?);
        let loc = self.mark_loc(start_pos);

        Ok(Toplevel::Import(ImportDeclaration { loc, specifiers, source }))
    }

    fn parse_import_named_or_namespace_specifier(
        &mut self,
        specifiers: &mut Vec<ImportSpecifier>,
    ) -> ParseResult<()> {
        match self.token {
            Token::Multiply => {
                specifiers.push(self.parse_import_namespace_specifier()?);
                Ok(())
            }
            Token::LeftBrace => {
                self.parse_named_specifiers(specifiers)?;
                Ok(())
            }
            _ => return self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_import_namespace_specifier(&mut self) -> ParseResult<ImportSpecifier> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::As)?;

        let local = p(self.parse_imported_identifier()?);
        let loc = self.mark_loc(start_pos);

        Ok(ImportSpecifier::Namespace(ImportNamespaceSpecifier { loc, local }))
    }

    fn parse_named_specifiers(&mut self, specifiers: &mut Vec<ImportSpecifier>) -> ParseResult<()> {
        self.advance()?;

        while self.token != Token::RightBrace {
            let start_pos = self.current_start_pos();
            let start_token = self.token.clone();

            // String literal must be the start of an import as specifier
            let spec = if let Token::StringLiteral(value) = &self.token {
                let imported =
                    ModuleName::String(StringLiteral { loc: self.loc, value: value.clone() });
                self.advance()?;

                self.expect(Token::As)?;

                let local = p(self.parse_imported_identifier()?);
                let loc = self.mark_loc(start_pos);

                ImportNamedSpecifier { loc, imported: Some(p(imported)), local }
            } else if let Some(id) = self.parse_identifier_name()? {
                // This is an import as specifier, so the id can be any name including reserved words
                if self.token == Token::As {
                    self.advance()?;

                    let local = p(self.parse_imported_identifier()?);
                    let loc = self.mark_loc(start_pos);

                    ImportNamedSpecifier { loc, imported: Some(p(ModuleName::Id(id))), local }
                } else {
                    // This is the binding for a simple named specifier, identifier cannot be a
                    // reserved word.
                    if self.is_reserved_word_in_current_context(&id.name) {
                        return self.error_unexpected_token(id.loc, &start_token);
                    }

                    ImportNamedSpecifier { loc: id.loc, imported: None, local: p(id) }
                }
            } else {
                return self.error_unexpected_token(self.loc, &self.token);
            };

            specifiers.push(ImportSpecifier::Named(spec));

            // List of specifiers has optional trailing comma
            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightBrace)?;

        Ok(())
    }

    fn parse_export_declaration(&mut self) -> ParseResult<Toplevel> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        match self.token {
            // Export named specifiers within braces
            Token::LeftBrace => {
                self.advance()?;

                let mut specifiers = vec![];

                // Parse list of specifiers between braces
                while self.token != Token::RightBrace {
                    let start_pos = self.current_start_pos();
                    let local = p(self.parse_module_name()?);

                    // Specifiers optionally have an export alias
                    let exported = if self.token == Token::As {
                        self.advance()?;
                        Some(p(self.parse_module_name()?))
                    } else {
                        None
                    };

                    let loc = self.mark_loc(start_pos);

                    specifiers.push(ExportSpecifier { loc, local, exported });

                    // List of specifiers has optional trailing comma
                    if self.token == Token::Comma {
                        self.advance()?;
                    } else {
                        break;
                    }
                }

                self.expect(Token::RightBrace)?;

                let source = if self.token == Token::From {
                    Some(p(self.parse_source()?))
                } else {
                    None
                };

                self.expect_semicolon()?;
                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportNamed(ExportNamedDeclaration {
                    loc,
                    declaration: None,
                    specifiers,
                    source,
                }))
            }
            // Export all declaration
            Token::Multiply => {
                self.advance()?;

                // Optional exported alias
                let exported = if self.token == Token::As {
                    self.advance()?;
                    Some(p(self.parse_module_name()?))
                } else {
                    None
                };

                // Required source
                let source = p(self.parse_source()?);
                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportAll(ExportAllDeclaration { loc, exported, source }))
            }
            // Export named declaration with a declaration statement
            Token::Function
            | Token::Async
            | Token::Class
            | Token::Var
            | Token::Let
            | Token::Const => {
                let declaration = Some(p(self.parse_statement_list_item()?));
                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportNamed(ExportNamedDeclaration {
                    loc,
                    declaration,
                    specifiers: vec![],
                    source: None,
                }))
            }
            // Export default declaration
            Token::Default => {
                self.advance()?;

                // Default function and class declarations have an optional name
                match self.token {
                    Token::Function | Token::Async => {
                        let declaration = p(Statement::FuncDecl(self.parse_function(false)?));
                        let loc = self.mark_loc(start_pos);

                        Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                    }
                    Token::Class => {
                        let declaration = p(Statement::ClassDecl(self.parse_class(false)?));
                        let loc = self.mark_loc(start_pos);

                        Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                    }
                    // Otherwise default export is an expression
                    _ => {
                        let expr_start_pos = self.current_start_pos();
                        let expr = self.parse_assignment_expression()?;

                        self.expect_semicolon()?;
                        let loc = self.mark_loc(start_pos);

                        let expr_stmt_loc = self.mark_loc(expr_start_pos);
                        let declaration =
                            p(Statement::Expr(ExpressionStatement { loc: expr_stmt_loc, expr }));

                        Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                    }
                }
            }
            _ => self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_module_name(&mut self) -> ParseResult<ModuleName> {
        if let Token::StringLiteral(value) = &self.token {
            let imported =
                ModuleName::String(StringLiteral { loc: self.loc, value: value.clone() });
            self.advance()?;
            Ok(imported)
        } else if let Some(id) = self.parse_identifier_name()? {
            Ok(ModuleName::Id(id))
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn parse_source(&mut self) -> ParseResult<StringLiteral> {
        self.expect(Token::From)?;

        if let Token::StringLiteral(value) = &self.token {
            let source = StringLiteral { loc: self.loc, value: value.clone() };
            self.advance()?;
            self.expect_semicolon()?;

            Ok(source)
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn reparse_expression_as_assignment_left_hand_side(
        &self,
        expr: Expression,
        start_pos: Pos,
    ) -> ParseResult<Pattern> {
        match self.reparse_left_hand_side_expression_as_pattern(expr) {
            Some(pattern) => Ok(pattern),
            None => {
                let loc = self.mark_loc(start_pos);
                self.error(loc, ParseError::InvalidAssignmentLeftHandSide)
            }
        }
    }

    fn reparse_expression_as_for_left_hand_side(
        &self,
        expr: Expression,
        start_pos: Pos,
    ) -> ParseResult<Pattern> {
        match self.reparse_left_hand_side_expression_as_pattern(expr) {
            Some(pattern) => Ok(pattern),
            None => {
                let loc = self.mark_loc(start_pos);
                self.error(loc, ParseError::InvalidForLeftHandSide)
            }
        }
    }

    fn reparse_expression_as_operator_assignment_left_hand_side(
        &self,
        expr: Expression,
        start_pos: Pos,
    ) -> ParseResult<Pattern> {
        let reparsed_pattern = match expr {
            // Only valid operator assignment left hand side expressions
            Expression::Id(_) | Expression::Member(_) | Expression::SuperMember(_) => {
                self.reparse_left_hand_side_expression_as_pattern(expr)
            }
            _ => None,
        };

        match reparsed_pattern {
            Some(pattern) => Ok(pattern),
            None => {
                let loc = self.mark_loc(start_pos);
                self.error(loc, ParseError::InvalidAssignmentLeftHandSide)
            }
        }
    }

    fn reparse_left_hand_side_expression_as_pattern(&self, expr: Expression) -> Option<Pattern> {
        match expr {
            Expression::Id(id) => {
                // Cannot assign to arguments or eval in strict mode
                if self.in_strict_mode {
                    match id.name.as_str() {
                        "arguments" | "eval" => return None,
                        _ => {}
                    }
                }

                Some(Pattern::Id(id))
            }
            Expression::Object(object) => self.reparse_object_expression_as_pattern(object),
            Expression::Array(object) => self.reparse_array_expression_as_pattern(object),
            Expression::Member(_) | Expression::SuperMember(_) => Some(Pattern::Reference(expr)),
            _ => None,
        }
    }

    fn reparse_object_expression_as_pattern(&self, expr: ObjectExpression) -> Option<Pattern> {
        if expr.is_parenthesized {
            return None;
        }

        let mut properties = vec![];

        for property in expr.properties {
            let property = if let PropertyKind::Spread(has_spread_comma) = property.kind {
                // Convert spread property to rest property

                // Trailing commas (or properties after) are not allowed on rest element
                if has_spread_comma {
                    return None;
                }

                // Object and array patterns are not allowed as rest elements
                let value = match *property.key {
                    Expression::Object(_) | Expression::Array(_) => return None,
                    lhs_pattern => {
                        p(self.reparse_left_hand_side_expression_as_pattern(lhs_pattern)?)
                    }
                };

                ObjectPatternProperty {
                    loc: property.loc,
                    key: None,
                    value,
                    is_computed: false,
                    is_rest: true,
                }
            } else if property.value.is_none() {
                // Shorthand properties
                let id_pattern =
                    p(self.reparse_left_hand_side_expression_as_pattern(*property.key)?);

                // Check the property kind to see if this is a shorthand pattern initializer
                let value = if let PropertyKind::PatternInitializer(initializer) = property.kind {
                    p(Pattern::Assign(AssignmentPattern {
                        loc: property.loc,
                        left: id_pattern,
                        right: initializer,
                    }))
                } else {
                    id_pattern
                };

                ObjectPatternProperty {
                    loc: property.loc,
                    key: None,
                    value,
                    is_computed: false,
                    is_rest: false,
                }
            } else {
                // If the value is an assignment expression with an identifier lhs, this can be
                // reparsed to an assignment pattern.
                let value = p(
                    self.reparse_expression_as_maybe_assignment_pattern(*property.value.unwrap())?
                );

                ObjectPatternProperty {
                    loc: property.loc,
                    key: Some(property.key),
                    value,
                    is_computed: property.is_computed,
                    is_rest: false,
                }
            };

            properties.push(property);
        }

        Some(Pattern::Object(ObjectPattern { loc: expr.loc, properties }))
    }

    fn reparse_array_expression_as_pattern(&self, expr: ArrayExpression) -> Option<Pattern> {
        if expr.is_parenthesized {
            return None;
        }

        let mut elements = vec![];

        for element in expr.elements {
            let element = match element {
                ArrayElement::Expression(expr) => {
                    let pattern = self.reparse_expression_as_maybe_assignment_pattern(expr)?;
                    ArrayPatternElement::Pattern(pattern)
                }
                ArrayElement::Hole => ArrayPatternElement::Hole,
                ArrayElement::Spread(spread) => {
                    // Trailing commas (or properties after) are not allowed on rest element
                    if spread.has_trailing_comma {
                        return None;
                    }

                    let argument =
                        p(self.reparse_left_hand_side_expression_as_pattern(*spread.argument)?);
                    ArrayPatternElement::Rest(RestElement { loc: spread.loc, argument })
                }
            };

            elements.push(element);
        }

        Some(Pattern::Array(ArrayPattern { loc: expr.loc, elements }))
    }

    // If the value is an assignment expression with an identifier lhs, it can be reparsed to an
    // assignment pattern. Otherwise it is parsed normally as a left hand side expression.
    fn reparse_expression_as_maybe_assignment_pattern(&self, expr: Expression) -> Option<Pattern> {
        match expr {
            Expression::Assign(AssignmentExpression {
                operator: AssignmentOperator::Equals,
                loc,
                left,
                right,
                is_parenthesized,
            }) => {
                // Parenthesized assignment expressions are invalid as pattern elements
                if is_parenthesized {
                    return None;
                }

                Some(Pattern::Assign(AssignmentPattern { loc, left, right }))
            }
            other_expr => Some(self.reparse_left_hand_side_expression_as_pattern(other_expr)?),
        }
    }

    // 8.5.4 AssignmentTargetType
    fn is_valid_assignment_target(&self, expr: &Expression) -> bool {
        match expr {
            Expression::Id(id) => {
                // Cannot assign to arguments or eval in strict mode
                if self.in_strict_mode {
                    match id.name.as_str() {
                        "arguments" | "eval" => false,
                        _ => true,
                    }
                } else {
                    true
                }
            }
            Expression::Member(_) | Expression::SuperMember(_) => true,
            _ => false,
        }
    }
}

pub fn parse_script(source: &Rc<Source>) -> ParseResult<Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let initial_state = parser.save();
    parser.advance()?;

    Ok(parser.parse_script(initial_state)?)
}

pub fn parse_module(source: &Rc<Source>) -> ParseResult<Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(parser.parse_module()?)
}

pub fn parse_script_for_eval(
    source: &Rc<Source>,
    inherit_strict_mode: bool,
) -> ParseResult<Program> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    // Inherit strict mode from context
    parser.set_in_strict_mode(inherit_strict_mode);

    let initial_state = parser.save();
    parser.advance()?;

    Ok(parser.parse_script(initial_state)?)
}

pub fn parse_function_params_for_function_constructor(
    source: &Rc<Source>,
    is_async: bool,
    is_generator: bool,
) -> ParseResult<Vec<FunctionParam>> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    parser.allow_await = is_async;
    parser.allow_yield = is_generator;
    parser.prevent_hashbang_comment();

    parser.advance()?;

    Ok(parser.parse_function_params_without_parens()?)
}

pub fn parse_function_body_for_function_constructor(
    source: &Rc<Source>,
    is_async: bool,
    is_generator: bool,
) -> ParseResult<Vec<Statement>> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    parser.allow_await = is_async;
    parser.allow_yield = is_generator;
    parser.prevent_hashbang_comment();

    let initial_state = parser.save();
    parser.advance()?;

    Ok(parser.parse_function_body_statements(initial_state)?)
}

pub fn parse_function_for_function_constructor(source: &Rc<Source>) -> ParseResult<P<Function>> {
    // Create and prime parser
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.advance()?;

    Ok(p(parser.parse_function(true)?))
}
