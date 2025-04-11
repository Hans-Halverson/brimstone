use std::borrow::Cow;
use std::cell::Cell;
use std::collections::HashSet;
use std::rc::Rc;

use bitflags::bitflags;
use num_bigint::BigInt;

use crate::common::options::Options;
use crate::common::unicode::{encode_utf16_codepoint, utf16_code_unit_count, utf8_byte_count};
use crate::common::wtf_8::Wtf8Str;
use crate::p;

use super::ast::*;
use super::context::ParseContext;
use super::lexer::{Lexer, SavedLexerState};
use super::lexer_stream::{HeapTwoByteCodeUnitLexerStream, Utf8LexerStream};
use super::loc::{Loc, Pos, EMPTY_LOC};
use super::parse_error::{LocalizedParseError, ParseError, ParseResult};
use super::regexp_parser::RegExpParser;
use super::scope_tree::{
    AstScopeNode, BindingKind, SavedScopeTreeState, ScopeNodeKind, ScopeTree,
    ANONYMOUS_DEFAULT_EXPORT_NAME, DERIVED_CONSTRUCTOR_BINDING_NAME, HOME_OBJECT_BINDING_NAME,
    STATIC_HOME_OBJECT_BINDING_NAME, THIS_NAME,
};
use super::source::Source;
use super::token::{RegExpToken, TemplatePartToken, Token};

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

struct PropertyNameResult<'a> {
    key: Expression<'a>,
    is_computed: bool,
    is_shorthand: bool,
    is_private: bool,
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct FunctionContext: u8 {
        /// Whether this is a function declaration or expression.
        const DECLARATION = 1 << 0;
        /// Whether this is an exported function declaration.
        const EXPORT = 1 << 1;
        /// If this is a function declaration, whether the function appears at the toplevel of a
        /// script or function body (ignoring labels) for the purpose of scoping.
        const TOPLEVEL = 1 << 2;
        /// If this is a function declaration, whether the function is labeled.
        const LABELED = 1 << 3;
    }
}

const ASYNC_ID: &Wtf8Str = Wtf8Str::from_str("async");
const GET_ID: &Wtf8Str = Wtf8Str::from_str("get");
const SET_ID: &Wtf8Str = Wtf8Str::from_str("set");
const STATIC_ID: &Wtf8Str = Wtf8Str::from_str("static");

struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token<'a>,
    loc: Loc,
    prev_loc: Loc,
    /// Whether the parser is currently parsing in strict mode
    in_strict_mode: bool,
    /// Whether the parser is currently in a context where an await expression is allowed
    allow_await: bool,
    /// Whether the parser is currently in a context where a yield expression is allowed
    allow_yield: bool,
    /// Whether the parser is currently in a context where an in expression is allowed
    allow_in: bool,
    /// The scope builder is used to build the scope tree while parsing.
    scope_builder: ScopeTree<'a>,
    /// The program kind that is currently being parsed - script vs module.
    program_kind: ProgramKind,
    /// Options set for the compiler
    options: Rc<Options>,
    /// Allocator used for allocating AST nodes
    alloc: AstAlloc<'a>,
}

/// A save point for the parser, can be used to restore the parser to a particular position.
struct ParserSaveState<'a> {
    saved_lexer_state: SavedLexerState,
    saved_scope_builder_state: SavedScopeTreeState,
    token: Token<'a>,
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
    fn new(
        lexer: Lexer<'a>,
        scope_builder: ScopeTree<'a>,
        options: Rc<Options>,
        alloc: AstAlloc<'a>,
    ) -> Parser<'a> {
        Parser {
            lexer,
            token: Token::Eof,
            loc: EMPTY_LOC,
            prev_loc: EMPTY_LOC,
            in_strict_mode: false,
            allow_await: false,
            allow_yield: false,
            allow_in: true,
            scope_builder,
            program_kind: ProgramKind::Script,
            options,
            alloc,
        }
    }

    pub fn error<T>(&self, loc: Loc, error: ParseError) -> ParseResult<T> {
        let source = (*self.lexer.source).clone();
        Err(LocalizedParseError { error, source_loc: Some((loc, source)) })
    }

    fn save(&self) -> ParserSaveState<'a> {
        ParserSaveState {
            saved_lexer_state: self.lexer.save(),
            saved_scope_builder_state: self.scope_builder.save(),
            token: self.token.clone(),
            loc: self.loc,
            prev_loc: self.prev_loc,
            in_strict_mode: self.in_strict_mode,
            allow_await: self.allow_await,
            allow_yield: self.allow_yield,
            allow_in: self.allow_in,
        }
    }

    fn restore(&mut self, save_state: ParserSaveState<'a>) {
        self.lexer.restore(&save_state.saved_lexer_state);
        self.scope_builder
            .restore(&save_state.saved_scope_builder_state);
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
        if result.is_err() {
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

    /// Return the next token.
    fn peek(&mut self) -> ParseResult<Token<'a>> {
        let lexer_state = self.lexer.save();
        let (token, _) = self.lexer.next()?;
        self.lexer.restore(&lexer_state);

        Ok(token)
    }

    /// Return the next token, along with whether it is directly preceded by a newline.
    fn peek_with_newline(&mut self) -> ParseResult<(Token<'a>, bool)> {
        let lexer_state = self.lexer.save();
        let (token, _) = self.lexer.next()?;
        let has_newline = self.lexer.is_new_line_before_current();
        self.lexer.restore(&lexer_state);

        Ok((token, has_newline))
    }

    fn expect(&mut self, token: Token) -> ParseResult<()> {
        if self.token != token {
            return self.error(self.loc, ParseError::new_expected_token(self.token.clone(), token));
        }

        self.advance()?;
        Ok(())
    }

    fn error_unexpected_token<T>(&self, loc: Loc, token: &Token) -> ParseResult<T> {
        self.error(loc, ParseError::new_unexpected_token(token.clone()))
    }

    fn error_expected_token<T>(
        &self,
        loc: Loc,
        actual: &Token,
        expected: &Token,
    ) -> ParseResult<T> {
        self.error(loc, ParseError::new_expected_token(actual.clone(), expected.clone()))
    }

    #[inline]
    fn current_start_pos(&self) -> Pos {
        self.loc.start
    }

    fn mark_loc(&self, start_pos: Pos) -> Loc {
        Loc { start: start_pos, end: self.prev_loc.end }
    }

    fn alloc_string(&self, string: String) -> AstString<'a> {
        AstString::from_string_in(string, self.alloc)
    }

    fn alloc_vec<U>(&self) -> AstSliceBuilder<'a, U> {
        AstSliceBuilder::new(ArenaVec::new_in(self.alloc))
    }

    fn alloc_vec_with_element<U>(&self, element: U) -> AstSliceBuilder<'a, U> {
        let mut vec = ArenaVec::new_in(self.alloc);
        vec.push(element);
        AstSliceBuilder::new(vec)
    }

    fn token_str(&self) -> &'a Wtf8Str {
        match self.token.as_cow() {
            Cow::Borrowed(name) => Wtf8Str::from_str(name),
            Cow::Owned(name) => AstString::from_string_in(name, self.alloc).into_arena_str(),
        }
    }

    fn add_binding(&mut self, id: &mut Identifier<'a>, kind: BindingKind<'a>) -> ParseResult<()> {
        match self.scope_builder.add_binding(id.name, kind) {
            Ok(scope) => {
                id.scope = TaggedResolvedScope::resolved(scope);
                Ok(())
            }
            Err(error) => self.error(id.loc, error),
        }
    }

    fn add_home_object_to_scope(&mut self, include_static: bool) {
        self.scope_builder
            .add_binding_to_current_node(&HOME_OBJECT_BINDING_NAME, BindingKind::HomeObject);

        if include_static {
            self.scope_builder.add_binding_to_current_node(
                &STATIC_HOME_OBJECT_BINDING_NAME,
                BindingKind::HomeObject,
            );
        }
    }

    fn add_anonymous_default_export_to_scope(
        &mut self,
        loc: Loc,
        kind: BindingKind<'a>,
    ) -> ParseResult<()> {
        if let Err(error) = self
            .scope_builder
            .add_binding(&ANONYMOUS_DEFAULT_EXPORT_NAME, kind)
        {
            return self.error(loc, error);
        }

        // Immediately mark as exported (since there is no id to visit during analysis)
        self.scope_builder
            .current_scope()
            .as_ref()
            .get_binding(&ANONYMOUS_DEFAULT_EXPORT_NAME)
            .set_is_exported(true);

        Ok(())
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
                    self.error(
                        self.loc,
                        ParseError::new_expected_token(other.clone(), Token::Semicolon),
                    )
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

    fn parse_script(
        mut self,
        initial_state: ParserSaveState<'a>,
    ) -> ParseResult<ParseProgramResult<'a>> {
        self.program_kind = ProgramKind::Script;

        let has_use_strict_directive = self.parse_directive_prologue()?;

        // Restore to initial state, then reparse directives with strict mode properly set
        self.restore(initial_state);

        if has_use_strict_directive {
            self.set_in_strict_mode(true);
        }

        // Mark the current eval scope as strict or sloppy, which must be known before parsing
        let mut current_scope = self.scope_builder.current_scope();
        if let ScopeNodeKind::Eval { is_direct, .. } = current_scope.as_ref().kind() {
            current_scope
                .as_mut()
                .set_kind(ScopeNodeKind::Eval { is_direct, is_strict: self.in_strict_mode });
        }

        // Re-prime the parser
        self.advance()?;

        let mut toplevels = self.alloc_vec();
        while self.token != Token::Eof {
            toplevels.push(self.parse_toplevel()?);
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        // Retrieve the global scope node
        let scope = self.scope_builder.get_ast_node_ptr(0);

        let program = Program::new(
            loc,
            toplevels.build(),
            ProgramKind::Script,
            scope,
            self.in_strict_mode,
            has_use_strict_directive,
        );

        let scope_tree = p!(self, self.scope_builder.finish_ast_scope_tree());
        let source = self.lexer.source.clone();

        Ok(ParseProgramResult { program: p!(self, program), scope_tree, source })
    }

    fn parse_directive_prologue(&mut self) -> ParseResult<bool> {
        // Parse a sequence of string literals followed by semicolons. If any is the literal
        // "use strict" then we are in strict mode. Be sure to restore to original state
        // afterwards, as earlier literals may need to be reparsed in strict mode.
        let mut has_use_strict_directive = false;

        while let Token::StringLiteral(str) = &self.token {
            // Use strict directive cannot have any escape characters in it, so check length
            let is_unscaped_use_strict =
                *str == "use strict" && self.loc.end - self.loc.start == 12;

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

    fn parse_module(mut self) -> ParseResult<ParseProgramResult<'a>> {
        self.program_kind = ProgramKind::Module;

        // Modules are always in strict mode
        self.set_in_strict_mode(true);

        // Allow top level await
        self.allow_await = true;

        let mut toplevels = self.alloc_vec();
        while self.token != Token::Eof {
            match self.token {
                Token::Import => toplevels.push(self.parse_import_declaration()?),
                Token::Export => toplevels.push(self.parse_export_declaration()?),
                _ => toplevels.push(self.parse_toplevel()?),
            }
        }

        // Start out at beginning of file
        let loc = self.mark_loc(0);

        // Retrieve the global scope node
        let scope = self.scope_builder.get_ast_node_ptr(0);

        let program = Program::new(
            loc,
            toplevels.build(),
            ProgramKind::Module,
            scope,
            self.in_strict_mode,
            /* has_use_strict_directive */ false,
        );

        let scope_tree = p!(self, self.scope_builder.finish_ast_scope_tree());
        let source = self.lexer.source.clone();

        Ok(ParseProgramResult { program: p!(self, program), scope_tree, source })
    }

    fn parse_toplevel(&mut self) -> ParseResult<Toplevel<'a>> {
        // Toplevel declarations are only considered "toplevel" from the perspective of scoping
        // in scripts.
        let function_ctx_flags = if self.program_kind == ProgramKind::Script {
            FunctionContext::TOPLEVEL
        } else {
            FunctionContext::empty()
        };

        let stmt = self.parse_statement_list_item(function_ctx_flags)?;
        Ok(Toplevel::Statement(stmt))
    }

    /// Parse a StatementListItem. Specify the flags to be used for function declarations.
    fn parse_statement_list_item(
        &mut self,
        function_ctx_flags: FunctionContext,
    ) -> ParseResult<Statement<'a>> {
        match self.token {
            Token::Const => {
                let var_decl = self.parse_variable_declaration(false)?;
                Ok(Statement::VarDecl(p!(self, var_decl)))
            }
            Token::Let => {
                if self.is_let_declaration_start()? {
                    let var_decl = self.parse_variable_declaration(false)?;
                    Ok(Statement::VarDecl(p!(self, var_decl)))
                } else {
                    self.parse_statement()
                }
            }
            Token::Class => Ok(Statement::ClassDecl(
                self.parse_class(/* is_true */ true, /* is_export */ false)?,
            )),
            _ => {
                if self.is_function_start()? {
                    return Ok(Statement::FuncDecl(
                        self.parse_function_declaration(function_ctx_flags)?,
                    ));
                }

                self.parse_statement_with_function_context(function_ctx_flags)
            }
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement<'a>> {
        self.parse_statement_with_function_context(FunctionContext::empty())
    }

    fn parse_statement_with_function_context(
        &mut self,
        function_ctx_flags: FunctionContext,
    ) -> ParseResult<Statement<'a>> {
        match self.token {
            Token::Var => {
                let var_decl = self.parse_variable_declaration(false)?;
                Ok(Statement::VarDecl(p!(self, var_decl)))
            }
            Token::LeftBrace => Ok(Statement::Block(p!(self, self.parse_block()?))),
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

                Ok(Statement::Empty(p!(self, loc)))
            }
            Token::Debugger => {
                let start_pos = self.current_start_pos();
                self.advance()?;
                self.expect_semicolon()?;

                let loc = self.mark_loc(start_pos);

                Ok(Statement::Debugger(p!(self, loc)))
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
                let expr = self.parse_outer_expression()?;

                // Parse labeled statement
                if self.token == Token::Colon {
                    if matches!(&expr.expr, Expression::Id(_)) {
                        self.advance()?;

                        let Expression::Id(label) = expr.expr else {
                            unreachable!()
                        };

                        // Functions can be labeled items
                        let body = if self.is_function_start()? {
                            let func = self.parse_function_declaration(
                                function_ctx_flags | FunctionContext::LABELED,
                            )?;

                            // Verify that labeled functions are not async or generators
                            if func.is_async() || func.is_generator() {
                                return self.error(
                                    func.loc,
                                    ParseError::AsyncOrGeneratorLabeledFunction(func.is_async()),
                                );
                            }

                            Statement::FuncDecl(func)
                        } else {
                            // From the perspective of function declaration scoping, function
                            // declarations inside arbitrarily nested within labels are still
                            // toplevel.
                            self.parse_statement_with_function_context(function_ctx_flags)?
                        };

                        let loc = self.mark_loc(start_pos);

                        let labeled_stmt = LabeledStatement {
                            loc,
                            label: p!(self, Label::new(label.loc, label.name)),
                            body,
                        };

                        return Ok(Statement::Labeled(p!(self, labeled_stmt)));
                    }
                }

                // Otherwise must be an expression statement
                self.expect_semicolon()?;
                let loc = self.mark_loc(start_pos);

                Ok(Statement::Expr(p!(self, ExpressionStatement { loc, expr })))
            }
        }
    }

    fn is_function_start(&mut self) -> ParseResult<bool> {
        match self.token {
            Token::Function => Ok(true),
            // Async keyword is only the start of a function if it is followed by the function
            // keyword without a newline in between.
            Token::Async => {
                let (next_token, has_newline) = self.peek_with_newline()?;
                Ok(next_token == Token::Function && !has_newline)
            }
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
    ) -> ParseResult<VariableDeclaration<'a>> {
        let start_pos = self.current_start_pos();
        let kind = match &self.token {
            Token::Var => VarKind::Var,
            Token::Let => VarKind::Let,
            Token::Const => VarKind::Const,
            _ => unreachable!(),
        };
        self.advance()?;

        // Gather comma separated declarators
        let mut declarations = self.alloc_vec();
        loop {
            let start_pos = self.current_start_pos();

            // Let and const declarations must keep track of the ending position of the initializer.
            // But this will only be known after parsing initializer.
            let binding_kind = match kind {
                VarKind::Var => BindingKind::Var,
                VarKind::Let => BindingKind::Let { init_pos: Cell::new(0) },
                VarKind::Const => BindingKind::Const { init_pos: Cell::new(0) },
            };

            let id = self.parse_pattern(binding_kind)?;

            let init = match self.token {
                Token::Equals => {
                    self.advance()?;
                    Some(wrap_outer(self.parse_assignment_expression()?))
                }
                _ => None,
            };

            let loc = self.mark_loc(start_pos);

            // Mark the end of the initializer now that it is known
            if kind != VarKind::Var {
                Self::set_binding_init_pos(&id, loc.end);
            }

            declarations.push(VariableDeclarator::new(loc, id, init));

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

        Ok(VariableDeclaration { loc, kind, declarations: declarations.build() })
    }

    fn set_binding_init_pos(pattern: &Pattern, pos: Pos) {
        pattern.iter_bound_names(&mut |id| {
            Self::set_id_binding_init_pos(id, pos);
        })
    }

    fn set_id_binding_init_pos(id: &Identifier, pos: Pos) {
        let binding = id.scope.unwrap_resolved().get_binding(id.name);
        match binding.kind() {
            BindingKind::Const { init_pos }
            | BindingKind::Let { init_pos }
            | BindingKind::Class { init_pos, .. }
            | BindingKind::CatchParameter { init_pos }
            | BindingKind::FunctionParameter { init_pos, .. } => init_pos.set(pos),
            _ => {}
        }
    }

    fn parse_function_declaration(
        &mut self,
        ctx_flags: FunctionContext,
    ) -> ParseResult<P<'a, Function<'a>>> {
        self.parse_function(ctx_flags | FunctionContext::DECLARATION)
    }

    fn parse_function_expression(&mut self) -> ParseResult<P<'a, Function<'a>>> {
        self.parse_function(FunctionContext::empty())
    }

    fn parse_function(&mut self, ctx_flags: FunctionContext) -> ParseResult<P<'a, Function<'a>>> {
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

        let is_decl = ctx_flags.contains(FunctionContext::DECLARATION);

        // Function node must be allocated on heap and then initialized later, so that a pointer to
        // the function node can be passed to the scope builder.
        let mut func = p!(self, Function::new_uninit(self.alloc));

        // For declarations name is required and await/yield inherited from surrounding context,
        // and name is introduces into current scope.
        let mut id = None;
        if is_decl {
            let is_export = ctx_flags.contains(FunctionContext::EXPORT);

            // Name is optional only in the case of export declarations
            if !is_export || self.token != Token::LeftParen {
                // Function is var scoped if it is toplevel, but if function has a label then it is
                // only var scoped if it is non-async and non-generator.
                let is_var_scoped = if ctx_flags.contains(FunctionContext::TOPLEVEL) {
                    if ctx_flags.contains(FunctionContext::LABELED) {
                        !is_async && !is_generator
                    } else {
                        true
                    }
                } else {
                    false
                };

                let binding_kind = BindingKind::Function {
                    is_lexical: !is_var_scoped,
                    is_expression: false,
                    func_node: AstPtr::from_ref(func.as_ref()),
                };

                id = Some(p!(self, self.parse_binding_identifier(Some(binding_kind))?));
            } else {
                // Anonymous exported functions must still be added to scope but under the anonymous
                // default export name.
                let kind = BindingKind::Function {
                    is_lexical: false,
                    is_expression: false,
                    func_node: AstPtr::from_ref(func.as_ref()),
                };

                let loc = self.mark_loc(start_pos);
                self.add_anonymous_default_export_to_scope(loc, kind)?;
            }
        }

        // Enter async/generator context for parsing the function arguments and body
        let did_allow_await = swap_and_save(&mut self.allow_await, is_async);
        let did_allow_yield = swap_and_save(&mut self.allow_yield, is_generator);

        // For expressions name is optional and is within this function's await/yield context.
        // Function expressions do not introduce name into current scope.
        if !is_decl && self.token != Token::LeftParen {
            id = Some(p!(self, self.parse_binding_identifier(None)?))
        }

        // Function scope node must contain the params and body, but not function name.
        let scope = self.enter_function_scope(
            start_pos, /* is_arrow */ false, /* is_expression */ !is_decl,
            /* is_derived_constructor */ false,
        )?;

        // Named function expressions bind name within function scope
        if !is_decl {
            if let Some(id) = &mut id {
                let binding_kind = BindingKind::Function {
                    is_lexical: false,
                    is_expression: true,
                    func_node: AstPtr::from_ref(func.as_ref()),
                };
                self.add_binding(id.as_mut(), binding_kind)?;
            }
        }

        let params = self.parse_function_params()?;
        let param_flags = self.analyze_function_params(&params);

        let (block, strict_flags) = self.parse_function_block_body(param_flags)?;
        let body = p!(self, FunctionBody::Block(block));

        let loc = self.mark_loc(start_pos);

        self.scope_builder.exit_scope();

        let mut flags = param_flags | strict_flags;
        if is_async {
            flags |= FunctionFlags::IS_ASYNC;
        }
        if is_generator {
            flags |= FunctionFlags::IS_GENERATOR;
        }

        func.init(loc, id, params, body, flags, scope);

        self.allow_await = did_allow_await;
        self.allow_yield = did_allow_yield;

        Ok(func)
    }

    fn enter_function_scope(
        &mut self,
        start_pos: Pos,
        is_arrow: bool,
        is_expression: bool,
        is_derived_constructor: bool,
    ) -> ParseResult<AstPtr<AstScopeNode<'a>>> {
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Function {
            id: start_pos,
            is_arrow,
            is_expression,
        });

        // All non-arrow functions have a `this`, which should be treated as a var-scoped binding
        // when determining if it is captured by an arrow function.
        if !is_arrow {
            let kind = BindingKind::ImplicitThis { in_derived_constructor: is_derived_constructor };
            self.scope_builder.add_binding(&THIS_NAME, kind).unwrap();
        }

        Ok(scope)
    }

    /// Parse a list of function parameters, returning the function parameters themselves and
    /// whether the list contains any parameter expressions.
    fn parse_function_params(&mut self) -> ParseResult<AstSlice<'a, FunctionParam<'a>>> {
        self.expect(Token::LeftParen)?;
        let params = self.parse_function_params_until_terminator(Token::RightParen)?;
        self.expect(Token::RightParen)?;

        Ok(params)
    }

    fn parse_function_params_until_terminator(
        &mut self,
        terminator: Token,
    ) -> ParseResult<AstSlice<'a, FunctionParam<'a>>> {
        // Read all function params until the terminator token
        let mut params = self.alloc_vec();
        let mut index = 0;

        while self.token != terminator {
            if self.token == Token::Spread {
                let rest_element =
                    self.parse_rest_element(BindingKind::new_function_parameter(index))?;
                Self::set_binding_init_pos(&rest_element.argument, self.prev_loc.end);

                params.push(FunctionParam::new_rest(rest_element));

                // Trailing commas are not allowed after rest elements, nor are any other params
                if self.token == Token::Comma {
                    return self.error(self.loc, ParseError::RestTrailingComma);
                } else {
                    break;
                }
            }

            let pattern = self.parse_pattern_including_assignment_pattern(
                BindingKind::new_function_parameter(index),
            )?;
            Self::set_binding_init_pos(&pattern, self.prev_loc.end);

            params.push(FunctionParam::new_pattern(pattern));

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }

            index += 1;
        }

        Ok(params.build())
    }

    fn parse_function_params_without_parens(
        &mut self,
    ) -> ParseResult<AstSlice<'a, FunctionParam<'a>>> {
        self.parse_function_params_until_terminator(Token::Eof)
    }

    /// Static analysis of parameters, returning the function flags that apply to the parameters.
    fn analyze_function_params(&mut self, params: &[FunctionParam]) -> FunctionFlags {
        let mut has_parameter_expressions = false;
        let mut has_binding_patterns = false;
        let mut has_rest_parameter = false;
        let mut has_duplicate_parameters = false;

        let mut parameter_names = HashSet::new();

        for param in params {
            if let FunctionParam::Rest { .. } = param {
                has_rest_parameter = true;
            }

            param.iter_patterns(&mut |patt| match patt {
                Pattern::Id(id) => {
                    if parameter_names.contains(id.name) {
                        has_duplicate_parameters = true;
                    } else {
                        parameter_names.insert(id.name);
                    }
                }
                Pattern::Array(_) => {
                    has_binding_patterns = true;
                }
                Pattern::Object(object_pattern) => {
                    has_binding_patterns = true;

                    let has_computed_property = object_pattern
                        .properties
                        .iter()
                        .any(|prop| prop.is_computed);

                    if has_computed_property {
                        has_parameter_expressions = true;
                    }
                }
                Pattern::Assign(_) => {
                    has_parameter_expressions = true;
                }
                Pattern::Member(_) | Pattern::SuperMember(_) => {}
            });
        }

        let mut flags = FunctionFlags::empty();
        if has_parameter_expressions {
            flags |= FunctionFlags::HAS_PARAMETER_EXPRESSIONS;
        }
        if !has_binding_patterns && !has_parameter_expressions && !has_rest_parameter {
            flags |= FunctionFlags::HAS_SIMPLE_PARAMETER_LIST;
        }
        if has_duplicate_parameters {
            flags |= FunctionFlags::HAS_DUPLICATE_PARAMETERS;
        }

        flags
    }

    /// Parse a function's block body, returning the body along with FunctionFlags indicating strict
    /// mode properties of the function.
    fn parse_function_block_body(
        &mut self,
        param_flags: FunctionFlags,
    ) -> ParseResult<(FunctionBlockBody<'a>, FunctionFlags)> {
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

        // The function body only needs its own scope when the function has parameter expressions
        let scope = if param_flags.contains(FunctionFlags::HAS_PARAMETER_EXPRESSIONS) {
            Some(self.scope_builder.enter_scope(ScopeNodeKind::FunctionBody))
        } else {
            None
        };

        let mut body = self.alloc_vec();
        while self.token != Token::RightBrace {
            body.push(self.parse_statement_list_item(FunctionContext::TOPLEVEL)?)
        }

        if scope.is_some() {
            self.scope_builder.exit_scope();
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        // Restore to strict mode context from before this function
        let is_strict_mode = self.in_strict_mode;
        self.set_in_strict_mode(old_in_strict_mode);

        let mut strict_flags = FunctionFlags::empty();
        if is_strict_mode {
            strict_flags |= FunctionFlags::IS_STRICT_MODE;
        }
        if has_use_strict_directive {
            strict_flags |= FunctionFlags::HAS_USE_STRICT_DIRECTIVE;
        }

        Ok((FunctionBlockBody { loc, body: body.build(), scope }, strict_flags))
    }

    fn parse_function_body_statements(
        &mut self,
        initial_state: ParserSaveState<'a>,
    ) -> ParseResult<AstSlice<'a, Statement<'a>>> {
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

        let mut body = self.alloc_vec();
        while self.token != Token::Eof {
            body.push(self.parse_statement_list_item(FunctionContext::TOPLEVEL)?)
        }

        // Restore to strict mode context from before this function
        self.set_in_strict_mode(old_in_strict_mode);

        Ok(body.build())
    }

    fn parse_block(&mut self) -> ParseResult<Block<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Block);

        let mut body = self.alloc_vec();
        while self.token != Token::RightBrace {
            body.push(self.parse_statement_list_item(FunctionContext::empty())?)
        }

        self.scope_builder.exit_scope();

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        Ok(Block { loc, body: body.build(), scope })
    }

    fn parse_if_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let test = self.parse_outer_expression()?;
        self.expect(Token::RightParen)?;

        let conseq = self.parse_statement()?;

        let altern = if self.token == Token::Else {
            self.advance()?;
            Some(self.parse_statement()?)
        } else {
            None
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::If(p!(self, IfStatement { loc, test, conseq, altern })))
    }

    fn parse_switch_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let discriminant = self.parse_outer_expression()?;
        self.expect(Token::RightParen)?;

        // Switch statements start a new block scope
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Switch);

        let mut cases = self.alloc_vec();
        self.expect(Token::LeftBrace)?;

        while self.token != Token::RightBrace {
            match self.token {
                Token::Case | Token::Default => {
                    let case_start_pos = self.current_start_pos();
                    let is_case = self.token == Token::Case;
                    self.advance()?;

                    let test = if is_case {
                        Some(self.parse_outer_expression()?)
                    } else {
                        None
                    };
                    self.expect(Token::Colon)?;

                    // Parse statement list, which will be terminated by the start of another case
                    // or the end of the switch.
                    let mut body = self.alloc_vec();
                    while self.token != Token::Case
                        && self.token != Token::Default
                        && self.token != Token::RightBrace
                    {
                        body.push(self.parse_statement_list_item(FunctionContext::empty())?)
                    }

                    let loc = self.mark_loc(case_start_pos);
                    cases.push(SwitchCase { loc, test, body: body.build() })
                }
                _ => return self.error_expected_token(self.loc, &self.token, &Token::Catch),
            }
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        self.scope_builder.exit_scope();

        let switch_stmt = SwitchStatement { loc, discriminant, cases: cases.build(), scope };

        Ok(Statement::Switch(p!(self, switch_stmt)))
    }

    fn parse_any_for_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // For scope must encompass any variables declared in the initializer, as well as the body.
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Block);

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

                    let init = p!(self, ForEachInit::VarDecl(var_decl));
                    self.parse_for_each_statement(init, start_pos, is_await, scope)?
                }
                _ => {
                    let init = Some(p!(self, ForInit::VarDecl(var_decl)));
                    self.expect(Token::Semicolon)?;
                    self.parse_for_statement(init, start_pos, scope)?
                }
            }
        } else if self.token == Token::Semicolon {
            // Empty init, but we know this is a regular for loop
            self.advance()?;
            self.parse_for_statement(None, start_pos, scope)?
        } else {
            // Otherwise init is an expression
            let expr_start_pos = self.current_start_pos();

            // For-await statements should allow `async of` to be treated as the identifier `async`
            // followed by the `of` keyword. This requires a special check.
            //
            // If parsed normally `async of` would be interpreted as the start of an async arrow
            // function and parsing would fail to force a backtrack.
            let expr = if is_await && self.token == Token::Async && self.peek()? == Token::Of {
                wrap_outer(Expression::Id(p!(self, self.parse_identifier_reference()?)))
            } else {
                // Restrict "in" when parsing for init
                let old_allow_in = swap_and_save(&mut self.allow_in, false);
                let expr = self.parse_outer_expression()?;
                self.allow_in = old_allow_in;

                expr
            };

            match (self.token.clone(), expr.expr) {
                // If this is a for each loop the parsed expression must actually be a pattern
                (Token::In, expr) | (Token::Of, expr) => {
                    let pattern =
                        self.reparse_expression_as_for_left_hand_side(expr, expr_start_pos)?;
                    let left = p!(self, ForEachInit::new_pattern(pattern));
                    self.parse_for_each_statement(left, start_pos, is_await, scope)?
                }
                // An in expression is actually `for (expr in right)`
                (_, Expression::Binary(binary)) if binary.operator == BinaryOperator::In => {
                    let binary = binary.into_inner();

                    self.expect(Token::RightParen)?;
                    let pattern =
                        self.reparse_expression_as_for_left_hand_side(binary.left, expr_start_pos)?;
                    let left = p!(self, ForEachInit::new_pattern(pattern));
                    let body = self.parse_statement()?;
                    let loc = self.mark_loc(start_pos);

                    let for_stmt = ForEachStatement {
                        loc,
                        kind: ForEachKind::In,
                        left,
                        right: wrap_outer(binary.right),
                        body,
                        is_await: false,
                        in_of_pos: binary.operator_pos,
                        scope,
                    };

                    Statement::ForEach(p!(self, for_stmt))
                }
                // Otherwise this is a regular for loop and the expression is used directly
                (_, expr) => {
                    let init = Some(p!(self, ForInit::Expression(wrap_outer(expr))));
                    self.expect(Token::Semicolon)?;
                    self.parse_for_statement(init, start_pos, scope)?
                }
            }
        };

        if is_await {
            match &for_stmt {
                Statement::ForEach(for_each_stmt) if for_each_stmt.kind == ForEachKind::Of => {}
                _ => return self.error(await_loc, ParseError::InvalidForAwait),
            }
        }

        self.scope_builder.exit_scope();

        Ok(for_stmt)
    }

    fn parse_for_statement(
        &mut self,
        init: Option<P<'a, ForInit<'a>>>,
        start_pos: Pos,
        scope: AstPtr<AstScopeNode<'a>>,
    ) -> ParseResult<Statement<'a>> {
        let test = match self.token {
            Token::Semicolon => None,
            _ => Some(self.parse_outer_expression()?),
        };
        self.expect(Token::Semicolon)?;

        let update = match self.token {
            Token::RightParen => None,
            _ => Some(self.parse_outer_expression()?),
        };

        self.expect(Token::RightParen)?;
        let body = self.parse_statement()?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::For(p!(self, ForStatement { loc, init, test, update, body, scope })))
    }

    fn parse_for_each_statement(
        &mut self,
        left: P<'a, ForEachInit<'a>>,
        start_pos: Pos,
        is_await: bool,
        scope: AstPtr<AstScopeNode<'a>>,
    ) -> ParseResult<Statement<'a>> {
        let kind = match self.token {
            Token::In => ForEachKind::In,
            Token::Of => ForEachKind::Of,
            _ => unreachable!(),
        };

        // Save the source location of the `in` or `of` keyword
        let in_of_pos = self.loc.start;

        self.advance()?;

        let right = match kind {
            ForEachKind::In => self.parse_outer_expression()?,
            ForEachKind::Of => wrap_outer(self.parse_assignment_expression()?),
        };

        // Mark the end of the right hand side now that it is known, in order to check for TDZ uses
        // in the right hand side during analysis.
        if let ForEachInit::VarDecl(var_decl) = left.as_ref() {
            if var_decl.kind != VarKind::Var {
                for declarator in var_decl.declarations.iter() {
                    Self::set_binding_init_pos(&declarator.id, self.prev_loc.end);
                }
            }
        }

        self.expect(Token::RightParen)?;
        let body = self.parse_statement()?;
        let loc = self.mark_loc(start_pos);

        let for_each_stmt =
            ForEachStatement { loc, kind, left, right, body, is_await, in_of_pos, scope };

        Ok(Statement::ForEach(p!(self, for_each_stmt)))
    }

    fn parse_while_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let test = self.parse_outer_expression()?;
        self.expect(Token::RightParen)?;

        let body = self.parse_statement()?;

        let loc = self.mark_loc(start_pos);

        Ok(Statement::While(p!(self, WhileStatement { loc, test, body })))
    }

    fn parse_do_while_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let body = self.parse_statement()?;

        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        let test = self.parse_outer_expression()?;
        self.expect(Token::RightParen)?;

        // A semicolon is always automatically inserted after a do while statement
        if self.token == Token::Semicolon {
            self.advance()?;
        }

        let loc = self.mark_loc(start_pos);

        Ok(Statement::DoWhile(p!(self, DoWhileStatement { loc, test, body })))
    }

    fn parse_with_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::LeftParen)?;
        let object = self.parse_outer_expression()?;
        self.expect(Token::RightParen)?;

        // With statements start a new special with scope to mark with boundary
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::With);
        let body = self.parse_statement()?;
        self.scope_builder.exit_scope();

        let loc = self.mark_loc(start_pos);

        Ok(Statement::With(p!(self, WithStatement { loc, object, body, scope })))
    }

    fn parse_try_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let block = p!(self, self.parse_block()?);

        // Optional handler block
        let handler = if self.token == Token::Catch {
            // Set up block scope before catch parameter is parsed, so it is included in block scope
            let scope = self.scope_builder.enter_scope(ScopeNodeKind::Block);

            let catch_start_pos = self.current_start_pos();
            self.advance()?;

            let param = if self.token == Token::LeftBrace {
                None
            } else {
                // Handler optionally has a single pattern as the parameter. Pattern binding must
                // track the ending position of the pattern, but this is only known afer parsing.
                self.expect(Token::LeftParen)?;

                let start_pos = self.current_start_pos();
                let param =
                    self.parse_pattern(BindingKind::CatchParameter { init_pos: Cell::new(0) })?;
                let loc = self.mark_loc(start_pos);

                Self::set_binding_init_pos(&param, loc.end);

                self.expect(Token::RightParen)?;
                Some(param)
            };

            let body = p!(self, self.parse_block()?);
            let loc = self.mark_loc(catch_start_pos);

            self.scope_builder.exit_scope();

            Some(p!(self, CatchClause::new(loc, param, body, scope)))
        } else {
            None
        };

        let finalizer = if self.token == Token::Finally {
            self.advance()?;
            Some(p!(self, self.parse_block()?))
        } else {
            None
        };

        // Must have at least one handler or finalizer
        if handler.is_none() && finalizer.is_none() {
            return self.error_expected_token(self.loc, &self.token, &Token::Catch);
        }

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Try(p!(self, TryStatement { loc, block, handler, finalizer })))
    }

    fn parse_throw_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        if self.lexer.is_new_line_before_current() {
            return self.error(self.loc, ParseError::ThrowArgumentOnNewLine);
        }

        let argument = self.parse_outer_expression()?;
        self.expect_semicolon()?;
        let loc = self.mark_loc(start_pos);

        Ok(Statement::Throw(p!(self, ThrowStatement { loc, argument })))
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = if self.maybe_expect_semicolon()? {
            None
        } else {
            let argument = self.parse_outer_expression()?;
            self.expect_semicolon()?;
            Some(argument)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Return(p!(self, ReturnStatement::new(loc, argument))))
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.maybe_expect_semicolon()? {
            None
        } else {
            let id = self.parse_label_identifier()?;
            let label = Label::new(id.loc, id.name);
            self.expect_semicolon()?;
            Some(label)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Break(p!(self, BreakStatement { loc, label })))
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let label = if self.maybe_expect_semicolon()? {
            None
        } else {
            let id = self.parse_label_identifier()?;
            let label = Label::new(id.loc, id.name);
            self.expect_semicolon()?;
            Some(label)
        };

        let loc = self.mark_loc(start_pos);

        Ok(Statement::Continue(p!(self, ContinueStatement { loc, label })))
    }

    fn parse_outer_expression(&mut self) -> ParseResult<OuterExpression<'a>> {
        Ok(wrap_outer(self.parse_expression()?))
    }

    /// Expression (https://tc39.es/ecma262/#sec-comma-operator)
    fn parse_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        let expr = self.parse_assignment_expression()?;

        if self.token == Token::Comma {
            let mut expressions = self.alloc_vec_with_element(expr);
            while self.token == Token::Comma {
                self.advance()?;
                expressions.push(self.parse_assignment_expression()?);
            }

            let loc = self.mark_loc(start_pos);

            Ok(Expression::Sequence(p!(
                self,
                SequenceExpression { loc, expressions: expressions.build() }
            )))
        } else {
            Ok(expr)
        }
    }

    /// AssignmentExpression (https://tc39.es/ecma262/#sec-assignment-operators)
    fn parse_assignment_expression(&mut self) -> ParseResult<Expression<'a>> {
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

    fn parse_non_arrow_assignment_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();

        if self.allow_yield && self.token == Token::Yield {
            return Ok(Expression::Yield(self.parse_yield_expression()?));
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
                let operator_pos = self.current_start_pos();

                let left = if operator == AssignmentOperator::Equals {
                    self.reparse_expression_as_assignment_left_hand_side(expr, start_pos)?
                } else {
                    self.reparse_expression_as_operator_assignment_left_hand_side(expr, start_pos)?
                };

                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                let assign_expr = AssignmentExpression {
                    loc,
                    left,
                    right,
                    operator,
                    operator_pos,
                    is_parenthesized: false,
                };

                Ok(Expression::Assign(p!(self, assign_expr)))
            }
        };

        // Force parsing as an async function if we see an arrow
        if self.token == Token::Arrow {
            return self.error_unexpected_token(self.loc, &Token::Arrow);
        }

        result
    }

    fn parse_arrow_function(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();

        // Function scope node must contain params and body
        let scope = self.enter_function_scope(
            start_pos, /* is_arrow */ true, /* is_expression */ false,
            /* is_derived_constructor */ false,
        )?;

        let is_async = self.token == Token::Async;
        if is_async {
            let async_loc = self.loc;
            self.advance()?;

            // Special case for when async is actually the single parameter: async => body
            if self.token == Token::Arrow {
                if self.lexer.is_new_line_before_current() {
                    return self.error(self.loc, ParseError::ArrowOnNewLine);
                }

                self.advance()?;

                let mut async_id = Identifier::new(async_loc, ASYNC_ID);
                self.add_binding(&mut async_id, BindingKind::new_function_parameter(0))?;
                Self::set_id_binding_init_pos(&async_id, self.prev_loc.end);

                let id_param = FunctionParam::new_pattern(Pattern::Id(p!(self, async_id)));
                let params = self.alloc_vec_with_element(id_param);
                let param_flags = self.analyze_function_params(&params);

                let (body, strict_flags) = self.parse_arrow_function_body(param_flags)?;
                let loc = self.mark_loc(start_pos);

                self.scope_builder.exit_scope();

                let func = Function::new(
                    loc,
                    /* id */ None,
                    params.build(),
                    body,
                    param_flags | strict_flags | FunctionFlags::IS_ARROW,
                    scope,
                    self.alloc,
                );

                return Ok(Expression::ArrowFunction(p!(self, func)));
            }

            // If newline appears after the async keyword this cannot be a valid async arrow
            // function so fail parsing and report the error from parsing as a non-arrow assignment.
            if self.lexer.is_new_line_before_current() {
                return self.error(self.loc, FAIL_TRY_PARSED_ERROR);
            }
        }

        // Enter async context for parsing the function arguments and body
        let did_allow_await = swap_and_save(&mut self.allow_await, is_async);

        // Arrow function params can be either parenthesized function params or a single id
        let params = match self.token {
            Token::LeftParen => self.parse_function_params()?,
            _ => {
                let id =
                    self.parse_binding_identifier(Some(BindingKind::new_function_parameter(0)))?;
                Self::set_id_binding_init_pos(&id, self.prev_loc.end);

                let id_param = FunctionParam::new_pattern(Pattern::Id(p!(self, id)));
                self.alloc_vec_with_element(id_param).build()
            }
        };
        let param_flags = self.analyze_function_params(&params);

        if self.token == Token::Arrow {
            if self.lexer.is_new_line_before_current() {
                return self.error(self.loc, ParseError::ArrowOnNewLine);
            }

            self.advance()?;
        } else {
            self.expect(Token::Arrow)?;
        }

        let (body, strict_flags) = self.parse_arrow_function_body(param_flags)?;
        let loc = self.mark_loc(start_pos);

        self.allow_await = did_allow_await;
        self.scope_builder.exit_scope();

        let mut flags = param_flags | strict_flags | FunctionFlags::IS_ARROW;
        if is_async {
            flags |= FunctionFlags::IS_ASYNC;
        }

        let func = Function::new(loc, /* id */ None, params, body, flags, scope, self.alloc);

        Ok(Expression::ArrowFunction(p!(self, func)))
    }

    /// Parse an arrow function's body, returning the body along with FunctionFlags indicating
    /// strict mode properties of the arrow function.
    fn parse_arrow_function_body(
        &mut self,
        param_flags: FunctionFlags,
    ) -> ParseResult<(P<'a, FunctionBody<'a>>, FunctionFlags)> {
        if self.token == Token::LeftBrace {
            let (block, strict_flags) = self.parse_function_block_body(param_flags)?;
            Ok((p!(self, FunctionBody::Block(block)), strict_flags))
        } else {
            // Arrow function expression body cannot have a "use strict" directive, so inherits
            // strict mode from surrounding context.
            let mut strict_flags = FunctionFlags::empty();
            if self.in_strict_mode {
                strict_flags |= FunctionFlags::IS_STRICT_MODE;
            }

            Ok((
                p!(self, FunctionBody::Expression(wrap_outer(self.parse_assignment_expression()?))),
                strict_flags,
            ))
        }
    }

    fn parse_yield_expression(&mut self) -> ParseResult<P<'a, YieldExpression<'a>>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Check for the end of an assignment expression, either due to ASI or encountering a token
        // that signals the end of an assignment expression.
        if self.maybe_expect_semicolon()? {
            let loc = self.mark_loc(start_pos);
            return Ok(p!(self, YieldExpression { loc, argument: None, is_delegate: false }));
        }

        let is_assignment_expression_end = matches!(
            self.token,
            Token::RightParen
                | Token::RightBracket
                | Token::Comma
                | Token::Colon
                | Token::In
                | Token::Of
        );
        if is_assignment_expression_end {
            let loc = self.mark_loc(start_pos);
            return Ok(p!(self, YieldExpression { loc, argument: None, is_delegate: false }));
        }

        let is_delegate = self.token == Token::Multiply;
        if is_delegate {
            self.advance()?;
        }

        let argument = Some(self.parse_assignment_expression()?);
        let loc = self.mark_loc(start_pos);

        Ok(p!(self, YieldExpression { loc, argument, is_delegate }))
    }

    /// ConditionalExpression (https://tc39.es/ecma262/#sec-conditional-operator)
    fn parse_conditional_expression(&mut self) -> ParseResult<Expression<'a>> {
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

            let conditional_expr = ConditionalExpression { loc, test: expr, conseq, altern };

            Ok(Expression::Conditional(p!(self, conditional_expr)))
        } else {
            Ok(expr)
        }
    }

    /// Precedence parsing for all binary operations and below.
    /// Corresponds to ShortCircuitExpression (https://tc39.es/ecma262/#sec-binary-logical-operators)
    fn parse_expression_with_precedence(
        &mut self,
        precedence: Precedence,
    ) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();

        // Private names must be the start of an `in` expression
        if self.token == Token::Hash {
            let private_name = Expression::Id(p!(self, self.parse_private_name()?));

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
            let current_expr_raw = current_expr.as_raw();
            let next_expr = self.parse_expression_infix(current_expr, precedence, start_pos)?;
            if current_expr_raw == next_expr.as_raw() {
                return Ok(next_expr);
            }

            current_expr = next_expr;
        }
    }

    fn parse_expression_prefix(&mut self) -> ParseResult<Expression<'a>> {
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
        left: Expression<'a>,
        precedence: Precedence,
        start_pos: Pos,
    ) -> ParseResult<Expression<'a>> {
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
        left: Expression<'a>,
        start_pos: Pos,
        operator: BinaryOperator,
        precedence: Precedence,
    ) -> ParseResult<Expression<'a>> {
        let operator_pos = self.current_start_pos();

        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        let binary_expr = BinaryExpression { loc, left, right, operator, operator_pos };

        Ok(Expression::Binary(p!(self, binary_expr)))
    }

    fn parse_logical_expression(
        &mut self,
        left: Expression<'a>,
        start_pos: Pos,
        operator: LogicalOperator,
        precedence: Precedence,
    ) -> ParseResult<Expression<'a>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(precedence)?;
        let loc = self.mark_loc(start_pos);

        let logical_expr = LogicalExpression { loc, left, right, operator };

        Ok(Expression::Logical(p!(self, logical_expr)))
    }

    fn parse_nullish_coalesce_expression(
        &mut self,
        left: Expression<'a>,
        start_pos: Pos,
    ) -> ParseResult<Expression<'a>> {
        self.advance()?;
        let right = self.parse_expression_with_precedence(Precedence::NullishCoalesce)?;
        let loc = self.mark_loc(start_pos);

        // Check if mixed with an unparenthesized logical left or right expression. Can check for
        // parens by comparing parent expression start/end pos to inner expression start/end pos.
        if let Expression::Logical(left_expr) = &left {
            if left_expr.operator != LogicalOperator::NullishCoalesce
                && left_expr.loc.start == loc.start
            {
                return self.error(loc, ParseError::NullishCoalesceMixedWithLogical);
            }
        } else if let Expression::Logical(right_expr) = &right {
            if right_expr.operator != LogicalOperator::NullishCoalesce
                && right_expr.loc.end == loc.end
            {
                return self.error(loc, ParseError::NullishCoalesceMixedWithLogical);
            }
        }

        let logical_expr =
            LogicalExpression { loc, left, right, operator: LogicalOperator::NullishCoalesce };

        Ok(Expression::Logical(p!(self, logical_expr)))
    }

    fn parse_update_expression_prefix(
        &mut self,
        operator: UpdateOperator,
    ) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        if !self.is_valid_assignment_target(&argument) {
            return self.error(loc, ParseError::InvalidUpdateExpressionArgument);
        }

        let update_expr = UpdateExpression { loc, operator, argument, is_prefix: true };

        Ok(Expression::Update(p!(self, update_expr)))
    }

    fn parse_update_expression_postfix(
        &mut self,
        argument: Expression<'a>,
        start_pos: Pos,
        operator: UpdateOperator,
    ) -> ParseResult<Expression<'a>> {
        self.advance()?;
        let loc = self.mark_loc(start_pos);

        if !self.is_valid_assignment_target(&argument) {
            return self.error(loc, ParseError::InvalidUpdateExpressionArgument);
        }

        let update_expr = UpdateExpression { loc, operator, argument, is_prefix: false };

        Ok(Expression::Update(p!(self, update_expr)))
    }

    fn parse_unary_expression(&mut self, operator: UnaryOperator) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        // Unary expressions cannot be followed directly by an exponentiation expression
        if self.token == Token::Exponent {
            return self.error(loc, ParseError::ExponentLHSUnary);
        }

        Ok(Expression::Unary(p!(self, UnaryExpression { loc, operator, argument })))
    }

    fn parse_await_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_expression_with_precedence(Precedence::Unary)?;
        let loc = self.mark_loc(start_pos);

        Ok(Expression::Await(p!(self, AwaitExpression { loc, argument })))
    }

    /// LeftHandSideExpression (https://tc39.es/ecma262/#sec-left-hand-side-expressions)
    fn parse_left_hand_side_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        let expr = match &self.token {
            Token::New => self.parse_new_expression()?,
            Token::Super => self.parse_super_expression(/* allow_call */ true)?,
            Token::Import => self.parse_import_expression(/* allow_call */ true)?,
            _ => self.parse_primary_expression()?,
        };

        self.parse_call_expression(
            expr, start_pos, /* allow_call */ true, /* in_optional_chain */ false,
        )
    }

    fn parse_call_expression(
        &mut self,
        expr: Expression<'a>,
        start_pos: Pos,
        allow_call: bool,
        in_optional_chain: bool,
    ) -> ParseResult<Expression<'a>> {
        let expr = self.parse_member_expression(expr, start_pos, allow_call, in_optional_chain)?;
        match self.token {
            Token::LeftParen if allow_call => {
                let arguments = self.parse_call_arguments()?;
                let loc = self.mark_loc(start_pos);

                // A potential sloppy direct eval needs to marked in the scope tree before analysis
                match &expr {
                    Expression::Id(id) if id.name == "eval" => {
                        if !self.in_strict_mode {
                            self.scope_builder.mark_sloppy_direct_eval();
                        }
                    }
                    _ => {}
                }

                let call_expr = Expression::Call(p!(
                    self,
                    CallExpression::new(loc, expr, arguments, /* is_optional */ false,)
                ));

                self.parse_call_expression(call_expr, start_pos, allow_call, in_optional_chain)
            }
            _ => Ok(expr),
        }
    }

    fn parse_member_expression(
        &mut self,
        expr: Expression<'a>,
        start_pos: Pos,
        allow_call: bool,
        in_optional_chain: bool,
    ) -> ParseResult<Expression<'a>> {
        match &self.token {
            Token::Period => {
                let operator_pos = self.current_start_pos();
                self.advance()?;

                let member_expr =
                    self.parse_property_member_expression(expr, start_pos, operator_pos, false)?;

                self.parse_call_expression(member_expr, start_pos, allow_call, in_optional_chain)
            }
            Token::LeftBracket => {
                let member_expr = self.parse_computed_member_expression(expr, start_pos, false)?;

                self.parse_call_expression(member_expr, start_pos, allow_call, in_optional_chain)
            }
            Token::TemplatePart(TemplatePartToken { raw, cooked, is_tail, is_head: _ }) => {
                if in_optional_chain {
                    return self.error(self.loc, ParseError::TaggedTemplateInChain);
                }

                // Malformed escape sequence errors are swalled in tagged template, and cooked
                // string is marked as None.
                let cooked = cooked.ok();

                let quasi = p!(
                    self,
                    self.parse_template_literal(raw, cooked, *is_tail, /* is_tagged */ true,)?
                );
                let loc = self.mark_loc(start_pos);

                let tagged_template_expr = Expression::TaggedTemplate(p!(
                    self,
                    TaggedTemplateExpression { loc, tag: expr, quasi }
                ));

                self.parse_call_expression(
                    tagged_template_expr,
                    start_pos,
                    allow_call,
                    in_optional_chain,
                )
            }
            // Start of an optional chain
            Token::QuestionDot => {
                let operator_pos = self.current_start_pos();
                self.advance()?;

                match self.token {
                    // Optional call
                    Token::LeftParen => {
                        let arguments = self.parse_call_arguments()?;
                        let loc = self.mark_loc(start_pos);

                        let call_expr = Expression::Call(p!(
                            self,
                            CallExpression::new(loc, expr, arguments, /* is_optional */ true,)
                        ));

                        let full_expr =
                            self.parse_call_expression(call_expr, start_pos, allow_call, true)?;

                        // Wrap in chain expression if this is the first optional part
                        if !in_optional_chain {
                            let loc = self.mark_loc(start_pos);
                            let chain_expr = ChainExpression { loc, expression: full_expr };
                            Ok(Expression::Chain(p!(self, chain_expr)))
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
                            let chain_expr = ChainExpression { loc, expression: full_expr };
                            Ok(Expression::Chain(p!(self, chain_expr)))
                        } else {
                            Ok(full_expr)
                        }
                    }
                    // Optional simple property access
                    _ => {
                        // Operator pos should point to the `.`, not the start of `?.`
                        let operator_pos = operator_pos + 1;
                        let member_expr = self.parse_property_member_expression(
                            expr,
                            start_pos,
                            operator_pos,
                            true,
                        )?;

                        let full_expr =
                            self.parse_call_expression(member_expr, start_pos, allow_call, true)?;

                        // Wrap in chain expression if this is the first optional part
                        if !in_optional_chain {
                            let loc = self.mark_loc(start_pos);
                            let chain_expr = ChainExpression { loc, expression: full_expr };
                            Ok(Expression::Chain(p!(self, chain_expr)))
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
        object: Expression<'a>,
        start_pos: Pos,
        operator_pos: Pos,
        is_optional: bool,
    ) -> ParseResult<Expression<'a>> {
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

        let member_expr = MemberExpression {
            loc,
            object,
            property: Expression::Id(p!(self, property)),
            is_computed: false,
            is_optional,
            is_private,
            operator_pos,
        };

        Ok(Expression::Member(p!(self, member_expr)))
    }

    fn parse_computed_member_expression(
        &mut self,
        object: Expression<'a>,
        start_pos: Pos,
        is_optional: bool,
    ) -> ParseResult<Expression<'a>> {
        let operator_pos = self.current_start_pos();

        self.advance()?;
        let property = self.parse_expression()?;
        self.expect(Token::RightBracket)?;
        let loc = self.mark_loc(start_pos);

        let member_expr = MemberExpression {
            loc,
            object,
            property,
            is_computed: true,
            is_optional,
            is_private: false,
            operator_pos,
        };

        Ok(Expression::Member(p!(self, member_expr)))
    }

    fn parse_new_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();

        self.advance()?;

        // New is followed by MemberExpression in the spec. This is a subset of left hand side
        // expressions which must start with only new expressions, meta properties, super
        // properties, and primary expressions.
        let callee_start_pos = self.current_start_pos();
        let callee = match self.token {
            // Parse new.target meta property
            Token::Period => {
                self.advance()?;
                return if self.token == Token::Target {
                    self.advance()?;
                    let loc = self.mark_loc(start_pos);

                    Ok(Expression::MetaProperty(p!(self, MetaProperty::new_target(loc))))
                } else {
                    self.error(self.loc, ParseError::ExpectedNewTarget)
                };
            }
            Token::New => self.parse_new_expression()?,
            Token::Import => self.parse_import_expression(/* allow_call */ false)?,
            Token::Super => self.parse_super_expression(/* allow_call */ false)?,
            _ => self.parse_primary_expression()?,
        };

        // Disallow call since parenthesized arguments should be attached to this new instead
        let callee = self.parse_member_expression(callee, callee_start_pos, false, false)?;

        let arguments = if self.token == Token::LeftParen {
            self.parse_call_arguments()?
        } else {
            AstSlice::new_empty()
        };

        let loc = self.mark_loc(start_pos);
        Ok(Expression::New(p!(self, NewExpression { loc, callee, arguments })))
    }

    fn parse_super_expression(&mut self, allow_call: bool) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        let super_loc = self.loc;
        self.advance()?;

        match self.token {
            Token::Period => {
                let operator_pos = self.current_start_pos();
                self.advance()?;

                let id = match self.parse_identifier_name()? {
                    Some(id) => id,
                    None => return self.error_unexpected_token(self.loc, &self.token),
                };

                let loc = self.mark_loc(start_pos);

                let super_member = SuperMemberExpression::new(
                    loc,
                    super_loc,
                    operator_pos,
                    Expression::Id(p!(self, id)),
                    /* is_computed */ false,
                );

                Ok(Expression::SuperMember(p!(self, super_member)))
            }
            Token::LeftBracket => {
                let operator_pos = self.current_start_pos();
                self.advance()?;

                let property = self.parse_expression()?;
                self.expect(Token::RightBracket)?;
                let loc = self.mark_loc(start_pos);

                let super_member = SuperMemberExpression::new(
                    loc,
                    super_loc,
                    operator_pos,
                    property,
                    /* is_computed */ true,
                );

                Ok(Expression::SuperMember(p!(self, super_member)))
            }
            Token::LeftParen if allow_call => {
                let arguments = self.parse_call_arguments()?;
                let loc = self.mark_loc(start_pos);

                let super_call = SuperCallExpression::new(loc, super_loc, arguments);

                Ok(Expression::SuperCall(p!(self, super_call)))
            }
            _ => self.error_expected_token(self.loc, &self.token, &Token::LeftParen),
        }
    }

    fn parse_import_expression(&mut self, allow_call: bool) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();

        self.advance()?;

        match &self.token {
            // Parse import.meta meta property
            Token::Period => {
                self.advance()?;
                if self.token == Token::Meta {
                    self.advance()?;
                    let loc = self.mark_loc(start_pos);

                    if self.program_kind != ProgramKind::Module {
                        return self.error(self.loc, ParseError::ImportMetaOutsideModule);
                    }

                    let meta_property = MetaProperty { loc, kind: MetaPropertyKind::ImportMeta };

                    Ok(Expression::MetaProperty(p!(self, meta_property)))
                } else {
                    self.error(self.loc, ParseError::ExpectedImportMeta)
                }
            }
            // Otherwise must be a dynamic import expression
            _ if allow_call => {
                self.expect(Token::LeftParen)?;

                // First argument is required, is the module specifier string
                let source = self.parse_assignment_expression()?;

                // Second argument is optional, is the options object
                let options = if self.token == Token::Comma {
                    self.advance()?;

                    // Allow a trailing comma after the source argument
                    if self.token == Token::RightParen {
                        None
                    } else {
                        let options = self.parse_assignment_expression()?;

                        // Allow a trailing comma after options argument
                        if self.token == Token::Comma {
                            self.advance()?;
                        }

                        Some(options)
                    }
                } else {
                    None
                };

                self.expect(Token::RightParen)?;

                let loc = self.mark_loc(start_pos);

                Ok(Expression::Import(p!(self, ImportExpression { loc, source, options })))
            }
            _ => {
                let loc = self.mark_loc(start_pos);
                self.error(loc, ParseError::ExpectedImportMeta)
            }
        }
    }

    fn parse_call_arguments(&mut self) -> ParseResult<AstSlice<'a, CallArgument<'a>>> {
        self.expect(Token::LeftParen)?;

        let mut arguments = self.alloc_vec();
        while self.token != Token::RightParen {
            if self.token == Token::Spread {
                arguments.push(CallArgument::Spread(self.parse_spread_element()?))
            } else {
                arguments.push(CallArgument::Expression(self.parse_assignment_expression()?));
            }

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightParen)?;

        Ok(arguments.build())
    }

    /// PrimaryExpression (https://tc39.es/ecma262/#sec-primary-expression)
    fn parse_primary_expression(&mut self) -> ParseResult<Expression<'a>> {
        match &self.token {
            Token::Null => {
                let loc = self.loc;
                self.advance()?;
                Ok(Expression::Null(p!(self, loc)))
            }
            Token::True | Token::False => {
                let value = self.token == Token::True;
                let loc = self.loc;
                self.advance()?;
                Ok(Expression::Boolean(p!(self, BooleanLiteral { loc, value })))
            }
            Token::NumberLiteral(value) => {
                let loc = self.loc;
                let value = *value;
                self.advance()?;
                Ok(Expression::Number(p!(self, NumberLiteral { loc, value })))
            }
            Token::StringLiteral(_) => Ok(Expression::String(self.parse_string_literal()?)),
            Token::BigIntLiteral { digits_slice, base } => {
                // Parse the BigInt value directly from the slice of source digits
                let value = BigInt::parse_bytes(digits_slice.as_bytes(), *base as u32).unwrap();

                let loc = self.loc;
                self.advance()?;

                Ok(Expression::BigInt(p!(self, BigIntLiteral::new(loc, value, self.alloc))))
            }
            // RegExp may be started by "/=" which is treated as a single token
            Token::Divide | Token::DivideEq => {
                let regexp = self.parse_regexp_literal()?;
                Ok(Expression::RegExp(p!(self, regexp)))
            }
            Token::This => {
                let loc = self.loc;
                self.advance()?;
                Ok(Expression::This(p!(self, ThisExpression { loc, scope: None })))
            }
            Token::LeftParen => {
                self.advance()?;
                let mut expr = self.parse_expression()?;
                self.expect(Token::RightParen)?;

                // Mark expression which need to know if they are parenthesized, in order for them
                // to be potentially reparsed into patterns.
                match &mut expr {
                    Expression::Assign(expr) => expr.is_parenthesized = true,
                    Expression::Object(expr) => expr.is_parenthesized = true,
                    Expression::Array(expr) => expr.is_parenthesized = true,
                    _ => {}
                }

                Ok(expr)
            }
            Token::LeftBrace => self.parse_object_expression(),
            Token::LeftBracket => self.parse_array_expression(),
            Token::Class => {
                let class =
                    self.parse_class(/* is_decl */ false, /* is_export */ false)?;
                Ok(Expression::Class(class))
            }
            Token::TemplatePart(TemplatePartToken { raw, cooked, is_tail, is_head: _ }) => {
                // Non-tagged template literals error on malformed escape sequences
                let cooked = match cooked {
                    Ok(cooked) => Some(*cooked),
                    Err(loc) => return self.error(*loc, ParseError::MalformedEscapeSeqence),
                };

                let template_literal =
                    self.parse_template_literal(raw, cooked, *is_tail, /* is_tagged */ false)?;
                Ok(Expression::Template(p!(self, template_literal)))
            }
            _ => {
                if self.is_function_start()? {
                    return Ok(Expression::Function(self.parse_function_expression()?));
                }

                // Check for the start of an async arrow function
                if self.token == Token::Async {
                    let async_loc = self.loc;
                    self.advance()?;

                    // `async [newline] id` is an `async` identifier with ASI followed by another
                    // identifier instead of the start of an async arrow function.
                    if self.lexer.is_new_line_before_current() {
                        let async_id = Identifier::new(async_loc, ASYNC_ID);
                        return Ok(Expression::Id(p!(self, async_id)));
                    }

                    // If followed by an identifier this is `async id`
                    let id_token = self.token.clone();
                    let id_loc = self.loc;
                    if self.parse_binding_identifier(None).is_ok() {
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
                        let async_id = Identifier::new(async_loc, ASYNC_ID);
                        return Ok(Expression::Id(p!(self, async_id)));
                    }
                }

                Ok(Expression::Id(p!(self, self.parse_identifier_reference()?)))
            }
        }
    }

    fn parse_string_literal(&mut self) -> ParseResult<P<'a, StringLiteral<'a>>> {
        if let Token::StringLiteral(value) = &self.token {
            let loc = self.loc;
            let value = *value;
            self.advance()?;

            Ok(p!(self, StringLiteral { loc, value }))
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn parse_identifier_reference(&mut self) -> ParseResult<Identifier<'a>> {
        self.parse_identifier()
    }

    /// Parse an identifier that may introduce a binding into the current scope. Add binding to
    /// scope if binding kind is provided.
    fn parse_binding_identifier(
        &mut self,
        binding_kind: Option<BindingKind<'a>>,
    ) -> ParseResult<Identifier<'a>> {
        let mut id = self.parse_identifier()?;

        if let Some(binding_kind) = binding_kind {
            self.add_binding(&mut id, binding_kind)?;
        }

        Ok(id)
    }

    fn parse_label_identifier(&mut self) -> ParseResult<Identifier<'a>> {
        self.parse_identifier()
    }

    fn parse_imported_identifier(&mut self) -> ParseResult<Identifier<'a>> {
        let mut id = self.parse_identifier()?;
        self.add_binding(&mut id, BindingKind::Import { is_namespace: false })?;
        Ok(id)
    }

    fn parse_imported_namespace_identifier(&mut self) -> ParseResult<Identifier<'a>> {
        let mut id = self.parse_identifier()?;
        self.add_binding(&mut id, BindingKind::Import { is_namespace: true })?;
        Ok(id)
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier<'a>> {
        if Self::is_identifier(&self.token, self.in_strict_mode, self.allow_await, self.allow_yield)
        {
            if let Token::Identifier(name) = &self.token {
                let name = *name;
                let loc = self.loc;
                self.advance()?;
                Ok(Identifier::new(loc, name))
            } else {
                let loc = self.loc;
                let name = self.token_str();

                self.advance()?;

                Ok(Identifier::new(loc, name))
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
            Token::Identifier(name) => {
                !Self::is_reserved_word(name.as_bytes(), in_strict_mode, allow_await, allow_yield)
            }
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
    fn parse_identifier_name(&mut self) -> ParseResult<Option<Identifier<'a>>> {
        match &self.token {
            Token::Identifier(name) => {
                let loc = self.loc;
                let name = *name;
                self.advance()?;
                Ok(Some(Identifier::new(loc, name)))
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
                let name = self.token_str();
                self.advance()?;
                Ok(Some(Identifier::new(loc, name)))
            }
            _ => Ok(None),
        }
    }

    fn is_reserved_word_in_current_context(&self, str: AstStr) -> bool {
        Self::is_reserved_word(
            str.as_bytes(),
            self.in_strict_mode,
            self.allow_await,
            self.allow_yield,
        )
    }

    fn is_reserved_word(
        str: &[u8],
        in_strict_mode: bool,
        allow_await: bool,
        allow_yield: bool,
    ) -> bool {
        match str {
            // Names that are always reserved
            b"break" | b"case" | b"catch" | b"class" | b"const" | b"continue" | b"debugger"
            | b"default" | b"delete" | b"do" | b"else" | b"enum" | b"export" | b"extends"
            | b"false" | b"finally" | b"for" | b"function" | b"if" | b"import" | b"in"
            | b"instanceof" | b"new" | b"null" | b"return" | b"super" | b"switch" | b"this"
            | b"throw" | b"true" | b"try" | b"typeof" | b"var" | b"void" | b"while" | b"with" => {
                true
            }
            // Names that are only reserved in strict mode
            b"let" | b"static" | b"implements" | b"interface" | b"package" | b"private"
            | b"protected" | b"public"
                if in_strict_mode =>
            {
                true
            }
            b"await" => allow_await,
            b"yield" => in_strict_mode || allow_yield,
            _ => false,
        }
    }

    fn parse_regexp_literal(&mut self) -> ParseResult<RegExpLiteral<'a>> {
        let start_pos = self.current_start_pos();

        self.advance_regexp_literal()?;

        if let Token::RegExpLiteral(RegExpToken { raw, pattern, flags }) = &self.token {
            let raw = *raw;
            let pattern = *pattern;
            let flags_string = *flags;

            self.advance()?;
            let loc = self.mark_loc(start_pos);
            let source = self.lexer.source.clone();

            // Start position of pattern is offset by one to account for the leading `/`
            let pattern_start_pos = start_pos + 1;

            // Start position of flags is offset by two `/` characters and the entire pattern
            let flags_start_pos = start_pos + 2 + pattern.len();
            let lexer_stream =
                Utf8LexerStream::new(flags_start_pos, source.clone(), flags_string.as_bytes());
            let flags = RegExpParser::parse_flags(lexer_stream)?;

            // If in unicode mode then can parse UTF-8 string directly as full code points
            let regexp = if flags.has_any_unicode_flag() {
                let create_lexer_stream =
                    || Utf8LexerStream::new(pattern_start_pos, source.clone(), pattern.as_bytes());
                p!(
                    self,
                    RegExpParser::parse_regexp(
                        &create_lexer_stream,
                        flags,
                        &self.options,
                        self.alloc,
                    )?
                )
            } else {
                // Otherwise must first translate UTF-8 to UTF-16, then treat as individual code
                // units.
                let mut utf16_pattern: Vec<u16> = vec![];
                let mut buf = [0; 2];

                for code_point in pattern.iter_code_points() {
                    let byte_length = encode_utf16_codepoint(&mut buf, code_point);
                    utf16_pattern.extend_from_slice(&buf[..byte_length]);
                }

                let create_lexer_stream =
                    || HeapTwoByteCodeUnitLexerStream::new(&utf16_pattern, Some(source.clone()));
                let parse_result = RegExpParser::parse_regexp(
                    &create_lexer_stream,
                    flags,
                    &self.options,
                    self.alloc,
                );

                match parse_result {
                    Ok(regexp) => p!(self, regexp),
                    Err(mut error) => {
                        // Error loc will be reported in the number of UTF-16 code units since the
                        // start of the regexp.
                        let loc = &mut error.source_loc.as_mut().unwrap().0;
                        let target_utf16_offset = loc.start;

                        // Convert from a UTF-16 code unit offset to UTF-8 byte offset
                        let mut utf8_offset = 0;
                        let mut utf16_offset = 0;

                        // Iterate through the code points in the pattern and keep track of the
                        // total UTF-8 and UTF-16 offsets. Stop when we reach the target UTF-16
                        // offset.
                        for code_point in pattern.iter_code_points() {
                            if utf16_offset == target_utf16_offset {
                                break;
                            }

                            utf8_offset += utf8_byte_count(code_point);
                            utf16_offset += utf16_code_unit_count(code_point);

                            debug_assert!(utf16_offset <= target_utf16_offset);
                        }

                        // Rewrite the loc to point to the position in the source string
                        let pos = pattern_start_pos + utf8_offset;
                        loc.start = pos;
                        loc.end = pos;

                        return Err(error);
                    }
                }
            };

            Ok(RegExpLiteral { loc, raw, pattern, flags: flags_string, regexp })
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn parse_template_literal(
        &mut self,
        raw: AstStr<'a>,
        cooked: Option<AstStr<'a>>,
        is_single_quasi: bool,
        is_tagged: bool,
    ) -> ParseResult<TemplateLiteral<'a>> {
        let start_pos = self.current_start_pos();

        let head_quasi = TemplateElement { loc: self.loc, raw, cooked };

        self.advance()?;

        let mut quasis = self.alloc_vec_with_element(head_quasi);
        let mut expressions = self.alloc_vec();

        if !is_single_quasi {
            loop {
                expressions.push(self.parse_expression()?);

                if self.token != Token::RightBrace {
                    return self.error_expected_token(self.loc, &self.token, &Token::RightBrace);
                }

                self.advance_template_part()?;

                if let Token::TemplatePart(TemplatePartToken {
                    raw,
                    cooked,
                    is_tail,
                    is_head: false,
                }) = &self.token
                {
                    // In non-tagged templates malformed escape sequences throw an error, otherwise
                    // malformed sequences are swallowed and cooked string is marked as None.
                    let cooked = match cooked {
                        Ok(cooked) => Some(*cooked),
                        Err(loc) => {
                            if is_tagged {
                                None
                            } else {
                                return self.error(*loc, ParseError::MalformedEscapeSeqence);
                            }
                        }
                    };

                    quasis.push(TemplateElement { loc: self.loc, raw, cooked });

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

        Ok(TemplateLiteral {
            loc,
            quasis: quasis.build(),
            expressions: expressions.build(),
        })
    }

    fn parse_spread_element(&mut self) -> ParseResult<SpreadElement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;
        let argument = self.parse_assignment_expression()?;
        let loc = self.mark_loc(start_pos);

        let has_trailing_comma = self.token == Token::Comma;

        Ok(SpreadElement { loc, argument, has_trailing_comma })
    }

    fn parse_array_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let mut elements = self.alloc_vec();
        while self.token != Token::RightBracket {
            match self.token {
                Token::Comma => {
                    let hole_pos = self.current_start_pos();
                    self.advance()?;
                    elements.push(ArrayElement::Hole(hole_pos));
                    continue;
                }
                Token::Spread => {
                    elements.push(ArrayElement::Spread(self.parse_spread_element()?));
                }
                _ => {
                    elements.push(ArrayElement::Expression(self.parse_assignment_expression()?));
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

        let array_expr =
            ArrayExpression { loc, elements: elements.build(), is_parenthesized: false };

        Ok(Expression::Array(p!(self, array_expr)))
    }

    fn parse_object_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Body of the object literal is in its own scope that contains the home object
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Block);
        self.add_home_object_to_scope(/* include_static */ false);

        let mut properties = self.alloc_vec();
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
                let (property, _) = self.parse_property(
                    PropertyContext::Object,
                    /* in_class_with_super */ false,
                    /* init_scope */ None,
                )?;
                properties.push(property);
            }

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        self.scope_builder.exit_scope();

        let object_expr = ObjectExpression {
            loc,
            properties: properties.build(),
            is_parenthesized: false,
            scope,
        };

        Ok(Expression::Object(p!(self, object_expr)))
    }

    fn parse_property(
        &mut self,
        prop_context: PropertyContext,
        in_class_with_super: bool,
        init_scope: Option<&mut Option<AstPtr<AstScopeNode<'a>>>>,
    ) -> ParseResult<(Property<'a>, bool)> {
        let start_pos = self.current_start_pos();

        // Handle getters and setters
        match self.token {
            Token::Get | Token::Set => {
                let id_loc = self.loc;
                let kind = if let Token::Get = self.token {
                    PropertyKind::Get
                } else {
                    PropertyKind::Set
                };

                self.advance()?;

                // Handle `get` or `set` as name of method
                if self.token == Token::LeftParen {
                    let id_name = if matches!(kind, PropertyKind::Get) {
                        GET_ID
                    } else {
                        SET_ID
                    };

                    let name = Expression::Id(p!(self, Identifier::new(id_loc, id_name)));

                    return self.parse_method_property(
                        name,
                        start_pos,
                        PropertyKind::Init,
                        /* is_async */ false,
                        /* is_generator */ false,
                        /* is_computed */ false,
                        /* is_private */ false,
                        /* is_derived_constructor */ false,
                    );
                }

                // Handle `get` or `set` as shorthand or init property
                let is_init_property = self.is_property_initializer(prop_context)
                    || self.is_pattern_initializer_in_object(prop_context);
                if is_init_property || self.is_property_end(prop_context) {
                    let id_name = if matches!(kind, PropertyKind::Get) {
                        GET_ID
                    } else {
                        SET_ID
                    };

                    let name = Expression::Id(p!(self, Identifier::new(id_loc, id_name)));

                    return self.parse_init_property(
                        name,
                        start_pos,
                        prop_context,
                        init_scope,
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
                    /* is_derived_constructor */ false,
                );
            }
            _ => (),
        }

        // Async method (or method with name async)
        if self.token == Token::Async {
            let async_loc = self.loc;
            self.advance()?;

            let newline_after_async = self.lexer.is_new_line_before_current();

            // Handle `async` as name of method: `async() {}`
            if self.token == Token::LeftParen {
                let async_id = Identifier::new(async_loc, ASYNC_ID);
                let name = Expression::Id(p!(self, async_id));

                return self.parse_method_property(
                    name,
                    start_pos,
                    PropertyKind::Init,
                    /* is_async */ false,
                    /* is_generator */ false,
                    /* is_computed */ false,
                    /* is_private */ false,
                    /* is_derived_constructor */ false,
                );
            }

            // Handle `async` as shorthand or init property
            let is_init_property = self.is_property_initializer(prop_context)
                || self.is_pattern_initializer_in_object(prop_context)
                || newline_after_async;
            if is_init_property || self.is_property_end(prop_context) {
                let async_id = Identifier::new(async_loc, ASYNC_ID);
                let name = Expression::Id(p!(self, async_id));

                return self.parse_init_property(
                    name,
                    start_pos,
                    prop_context,
                    init_scope,
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
                /* id_derived_constructor */ false,
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
                /* is_derived_constructor */ false,
            );
        }

        // Regular init and method properties
        let property_name = self.parse_property_name(prop_context)?;

        match self.token {
            Token::LeftParen => {
                let is_derived_constructor = if in_class_with_super
                    && !property_name.is_computed
                    && !property_name.is_private
                {
                    Self::is_constructor_key(&property_name.key)
                } else {
                    false
                };

                self.parse_method_property(
                    property_name.key,
                    start_pos,
                    PropertyKind::Init,
                    /* is_async */ false,
                    /* is_generator */ false,
                    property_name.is_computed,
                    property_name.is_private,
                    is_derived_constructor,
                )
            }
            _ => self.parse_init_property(
                property_name.key,
                start_pos,
                prop_context,
                init_scope,
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

    fn get_property_initializer(&mut self, prop_context: PropertyContext) -> Token<'a> {
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
    ) -> ParseResult<PropertyNameResult<'a>> {
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
            Token::NumberLiteral(_) | Token::StringLiteral(_) | Token::BigIntLiteral { .. } => {
                self.parse_primary_expression()?
            }
            // Private properties are only allowed in classes
            Token::Hash if prop_context == PropertyContext::Class => {
                is_private = true;
                Expression::Id(p!(self, self.parse_private_name()?))
            }
            _ => match self.parse_identifier_name()? {
                Some(key) => {
                    is_identifier = true;
                    Expression::Id(p!(self, key))
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
        key: Expression<'a>,
        start_pos: Pos,
        prop_context: PropertyContext,
        init_scope: Option<&mut Option<AstPtr<AstScopeNode<'a>>>>,
        is_computed: bool,
        is_shorthand: bool,
        is_private: bool,
    ) -> ParseResult<(Property<'a>, bool)> {
        // If there is a field initializer scope then the initializer should be parsed in it
        let scope_to_restore = if let Some(init_scope) = init_scope {
            let scope_to_restore = self.scope_builder.current_scope().as_ref().id();
            self.enter_init_scope(init_scope, start_pos)?;
            Some(scope_to_restore)
        } else {
            None
        };

        let value = if is_shorthand {
            if prop_context != PropertyContext::Class {
                let key_id = key.to_id();
                if self.is_reserved_word_in_current_context(key_id.name) {
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

        if let Some(scope_to_restore) = scope_to_restore {
            self.scope_builder.set_current_scope(scope_to_restore);
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
        key: Expression<'a>,
        start_pos: Pos,
        kind: PropertyKind<'a>,
        is_async: bool,
        is_generator: bool,
        is_computed: bool,
        is_private: bool,
        is_derived_constructor: bool,
    ) -> ParseResult<(Property<'a>, bool)> {
        // Enter async/generator context for parsing the function arguments and body
        let did_allow_await = swap_and_save(&mut self.allow_await, is_async);
        let did_allow_yield = swap_and_save(&mut self.allow_yield, is_generator);

        // Function scope node must contain params and body
        let scope = self.enter_function_scope(
            start_pos,
            /* is_arrow */ false,
            /* is_expression */ false,
            is_derived_constructor,
        )?;

        // Derived constructors add self as a fake binding, since they will need to be looked up
        // by super calls.
        if is_derived_constructor {
            self.scope_builder
                .add_binding(&DERIVED_CONSTRUCTOR_BINDING_NAME, BindingKind::DerivedConstructor)
                .unwrap();
        }

        let params = self.parse_function_params()?;
        let param_flags = self.analyze_function_params(&params);

        let (block, strict_flags) = self.parse_function_block_body(param_flags)?;
        let body = p!(self, FunctionBody::Block(block));
        let loc = self.mark_loc(start_pos);

        self.scope_builder.exit_scope();

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

        let mut flags = param_flags | strict_flags;
        if is_async {
            flags |= FunctionFlags::IS_ASYNC;
        }
        if is_generator {
            flags |= FunctionFlags::IS_GENERATOR;
        }

        let func = Function::new(loc, /* id */ None, params, body, flags, scope, self.alloc);

        let property = Property {
            loc,
            key,
            is_computed,
            is_method: true,
            kind,
            value: Some(Expression::Function(p!(self, func))),
        };

        self.allow_await = did_allow_await;
        self.allow_yield = did_allow_yield;

        Ok((property, is_private))
    }

    fn parse_class(&mut self, is_decl: bool, is_export: bool) -> ParseResult<P<'a, Class<'a>>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Enter strict mode for entire class, saving strict mode context from beforehand
        let old_in_strict_mode = self.in_strict_mode;
        self.set_in_strict_mode(true);

        // Id is optional only for class expresssions and anonymous default exports.
        let id = if (is_decl && !is_export)
            || (self.token != Token::LeftBrace && self.token != Token::Extends)
        {
            // Only introduce a binding for class declarations
            let binding_kind = if is_decl {
                Some(BindingKind::Class { in_body_scope: false, init_pos: Cell::new(0) })
            } else {
                None
            };

            Some(p!(self, self.parse_binding_identifier(binding_kind)?))
        } else if is_export {
            // Anonymous exported classes must still be added to scope but under the anonymous
            // default export name.
            let kind = BindingKind::Class { in_body_scope: false, init_pos: Cell::new(0) };
            let loc = self.mark_loc(start_pos);
            self.add_anonymous_default_export_to_scope(loc, kind)?;
            None
        } else {
            None
        };

        // Body (including extends clause) of the class in in its own scope
        let scope = self.scope_builder.enter_scope(ScopeNodeKind::Class);

        let super_class = if self.token == Token::Extends {
            self.advance()?;
            Some(wrap_outer(self.parse_left_hand_side_expression()?))
        } else {
            None
        };

        // If a lexical binding was introduced in the parent scope, mark the location after which
        // the binding was initialized and can be accessed without a TDZ check.
        let binding_init_pos = self.prev_loc.end;
        if let Some(id) = id.as_ref() {
            if is_decl {
                Self::set_id_binding_init_pos(id, binding_init_pos);
            }
        }

        // Always add class name to the body scope
        if let Some(id) = id.as_ref() {
            self.scope_builder
                .add_binding(
                    id.name,
                    BindingKind::Class {
                        in_body_scope: true,
                        init_pos: Cell::new(binding_init_pos),
                    },
                )
                .unwrap();
        }

        // Class body scope always contains the home object and static home object
        self.add_home_object_to_scope(/* include_static */ true);

        let mut body = self.alloc_vec();

        let mut field_init_scope = None;
        let mut static_init_scope = None;

        self.expect(Token::LeftBrace)?;
        while self.token != Token::RightBrace {
            // Empty semicolon statements are allowed in class body
            if self.token == Token::Semicolon {
                self.advance()?;
                continue;
            }

            body.push(self.parse_class_element(
                /* has_super_class */ super_class.is_some(),
                &mut field_init_scope,
                &mut static_init_scope,
            )?);
        }

        self.advance()?;
        let loc = self.mark_loc(start_pos);

        self.scope_builder.exit_scope();

        // Restore to strict mode context from beforehand
        self.set_in_strict_mode(old_in_strict_mode);

        Ok(p!(
            self,
            Class::new(
                loc,
                id,
                super_class,
                body.build(),
                scope,
                field_init_scope,
                static_init_scope
            )
        ))
    }

    fn parse_private_name(&mut self) -> ParseResult<Identifier<'a>> {
        let hash_loc = self.loc;
        self.expect(Token::Hash)?;

        // Hash must be directly followed by the identifier, without any characters between
        if self.loc.start != hash_loc.end {
            return self.error(hash_loc, ParseError::HashNotFollowedByIdentifier);
        }

        match self.parse_identifier_name()? {
            Some(id) => Ok(id),
            None => self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_class_element(
        &mut self,
        has_super_class: bool,
        field_init_scope: &mut Option<AstPtr<AstScopeNode<'a>>>,
        static_init_scope: &mut Option<AstPtr<AstScopeNode<'a>>>,
    ) -> ParseResult<ClassElement<'a>> {
        let start_pos = self.current_start_pos();

        // Every class element can start with a `static` modifier
        let is_static = self.token == Token::Static;
        if is_static {
            let static_loc = self.loc;
            self.advance()?;

            // Check for static initializer
            if self.token == Token::LeftBrace {
                // Static initializer is within the static initializer function scope
                let scope_to_restore = self.scope_builder.current_scope().as_ref().id();
                self.enter_init_scope(static_init_scope, static_loc.start)?;

                // Static initializer is within its own scope
                let scope = self
                    .scope_builder
                    .enter_scope(ScopeNodeKind::StaticInitializer);

                let params = self.alloc_vec();
                let param_flags = self.analyze_function_params(&params);

                // Await expressions are allowed in static initializer blocks from the perspective
                // of parsing, but will early error in analysis.
                let did_allow_await = swap_and_save(&mut self.allow_await, true);

                // Yield expressions are not allowed in static initializer blocks
                let did_allow_yield = swap_and_save(&mut self.allow_yield, false);

                let (block, _) = self.parse_function_block_body(param_flags)?;

                self.allow_yield = did_allow_yield;
                self.allow_await = did_allow_await;

                self.scope_builder.set_current_scope(scope_to_restore);

                let loc = self.mark_loc(start_pos);

                // Static initializers implemented as method with block body. All fields except for
                // the kind and the function's block body are ignored, so put in placeholders.
                return Ok(ClassElement::Method(ClassMethod::new(
                    loc,
                    /* key */
                    wrap_outer(Expression::Null(p!(self, loc))),
                    /* value */
                    p!(
                        self,
                        Function::new(
                            loc,
                            None,
                            params.build(),
                            p!(self, FunctionBody::Block(block)),
                            param_flags | FunctionFlags::IS_STRICT_MODE,
                            scope,
                            self.alloc,
                        )
                    ),
                    ClassMethodKind::StaticInitializer,
                    /* is_computed */ false,
                    /* is_static */ false,
                    /* is_private */ false,
                )));
            }

            // Handle `static` as name of method: `static() {}`
            if self.token == Token::LeftParen {
                let static_id = Identifier::new(static_loc, STATIC_ID);
                let name = Expression::Id(p!(self, static_id));

                let (property, is_private) = self.parse_method_property(
                    name,
                    start_pos,
                    PropertyKind::Init,
                    /* is_async */ false,
                    /* is_generator */ false,
                    /* is_computed */ false,
                    /* is_private */ false,
                    /* is_derived_constructor */ false,
                )?;
                let loc = self.mark_loc(start_pos);

                return Ok(ClassElement::Method(self.reparse_property_as_class_method(
                    loc,
                    property,
                    /* is_static */ false,
                    is_private,
                    field_init_scope,
                )?));
            }

            // Handle `static` as shorthand or init property
            let is_init_property = self.is_property_initializer(PropertyContext::Class);
            if is_init_property || self.is_property_end(PropertyContext::Class) {
                let static_id = Identifier::new(static_loc, STATIC_ID);
                let name = Expression::Id(p!(self, static_id));

                let (property, is_private) = self.parse_init_property(
                    name,
                    start_pos,
                    PropertyContext::Class,
                    Some(field_init_scope),
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

        // Property may need to be parsed in an initializer function scope
        let init_scope = if is_static {
            static_init_scope
        } else {
            field_init_scope
        };

        // Parse an object property because syntax is almost identical to class property
        let (property, is_private) =
            self.parse_property(PropertyContext::Class, has_super_class, Some(init_scope))?;
        let loc = self.mark_loc(start_pos);

        // All private names are added to the current class scope
        if is_private {
            let private_name = self.alloc_string(format!("#{}", &property.key.to_id().name));
            self.scope_builder
                .add_binding_to_current_node(private_name.as_arena_str(), BindingKind::PrivateName);

            // Intentionally leak private name so it remains in arena and is not deallocated
            std::mem::forget(private_name);
        }

        // Translate from object property to class property or method
        if property.is_method {
            Ok(ClassElement::Method(self.reparse_property_as_class_method(
                loc, property, is_static, is_private, init_scope,
            )?))
        } else {
            Ok(ClassElement::Property(
                self.reparse_property_as_class_property(loc, property, is_static, is_private),
            ))
        }
    }

    /// Enter an initializer scope, creating it if necessary.
    fn enter_init_scope(
        &mut self,
        scope: &mut Option<AstPtr<AstScopeNode<'a>>>,
        start_pos: Pos,
    ) -> ParseResult<()> {
        if let Some(scope) = scope {
            self.scope_builder.set_current_scope(scope.as_ref().id());
            return Ok(());
        }

        let init_scope = self.enter_function_scope(
            start_pos, /* is_arrow */ false, /* is_expression */ false,
            /* is_derived_constructor */ false,
        )?;
        *scope = Some(init_scope);

        Ok(())
    }

    fn reparse_property_as_class_method(
        &mut self,
        loc: Loc,
        property: Property<'a>,
        is_static: bool,
        is_private: bool,
        init_scope: &mut Option<AstPtr<AstScopeNode<'a>>>,
    ) -> ParseResult<ClassMethod<'a>> {
        let Property { key, value, is_computed, kind, .. } = property;

        // Private methods will be created in the appropriate class initializer scope..
        // Scope is created but not entered yet.
        if is_private {
            let scope_to_restore = self.scope_builder.current_scope().as_ref().id();
            self.enter_init_scope(init_scope, loc.start)?;
            self.scope_builder.set_current_scope(scope_to_restore);
        }

        let func_value = if let Expression::Function(func) = value.unwrap() {
            func
        } else {
            unreachable!("method properties must have function expression")
        };

        let kind = match kind {
            PropertyKind::Get => ClassMethodKind::Get,
            PropertyKind::Set => ClassMethodKind::Set,
            PropertyKind::Init if is_static => ClassMethodKind::Method,
            PropertyKind::Init => {
                if Self::is_constructor_key(&key) && !is_static && !is_computed {
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

        Ok(ClassMethod::new(
            loc,
            wrap_outer(key),
            func_value,
            kind,
            is_computed,
            is_static,
            is_private,
        ))
    }

    fn is_constructor_key(key: &Expression) -> bool {
        match key {
            Expression::Id(id) => id.name == "constructor",
            Expression::String(str) => str.value == "constructor",
            _ => false,
        }
    }

    fn reparse_property_as_class_property(
        &mut self,
        loc: Loc,
        property: Property<'a>,
        is_static: bool,
        is_private: bool,
    ) -> ClassProperty<'a> {
        let Property { key, value, is_computed, .. } = property;

        let key = wrap_outer(key);
        let value = value.map(wrap_outer);

        ClassProperty { loc, key, value, is_computed, is_static, is_private }
    }

    fn parse_pattern(&mut self, binding_kind: BindingKind<'a>) -> ParseResult<Pattern<'a>> {
        match &self.token {
            Token::LeftBracket => self.parse_array_pattern(binding_kind),
            Token::LeftBrace => self.parse_object_pattern(binding_kind),
            _ => Ok(Pattern::Id(p!(self, self.parse_binding_identifier(Some(binding_kind))?))),
        }
    }

    fn parse_pattern_including_assignment_pattern(
        &mut self,
        binding_kind: BindingKind<'a>,
    ) -> ParseResult<Pattern<'a>> {
        let start_pos = self.current_start_pos();
        let patt = self.parse_pattern(binding_kind)?;
        self.parse_assignment_pattern(patt, start_pos)
    }

    /// Parse an assignment pattern if one exists, otherwise return left hand side
    fn parse_assignment_pattern(
        &mut self,
        left: Pattern<'a>,
        start_pos: Pos,
    ) -> ParseResult<Pattern<'a>> {
        // If followed by an equals sign, this is an assignment pattern
        match self.token {
            Token::Equals => {
                self.advance()?;
                let right = self.parse_assignment_expression()?;
                let loc = self.mark_loc(start_pos);

                Ok(Pattern::Assign(p!(self, AssignmentPattern { loc, left, right })))
            }
            _ => Ok(left),
        }
    }

    fn parse_array_pattern(&mut self, binding_kind: BindingKind<'a>) -> ParseResult<Pattern<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // In expressions are always allowed within array patterns. Needed in case an array pattern
        // with in expression initializer appears in for init.
        let old_allow_in = swap_and_save(&mut self.allow_in, true);

        let mut elements = self.alloc_vec();
        while self.token != Token::RightBracket {
            match self.token {
                Token::Comma => {
                    let hole_pos = self.current_start_pos();
                    self.advance()?;
                    elements.push(ArrayPatternElement::Hole(hole_pos));
                }
                Token::Spread => {
                    let rest_element = self.parse_rest_element(binding_kind)?;
                    elements.push(ArrayPatternElement::Rest(rest_element));

                    // Trailing commas are not allowed after rest elements, nor are any other elements
                    if self.token == Token::Comma {
                        return self.error(self.loc, ParseError::RestTrailingComma);
                    } else {
                        break;
                    }
                }
                _ => {
                    let pattern =
                        self.parse_pattern_including_assignment_pattern(binding_kind.clone())?;
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

        Ok(Pattern::Array(p!(self, ArrayPattern { loc, elements: elements.build() })))
    }

    fn parse_rest_element(
        &mut self,
        binding_kind: BindingKind<'a>,
    ) -> ParseResult<RestElement<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        let argument = self.parse_pattern(binding_kind)?;
        let loc = self.mark_loc(start_pos);

        Ok(RestElement { loc, argument })
    }

    fn parse_object_pattern(&mut self, binding_kind: BindingKind<'a>) -> ParseResult<Pattern<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // In expressions are always allowed within object patterns. Needed in case an object pattern
        // with in expression initializer appears in for init.
        let old_allow_in = swap_and_save(&mut self.allow_in, true);

        let mut properties = self.alloc_vec();
        while self.token != Token::RightBrace {
            if self.token == Token::Spread {
                properties.push(self.parse_object_pattern_rest_property(binding_kind)?);

                // Trailing commas are not allowed after rest elements, nor are any other properties
                if self.token == Token::Comma {
                    return self.error(self.loc, ParseError::RestTrailingComma);
                } else {
                    break;
                }
            }

            properties.push(self.parse_object_pattern_property(binding_kind.clone())?);

            if self.token == Token::RightBrace {
                break;
            }

            self.expect(Token::Comma)?;
        }

        self.expect(Token::RightBrace)?;
        let loc = self.mark_loc(start_pos);

        self.allow_in = old_allow_in;

        Ok(Pattern::Object(p!(self, ObjectPattern { loc, properties: properties.build() })))
    }

    fn parse_object_pattern_property(
        &mut self,
        binding_kind: BindingKind<'a>,
    ) -> ParseResult<ObjectPatternProperty<'a>> {
        let start_pos = self.current_start_pos();

        let property_name = self.parse_property_name(PropertyContext::Pattern)?;

        // Shorthand property
        if property_name.is_shorthand {
            let value = if let Expression::Id(mut id) = property_name.key {
                if self.is_reserved_word_in_current_context(id.name) {
                    return self.error(id.loc, ParseError::IdentifierIsReservedWord);
                }

                // Add binding to scope since this is a shorthand property
                self.add_binding(&mut id, binding_kind)?;

                Pattern::Id(id)
            } else {
                unreachable!()
            };

            // Shorthand property may be followed by assignment pattern
            let value = self.parse_assignment_pattern(value, start_pos)?;
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
        let value = self.parse_pattern_including_assignment_pattern(binding_kind)?;
        let loc = self.mark_loc(start_pos);

        Ok(ObjectPatternProperty {
            loc,
            key: Some(property_name.key),
            value,
            is_computed: property_name.is_computed,
            is_rest: false,
        })
    }

    fn parse_object_pattern_rest_property(
        &mut self,
        binding_kind: BindingKind<'a>,
    ) -> ParseResult<ObjectPatternProperty<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        // Only identifiers are allowed as rest property arguments
        let id_argument = self.parse_binding_identifier(Some(binding_kind))?;
        let loc = self.mark_loc(start_pos);

        Ok(ObjectPatternProperty {
            loc,
            key: None,
            value: Pattern::Id(p!(self, id_argument)),
            is_computed: false,
            is_rest: true,
        })
    }

    fn parse_import_declaration(&mut self) -> ParseResult<Toplevel<'a>> {
        let start_pos = self.current_start_pos();

        // May be the start of a toplevel import expression or import.meta expression. In either
        // case parse as an expression statement.
        let next_token = self.peek()?;
        if next_token == Token::LeftParen || next_token == Token::Period {
            let expr = self.parse_outer_expression()?;

            // Parse the rest of the expression statement
            self.expect_semicolon()?;
            let loc = self.mark_loc(start_pos);

            let expr_stmt = Statement::Expr(p!(self, ExpressionStatement { loc, expr }));

            return Ok(Toplevel::Statement(expr_stmt));
        }

        // Otherwise this is a standard import declaration so advance past the `import` token
        self.advance()?;

        // No specifiers
        if matches!(&self.token, Token::StringLiteral(_)) {
            let source = self.parse_string_literal()?;

            let attributes = self.parse_optional_import_attributes()?;
            self.expect_semicolon()?;

            let loc = self.mark_loc(start_pos);

            return Ok(Toplevel::Import(ImportDeclaration {
                loc,
                specifiers: AstSlice::new_empty(),
                source,
                attributes,
            }));
        }

        // Check for default specifier, which must be at start
        let mut specifiers = self.alloc_vec();

        if let Token::Identifier(_) = self.token {
            // Starts with default specifier
            let local = p!(self, self.parse_imported_identifier()?);

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

        let (source, attributes) = self.parse_source_and_attributes()?;
        let loc = self.mark_loc(start_pos);

        Ok(Toplevel::Import(ImportDeclaration {
            loc,
            specifiers: specifiers.build(),
            source,
            attributes,
        }))
    }

    fn parse_optional_import_attributes(
        &mut self,
    ) -> ParseResult<Option<P<'a, ImportAttributes<'a>>>> {
        if self.token != Token::With {
            return Ok(None);
        }

        self.advance()?;
        self.expect(Token::LeftBrace)?;

        let mut attributes = self.alloc_vec();

        while self.token != Token::RightBrace {
            let attribute = self.parse_import_attribute()?;
            attributes.push(attribute);

            if self.token == Token::Comma {
                self.advance()?;
            } else {
                break;
            }
        }

        self.expect(Token::RightBrace)?;

        Ok(Some(p!(self, ImportAttributes { attributes: attributes.build() })))
    }

    fn parse_import_attribute(&mut self) -> ParseResult<ImportAttribute<'a>> {
        let start_pos = self.current_start_pos();

        let key = if matches!(&self.token, Token::StringLiteral(_)) {
            Expression::String(self.parse_string_literal()?)
        } else {
            let id = self.parse_identifier_name()?;

            if id.is_none() {
                return self.error(self.loc, ParseError::ImportAttributeInvalidKey);
            }

            Expression::Id(p!(self, id.unwrap()))
        };

        self.expect(Token::Colon)?;

        // Value must be a string literal
        let value = if matches!(&self.token, Token::StringLiteral(_)) {
            self.parse_string_literal()?
        } else {
            return self.error(self.loc, ParseError::ImportAttributeInvalidValue);
        };

        let loc = self.mark_loc(start_pos);

        Ok(ImportAttribute { loc, key, value })
    }

    fn parse_import_named_or_namespace_specifier(
        &mut self,
        specifiers: &mut AstSliceBuilder<'a, ImportSpecifier<'a>>,
    ) -> ParseResult<()> {
        match self.token {
            Token::Multiply => {
                specifiers.push(self.parse_import_namespace_specifier()?);
                Ok(())
            }
            Token::LeftBrace => {
                self.parse_import_named_specifiers(specifiers)?;
                Ok(())
            }
            _ => self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_import_namespace_specifier(&mut self) -> ParseResult<ImportSpecifier<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        self.expect(Token::As)?;

        let local = p!(self, self.parse_imported_namespace_identifier()?);
        let loc = self.mark_loc(start_pos);

        Ok(ImportSpecifier::Namespace(ImportNamespaceSpecifier { loc, local }))
    }

    fn parse_import_named_specifiers(
        &mut self,
        specifiers: &mut AstSliceBuilder<'a, ImportSpecifier<'a>>,
    ) -> ParseResult<()> {
        self.advance()?;

        while self.token != Token::RightBrace {
            let start_pos = self.current_start_pos();
            let start_token = self.token.clone();

            // String literal must be the start of an import as specifier
            let spec = if let Token::StringLiteral(value) = &self.token {
                let imported = self.create_export_name_string(self.loc, value)?;
                self.advance()?;

                self.expect(Token::As)?;

                let local = p!(self, self.parse_imported_identifier()?);
                let loc = self.mark_loc(start_pos);

                ImportNamedSpecifier { loc, imported: Some(p!(self, imported)), local }
            } else if let Some(mut id) = self.parse_identifier_name()? {
                // This is an import as specifier, so the id can be any name including reserved words
                if self.token == Token::As {
                    self.advance()?;

                    let imported = self.create_export_name_identifier(id);
                    let local = p!(self, self.parse_imported_identifier()?);
                    let loc = self.mark_loc(start_pos);

                    ImportNamedSpecifier { loc, imported: Some(p!(self, imported)), local }
                } else {
                    // This is the binding for a simple named specifier, identifier cannot be a
                    // reserved word.
                    if self.is_reserved_word_in_current_context(id.name) {
                        return self.error_unexpected_token(id.loc, &start_token);
                    }

                    // Must add an import binding since not parsed as an imported identifier
                    self.add_binding(&mut id, BindingKind::Import { is_namespace: false })?;

                    ImportNamedSpecifier { loc: id.loc, imported: None, local: p!(self, id) }
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

    fn parse_export_declaration(&mut self) -> ParseResult<Toplevel<'a>> {
        let start_pos = self.current_start_pos();
        self.advance()?;

        match self.token {
            // Export named specifiers within braces
            Token::LeftBrace => {
                self.advance()?;

                let mut specifiers = self.alloc_vec();

                // Parse list of specifiers between braces
                while self.token != Token::RightBrace {
                    let start_pos = self.current_start_pos();
                    let local = p!(self, self.parse_export_name()?);

                    // Specifiers optionally have an export alias
                    let exported = if self.token == Token::As {
                        self.advance()?;
                        Some(p!(self, self.parse_export_name()?))
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

                let (source, source_attributes) = if self.token == Token::From {
                    let (source, attributes) = self.parse_source_and_attributes()?;
                    (Some(source), attributes)
                } else {
                    // If there is no `from` clause then the local name of each specifier must be an
                    // identifier that is not a reserved word.
                    for specifier in specifiers.iter() {
                        match specifier.local.as_ref() {
                            ExportName::Id(id) => {
                                if self.is_reserved_word_in_current_context(id.name) {
                                    return self
                                        .error(id.loc, ParseError::IdentifierIsReservedWord);
                                }
                            }
                            ExportName::String(string) => {
                                return self
                                    .error(string.loc, ParseError::DirectExportNameIsString);
                            }
                        }
                    }

                    self.expect_semicolon()?;

                    (None, None)
                };

                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportNamed(ExportNamedDeclaration {
                    loc,
                    declaration: None,
                    specifiers: specifiers.build(),
                    source,
                    source_attributes,
                }))
            }
            // Export all declaration
            Token::Multiply => {
                self.advance()?;

                // Optional exported alias
                let exported = if self.token == Token::As {
                    self.advance()?;
                    Some(p!(self, self.parse_export_name()?))
                } else {
                    None
                };

                // Required source
                let (source, source_attributes) = self.parse_source_and_attributes()?;
                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportAll(ExportAllDeclaration {
                    loc,
                    exported,
                    source,
                    source_attributes,
                }))
            }
            // Export named declaration with a declaration statement
            Token::Function
            | Token::Async
            | Token::Class
            | Token::Var
            | Token::Let
            | Token::Const => {
                // From the perspective of function declaration scoping, being inside an export
                // declaration makes them not toplevel.
                let declaration = Some(self.parse_statement_list_item(FunctionContext::empty())?);
                let loc = self.mark_loc(start_pos);

                Ok(Toplevel::ExportNamed(ExportNamedDeclaration {
                    loc,
                    declaration,
                    specifiers: AstSlice::new_empty(),
                    source: None,
                    source_attributes: None,
                }))
            }
            // Export default declaration
            Token::Default => {
                self.advance()?;

                // Default function and class declarations have an optional name
                if self.token == Token::Class {
                    let declaration = ExportDefaultKind::Class(
                        self.parse_class(/* is_decl */ true, /* is_export */ true)?,
                    );
                    let loc = self.mark_loc(start_pos);

                    Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                } else if self.is_function_start()? {
                    let declaration = ExportDefaultKind::Function(
                        self.parse_function_declaration(FunctionContext::EXPORT)?,
                    );
                    let loc = self.mark_loc(start_pos);

                    Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                } else {
                    // Otherwise default export is an expression
                    let expr = wrap_outer(self.parse_assignment_expression()?);
                    let declaration = ExportDefaultKind::Expression(expr);

                    self.expect_semicolon()?;
                    let loc = self.mark_loc(start_pos);

                    // Exported expressions are always anonymous and are added to bindings with a
                    // special type.
                    self.add_anonymous_default_export_to_scope(
                        loc,
                        BindingKind::DefaultExportExpression,
                    )?;

                    Ok(Toplevel::ExportDefault(ExportDefaultDeclaration { loc, declaration }))
                }
            }
            _ => self.error_unexpected_token(self.loc, &self.token),
        }
    }

    fn parse_export_name(&mut self) -> ParseResult<ExportName<'a>> {
        if let Token::StringLiteral(value) = &self.token {
            let export_name = self.create_export_name_string(self.loc, value)?;
            self.advance()?;

            Ok(export_name)
        } else if let Some(id) = self.parse_identifier_name()? {
            Ok(self.create_export_name_identifier(id))
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn create_export_name_string(
        &mut self,
        loc: Loc,
        value: AstStr<'a>,
    ) -> ParseResult<ExportName<'a>> {
        if !value.is_well_formed() {
            return self.error(loc, ParseError::ExportNameNotWellFormed);
        }

        Ok(ExportName::String(StringLiteral { loc, value }))
    }

    fn create_export_name_identifier(&mut self, id: Identifier<'a>) -> ExportName<'a> {
        // Do not need to check for IsStringWellFormedUnicode since Rust strings are guaranteed
        // to be valid UTF-8 and cannot have any unpaired surrogates.
        ExportName::Id(Identifier::new(id.loc, id.name))
    }

    fn parse_source_and_attributes(
        &mut self,
    ) -> ParseResult<(P<'a, StringLiteral<'a>>, Option<P<'a, ImportAttributes<'a>>>)> {
        self.expect(Token::From)?;

        if matches!(&self.token, Token::StringLiteral(_)) {
            let source = self.parse_string_literal()?;
            let attributes = self.parse_optional_import_attributes()?;
            self.expect_semicolon()?;

            Ok((source, attributes))
        } else {
            self.error_unexpected_token(self.loc, &self.token)
        }
    }

    fn reparse_expression_as_assignment_left_hand_side(
        &self,
        expr: Expression<'a>,
        start_pos: Pos,
    ) -> ParseResult<Pattern<'a>> {
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
        expr: Expression<'a>,
        start_pos: Pos,
    ) -> ParseResult<Pattern<'a>> {
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
        expr: Expression<'a>,
        start_pos: Pos,
    ) -> ParseResult<Pattern<'a>> {
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

    fn reparse_left_hand_side_expression_as_pattern(
        &self,
        expr: Expression<'a>,
    ) -> Option<Pattern<'a>> {
        match expr {
            Expression::Id(id) => {
                // Cannot assign to arguments or eval in strict mode
                if self.in_strict_mode {
                    match id.name.as_bytes() {
                        b"arguments" | b"eval" => return None,
                        _ => {}
                    }
                }

                Some(Pattern::Id(id))
            }
            Expression::Object(object) => {
                self.reparse_object_expression_as_pattern(object.into_inner())
            }
            Expression::Array(array) => {
                self.reparse_array_expression_as_pattern(array.into_inner())
            }
            Expression::Member(expr) => Some(Pattern::Member(expr)),
            Expression::SuperMember(expr) => Some(Pattern::SuperMember(expr)),
            _ => None,
        }
    }

    fn reparse_object_expression_as_pattern(
        &self,
        expr: ObjectExpression<'a>,
    ) -> Option<Pattern<'a>> {
        if expr.is_parenthesized {
            return None;
        }

        let mut properties = self.alloc_vec();

        for property in expr.properties.into_vec(self.alloc).into_iter() {
            let property = if let PropertyKind::Spread(has_spread_comma) = property.kind {
                // Convert spread property to rest property

                // Trailing commas (or properties after) are not allowed on rest element
                if has_spread_comma {
                    return None;
                }

                // Object and array patterns are not allowed as rest elements
                let value = match property.key {
                    Expression::Object(_) | Expression::Array(_) => return None,
                    lhs_pattern => {
                        self.reparse_left_hand_side_expression_as_pattern(lhs_pattern)?
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
                let id_pattern = self.reparse_left_hand_side_expression_as_pattern(property.key)?;

                // Check the property kind to see if this is a shorthand pattern initializer
                let value = if let PropertyKind::PatternInitializer(initializer) = property.kind {
                    let assign_pattern = AssignmentPattern {
                        loc: property.loc,
                        left: id_pattern,
                        right: initializer,
                    };

                    Pattern::Assign(p!(self, assign_pattern))
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
                let value =
                    self.reparse_expression_as_maybe_assignment_pattern(property.value.unwrap())?;

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

        let object_pattern = ObjectPattern { loc: expr.loc, properties: properties.build() };

        Some(Pattern::Object(p!(self, object_pattern)))
    }

    fn reparse_array_expression_as_pattern(
        &self,
        expr: ArrayExpression<'a>,
    ) -> Option<Pattern<'a>> {
        if expr.is_parenthesized {
            return None;
        }

        let mut elements = self.alloc_vec();

        for element in expr.elements.into_vec(self.alloc).into_iter() {
            let element = match element {
                ArrayElement::Expression(expr) => {
                    let pattern = self.reparse_expression_as_maybe_assignment_pattern(expr)?;
                    ArrayPatternElement::Pattern(pattern)
                }
                ArrayElement::Hole(pos) => ArrayPatternElement::Hole(pos),
                ArrayElement::Spread(spread) => {
                    // Trailing commas (or properties after) are not allowed on rest element
                    if spread.has_trailing_comma {
                        return None;
                    }

                    let argument =
                        self.reparse_left_hand_side_expression_as_pattern(spread.argument)?;
                    ArrayPatternElement::Rest(RestElement { loc: spread.loc, argument })
                }
            };

            elements.push(element);
        }

        let array_pattern = ArrayPattern { loc: expr.loc, elements: elements.build() };

        Some(Pattern::Array(p!(self, array_pattern)))
    }

    // If the value is an assignment expression with an identifier lhs, it can be reparsed to an
    // assignment pattern. Otherwise it is parsed normally as a left hand side expression.
    fn reparse_expression_as_maybe_assignment_pattern(
        &self,
        expr: Expression<'a>,
    ) -> Option<Pattern<'a>> {
        match expr {
            Expression::Assign(assign) if assign.operator == AssignmentOperator::Equals => {
                let AssignmentExpression { loc, left, right, is_parenthesized, .. } =
                    assign.into_inner();

                // Parenthesized assignment expressions are invalid as pattern elements
                if is_parenthesized {
                    return None;
                }

                Some(Pattern::Assign(p!(self, AssignmentPattern { loc, left, right })))
            }
            other_expr => Some(self.reparse_left_hand_side_expression_as_pattern(other_expr)?),
        }
    }

    /// AssignmentTargetType (https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype)
    fn is_valid_assignment_target(&self, expr: &Expression<'a>) -> bool {
        match expr {
            Expression::Id(id) => {
                // Cannot assign to arguments or eval in strict mode
                if self.in_strict_mode {
                    !matches!(id.name.as_bytes(), b"arguments" | b"eval")
                } else {
                    true
                }
            }
            Expression::Member(_) | Expression::SuperMember(_) => true,
            _ => false,
        }
    }
}

#[inline]
fn wrap_outer(expr: Expression) -> OuterExpression {
    OuterExpression { expr, has_assign_expr: false }
}

pub struct ParseProgramResult<'a> {
    pub program: P<'a, Program<'a>>,
    pub scope_tree: P<'a, ScopeTree<'a>>,
    pub source: Rc<Source>,
}

pub struct ParseFunctionResult<'a> {
    pub function: P<'a, Function<'a>>,
    pub scope_tree: P<'a, ScopeTree<'a>>,
}

pub fn parse_script(pcx: &ParseContext, options: Rc<Options>) -> ParseResult<ParseProgramResult> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let lexer = Lexer::new(pcx.source(), alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_global(alloc), options, alloc);

    let initial_state = parser.save();
    parser.advance()?;

    parser.parse_script(initial_state)
}

pub fn parse_module(pcx: &ParseContext, options: Rc<Options>) -> ParseResult<ParseProgramResult> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let lexer = Lexer::new(pcx.source(), alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_module(alloc), options, alloc);
    parser.advance()?;

    parser.parse_module()
}

pub fn parse_script_for_eval(
    pcx: &ParseContext,
    options: Rc<Options>,
    is_direct: bool,
    inherit_strict_mode: bool,
) -> ParseResult<ParseProgramResult> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let lexer = Lexer::new(pcx.source(), alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_eval(is_direct, alloc), options, alloc);

    // Inherit strict mode from context
    parser.set_in_strict_mode(inherit_strict_mode);

    let initial_state = parser.save();
    parser.advance()?;

    parser.parse_script(initial_state)
}

pub fn parse_function_params_for_function_constructor(
    pcx: &ParseContext,
    options: Rc<Options>,
    is_async: bool,
    is_generator: bool,
) -> ParseResult<()> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let lexer = Lexer::new(pcx.source(), alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_global(alloc), options, alloc);

    parser.allow_await = is_async;
    parser.allow_yield = is_generator;
    parser.prevent_hashbang_comment();

    parser.advance()?;

    parser.parse_function_params_without_parens()?;

    Ok(())
}

pub fn parse_function_body_for_function_constructor(
    pcx: &ParseContext,
    options: Rc<Options>,
    is_async: bool,
    is_generator: bool,
) -> ParseResult<()> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let lexer = Lexer::new(pcx.source(), alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_global(alloc), options, alloc);

    parser.allow_await = is_async;
    parser.allow_yield = is_generator;
    parser.prevent_hashbang_comment();

    let initial_state = parser.save();
    parser.advance()?;

    parser.parse_function_body_statements(initial_state)?;

    Ok(())
}

pub fn parse_function_for_function_constructor(
    pcx: &ParseContext,
    options: Rc<Options>,
) -> ParseResult<ParseFunctionResult> {
    // Create and prime parser
    let alloc = pcx.alloc();
    let source = pcx.source();

    let lexer = Lexer::new(source, alloc);
    let mut parser = Parser::new(lexer, ScopeTree::new_global(alloc), options, alloc);
    parser.advance()?;

    let func_node = parser.parse_function_declaration(FunctionContext::TOPLEVEL)?;
    let scope_tree = p!(parser, parser.scope_builder.finish_ast_scope_tree());

    Ok(ParseFunctionResult { function: func_node, scope_tree })
}
