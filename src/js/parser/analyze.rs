use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use allocator_api2::alloc::Global;

use crate::{
    js::{
        common::wtf_8::{Wtf8Str, Wtf8String},
        parser::{
            parse_error::InvalidDuplicateParametersReason,
            scope_tree::{VMLocation, ARGUMENTS_NAME},
        },
    },
    must, visit_opt, visit_vec,
};

use super::{
    ast::*,
    ast_visitor::*,
    loc::Loc,
    parser::{ParseFunctionResult, ParseProgramResult},
    scope_tree::{
        AstScopeNode, ScopeNodeId, ScopeTree, DERIVED_CONSTRUCTOR_BINDING_NAME,
        NEW_TARGET_BINDING_NAME, THIS_NAME,
    },
    source::Source,
    LocalizedParseError, LocalizedParseErrors, ParseContext, ParseError,
};

pub struct Analyzer<'a> {
    /// Current source that is being analyzed
    source: Rc<Source>,
    /// Accumulator or errors reported during analysis
    errors: Vec<LocalizedParseError>,
    /// Scope tree for the AST that is being analyzed
    scope_tree: P<'a, ScopeTree<'a>>,
    /// Number of nested strict mode contexts the visitor is currently in
    strict_mode_context_depth: u64,
    /// Set of all names exported by the current module
    export_names: HashSet<&'a Wtf8Str>,
    /// Set of labels defined where the visitor is currently in
    labels: HashMap<&'a Wtf8Str, LabelInfo>,
    /// Number of labeled statements that the visitor is currently inside. Multiple labels on the
    /// same statement all count as a single "label depth", and will receive the same label id.
    label_depth: LabelId,
    /// Number of nested breakable statements the visitor is currently inside
    breakable_depth: usize,
    /// Number of nested iterable statements the visitor is currently inside
    iterable_depth: usize,
    /// The functions that the visitor is currently inside, if any
    function_stack: Vec<FunctionStackEntry>,
    /// The classes that the visitor is currently inside, if any
    class_stack: Vec<ClassStackEntry>,
    /// Stack of scopes that the visitor is currently inside
    scope_stack: Vec<AstPtr<AstScopeNode<'a>>>,
    /// Whether the "arguments" identifier is currently disallowed due to being in a class initializer
    allow_arguments: bool,
    /// Whether a return statement is allowed
    allow_return_stack: Vec<bool>,
    /// Whether a super member expression is allowed
    allow_super_member_stack: Vec<AllowSuperStackEntry>,
    /// Whether the visitor is inside function parameters
    in_parameters_stack: Vec<bool>,
    /// Whether the current expression context has an assignment expression. A single value is
    /// needed instead of a stack, since we ensure that there is only one `has_assign_expr` context
    /// around each expression.
    has_assign_expr: bool,
    /// Whether we are in a module and there is a top-level await
    has_top_level_await: bool,
}

pub struct AnalyzedProgramResult<'a> {
    pub program: P<'a, Program<'a>>,
    pub scope_tree: P<'a, ScopeTree<'a>>,
    pub source: Rc<Source>,
}

pub struct AnalyzedFunctionResult<'a> {
    pub function: P<'a, Function<'a>>,
    pub scope_tree: P<'a, ScopeTree<'a>>,
    pub source: Rc<Source>,
}

struct FunctionStackEntry {
    is_arrow_function: bool,
    is_method: bool,
    is_static: bool,
    is_derived_constructor: bool,
    is_static_initializer: bool,
    is_class_field_initializer: bool,
}

enum AllowSuperStackEntry {
    Allow {
        /// Whether the super member expression references the static home object.
        is_static: bool,
    },
    Disallow,
}

struct ClassStackEntry {
    // All private names bound in the body of this class
    private_names: HashMap<Wtf8String, PrivateNameUsage>,
    // Whether this class extends a base class
    is_derived: bool,
}

struct LabelInfo {
    // Id of this label
    label_id: LabelId,
    // Whether this label can be a continue target
    is_continue_target: bool,
}

// Saved state from entering a function or class that can be restored from
struct AnalyzerSavedState<'a> {
    labels: HashMap<&'a Wtf8Str, LabelInfo>,
    label_depth: LabelId,
    breakable_depth: usize,
    iterable_depth: usize,
    allow_arguments: bool,
    has_assign_expr: bool,
}

pub struct PrivateNameUsage {
    is_static: bool,
    has_getter: bool,
    has_setter: bool,
}

impl PrivateNameUsage {
    // A generic usage marking that a private name was used
    pub fn used() -> PrivateNameUsage {
        PrivateNameUsage { is_static: false, has_getter: true, has_setter: true }
    }
}

impl<'a> Analyzer<'a> {
    pub fn new(source: Rc<Source>, scope_tree: P<'a, ScopeTree<'a>>) -> Analyzer<'a> {
        Analyzer {
            source,
            errors: Vec::new(),
            scope_tree,
            strict_mode_context_depth: 0,
            export_names: HashSet::new(),
            labels: HashMap::new(),
            label_depth: 0,
            breakable_depth: 0,
            iterable_depth: 0,
            function_stack: vec![],
            class_stack: vec![],
            scope_stack: vec![],
            allow_arguments: true,
            allow_return_stack: vec![false],
            allow_super_member_stack: vec![],
            in_parameters_stack: vec![],
            has_assign_expr: false,
            has_top_level_await: false,
        }
    }

    fn is_in_strict_mode_context(&self) -> bool {
        self.strict_mode_context_depth > 0
    }

    fn enter_strict_mode_context(&mut self) {
        self.strict_mode_context_depth += 1;
    }

    fn exit_strict_mode_context(&mut self) {
        self.strict_mode_context_depth -= 1;
    }

    fn emit_error(&mut self, loc: Loc, error: ParseError) {
        let source_loc = Some((loc, self.source.clone()));
        self.errors.push(LocalizedParseError { error, source_loc })
    }

    fn inc_breakable_depth(&mut self) {
        self.breakable_depth += 1;
    }

    fn dec_breakable_depth(&mut self) {
        self.breakable_depth -= 1;
    }

    fn inc_iterable_depth(&mut self) {
        // Every iterable is also breakable
        self.iterable_depth += 1;
        self.inc_breakable_depth();
    }

    fn dec_iterable_depth(&mut self) {
        // Every iterable is also breakable
        self.iterable_depth -= 1;
        self.dec_breakable_depth();
    }

    fn is_in_breakable(&self) -> bool {
        self.breakable_depth > 0
    }

    fn is_in_iterable(&self) -> bool {
        self.iterable_depth > 0
    }

    fn enter_has_assign_expr_context(&mut self) {
        self.has_assign_expr = false;
    }

    fn exit_has_assign_expr_context(&mut self) -> bool {
        let has_assign_expr = self.has_assign_expr;
        self.has_assign_expr = false;
        has_assign_expr
    }

    fn is_in_non_arrow_function(&self) -> bool {
        self.enclosing_non_arrow_function().is_some()
    }

    fn enclosing_non_arrow_function(&self) -> Option<&FunctionStackEntry> {
        self.function_stack
            .iter()
            .rev()
            .find(|func| !func.is_arrow_function)
    }

    fn current_function(&self) -> Option<&FunctionStackEntry> {
        self.function_stack.last()
    }

    fn is_at_top_level(&self) -> bool {
        self.function_stack.is_empty() && self.class_stack.is_empty()
    }

    // Save state before visiting a function or class. This prevents labels and some context from
    // leaking into the inner function or class.
    fn save_state(&mut self) -> AnalyzerSavedState<'a> {
        let mut state = AnalyzerSavedState {
            labels: HashMap::new(),
            label_depth: self.label_depth,
            breakable_depth: self.breakable_depth,
            iterable_depth: self.iterable_depth,
            allow_arguments: self.allow_arguments,
            has_assign_expr: self.has_assign_expr,
        };

        std::mem::swap(&mut self.labels, &mut state.labels);
        self.label_depth = 0;
        self.breakable_depth = 0;
        self.iterable_depth = 0;

        state
    }

    // Restore state after visiting a function or class
    fn restore_state(&mut self, mut state: AnalyzerSavedState<'a>) {
        std::mem::swap(&mut self.labels, &mut state.labels);
        self.label_depth = state.label_depth;
        self.breakable_depth = state.breakable_depth;
        self.iterable_depth = state.iterable_depth;
        self.allow_arguments = state.allow_arguments;
        self.has_assign_expr = state.has_assign_expr;
    }

    fn error_if_strict_eval_or_arguments(&mut self, id: &Identifier) {
        if !self.is_in_strict_mode_context() {
            return;
        }

        if id.name == "eval" {
            self.emit_error(id.loc, ParseError::AssignEvalInStrictMode);
        } else if id.name == "arguments" {
            self.emit_error(id.loc, ParseError::AssignArgumentsInStrictMode);
        }
    }

    fn enter_scope(&mut self, scope: AstPtr<AstScopeNode<'a>>) {
        self.scope_stack.push(scope);
    }

    fn exit_scope_without_finishing(&mut self) {
        self.scope_stack.pop();
    }

    fn exit_scope(&mut self) {
        if let Some(exited_scope_node) = self.scope_stack.pop() {
            self.scope_tree
                .finish_vm_scope_node(exited_scope_node.as_ref().id(), None);
        }
    }

    fn exit_scope_with_mapped_arguments_object(&mut self, num_parameters: usize) {
        if let Some(exited_scope_node) = self.scope_stack.pop() {
            self.scope_tree
                .finish_vm_scope_node(exited_scope_node.as_ref().id(), Some(num_parameters));
        }
    }

    fn exit_scope_with_class_fields(&mut self, num_extra_fields: usize) {
        if let Some(exited_scope_node) = self.scope_stack.pop() {
            self.scope_tree
                .finish_vm_scope_node(exited_scope_node.as_ref().id(), Some(num_extra_fields));
        }
    }

    fn current_scope_id(&self) -> ScopeNodeId {
        self.scope_stack.last().unwrap().as_ref().id()
    }

    fn finish(mut self) -> P<'a, ScopeTree<'a>> {
        self.scope_tree.finish_vm_scope_tree();
        self.scope_tree
    }
}

impl<'a> AstVisitor<'a> for Analyzer<'a> {
    fn visit_program(&mut self, program: &mut Program<'a>) {
        if program.is_strict_mode {
            self.enter_strict_mode_context();
        }

        self.enter_scope(program.scope);

        visit_vec!(self, program.toplevels, visit_toplevel);

        self.exit_scope();

        // Set properties determined during analysis
        program.has_top_level_await = self.has_top_level_await;

        if program.is_strict_mode {
            self.exit_strict_mode_context();
        }
    }

    fn visit_function_declaration(&mut self, func: &mut Function<'a>) {
        self.visit_function_common(
            func, /* is_arrow_function */ false, /* is_method */ false,
            /* is_static_method */ false, /*is_derived_constructor */ false,
            /* is_static_initializer */ false,
        );
    }

    fn visit_function_param(&mut self, param: &mut FunctionParam<'a>) {
        match param {
            FunctionParam::Pattern { pattern, has_assign_expr } => {
                // Pattern is in its own "has assignment expression" context
                self.enter_has_assign_expr_context();
                self.visit_pattern(pattern);
                *has_assign_expr = self.exit_has_assign_expr_context();
            }
            FunctionParam::Rest { rest, has_assign_expr } => {
                // Rest element is in its own "has_assignment_expression context"
                self.enter_has_assign_expr_context();
                self.visit_rest_element(rest);
                *has_assign_expr = self.exit_has_assign_expr_context();
            }
        }
    }

    fn visit_function_block_body(&mut self, body: &mut FunctionBlockBody<'a>) {
        // Body of function may have its own scope
        if let Some(scope) = body.scope {
            self.enter_scope(scope);
        }

        default_visit_function_block_body(self, body);

        if body.scope.is_some() {
            self.exit_scope();
        }
    }

    fn visit_outer_expression(&mut self, expr: &mut OuterExpression<'a>) {
        // Outer expressions are in their own "has assignment expression" context
        self.enter_has_assign_expr_context();
        self.visit_expression(&mut expr.expr);
        expr.has_assign_expr = self.exit_has_assign_expr_context();
    }

    fn visit_expression_statement(&mut self, stmt: &mut ExpressionStatement<'a>) {
        if let Expression::Assign(assign) = &mut stmt.expr.expr {
            // Top level assignment expression "has assignment expression" context refers to whether
            // the left or right side of the assignment expression has an assignment expression, and
            // is not trivially true.
            self.enter_has_assign_expr_context();
            default_visit_assignment_expression(self, assign);
            stmt.expr.has_assign_expr = self.exit_has_assign_expr_context();
        } else {
            default_visit_expression_statement(self, stmt);
        }
    }

    fn visit_switch_statement(&mut self, stmt: &mut SwitchStatement<'a>) {
        self.inc_breakable_depth();

        let mut seen_default = false;

        self.visit_outer_expression(&mut stmt.discriminant);

        // Body of switch statement is in its own scope
        self.enter_scope(stmt.scope);

        for case in stmt.cases.iter_mut() {
            if case.test.is_none() {
                if !seen_default {
                    seen_default = true;
                } else {
                    self.emit_error(case.loc, ParseError::SwitchMultipleDefaults);
                }
            }

            self.visit_switch_case(case);
        }

        self.exit_scope();
        self.dec_breakable_depth();
    }

    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration<'a>) {
        self.visit_variable_declaration_common(var_decl, false)
    }

    fn visit_labeled_statement(&mut self, stmt: &mut LabeledStatement<'a>) {
        let (inner_stmt, label_stack) = self.push_all_labels(stmt);

        self.visit_statement(inner_stmt);

        self.pop_all_labels(label_stack);
    }

    fn visit_function_expression(&mut self, func: &mut Function<'a>) {
        self.visit_function_common(
            func, /* is_arrow_function */ false, /* is_method */ false,
            /* is_static_method */ false, /* is_derived_constructor */ false,
            /* is_static_initializer */ false,
        );
    }

    fn visit_arrow_function(&mut self, func: &mut Function<'a>) {
        self.visit_function_common(
            func, /* is_arrow_function */ true, /* is_method */ false,
            /* is_static_method */ false, /*is_derived_constructor */ false,
            /* is_static_initializer */ false,
        );
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement<'a>) {
        match self.allow_return_stack.last() {
            Some(true) => {}
            _ => self.emit_error(stmt.loc, ParseError::ReturnOutsideFunction),
        }

        // If this is a return inside a derived constructor then we must resolve `this`, since it
        // may be implicitly returned.
        if matches!(
            self.current_function(),
            Some(FunctionStackEntry { is_derived_constructor: true, .. })
        ) {
            self.resolve_this_use(stmt.loc, |scope| stmt.this_scope = Some(scope));
        }

        default_visit_return_statement(self, stmt);
    }

    fn visit_break_statement(&mut self, stmt: &mut BreakStatement<'a>) {
        self.visit_label_use(stmt.label.as_mut(), false);

        if stmt.label.is_none() && !self.is_in_breakable() {
            self.emit_error(stmt.loc, ParseError::UnlabeledBreakOutsideBreakable);
        }
    }

    fn visit_continue_statement(&mut self, stmt: &mut ContinueStatement<'a>) {
        self.visit_label_use(stmt.label.as_mut(), true);

        if !self.is_in_iterable() {
            self.emit_error(stmt.loc, ParseError::ContinueOutsideIterable);
        }
    }

    fn visit_with_statement(&mut self, stmt: &mut WithStatement<'a>) {
        if self.is_in_strict_mode_context() {
            self.emit_error(stmt.loc, ParseError::WithInStrictMode);
        }

        self.check_for_labeled_function(&stmt.body);

        self.visit_outer_expression(&mut stmt.object);

        // Must conservatively use VM scope locations for all visible bindings so that they can be
        // dynamcally looked up from within the with statement.
        let current_scope_id = self.current_scope_id();
        self.scope_tree
            .support_dynamic_access_in_visible_bindings(current_scope_id);

        // With statement bodies are in their own scope
        self.enter_scope(stmt.scope);
        self.visit_statement(&mut stmt.body);
        self.exit_scope();
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStatement<'a>) {
        self.check_for_labeled_function(&stmt.conseq);
        if let Some(altern) = stmt.altern.as_ref() {
            self.check_for_labeled_function(altern);
        }

        default_visit_if_statement(self, stmt);
    }

    fn visit_while_statement(&mut self, stmt: &mut WhileStatement<'a>) {
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_while_statement(self, stmt);

        self.dec_iterable_depth();
    }

    fn visit_do_while_statement(&mut self, stmt: &mut DoWhileStatement<'a>) {
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_do_while_statement(self, stmt);

        self.dec_iterable_depth();
    }

    fn visit_for_statement(&mut self, stmt: &mut ForStatement<'a>) {
        self.inc_iterable_depth();
        self.enter_scope(stmt.scope);

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_statement(self, stmt);

        self.exit_scope();
        self.dec_iterable_depth();
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement<'a>) {
        self.inc_iterable_depth();
        self.enter_scope(stmt.scope);

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_each_statement(self, stmt);

        self.exit_scope();
        self.dec_iterable_depth();
    }

    fn visit_for_each_init(&mut self, init: &mut ForEachInit<'a>) {
        match init {
            ForEachInit::Pattern { pattern, has_assign_expr } => {
                // Pattern is in its own "has assignment expression" context
                self.enter_has_assign_expr_context();
                self.visit_pattern(pattern);
                *has_assign_expr = self.exit_has_assign_expr_context();
            }
            ForEachInit::VarDecl(decl) => self.visit_variable_declaration_common(decl, true),
        }
    }

    fn visit_catch_clause(&mut self, catch: &mut CatchClause<'a>) {
        // Catch scope includes the catch parameter
        self.enter_scope(catch.scope);

        // Param is in its own "has assignment expression" context
        if let Some(param) = &mut catch.param {
            self.enter_has_assign_expr_context();
            self.visit_pattern(param);
            catch.param_has_assign_expr = self.exit_has_assign_expr_context();
        }

        self.visit_block(&mut catch.body);

        self.exit_scope();

        // Error if any catch parameter bindings conflict with bindings in the body of the catch
        let catch_body_scope = catch.body.scope.as_ref();
        for (name, _) in catch.scope.as_ref().iter_bindings() {
            if let Some(err) = catch_body_scope.error_if_lexical_name_already_declared(name) {
                self.emit_error(catch.loc, err);
            }
        }
    }

    fn visit_block(&mut self, block: &mut Block<'a>) {
        self.enter_scope(block.scope);
        default_visit_block(self, block);
        self.exit_scope();
    }

    fn visit_member_expression(&mut self, expr: &mut MemberExpression<'a>) {
        if expr.is_private {
            self.visit_private_name_use(&mut expr.property);
        }

        self.visit_expression(&mut expr.object);

        // Visit computed property expressions, but not id used as named property
        if expr.is_computed {
            self.visit_expression(&mut expr.property);
        }
    }

    fn visit_binary_expression(&mut self, expr: &mut BinaryExpression<'a>) {
        if expr.operator == BinaryOperator::InPrivate {
            self.visit_private_name_use(&mut expr.left);
            self.visit_expression(&mut expr.right);
        } else {
            default_visit_binary_expression(self, expr);
        }
    }

    fn visit_assignment_expression(&mut self, expr: &mut AssignmentExpression<'a>) {
        // Mark the current context as having an assignment expression
        self.has_assign_expr = true;

        default_visit_assignment_expression(self, expr);
    }

    fn visit_object_expression(&mut self, expr: &mut ObjectExpression<'a>) {
        // Do not allow duplicate __proto__ initializer properties
        let mut has_proto_init = false;
        for property in expr.properties.iter() {
            if let PropertyKind::Init = property.kind {
                // Must be a simple proto initializer
                if !property.is_computed && !property.is_method && property.value.is_some() {
                    let is_proto_key = match property.key.as_ref() {
                        Expression::Id(id) if id.name == "__proto__" => true,
                        Expression::String(lit) if lit.value == "__proto__" => true,
                        _ => false,
                    };

                    if is_proto_key {
                        if has_proto_init {
                            self.emit_error(property.loc, ParseError::DuplicateProtoProperty);
                        }

                        has_proto_init = true;
                    }
                }
            }
        }

        self.enter_scope(expr.scope);
        default_visit_object_expression(self, expr);
        self.exit_scope();
    }

    fn visit_property(&mut self, prop: &mut Property<'a>) {
        if let PropertyKind::PatternInitializer(_) = prop.kind {
            self.emit_error(prop.loc, ParseError::InvalidPatternInitializer);
        }

        // Visit key expression for computed and shorthand properties, but not id of named property
        if prop.is_computed || prop.value.is_none() {
            self.visit_expression(&mut prop.key);
        }

        if prop.is_method {
            let func = if let Some(Expression::Function(func)) = prop.value.as_deref_mut() {
                func
            } else {
                unreachable!("method must have function value");
            };

            self.visit_function_common(
                func, /* is_arrow_function */ false, /* is_method */ true,
                /* is_static_method */ false, /*is_derived_constructor */ false,
                /* is_static_initializer */ false,
            );
        } else {
            visit_opt!(self, prop.value, visit_expression);
        }
    }

    fn visit_object_pattern_property(&mut self, prop: &mut ObjectPatternProperty<'a>) {
        // Visit pattern for computed properties, but not id of named property
        if prop.is_computed {
            visit_opt!(self, prop.key, visit_expression);
        }

        self.visit_pattern(&mut prop.value);
    }

    fn visit_meta_property(&mut self, expr: &mut MetaProperty<'a>) {
        match &mut expr.kind {
            // new.target is treated as a binding that is implicitly created on a parent function
            // scope. Store the scope node of the function that contains this new.target expression.
            MetaPropertyKind::NewTarget { ref mut scope } => {
                if self.is_in_non_arrow_function() {
                    self.resolve_new_target_use(scope, expr.loc);
                } else {
                    self.emit_error(expr.loc, ParseError::NewTargetOutsideFunction);
                }
            }
            MetaPropertyKind::ImportMeta => {}
        }
    }

    fn visit_await_expression(&mut self, expr: &mut AwaitExpression<'a>) {
        // Mark if the program has a top level await
        if self.is_at_top_level() {
            self.has_top_level_await = true;
        }

        if let Some(true) = self.in_parameters_stack.last() {
            self.emit_error(expr.loc, ParseError::AwaitInParameters)
        } else if let Some(FunctionStackEntry { is_static_initializer: true, .. }) =
            self.current_function()
        {
            self.emit_error(expr.loc, ParseError::AwaitInStaticInitializer)
        }

        default_visit_await_expression(self, expr)
    }

    fn visit_yield_expression(&mut self, expr: &mut YieldExpression<'a>) {
        if let Some(true) = self.in_parameters_stack.last() {
            self.emit_error(expr.loc, ParseError::YieldInParameters)
        }

        default_visit_yield_expression(self, expr)
    }

    fn visit_super_member_expression(&mut self, expr: &mut SuperMemberExpression<'a>) {
        match self.allow_super_member_stack.last() {
            Some(AllowSuperStackEntry::Allow { is_static }) => {
                expr.is_static = *is_static;

                // Resolve home object
                let home_object_name = expr.home_object_name();
                self.resolve_use(&mut expr.home_object_scope, home_object_name, expr.super_);

                // Also resolve `this` which may be used by the super member expression
                self.resolve_this_use(expr.loc, |scope| expr.this_scope = Some(scope));
            }
            _ => self.emit_error(expr.loc, ParseError::SuperPropertyOutsideMethod),
        }

        default_visit_super_member_expression(self, expr)
    }

    fn visit_super_call_expression(&mut self, expr: &mut SuperCallExpression<'a>) {
        match self.enclosing_non_arrow_function() {
            Some(FunctionStackEntry { is_derived_constructor: true, .. }) => {
                // A super call implicitly uses the current derived constructor, new.target, and
                // this, so resolve them.
                self.resolve_derived_constructor_use(&mut expr.constructor_scope, expr.loc);
                self.resolve_new_target_use(&mut expr.new_target_scope, expr.loc);
                self.resolve_this_use(expr.loc, |scope| expr.this_scope = scope);
            }
            _ => self.emit_error(expr.loc, ParseError::SuperCallOutsideDerivedConstructor),
        }

        default_visit_super_call_expression(self, expr)
    }

    fn visit_unary_expression(&mut self, expr: &mut UnaryExpression<'a>) {
        if expr.operator == UnaryOperator::Delete && self.is_in_strict_mode_context() {
            match expr.argument.as_ref() {
                Expression::Id(_) => {
                    self.emit_error(expr.loc, ParseError::DeleteIdentifierInStrictMode);
                }
                Expression::Member(member_expr) if member_expr.is_private => {
                    self.emit_error(expr.loc, ParseError::DeletePrivateProperty);
                }
                _ => {}
            }
        }

        default_visit_unary_expression(self, expr);
    }

    fn visit_this_expression(&mut self, this: &mut ThisExpression<'a>) {
        self.resolve_this_use(this.loc, |scope| this.scope = Some(scope));
    }

    fn visit_call_expression(&mut self, expr: &mut CallExpression<'a>) {
        // If a potential direct eval is ever seen, conservatively force all visible bindings to
        // have VM scope locations instead of local registers so they can be dynamically looked up.
        match expr.callee.as_ref() {
            Expression::Id(Identifier { name, .. }) if name == "eval" && !expr.is_optional => {
                let current_scope_id = self.current_scope_id();
                self.scope_tree
                    .support_dynamic_access_in_visible_bindings(current_scope_id);

                if let Some(current_func) = self.enclosing_non_arrow_function() {
                    expr.maybe_eval_in_function = true;
                    expr.maybe_eval_in_method = current_func.is_method;
                    expr.maybe_eval_in_static = current_func.is_static;
                    expr.maybe_eval_in_derived_constructor = current_func.is_derived_constructor;
                    expr.maybe_eval_in_static_initializer = current_func.is_static_initializer;
                    expr.maybe_eval_in_class_field_initializer =
                        current_func.is_class_field_initializer;
                }
            }
            _ => {}
        }

        default_visit_call_expression(self, expr);
    }

    fn visit_identifier_expression(&mut self, id: &mut Identifier<'a>) {
        if id.name == "arguments" && !self.allow_arguments {
            self.emit_error(id.loc, ParseError::ArgumentsInClassInitializer);
        }

        self.resolve_identifier_use(id);

        default_visit_identifier_expression(self, id)
    }

    fn visit_identifier_pattern(&mut self, id: &mut Identifier<'a>) {
        if id.name == "arguments" && !self.allow_arguments {
            self.emit_error(id.loc, ParseError::ArgumentsInClassInitializer);
        } else {
            self.error_if_strict_eval_or_arguments(id);
        }

        // Def patterns will already have scope node added, but use patterns will not. Resolve ids
        // in use patterns at this point.
        if id.scope.is_unresolved() {
            self.resolve_identifier_use(id);
        }

        default_visit_identifier_pattern(self, id)
    }

    fn visit_variable_declarator(&mut self, decl: &mut VariableDeclarator<'a>) {
        // Variable declarator pattern is in its own "has assignment expression" context
        self.enter_has_assign_expr_context();
        self.visit_pattern(&mut decl.id);
        decl.id_has_assign_expr = self.exit_has_assign_expr_context();

        visit_opt!(self, decl.init, visit_outer_expression);
    }

    fn visit_class(&mut self, class: &mut Class<'a>) {
        // Entire class is in strict mode, including super class expression
        self.enter_strict_mode_context();

        if let Some(class_id) = class.id.as_deref() {
            self.error_if_strict_eval_or_arguments(class_id);
        }

        // Save analyzer context before descending into class body
        let saved_state = self.save_state();

        // Body (including extends clause) is in its own scope
        self.enter_scope(class.scope);

        if let Some(super_class) = class.super_class.as_deref_mut() {
            self.visit_outer_expression(super_class);
        }

        let private_names = self.collect_class_private_names(class);
        let num_private_names = private_names.len();

        self.class_stack
            .push(ClassStackEntry { private_names, is_derived: class.super_class.is_some() });

        // Mark the constructor if it is found, erroring if multiple are found
        let mut constructor = None;

        // Determine the number of computed and private properties in the class, as each will need
        // a scope slot to be stored between name evaluation and field definition.
        let mut num_extra_scope_field_names = num_private_names;

        let field_init_scope = class.fields_initializer_scope;
        let static_init_scope = class.static_initializer_scope;

        for element in class.body.iter_mut() {
            match element {
                ClassElement::Method(method) => {
                    if method.kind == ClassMethodKind::Constructor {
                        if constructor.is_none() {
                            constructor = Some(AstPtr::from_ref(method));
                        } else {
                            self.emit_error(method.loc, ParseError::MultipleConstructors);
                        }
                    }

                    self.visit_class_method(method);
                }
                ClassElement::Property(property) => {
                    let is_computed = property.is_computed
                        || matches!(
                            &property.key.expr,
                            Expression::Number(_) | Expression::BigInt(_)
                        );

                    if is_computed {
                        num_extra_scope_field_names += 1;
                    }

                    let init_scope = if property.is_static {
                        static_init_scope
                    } else {
                        field_init_scope
                    };

                    self.visit_class_property(property, init_scope);
                }
            }
        }

        // Finish the init scopes if they exist since all fields have been visited
        if let Some(scope) = field_init_scope {
            self.scope_tree
                .finish_vm_scope_node(scope.as_ref().id(), None);
        }
        if let Some(scope) = static_init_scope {
            self.scope_tree
                .finish_vm_scope_node(scope.as_ref().id(), None);
        }

        // Then finish and exit the class scope
        self.exit_scope_with_class_fields(num_extra_scope_field_names);

        class.constructor = constructor;

        self.class_stack.pop();

        // Restore analyzer context after visiting class
        self.restore_state(saved_state);

        self.exit_strict_mode_context();
    }

    fn visit_import_declaration(&mut self, import: &mut ImportDeclaration<'a>) {
        for specifier in import.specifiers.iter() {
            match specifier {
                ImportSpecifier::Default(ImportDefaultSpecifier { local, .. })
                | ImportSpecifier::Named(ImportNamedSpecifier { local, .. })
                | ImportSpecifier::Namespace(ImportNamespaceSpecifier { local, .. }) => {
                    self.error_if_strict_eval_or_arguments(local);
                }
            }
        }

        if let Some(attributes) = &mut import.attributes {
            self.visit_import_attributes(attributes);
        }
    }

    fn visit_import_attributes(&mut self, attributes: &mut ImportAttributes<'a>) {
        // Check for duplicate keys
        let mut seen_keys: HashSet<&[u8]> = HashSet::new();

        for attribute in attributes.attributes.iter() {
            let name_slice = match attribute.key.as_ref() {
                Expression::Id(id) => id.name.as_bytes(),
                Expression::String(literal) => literal.value.as_bytes(),
                _ => unreachable!("import attribute key must be id or string"),
            };

            if !seen_keys.insert(name_slice) {
                self.emit_error(attribute.loc, ParseError::DuplicateImportAttribute);
            }
        }
    }

    fn visit_export_default_declaration(&mut self, export: &mut ExportDefaultDeclaration<'a>) {
        default_visit_export_default_declaration(self, export);

        // Mark local bindings as exported
        if let Some(id) = export.id() {
            id.get_binding().set_is_exported(true);
        }

        self.add_export(export.loc, Wtf8Str::from_str("default"));
    }

    fn visit_export_named_declaration(&mut self, export: &mut ExportNamedDeclaration<'a>) {
        default_visit_export_named_declaration(self, export);

        // Mark local bindings as exported
        export.iter_declaration_ids(&mut |id| {
            id.get_binding().set_is_exported(true);
            self.add_export(id.loc, id.name.as_arena_str());
        });

        for specifier in export.specifiers.iter_mut() {
            // Only need to resolve the specifier's local binding if it is not a re-export
            if export.source.is_none() {
                // Local is guaranteed to be an identifier if this is not a re-export
                let local_id = specifier.local.to_id_mut();
                self.resolve_identifier_use(local_id);

                if local_id.scope.is_resolved() {
                    local_id.get_binding().set_is_exported(true);
                } else {
                    self.emit_error(local_id.loc, ParseError::UnresolvedExport);
                }
            }

            // Export name is the exported name if it exists, otherwise the local name
            let export_name = specifier
                .exported
                .as_deref()
                .unwrap_or(specifier.local.as_ref());
            self.add_exported_name(export_name);
        }

        if let Some(attributes) = &mut export.source_attributes {
            self.visit_import_attributes(attributes);
        }
    }

    fn visit_export_all_declaration(&mut self, export: &mut ExportAllDeclaration<'a>) {
        default_visit_export_all_declaration(self, export);

        if let Some(export_name) = export.exported.as_deref() {
            self.add_exported_name(export_name);
        }

        if let Some(attributes) = &mut export.source_attributes {
            self.visit_import_attributes(attributes);
        }
    }
}

impl<'a> Analyzer<'a> {
    fn visit_function_common(
        &mut self,
        func: &mut Function<'a>,
        is_arrow_function: bool,
        is_method: bool,
        is_static_method: bool,
        is_derived_constructor: bool,
        is_static_initializer: bool,
    ) {
        self.function_stack.push(FunctionStackEntry {
            is_arrow_function,
            is_method,
            is_static: is_static_method,
            is_derived_constructor,
            is_static_initializer,
            is_class_field_initializer: false,
        });

        // Return is not allowed in static initializers, but is allowed in all other functions
        self.allow_return_stack.push(!is_static_initializer);

        // Await and yield are allowed in functions (we already ensure await and yield expressions
        // only occur in async/generator functions in the parser).
        self.in_parameters_stack.push(false);

        // Super member expressions are allowed in methods, and in arrow functions are inherited
        // from surrounding context.
        if !is_arrow_function {
            let entry = if is_method {
                let is_static = is_static_method || is_static_initializer;
                AllowSuperStackEntry::Allow { is_static }
            } else {
                AllowSuperStackEntry::Disallow
            };
            self.allow_super_member_stack.push(entry);
        }

        // Save analyzer context before descending into function
        let saved_state = self.save_state();

        // Enter strict mode context if applicable
        if func.is_strict_mode() {
            self.enter_strict_mode_context();
        }

        // Arguments are always allowed directly within non-arrow functions, but not within static
        // initializers. Arrow functions inherit from lexical context.
        if is_static_initializer {
            self.allow_arguments = false;
        } else if !is_arrow_function {
            self.allow_arguments = true;
        }

        // Check function name, which cannot be "eval" or "arguments" in strict mode
        visit_opt!(self, func.id, visit_identifier);

        if !is_method {
            if let Some(func_id) = &func.id {
                self.error_if_strict_eval_or_arguments(func_id);
            }
        }

        // Function parameters and body (but not function name) are in the function scope
        self.enter_scope(func.scope);

        // Visit and analyze function parameters. Await and yield expressions are not allowed within
        // function parameters.
        self.in_parameters_stack.push(true);

        for param in func.params.iter_mut() {
            self.visit_function_param(param);
        }

        self.in_parameters_stack.pop();

        for (param_index, param) in func.params.iter_mut().enumerate() {
            // Check if this is a top level id pattern, optionally with a default
            let toplevel_id = match param {
                FunctionParam::Pattern { pattern: Pattern::Id(id), .. } => Some(id),
                FunctionParam::Pattern {
                    pattern: Pattern::Assign(AssignmentPattern { left, .. }),
                    ..
                } => {
                    if let Pattern::Id(id) = left.as_mut() {
                        Some(id)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            // If this is a top level id pattern then the binding's VM location is the argument
            // directly by index. This may be overriden later if captured.
            if let Some(id) = toplevel_id {
                let scope = id.scope.unwrap_resolved_mut();
                let binding = scope.get_binding_mut(id.name.as_arena_str());

                // Allow later arguments to overwrite earlier ones but do not overwrite a WithVar.
                if !binding.needs_tdz_check()
                    && !matches!(binding.vm_location(), Some(VMLocation::WithVar))
                {
                    binding.set_vm_location(VMLocation::Argument(param_index));
                }
            }
        }

        // Functions with an explicit "use strict" in their body must have a simple parameter list
        if func.has_use_strict_directive() && !func.has_simple_parameter_list() {
            self.emit_error(func.loc, ParseError::UseStrictFunctionNonSimpleParameterList);
        }

        // Duplicate parameters are not allowed in certain contexts
        if func.has_duplicate_parameters() {
            let invalid_reason = if self.is_in_strict_mode_context() {
                Some(InvalidDuplicateParametersReason::StrictMode)
            } else if is_arrow_function {
                Some(InvalidDuplicateParametersReason::ArrowFunction)
            } else if is_method {
                Some(InvalidDuplicateParametersReason::Method)
            } else if !func.has_simple_parameter_list() {
                Some(InvalidDuplicateParametersReason::NonSimpleParameters)
            } else {
                None
            };

            if let Some(invalid_reason) = invalid_reason {
                self.emit_error(func.loc, ParseError::InvalidDuplicateParameters(invalid_reason));
            }
        }

        // If there is a potential eval in the function params then the function body VM scope must
        // always be created, even if empty. This is used to distinguish between evals that occur in
        // the function body vs function params scope when checking for conflicts with parameter
        // names.
        if func.scope.as_ref().supports_dynamic_bindings() {
            if let FunctionBody::Block(body) = func.body.as_mut() {
                if let Some(mut scope) = body.scope {
                    scope.as_mut().set_allow_empty_vm_node(true);
                }
            }
        }

        // Visit function body
        self.visit_function_body(&mut func.body);

        // Check if the arguments object or new.target is needed. These may be needed if the
        // function has an "arguments" or new.target binding with the right kind, which may have
        // been added due to an implicit use or potential dynamic lookup found during analysis.
        if !is_arrow_function {
            let arguments_binding_opt = func.scope.as_ref().get_binding_opt(&ARGUMENTS_NAME);
            let is_arguments_object_needed = arguments_binding_opt
                .map(|binding| binding.kind().is_valid_arguments_kind())
                .unwrap_or(false);
            func.set_is_arguments_object_needed(is_arguments_object_needed);

            if func.scope.as_ref().has_binding(&NEW_TARGET_BINDING_NAME) {
                func.set_is_new_target_needed(true);
            }
        }

        // Only exit scope after visiting body and determining if arguments object is needed. We
        // need to know whether the arguments object is needed before creating the corresponding
        // VM scope node, as arguments may need to be added to VM scope for mapped arguments object.
        if func.needs_mapped_arguments_object() {
            self.exit_scope_with_mapped_arguments_object(func.params.len());
        } else {
            self.exit_scope();
        }

        if func.is_strict_mode() {
            self.exit_strict_mode_context();
        }

        if !is_arrow_function {
            self.allow_super_member_stack.pop();
        }

        self.in_parameters_stack.pop();
        self.allow_return_stack.pop();
        self.function_stack.pop();

        // Restore analyzer context after visiting function
        self.restore_state(saved_state);
    }

    fn collect_class_private_names(
        &mut self,
        class: &mut Class<'a>,
    ) -> HashMap<Wtf8String, PrivateNameUsage> {
        // Create new private name scope for stack and initialize with defined private names
        let mut private_names = HashMap::new();
        let mut has_private_accessor_pair = false;

        for element in class.body.iter_mut() {
            let private_id = match element {
                ClassElement::Property(ClassProperty { is_private: true, key, .. }) => {
                    let private_id = key.expr.to_id_mut();

                    // If this name has been used at all so far it is a duplicate name
                    if private_names.contains_key(private_id.name.as_str()) {
                        self.emit_error(
                            private_id.loc,
                            ParseError::new_duplicate_private_name(
                                private_id.name.clone_in(Global),
                            ),
                        );
                    } else {
                        // Create a complete usage that does not allow any other uses of this name
                        let usage = PrivateNameUsage {
                            is_static: false,
                            has_getter: true,
                            has_setter: true,
                        };
                        private_names.insert(private_id.name.clone_in(Global), usage);
                    }

                    private_id
                }

                ClassElement::Method(ClassMethod {
                    is_private: true,
                    key,
                    is_static,
                    kind,
                    ..
                }) => {
                    let private_id = key.expr.to_id_mut();

                    // Check for duplicate name definitions. Only allow multiple definitions if
                    // there is exactly one getter and setter that have the same static property.
                    match private_names.get_mut(private_id.name.as_str()) {
                        // Mark usage for private name and its method type
                        None => {
                            let is_static = *is_static;
                            let usage = if *kind == ClassMethodKind::Get {
                                PrivateNameUsage { is_static, has_getter: true, has_setter: false }
                            } else if *kind == ClassMethodKind::Set {
                                PrivateNameUsage { is_static, has_getter: false, has_setter: true }
                            } else {
                                PrivateNameUsage { is_static, has_getter: true, has_setter: true }
                            };

                            private_names.insert(private_id.name.clone_in(Global), usage);
                        }
                        // This private name has already been seen. Only avoid erroring if this use
                        // is a getter or setter which has not yet been seen.
                        Some(usage) => {
                            let is_duplicate = if *kind == ClassMethodKind::Get {
                                let had_getter = usage.has_getter;
                                usage.has_getter = true;
                                had_getter || *is_static != usage.is_static
                            } else if *kind == ClassMethodKind::Set {
                                let had_setter = usage.has_setter;
                                usage.has_setter = true;
                                had_setter || *is_static != usage.is_static
                            } else {
                                true
                            };

                            if usage.has_getter && usage.has_setter {
                                has_private_accessor_pair = true;
                            }

                            if is_duplicate {
                                let private_name = private_id.name.clone_in(Global);
                                self.emit_error(
                                    private_id.loc,
                                    ParseError::new_duplicate_private_name(private_name),
                                );
                            }
                        }
                    }

                    private_id
                }
                _ => continue,
            };

            if private_id.name == "constructor" {
                self.emit_error(private_id.loc, ParseError::PrivateNameConstructor);
            }

            self.resolve_private_identifier_use(private_id);
        }

        // If any private accessor pair was encountered then mark the first part of every pair in
        // the AST so that it can be identified during bytecode generation.
        if has_private_accessor_pair {
            let mut marked_pairs = HashSet::new();
            for element in class.body.iter_mut() {
                if let ClassElement::Method(ClassMethod {
                    is_private: true,
                    kind: ClassMethodKind::Get | ClassMethodKind::Set,
                    key,
                    is_private_pair_start,
                    ..
                }) = element
                {
                    let private_id = key.expr.to_id();
                    let usage = private_names.get(private_id.name.as_str()).unwrap();
                    if usage.has_getter && usage.has_setter {
                        if marked_pairs.insert(&private_id.name) {
                            *is_private_pair_start = true;
                        }
                    }
                }
            }
        }

        private_names
    }

    fn visit_class_method(&mut self, method: &mut ClassMethod<'a>) {
        let key_name_bytes = if method.is_computed {
            None
        } else {
            match &method.key.expr {
                Expression::String(name) => Some(name.value.as_bytes()),
                Expression::Id(id) => Some(id.name.as_bytes()),
                _ => None,
            }
        };

        // Constructors may be simple methods, without being async, generator, getter, or setter.
        // Static constructors are allowed however.
        let is_bad_constructor = match method.kind {
            ClassMethodKind::Constructor => method.value.is_async() || method.value.is_generator(),
            ClassMethodKind::Get | ClassMethodKind::Set if !method.is_static => {
                key_name_bytes.is_some_and(|name| name == "constructor".as_bytes())
            }
            _ => false,
        };

        if is_bad_constructor {
            self.emit_error(method.loc, ParseError::NonSimpleConstructor);
        }

        if method.is_static
            && !method.is_private
            && key_name_bytes.is_some_and(|name| name == "prototype".as_bytes())
        {
            self.emit_error(method.loc, ParseError::ClassStaticPrototype);
        }

        if method.is_computed {
            self.visit_outer_expression(&mut method.key);
        }

        let is_derived_constructor = method.kind == ClassMethodKind::Constructor
            && self.class_stack.last().unwrap().is_derived;

        self.visit_function_common(
            &mut method.value,
            /* is_arrow_function */ false,
            /* is_method */ true,
            method.is_static,
            is_derived_constructor,
            method.kind == ClassMethodKind::StaticInitializer,
        );
    }

    fn visit_class_property(
        &mut self,
        prop: &mut ClassProperty<'a>,
        init_scope: Option<AstPtr<AstScopeNode<'a>>>,
    ) {
        let key_name_bytes = if prop.is_computed {
            None
        } else {
            match &prop.key.expr {
                Expression::String(name) => Some(name.value.as_bytes()),
                Expression::Id(id) => Some(id.name.as_bytes()),
                _ => None,
            }
        };

        match key_name_bytes {
            Some(name) if name == "constructor".as_bytes() => {
                self.emit_error(prop.loc, ParseError::NonSimpleConstructor)
            }
            Some(name) if name == "prototype".as_bytes() && prop.is_static => {
                self.emit_error(prop.loc, ParseError::ClassStaticPrototype)
            }
            _ => {}
        }

        if prop.is_computed {
            self.visit_outer_expression(&mut prop.key);
        }

        // "arguments" is not allowed in initializer (but is allowed in key)
        let old_allow_arguments = self.allow_arguments;
        self.allow_arguments = false;

        // Class field initializers are in their own function scope. Class field initializer bodies
        // are treated as (potentially static) methods.
        if let Some(init_scope) = init_scope {
            self.enter_scope(init_scope);
        }

        self.allow_super_member_stack
            .push(AllowSuperStackEntry::Allow { is_static: prop.is_static });
        self.function_stack.push(FunctionStackEntry {
            is_arrow_function: false,
            is_method: true,
            is_static: prop.is_static,
            is_derived_constructor: false,
            is_static_initializer: false,
            is_class_field_initializer: true,
        });

        visit_opt!(self, prop.value, visit_outer_expression);

        self.function_stack.pop();
        self.allow_super_member_stack.pop();

        // Do not finish init scope yet since there may be other fields
        if init_scope.is_some() {
            self.exit_scope_without_finishing();
        }

        self.allow_arguments = old_allow_arguments;
    }

    fn visit_variable_declaration_common(
        &mut self,
        var_decl: &mut VariableDeclaration<'a>,
        is_for_each_init: bool,
    ) {
        for declaration in var_decl.declarations.iter() {
            if var_decl.kind == VarKind::Const && declaration.init.is_none() && !is_for_each_init {
                self.emit_error(declaration.loc, ParseError::ConstWithoutInitializer);
            }

            // Check for invalid names depending on context
            must!(declaration.iter_bound_names(&mut |id| {
                if var_decl.kind != VarKind::Var && id.name == "let" {
                    self.emit_error(id.loc, ParseError::LetNameInLexicalDeclaration);
                }

                Ok(())
            }));
        }

        default_visit_variable_declaration(self, var_decl);
    }

    // Visit all nested labeled statements, adding labels to scope and returning the inner
    // non-labeled statement. Labels that are directly nested within each other all have the same
    // label id.
    fn push_all_labels<'b>(
        &mut self,
        stmt: &'b mut LabeledStatement<'a>,
    ) -> (&'b mut Statement<'a>, Vec<(&'a Wtf8Str, bool)>) {
        // Always use 1 more than the current lable depth, so that a label id of 0 marks the
        // empty label.
        let label_id = self.label_depth + 1;
        self.label_depth += 1;

        let mut labels = vec![];
        // Keep track of duplicate labels so that we don't pop the duplicate labels at the end
        let is_label_duplicate = self.visit_label_def(stmt, label_id);
        let label_name = stmt.label.name.as_arena_str();
        labels.push((label_name, is_label_duplicate));

        let mut inner_stmt = stmt.body.as_mut();
        while let Statement::Labeled(stmt) = inner_stmt {
            let is_label_duplicate = self.visit_label_def(stmt, label_id);
            let label_name = stmt.label.name.as_arena_str();
            labels.push((label_name, is_label_duplicate));

            inner_stmt = stmt.body.as_mut()
        }

        // Only some statements can be a continue target
        let is_continue_target = matches!(
            inner_stmt,
            Statement::While(_)
                | Statement::DoWhile(_)
                | Statement::For(_)
                | Statement::ForEach(_)
                | Statement::Switch(_)
        );

        if is_continue_target {
            for (label, _) in &labels {
                self.labels.get_mut(label).unwrap().is_continue_target = true;
            }
        }

        (inner_stmt, labels)
    }

    fn pop_all_labels(&mut self, label_stack: Vec<(&'a Wtf8Str, bool)>) {
        // Exit all label scopes in reverse order that they were entered
        for (label_name, is_duplicate) in label_stack.iter().rev() {
            if !is_duplicate {
                self.labels.remove(label_name);
            }
        }

        self.label_depth -= 1;
    }

    fn visit_label_def(&mut self, stmt: &mut LabeledStatement<'a>, label_id: LabelId) -> bool {
        let label_name = &stmt.label.name.as_arena_str();
        let is_duplicate = self.labels.contains_key(label_name);

        if is_duplicate {
            self.emit_error(stmt.label.loc, ParseError::DuplicateLabel);
        } else {
            self.labels
                .insert(label_name, LabelInfo { label_id, is_continue_target: false });
        }

        stmt.label.id = label_id;

        // Annex B: Always error on labeled function declarations in strict mode
        if self.is_in_strict_mode_context() {
            if let Statement::FuncDecl(_) = stmt.body.as_ref() {
                self.emit_error(stmt.label.loc, ParseError::InvalidLabeledFunction(true));
            }
        }

        is_duplicate
    }

    // Annex B: Some labeled function declarations are always disallowed. Only error in
    // non-strict mode as all labeled function declarations are disallowed in strict mode.
    fn check_for_labeled_function(&mut self, stmt: &Statement) {
        if let Statement::Labeled(labeled) = stmt {
            // Descend past nested labels to labeled statement
            let mut current_labeled = labeled;
            while let Statement::Labeled(next_labeled) = current_labeled.body.as_ref() {
                current_labeled = next_labeled;
            }

            if let Statement::FuncDecl(_) = current_labeled.body.as_ref() {
                if !self.is_in_strict_mode_context() {
                    self.emit_error(labeled.label.loc, ParseError::InvalidLabeledFunction(false))
                }
            }
        }
    }

    fn visit_label_use(&mut self, label: Option<&mut Label<'a>>, is_continue: bool) {
        if let Some(label) = label {
            match self.labels.get(label.name.as_arena_str()) {
                None => self.emit_error(label.loc, ParseError::LabelNotFound),
                Some(label_info) if is_continue && !label_info.is_continue_target => {
                    self.emit_error(label.loc, ParseError::LabelNotFound)
                }
                Some(label_info) => {
                    label.id = label_info.label_id;
                }
            }
        }
    }

    fn visit_private_name_use(&mut self, expr: &mut Expression<'a>) {
        let id = expr.to_id_mut();

        if self.class_stack.is_empty() {
            self.emit_error(id.loc, ParseError::PrivateNameOutsideClass);
        } else {
            // Check if private name is defined in this class or a parent class in its scope
            let mut is_defined = false;
            for class_entry in self.class_stack.iter().rev() {
                if class_entry.private_names.contains_key(id.name.as_str()) {
                    is_defined = true;
                    break;
                }
            }

            self.resolve_private_identifier_use(id);

            if !is_defined {
                let private_name = id.name.clone_in(Global);
                self.emit_error(id.loc, ParseError::new_private_name_not_defined(private_name));
            }
        }
    }

    fn add_export(&mut self, loc: Loc, name: &'a Wtf8Str) {
        if self.export_names.contains(&name) {
            let boxed_name = Box::new(name.to_owned_in(Global));
            self.emit_error(loc, ParseError::DuplicateExport(boxed_name));
            return;
        }

        self.export_names.insert(name);
    }

    fn add_exported_name(&mut self, export_name: &ExportName<'a>) {
        match export_name {
            ExportName::Id(id) => {
                self.add_export(id.loc, id.name.as_arena_str());
            }
            ExportName::String(string) => {
                self.add_export(string.loc, string.value);
            }
        }
    }

    fn resolve_use(
        &mut self,
        result_scope: &mut TaggedResolvedScope<'a>,
        name: &Wtf8Str,
        loc: Loc,
    ) {
        let current_scope = self.scope_stack.last().unwrap().as_ref().id();
        let (def_scope, _) = self.scope_tree.resolve_use(current_scope, name, loc);
        *result_scope = def_scope;
    }

    fn resolve_identifier_use(&mut self, id: &mut Identifier<'a>) {
        self.resolve_use(&mut id.scope, &id.name, id.loc);
    }

    fn resolve_private_identifier_use(&mut self, private_id: &mut Identifier<'a>) {
        // Private name has a "#" prefix
        let private_name = Wtf8String::from_string(format!("#{}", &private_id.name));
        self.resolve_use(&mut private_id.scope, &private_name, private_id.loc);
    }

    fn resolve_this_use(&mut self, loc: Loc, mut set_scope: impl FnMut(AstPtr<AstScopeNode<'a>>)) {
        let current_scope = self.scope_stack.last().unwrap().as_ref().id();
        let (def_scope, is_capture) = self.scope_tree.resolve_use(current_scope, &THIS_NAME, loc);

        // Only set scope if this is a capture of a `this` binding or if `this` is for a derived
        // constructor.
        let is_derived_constructor_this = matches!(
            self.enclosing_non_arrow_function(),
            Some(FunctionStackEntry { is_derived_constructor: true, .. })
        );

        if is_capture || is_derived_constructor_this {
            set_scope(AstPtr::from_ref(def_scope.unwrap_resolved()));
        }
    }

    fn resolve_new_target_use(&mut self, result_scope: &mut TaggedResolvedScope<'a>, loc: Loc) {
        self.resolve_use(result_scope, &NEW_TARGET_BINDING_NAME, loc);
    }

    fn resolve_derived_constructor_use(
        &mut self,
        result_scope: &mut TaggedResolvedScope<'a>,
        loc: Loc,
    ) {
        self.resolve_use(result_scope, &DERIVED_CONSTRUCTOR_BINDING_NAME, loc);
    }
}

pub fn analyze(
    parse_result: ParseProgramResult,
) -> Result<AnalyzedProgramResult, LocalizedParseErrors> {
    let ParseProgramResult { mut program, scope_tree, source } = parse_result;

    let mut analyzer = Analyzer::new(source.clone(), scope_tree);
    analyzer.visit_program(&mut program);

    if analyzer.errors.is_empty() {
        let scope_tree = analyzer.finish();
        Ok(AnalyzedProgramResult { program, scope_tree, source })
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_for_eval<'a>(
    pcx: &'a ParseContext,
    parse_result: ParseProgramResult<'a>,
    private_names: Option<HashMap<Wtf8String, PrivateNameUsage>>,
    in_function: bool,
    in_method: bool,
    in_static: bool,
    in_derived_constructor: bool,
    in_static_initializer: bool,
    in_class_field_initializer: bool,
) -> Result<AnalyzedProgramResult<'a>, LocalizedParseErrors> {
    let ParseProgramResult { mut program, scope_tree, .. } = parse_result;

    let source = pcx.source().clone();
    let mut analyzer = Analyzer::new(source.clone(), scope_tree);

    // Initialize private names from surrounding context if supplied
    if let Some(private_names) = private_names {
        analyzer
            .class_stack
            .push(ClassStackEntry { private_names, is_derived: false });
    }

    // If in function add a synthetic function entry
    if in_function {
        analyzer.function_stack.push(FunctionStackEntry {
            is_arrow_function: false,
            is_method: in_method,
            is_static: in_static,
            is_derived_constructor: in_derived_constructor,
            is_static_initializer: in_static_initializer,
            is_class_field_initializer: in_class_field_initializer,
        });

        if in_method || in_class_field_initializer {
            analyzer
                .allow_super_member_stack
                .push(AllowSuperStackEntry::Allow { is_static: in_static });
        }
    }

    // If in class field initializer then "arguments" is not allowed
    if in_class_field_initializer {
        analyzer.allow_arguments = false;
    }

    analyzer.visit_program(&mut program);

    if analyzer.errors.is_empty() {
        let scope_tree = analyzer.finish();
        Ok(AnalyzedProgramResult { program, scope_tree, source })
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_function_for_function_constructor<'a>(
    pcx: &'a ParseContext,
    parse_result: ParseFunctionResult<'a>,
) -> Result<AnalyzedFunctionResult<'a>, LocalizedParseErrors> {
    let ParseFunctionResult { mut function, scope_tree, .. } = parse_result;

    let source = pcx.source().clone();
    let mut analyzer = Analyzer::new(source.clone(), scope_tree);
    analyzer.visit_function_expression(&mut function);

    if analyzer.errors.is_empty() {
        let scope_tree = analyzer.finish();
        Ok(AnalyzedFunctionResult { function, scope_tree, source })
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}
