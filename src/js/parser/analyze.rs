use std::{collections::HashMap, rc::Rc};

use crate::{
    js::{
        parser::{parse_error::InvalidDuplicateParametersReason, scope_tree::VMLocation},
        runtime::EvalResult,
    },
    must, visit_opt, visit_vec,
};

use super::{
    ast::*,
    ast_visitor::*,
    loc::Loc,
    parser::{ParseFunctionResult, ParseProgramResult},
    scope_tree::{AstScopeNode, ScopeNodeId, ScopeTree, NEW_TARGET_BINDING_NAME},
    source::Source,
    LocalizedParseError, LocalizedParseErrors, ParseError,
};

pub struct Analyzer<'a> {
    // Current source that is being analyzed
    source: Rc<Source>,
    // Accumulator or errors reported during analysis
    errors: Vec<LocalizedParseError>,
    /// Scope tree for the AST that is being analyzed
    scope_tree: &'a mut ScopeTree,
    // Number of nested strict mode contexts the visitor is currently in
    strict_mode_context_depth: u64,
    // Set of labels defined where the visitor is currently in
    labels: HashMap<String, LabelInfo>,
    // Number of labeled statements that the visitor is currently inside. Multiple labels on the
    // same statement all count as a single "label depth", and will receive the same label id.
    label_depth: LabelId,
    // Number of nested breakable statements the visitor is currently inside
    breakable_depth: usize,
    // Number of nested iterable statements the visitor is currently inside
    iterable_depth: usize,
    // The functions that the visitor is currently inside, if any
    function_stack: Vec<FunctionStackEntry>,
    // The classes that the visitor is currently inside, if any
    class_stack: Vec<ClassStackEntry>,
    /// Stack of scopes that the visitor is currently inside
    scope_stack: Vec<AstPtr<AstScopeNode>>,
    // Whether the "arguments" identifier is currently disallowed due to being in a class initializer
    allow_arguments: bool,
    // Whether a return statement is allowed
    allow_return_stack: Vec<bool>,
    // Whether a super member expression is allowed
    allow_super_member_stack: Vec<bool>,
    /// Whether the current expression context has an assignment expression. A single value is
    /// needed instead of a stack, since we ensure that there is only one `has_assign_expr` context
    /// around each expression.
    has_assign_expr: bool,
}

struct FunctionStackEntry {
    is_arrow_function: bool,
    _is_method: bool,
    is_derived_constructor: bool,
}

struct ClassStackEntry {
    // All private names bound in the body of this class
    private_names: HashMap<String, PrivateNameUsage>,
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
struct AnalyzerSavedState {
    labels: HashMap<String, LabelInfo>,
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
    pub fn new(source: Rc<Source>, scope_tree: &'a mut ScopeTree) -> Analyzer<'a> {
        Analyzer {
            source,
            errors: Vec::new(),
            scope_tree,
            strict_mode_context_depth: 0,
            labels: HashMap::new(),
            label_depth: 0,
            breakable_depth: 0,
            iterable_depth: 0,
            function_stack: vec![],
            class_stack: vec![],
            scope_stack: vec![],
            allow_arguments: true,
            allow_return_stack: vec![false],
            allow_super_member_stack: vec![false],
            has_assign_expr: false,
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

    // Save state before visiting a function or class. This prevents labels and some context from
    // leaking into the inner function or class.
    fn save_state(&mut self) -> AnalyzerSavedState {
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
    fn restore_state(&mut self, mut state: AnalyzerSavedState) {
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

        match id.name.as_str() {
            "eval" => self.emit_error(id.loc, ParseError::AssignEvalInStrictMode),
            "arguments" => self.emit_error(id.loc, ParseError::AssignArgumentsInStrictMode),
            _ => {}
        }
    }

    fn enter_scope(&mut self, scope: AstPtr<AstScopeNode>) {
        self.scope_stack.push(scope);
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

    fn exit_scope_with_class_fields(&mut self, num_computed_fields: usize) {
        if let Some(exited_scope_node) = self.scope_stack.pop() {
            self.scope_tree
                .finish_vm_scope_node(exited_scope_node.as_ref().id(), Some(num_computed_fields));
        }
    }

    fn current_scope_id(&self) -> ScopeNodeId {
        self.scope_stack.last().unwrap().as_ref().id()
    }

    fn finish(self) {
        self.scope_tree.finish_vm_scope_tree();
    }
}

impl<'a> AstVisitor for Analyzer<'a> {
    fn visit_program(&mut self, program: &mut Program) {
        if program.is_strict_mode {
            self.enter_strict_mode_context();
        }

        self.enter_scope(program.scope);

        visit_vec!(self, program.toplevels, visit_toplevel);

        self.exit_scope();

        if program.is_strict_mode {
            self.exit_strict_mode_context();
        }
    }

    fn visit_function_declaration(&mut self, func: &mut Function) {
        self.visit_function_common(
            func, /* is_arrow_function */ false, /* is_method */ false,
            /*is_derived_constructor */ false, /* is_static_initializer */ false,
        );
    }

    fn visit_function_param(&mut self, param: &mut FunctionParam) {
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

    fn visit_function_block_body(&mut self, body: &mut FunctionBlockBody) {
        // Body of function may have its own scope
        if let Some(scope) = body.scope {
            self.enter_scope(scope);
        }

        default_visit_function_block_body(self, body);

        if body.scope.is_some() {
            self.exit_scope();
        }
    }

    fn visit_outer_expression(&mut self, expr: &mut OuterExpression) {
        // Outer expressions are in their own "has assignment expression" context
        self.enter_has_assign_expr_context();
        self.visit_expression(&mut expr.expr);
        expr.has_assign_expr = self.exit_has_assign_expr_context();
    }

    fn visit_expression_statement(&mut self, stmt: &mut ExpressionStatement) {
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

    fn visit_switch_statement(&mut self, stmt: &mut SwitchStatement) {
        self.inc_breakable_depth();

        let mut seen_default = false;

        self.visit_outer_expression(&mut stmt.discriminant);

        // Body of switch statement is in its own scope
        self.enter_scope(stmt.scope);

        for case in &mut stmt.cases {
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

    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration) {
        self.visit_variable_declaration_common(var_decl, false)
    }

    fn visit_labeled_statement(&mut self, stmt: &mut LabeledStatement) {
        let (inner_stmt, label_stack) = self.push_all_labels(stmt);

        self.visit_statement(inner_stmt);

        self.pop_all_labels(label_stack);
    }

    fn visit_function_expression(&mut self, func: &mut Function) {
        self.visit_function_common(
            func, /* is_arrow_function */ false, /* is_method */ false,
            /*is_derived_constructor */ false, /* is_static_initializer */ false,
        );
    }

    fn visit_arrow_function(&mut self, func: &mut Function) {
        self.visit_function_common(
            func, /* is_arrow_function */ true, /* is_method */ false,
            /*is_derived_constructor */ false, /* is_static_initializer */ false,
        );
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) {
        match self.allow_return_stack.last() {
            Some(true) => {}
            _ => self.emit_error(stmt.loc, ParseError::ReturnOutsideFunction),
        }

        default_visit_return_statement(self, stmt);
    }

    fn visit_break_statement(&mut self, stmt: &mut BreakStatement) {
        self.visit_label_use(stmt.label.as_mut(), false);

        if stmt.label.is_none() && !self.is_in_breakable() {
            self.emit_error(stmt.loc, ParseError::UnlabeledBreakOutsideBreakable);
        }
    }

    fn visit_continue_statement(&mut self, stmt: &mut ContinueStatement) {
        self.visit_label_use(stmt.label.as_mut(), true);

        if !self.is_in_iterable() {
            self.emit_error(stmt.loc, ParseError::ContinueOutsideIterable);
        }
    }

    fn visit_with_statement(&mut self, stmt: &mut WithStatement) {
        if self.is_in_strict_mode_context() {
            self.emit_error(stmt.loc, ParseError::WithInStrictMode);
        }

        self.check_for_labeled_function(&stmt.body);

        self.visit_outer_expression(&mut stmt.object);

        // Must conservatively use VM scope locations for all visible bindings so that they can be
        // dynamcally looked up from within the with statement.
        self.scope_tree
            .support_dynamic_access_in_visible_bindings(self.current_scope_id());

        // With statement bodies are in their own scope
        self.enter_scope(stmt.scope);
        self.visit_statement(&mut stmt.body);
        self.exit_scope();
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStatement) {
        self.check_for_labeled_function(&stmt.conseq);
        if let Some(altern) = stmt.altern.as_ref() {
            self.check_for_labeled_function(altern);
        }

        default_visit_if_statement(self, stmt);
    }

    fn visit_while_statement(&mut self, stmt: &mut WhileStatement) {
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_while_statement(self, stmt);

        self.dec_iterable_depth();
    }

    fn visit_do_while_statement(&mut self, stmt: &mut DoWhileStatement) {
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_do_while_statement(self, stmt);

        self.dec_iterable_depth();
    }

    fn visit_for_statement(&mut self, stmt: &mut ForStatement) {
        self.inc_iterable_depth();
        self.enter_scope(stmt.scope);

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_statement(self, stmt);

        self.exit_scope();
        self.dec_iterable_depth();
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement) {
        self.inc_iterable_depth();
        self.enter_scope(stmt.scope);

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_each_statement(self, stmt);

        self.exit_scope();
        self.dec_iterable_depth();
    }

    fn visit_for_each_init(&mut self, init: &mut ForEachInit) {
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

    fn visit_catch_clause(&mut self, catch: &mut CatchClause) {
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

    fn visit_block(&mut self, block: &mut Block) {
        self.enter_scope(block.scope);
        default_visit_block(self, block);
        self.exit_scope();
    }

    fn visit_member_expression(&mut self, expr: &mut MemberExpression) {
        if expr.is_private {
            self.visit_private_name_use(&expr.property);
        }

        self.visit_expression(&mut expr.object);

        // Visit computed property expressions, but not id used as named property
        if expr.is_computed {
            self.visit_expression(&mut expr.property);
        }
    }

    fn visit_binary_expression(&mut self, expr: &mut BinaryExpression) {
        if expr.operator == BinaryOperator::InPrivate {
            self.visit_private_name_use(&expr.left);
        }

        default_visit_binary_expression(self, expr);
    }

    fn visit_assignment_expression(&mut self, expr: &mut AssignmentExpression) {
        // Mark the current context as having an assignment expression
        self.has_assign_expr = true;

        default_visit_assignment_expression(self, expr);
    }

    fn visit_object_expression(&mut self, expr: &mut ObjectExpression) {
        // Do not allow duplicate __proto__ initializer properties
        let mut has_proto_init = false;
        for property in &expr.properties {
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

        default_visit_object_expression(self, expr);
    }

    fn visit_property(&mut self, prop: &mut Property) {
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
                /*is_derived_constructor */ false, /* is_static_initializer */ false,
            );
        } else {
            visit_opt!(self, prop.value, visit_expression);
        }
    }

    fn visit_object_pattern_property(&mut self, prop: &mut ObjectPatternProperty) {
        // Visit pattern for computed properties, but not id of named property
        if prop.is_computed {
            visit_opt!(self, prop.key, visit_expression);
        }

        self.visit_pattern(&mut prop.value);
    }

    fn visit_meta_property(&mut self, expr: &mut MetaProperty) {
        match &mut expr.kind {
            // new.target is treated as a binding that is implicitly created on a parent function
            // scope. Store the scope node of the function that contains this new.target expression.
            MetaPropertyKind::NewTarget { scope } => {
                if self.is_in_non_arrow_function() {
                    let current_scope = self.scope_stack.last().unwrap().as_ref().id();
                    let (def_scope, _) = self.scope_tree.resolve_use(
                        current_scope,
                        NEW_TARGET_BINDING_NAME,
                        expr.loc,
                    );
                    *scope = def_scope;
                } else {
                    self.emit_error(expr.loc, ParseError::NewTargetOutsideFunction);
                }
            }
            MetaPropertyKind::ImportMeta => {}
        }
    }

    fn visit_super_member_expression(&mut self, expr: &mut SuperMemberExpression) {
        match self.allow_super_member_stack.last() {
            Some(true) => {}
            _ => self.emit_error(expr.loc, ParseError::SuperPropertyOutsideMethod),
        }

        default_visit_super_member_expression(self, expr)
    }

    fn visit_super_call_expression(&mut self, expr: &mut SuperCallExpression) {
        match self.enclosing_non_arrow_function() {
            Some(FunctionStackEntry { is_derived_constructor: true, .. }) => {}
            _ => self.emit_error(expr.loc, ParseError::SuperCallOutsideDerivedConstructor),
        }

        default_visit_super_call_expression(self, expr)
    }

    fn visit_unary_expression(&mut self, expr: &mut UnaryExpression) {
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

    fn visit_this_expression(&mut self, this: &mut ThisExpression) {
        self.resolve_this_use(this);
    }

    fn visit_call_expression(&mut self, expr: &mut CallExpression) {
        // If a potential direct eval is ever seen, conservatively force all visible bindings to
        // have VM scope locations instead of local registers so they can be dynamically looked up.
        match expr.callee.as_ref() {
            Expression::Id(Identifier { name, .. }) if name == "eval" && !expr.is_optional => {
                self.scope_tree
                    .support_dynamic_access_in_visible_bindings(self.current_scope_id());
            }
            _ => {}
        }

        default_visit_call_expression(self, expr);
    }

    fn visit_identifier_expression(&mut self, id: &mut Identifier) {
        if id.name == "arguments" && !self.allow_arguments {
            self.emit_error(id.loc, ParseError::ArgumentsInClassInitializer);
        }

        self.resolve_identifier_use(id);

        default_visit_identifier_expression(self, id)
    }

    fn visit_identifier_pattern(&mut self, id: &mut Identifier) {
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

    fn visit_variable_declarator(&mut self, decl: &mut VariableDeclarator) {
        // Variable declarator pattern is in its own "has assignment expression" context
        self.enter_has_assign_expr_context();
        self.visit_pattern(&mut decl.id);
        decl.id_has_assign_expr = self.exit_has_assign_expr_context();

        visit_opt!(self, decl.init, visit_outer_expression);
    }

    fn visit_class(&mut self, class: &mut Class) {
        // Entire class is in strict mode, including super class expression
        self.enter_strict_mode_context();

        if let Some(class_id) = class.id.as_deref() {
            self.error_if_strict_eval_or_arguments(class_id);
        }

        if let Some(super_class) = class.super_class.as_deref_mut() {
            self.visit_outer_expression(super_class);
        }

        // Save analyzer context before descending into class body
        let saved_state = self.save_state();

        let private_names = self.collect_class_private_names(class);

        self.class_stack
            .push(ClassStackEntry { private_names, is_derived: class.super_class.is_some() });
        self.allow_super_member_stack.push(true);

        // Mark the constructor if it is found, erroring if multiple are found
        let mut constructor = None;

        // Body is in its own scope
        self.enter_scope(class.scope);

        // Determine the number of computed field names in the class, as each will need a scope
        // slot to be stored between name evaluation and field definition.
        let mut num_computed_field_names = 0;

        for element in &mut class.body {
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
                        || property.is_private
                        || matches!(
                            &property.key.expr,
                            Expression::Number(_) | Expression::BigInt(_)
                        );

                    if is_computed {
                        num_computed_field_names += 1;
                    }

                    self.visit_class_property(property);
                }
            }
        }

        self.exit_scope_with_class_fields(num_computed_field_names);

        class.constructor = constructor;

        self.allow_super_member_stack.pop();
        self.class_stack.pop();

        // Restore analyzer context after visiting class
        self.restore_state(saved_state);

        self.exit_strict_mode_context();
    }
}

impl Analyzer<'_> {
    fn visit_function_common(
        &mut self,
        func: &mut Function,
        is_arrow_function: bool,
        is_method: bool,
        is_derived_constructor: bool,
        is_static_initializer: bool,
    ) {
        self.function_stack.push(FunctionStackEntry {
            is_arrow_function,
            _is_method: is_method,
            is_derived_constructor,
        });

        // Return is not allowed in static initializers, but is allowed in all other functions
        self.allow_return_stack.push(!is_static_initializer);

        // Super member expressions are allowed in methods, and in arrow functions are inherited
        // from surrounding context.
        if !is_arrow_function {
            self.allow_super_member_stack.push(is_method);
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

        // Visit and analyze function parameters
        for param in &mut func.params {
            self.visit_function_param(param);
        }

        let mut param_index = 0;
        for param in &mut func.params {
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
                let binding = scope.get_binding_mut(&id.name);

                // Allow later arguments to overwrite earlier ones but do not overwrite a WithVar.
                if !binding.needs_tdz_check()
                    && !matches!(binding.vm_location(), Some(VMLocation::WithVar))
                {
                    binding.set_vm_location(VMLocation::Argument(param_index));
                }
            }

            param_index += 1;
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
            let arguments_binding_opt = func.scope.as_ref().get_binding_opt("arguments");
            let is_arguments_object_needed = arguments_binding_opt
                .map(|binding| binding.kind().is_valid_arguments_kind())
                .unwrap_or(false);
            func.set_is_arguments_object_needed(is_arguments_object_needed);

            if func.scope.as_ref().has_binding(NEW_TARGET_BINDING_NAME) {
                func.set_is_new_target_object_needed(true);
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

        self.allow_return_stack.pop();
        self.function_stack.pop();

        // Restore analyzer context after visiting function
        self.restore_state(saved_state);
    }

    fn collect_class_private_names(&mut self, class: &Class) -> HashMap<String, PrivateNameUsage> {
        // Create new private name scope for stack and initialize with defined private names
        let mut private_names = HashMap::new();
        for element in &class.body {
            match element {
                ClassElement::Property(ClassProperty { is_private: true, key, .. }) => {
                    let private_id = key.expr.to_id();

                    // If this name has been used at all so far it is a duplicate name
                    if private_names.contains_key(&private_id.name) {
                        self.emit_error(
                            private_id.loc,
                            ParseError::DuplicatePrivateName(private_id.name.clone()),
                        );
                    } else {
                        // Create a complete usage that does not allow any other uses of this name
                        let usage = PrivateNameUsage {
                            is_static: false,
                            has_getter: true,
                            has_setter: true,
                        };
                        private_names.insert(private_id.name.clone(), usage);
                    }

                    if private_id.name == "constructor" {
                        self.emit_error(private_id.loc, ParseError::PrivateNameConstructor);
                    }
                }

                ClassElement::Method(ClassMethod {
                    is_private: true,
                    key,
                    is_static,
                    kind,
                    ..
                }) => {
                    let private_id = key.expr.to_id();

                    // Check for duplicate name definitions. Only allow multiple definitions if
                    // there is exactly one getter and setter that have the same static property.
                    match private_names.get_mut(&private_id.name) {
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

                            private_names.insert(private_id.name.clone(), usage);
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

                            if is_duplicate {
                                self.emit_error(
                                    private_id.loc,
                                    ParseError::DuplicatePrivateName(private_id.name.clone()),
                                );
                            }
                        }
                    }

                    if private_id.name == "constructor" {
                        self.emit_error(private_id.loc, ParseError::PrivateNameConstructor);
                    }
                }
                _ => {}
            }
        }

        private_names
    }

    fn visit_class_method(&mut self, method: &mut ClassMethod) {
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
                key_name_bytes.map_or(false, |name| name == "constructor".as_bytes())
            }
            _ => false,
        };

        if is_bad_constructor {
            self.emit_error(method.loc, ParseError::NonSimpleConstructor);
        }

        if method.is_static
            && !method.is_private
            && key_name_bytes.map_or(false, |name| name == "prototype".as_bytes())
        {
            self.emit_error(method.loc, ParseError::ClassStaticPrototype);
        }

        self.visit_outer_expression(&mut method.key);

        let is_derived_constructor = method.kind == ClassMethodKind::Constructor
            && self.class_stack.last().unwrap().is_derived;

        self.visit_function_common(
            &mut method.value,
            /* is_arrow_function */ false,
            /* is_method */ true,
            is_derived_constructor,
            method.kind == ClassMethodKind::StaticInitializer,
        );
    }

    fn visit_class_property(&mut self, prop: &mut ClassProperty) {
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

        self.visit_outer_expression(&mut prop.key);

        // "arguments" is not allowed in initializer (but is allowed in key)
        let old_allow_arguments = self.allow_arguments;
        self.allow_arguments = false;

        visit_opt!(self, prop.value, visit_outer_expression);

        self.allow_arguments = old_allow_arguments;
    }

    fn visit_variable_declaration_common(
        &mut self,
        var_decl: &mut VariableDeclaration,
        is_for_each_init: bool,
    ) {
        for declaration in &var_decl.declarations {
            if var_decl.kind == VarKind::Const && declaration.init.is_none() && !is_for_each_init {
                self.emit_error(declaration.loc, ParseError::ConstWithoutInitializer);
            }

            // Check for invalid names depending on context
            must!(declaration.iter_bound_names(&mut |id| {
                if var_decl.kind != VarKind::Var && id.name == "let" {
                    self.emit_error(id.loc, ParseError::LetNameInLexicalDeclaration);
                }

                ().into()
            }));
        }

        default_visit_variable_declaration(self, var_decl);
    }

    // Visit all nested labeled statements, adding labels to scope and returning the inner
    // non-labeled statement. Labels that are directly nested within each other all have the same
    // label id.
    fn push_all_labels<'a>(
        &mut self,
        stmt: &'a mut LabeledStatement,
    ) -> (&'a mut Statement, Vec<(String, bool)>) {
        // Always use 1 more than the current lable depth, so that a label id of 0 marks the
        // empty label.
        let label_id = self.label_depth + 1;
        self.label_depth += 1;

        let mut labels = vec![];
        // Keep track of duplicate labels so that we don't pop the duplicate labels at the end
        let is_label_duplicate = self.visit_label_def(stmt, label_id);
        labels.push((stmt.label.name.clone(), is_label_duplicate));

        let mut inner_stmt = stmt.body.as_mut();
        while let Statement::Labeled(stmt) = inner_stmt {
            let is_label_duplicate = self.visit_label_def(stmt, label_id);
            labels.push((stmt.label.name.clone(), is_label_duplicate));

            inner_stmt = stmt.body.as_mut()
        }

        // Only some statements can be a continue target
        let is_continue_target = match inner_stmt {
            Statement::While(_)
            | Statement::DoWhile(_)
            | Statement::For(_)
            | Statement::ForEach(_)
            | Statement::Switch(_) => true,
            _ => false,
        };

        if is_continue_target {
            for (label, _) in &labels {
                self.labels.get_mut(label).unwrap().is_continue_target = true;
            }
        }

        (inner_stmt, labels)
    }

    fn pop_all_labels(&mut self, label_stack: Vec<(String, bool)>) {
        // Exit all label scopes in reverse order that they were entered
        for (label_name, is_duplicate) in label_stack.iter().rev() {
            if !is_duplicate {
                self.labels.remove(label_name);
            }
        }

        self.label_depth -= 1;
    }

    fn visit_label_def(&mut self, stmt: &mut LabeledStatement, label_id: LabelId) -> bool {
        let label_name = &stmt.label.name;
        let is_duplicate = self.labels.contains_key(label_name);

        if is_duplicate {
            self.emit_error(stmt.label.loc, ParseError::DuplicateLabel);
        } else {
            self.labels
                .insert(label_name.clone(), LabelInfo { label_id, is_continue_target: false });
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

    fn visit_label_use(&mut self, label: Option<&mut Label>, is_continue: bool) {
        if let Some(label) = label {
            match self.labels.get(&label.name) {
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

    fn visit_private_name_use(&mut self, expr: &Expression) {
        let id = expr.to_id();

        if self.class_stack.is_empty() {
            self.emit_error(id.loc, ParseError::PrivateNameOutsideClass);
        } else {
            // Check if private name is defined in this class or a parent class in its scope
            let mut is_defined = false;
            for class_entry in self.class_stack.iter().rev() {
                if class_entry.private_names.contains_key(&id.name) {
                    is_defined = true;
                    break;
                }
            }

            if !is_defined {
                self.emit_error(id.loc, ParseError::PrivateNameNotDefined(id.name.clone()));
            }
        }
    }

    fn resolve_identifier_use(&mut self, id: &mut Identifier) {
        let current_scope = self.scope_stack.last().unwrap().as_ref().id();
        let (def_scope, _) = self.scope_tree.resolve_use(current_scope, &id.name, id.loc);
        id.scope = def_scope;
    }

    fn resolve_this_use(&mut self, this: &mut ThisExpression) {
        let current_scope = self.scope_stack.last().unwrap().as_ref().id();
        let (def_scope, is_capture) = self.scope_tree.resolve_use(current_scope, "this", this.loc);

        // Only set scope is this is a capture of a `this` binding
        if is_capture {
            this.scope = Some(AstPtr::from_ref(def_scope.unwrap_resolved()));
        }
    }
}

pub fn analyze(
    parse_result: &mut ParseProgramResult,
    source: Rc<Source>,
) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source, &mut parse_result.scope_tree);
    analyzer.visit_program(&mut parse_result.program);

    if analyzer.errors.is_empty() {
        analyzer.finish();
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_for_eval(
    parse_result: &mut ParseProgramResult,
    source: Rc<Source>,
    private_names: Option<HashMap<String, PrivateNameUsage>>,
    in_function: bool,
    in_method: bool,
    in_derived_constructor: bool,
    in_class_field_initializer: bool,
) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source, &mut parse_result.scope_tree);

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
            _is_method: in_method,
            is_derived_constructor: in_derived_constructor,
        });

        if in_method {
            analyzer.allow_super_member_stack.push(true);
        }
    }

    // If in class field initializer then "arguments" is not allowed
    if in_class_field_initializer {
        analyzer.allow_arguments = false;
    }

    analyzer.visit_program(&mut parse_result.program);

    if analyzer.errors.is_empty() {
        analyzer.finish();
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_function_for_function_constructor(
    parse_result: &mut ParseFunctionResult,
    source: Rc<Source>,
) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source, &mut parse_result.scope_tree);
    analyzer.visit_function_expression(&mut parse_result.function);

    if analyzer.errors.is_empty() {
        analyzer.finish();
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}
