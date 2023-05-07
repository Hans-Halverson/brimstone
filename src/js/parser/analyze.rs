use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{js::parser::parse_error::InvalidDuplicateParametersReason, visit_opt, visit_vec};

use super::{
    ast::*,
    ast_visitor::*,
    loc::Loc,
    scope::{NameKind, ScopeBuilder},
    source::Source,
    LocalizedParseError, LocalizedParseErrors, ParseError,
};

pub struct Analyzer {
    // Current source that is being analyzed
    source: Rc<Source>,
    // Accumulator or errors reported during analysis
    errors: Vec<LocalizedParseError>,
    scope_builder: ScopeBuilder,
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
    // Whether the "arguments" identifier is currently disallowed due to being in a class initializer
    allow_arguments: bool,
    // Whether a return statement is allowed
    allow_return_stack: Vec<bool>,
    // Whether a super member expression is allowed
    allow_super_member_stack: Vec<bool>,
}

struct FunctionStackEntry {
    // Function is optional in case this is a synthetic function entry, e.g. representing analysis
    // during eval code.
    func: Option<AstPtr<Function>>,
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

impl<'a> Analyzer {
    pub fn new(source: Rc<Source>) -> Analyzer {
        Analyzer {
            source,
            errors: Vec::new(),
            scope_builder: ScopeBuilder::new(),
            strict_mode_context_depth: 0,
            labels: HashMap::new(),
            label_depth: 0,
            breakable_depth: 0,
            iterable_depth: 0,
            function_stack: vec![],
            class_stack: vec![],
            allow_arguments: true,
            allow_return_stack: vec![false],
            allow_super_member_stack: vec![false],
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
    }

    fn add_var_declared_id(&mut self, id: &Identifier, kind: NameKind) {
        let error_opt = self
            .scope_builder
            .add_var_declared_name(&id.name, &id.loc, kind);

        if let Some(error) = error_opt {
            self.emit_error(id.loc, error);
        }
    }

    fn add_lex_declared_id(&mut self, id: &Identifier, kind: NameKind) {
        let error_opt = self
            .scope_builder
            .add_lex_declared_name(&id.name, &id.loc, kind);

        if let Some(error) = error_opt {
            self.emit_error(id.loc, error);
        }
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
}

impl<'a> AstVisitor for Analyzer {
    fn visit_program(&mut self, program: &mut Program) {
        if program.is_strict_mode {
            self.enter_strict_mode_context();
        }

        self.scope_builder.enter_toplevel_scope(program);

        for toplevel in &mut program.toplevels {
            match toplevel {
                Toplevel::Statement(stmt) => self.visit_top_level_declaration_statement(stmt),
                Toplevel::Import(import) => self.visit_import_declaration(import),
                Toplevel::ExportDefault(export) => self.visit_export_default_declaration(export),
                Toplevel::ExportNamed(export) => self.visit_export_named_declaration(export),
                Toplevel::ExportAll(export) => self.visit_export_all_declaration(export),
            }
        }

        self.scope_builder.exit_scope();

        if program.is_strict_mode {
            self.exit_strict_mode_context();
        }
    }

    fn visit_function_declaration(&mut self, func: &mut Function) {
        self.visit_function_declaration_common(func, true)
    }

    fn visit_class_declaration(&mut self, class: &mut Class) {
        self.scope_builder.add_class_decl(class);

        if let Some(class_id) = class.id.as_deref() {
            self.add_lex_declared_id(class_id, NameKind::Class);
        }

        self.visit_class_common(class)
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.scope_builder.enter_block_scope(block);
        default_visit_block(self, block);
        self.scope_builder.exit_scope();
    }

    fn visit_switch_statement(&mut self, stmt: &mut SwitchStatement) {
        self.scope_builder.enter_switch_scope(stmt);
        self.inc_breakable_depth();

        let mut seen_default = false;

        self.visit_expression(&mut stmt.discriminant);

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

        self.dec_breakable_depth();
        self.scope_builder.exit_scope();
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
        // Arrow functions do not provide an arguments object
        func.is_arguments_object_needed = false;

        self.visit_function_common(
            func, /* is_arrow_function */ true, /* is_method */ false,
            /*is_derived_constructor */ false, /* is_static_initializer */ false,
        );
    }

    fn visit_class_expression(&mut self, class: &mut Class) {
        self.visit_class_common(class)
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

        default_visit_with_statement(self, stmt);
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
        self.scope_builder.enter_for_scope();
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_statement(self, stmt);

        self.dec_iterable_depth();
        self.scope_builder.exit_scope();
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement) {
        self.scope_builder.enter_for_scope();
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_each_statement(self, stmt);

        self.dec_iterable_depth();
        self.scope_builder.exit_scope();
    }

    fn visit_for_each_init(&mut self, init: &mut ForEachInit) {
        match init {
            ForEachInit::Pattern(patt) => self.visit_pattern(patt),
            ForEachInit::VarDecl(decl) => self.visit_variable_declaration_common(decl, true),
        }
    }

    fn visit_catch_clause(&mut self, catch: &mut CatchClause) {
        self.scope_builder.enter_block_scope(&catch.body);

        if let Some(param) = catch.param.as_deref_mut() {
            self.visit_pattern(param);

            // Parameter bindings are treated as lexical declarations scoped to the catch body
            param.iter_bound_names(&mut |id| {
                self.add_lex_declared_id(id, NameKind::CatchParameter);
                ().into()
            });
        }

        default_visit_block(self, &mut catch.body);

        self.scope_builder.exit_scope();
    }

    fn visit_member_expression(&mut self, expr: &mut MemberExpression) {
        if expr.is_private {
            self.visit_private_name_use(&expr.property);
        }

        default_visit_member_expression(self, expr);
    }

    fn visit_binary_expression(&mut self, expr: &mut BinaryExpression) {
        if expr.operator == BinaryOperator::InPrivate {
            self.visit_private_name_use(&expr.left);
        }

        default_visit_binary_expression(self, expr);
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

        self.visit_expression(&mut prop.key);

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

    fn visit_identifier(&mut self, id: &mut Identifier) {
        // Current function conservatively needs arguments object if its body contains the
        // identifiers "arguments" or "eval".
        match id.name.as_str() {
            "arguments" | "eval" => {
                if let Some(FunctionStackEntry { func: Some(func), .. }) =
                    self.enclosing_non_arrow_function()
                {
                    func.as_mut().is_arguments_object_needed = true;
                }
            }
            _ => {}
        }
    }

    fn visit_meta_property(&mut self, expr: &mut MetaProperty) {
        match expr.kind {
            MetaPropertyKind::NewTarget => {
                if !self.is_in_non_arrow_function() {
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

    fn visit_identifier_expression(&mut self, id: &mut Identifier) {
        if id.name == "arguments" && !self.allow_arguments {
            self.emit_error(id.loc, ParseError::ArgumentsInClassInitializer);
        }

        default_visit_identifier_expression(self, id)
    }

    fn visit_identifier_pattern(&mut self, id: &mut Identifier) {
        if id.name == "arguments" && !self.allow_arguments {
            self.emit_error(id.loc, ParseError::ArgumentsInClassInitializer);
        } else {
            self.error_if_strict_eval_or_arguments(id);
        }

        default_visit_identifier_pattern(self, id)
    }
}

impl Analyzer {
    fn visit_top_level_declaration_statement(&mut self, stmt: &mut Statement) {
        match stmt {
            // Toplevel function declarations are treated as var scoped decls
            Statement::FuncDecl(func_decl) => {
                self.visit_function_declaration_common(func_decl, false)
            }
            // Find statement under labels, if it is a function it is a var scoped decl
            Statement::Labeled(ref mut labeled_stmt) => {
                let (inner_stmt, label_stack) = self.push_all_labels(labeled_stmt);

                if let Statement::FuncDecl(func_decl) = inner_stmt {
                    self.visit_function_declaration_common(func_decl, false);
                } else {
                    self.visit_statement(inner_stmt);
                }

                self.pop_all_labels(label_stack);
            }
            _ => self.visit_statement(stmt),
        }
    }

    fn visit_function_declaration_common(&mut self, func: &mut Function, is_lex_scoped_decl: bool) {
        self.scope_builder.add_func_decl(func, is_lex_scoped_decl);

        if let Some(func_id) = func.id.as_deref() {
            if is_lex_scoped_decl {
                self.add_lex_declared_id(func_id, NameKind::Function);
            } else {
                self.add_var_declared_id(func_id, NameKind::Function);
            }
        }

        self.visit_function_common(
            func, /* is_arrow_function */ false, /* is_method */ false,
            /*is_derived_constructor */ false, /* is_static_initializer */ false,
        );
    }

    fn visit_function_common(
        &mut self,
        func: &mut Function,
        is_arrow_function: bool,
        is_method: bool,
        is_derived_constructor: bool,
        is_static_initializer: bool,
    ) {
        self.function_stack.push(FunctionStackEntry {
            func: Some(AstPtr::from_ref(func)),
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
        if func.is_strict_mode {
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

        // Enter function scope for params and body
        self.scope_builder.enter_function_scope(func);

        // Visit and analyze function parameters
        visit_vec!(self, func.params, visit_function_param);

        // Static analysis of parameters and other function properties once body has been visited
        let mut has_parameter_expressions = false;
        let mut has_binding_patterns = false;
        let mut has_rest_parameter = false;
        let mut has_duplicate_parameters = false;
        let mut has_argument_parameter = false;

        let mut parameter_names = HashSet::new();

        for param in &func.params {
            if let FunctionParam::Rest(_) = param {
                has_rest_parameter = true;
            }

            param.iter_patterns(&mut |patt| match patt {
                Pattern::Id(id) => {
                    if parameter_names.contains(&id.name) {
                        has_duplicate_parameters = true;
                    } else {
                        parameter_names.insert(&id.name);
                    }

                    // Function parameters are treated as variable declarations scoped to func body
                    self.add_var_declared_id(id, NameKind::FunctionParameter);

                    // Arguments object is not needed if "arguments" is a bound name in the
                    // function parameters.
                    if id.name == "arguments" {
                        has_argument_parameter = true;
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
                Pattern::Reference(_) => {}
            });
        }

        func.has_parameter_expressions = has_parameter_expressions;
        func.has_simple_parameter_list =
            !has_binding_patterns && !has_parameter_expressions && !has_rest_parameter;
        func.has_duplicate_parameters = has_duplicate_parameters;

        // Functions with an explicit "use strict" in their body must have a simple parameter list
        if func.has_use_strict_directive && !func.has_simple_parameter_list {
            self.emit_error(func.loc, ParseError::UseStrictFunctionNonSimpleParameterList);
        }

        // Duplicate parameters are not allowed in certain contexts
        if has_duplicate_parameters {
            let invalid_reason = if self.is_in_strict_mode_context() {
                Some(InvalidDuplicateParametersReason::StrictMode)
            } else if is_arrow_function {
                Some(InvalidDuplicateParametersReason::ArrowFunction)
            } else if is_method {
                Some(InvalidDuplicateParametersReason::Method)
            } else if !func.has_simple_parameter_list {
                Some(InvalidDuplicateParametersReason::NonSimpleParameters)
            } else {
                None
            };

            if let Some(invalid_reason) = invalid_reason {
                self.emit_error(func.loc, ParseError::InvalidDuplicateParameters(invalid_reason));
            }
        }

        // Visit function body
        match *func.body {
            FunctionBody::Block(ref mut block) => {
                for stmt in &mut block.body {
                    self.visit_top_level_declaration_statement(stmt)
                }
            }
            FunctionBody::Expression(ref mut expr) => self.visit_expression(expr),
        }

        // Arguments object may have been set to needed based on analysis of function body
        let mut is_arguments_object_needed =
            func.is_arguments_object_needed && !has_argument_parameter;

        // Arguments object is not needed if "arguments" appears in the lexically declared names, or
        // as a function var declared name.
        if is_arguments_object_needed && !func.has_parameter_expressions {
            for var_decl in func.var_decls() {
                match var_decl {
                    VarDecl::Func(_) => {
                        var_decl.iter_bound_names(&mut |id| {
                            if id.name == "arguments" {
                                is_arguments_object_needed = false;
                            }

                            ().into()
                        });
                    }
                    _ => {}
                }
            }

            for lex_decl in func.lex_decls() {
                lex_decl.iter_bound_names(&mut |id| {
                    if id.name == "arguments" {
                        is_arguments_object_needed = false;
                    }

                    ().into()
                });
            }
        }

        func.is_arguments_object_needed = is_arguments_object_needed;

        self.scope_builder.exit_scope();

        if func.is_strict_mode {
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

    fn visit_class_common(&mut self, class: &mut Class) {
        // Save analyzer context before descending into class
        let saved_state = self.save_state();

        // Entire class is in strict mode
        self.enter_strict_mode_context();

        if let Some(class_id) = class.id.as_deref() {
            self.error_if_strict_eval_or_arguments(class_id);
        }

        if let Some(super_class) = class.super_class.as_deref_mut() {
            self.visit_expression(super_class);
        }

        // Create new private name scope for stack and initialize with defined private names
        let mut private_names = HashMap::new();
        for element in &class.body {
            match element {
                ClassElement::Property(ClassProperty { is_private: true, key, .. }) => {
                    let private_id = key.to_id();

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
                    let private_id = key.to_id();

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

        self.class_stack
            .push(ClassStackEntry { private_names, is_derived: class.super_class.is_some() });
        self.allow_super_member_stack.push(true);

        // Mark the constructor if it is found, erroring if multiple are found
        let mut constructor = None;

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
                ClassElement::Property(prop) => {
                    self.visit_class_property(prop);
                }
            }
        }

        class.constructor = constructor;

        self.allow_super_member_stack.pop();
        self.class_stack.pop();

        self.exit_strict_mode_context();

        // Restore analyzer context after visiting class
        self.restore_state(saved_state);
    }

    fn visit_class_method(&mut self, method: &mut ClassMethod) {
        let key_name = if method.is_computed {
            None
        } else {
            match method.key.as_ref() {
                Expression::String(name) => Some(&name.value),
                Expression::Id(id) => Some(&id.name),
                _ => None,
            }
        };

        // Constructors may be simple methods, without being async, generator, getter, or setter.
        // Static constructors are allowed however.
        let is_bad_constructor = match method.kind {
            ClassMethodKind::Constructor => method.value.is_async || method.value.is_generator,
            ClassMethodKind::Get | ClassMethodKind::Set if !method.is_static => {
                key_name.map_or(false, |name| name == "constructor")
            }
            _ => false,
        };

        if is_bad_constructor {
            self.emit_error(method.loc, ParseError::NonSimpleConstructor);
        }

        if method.is_static
            && !method.is_private
            && key_name.map_or(false, |name| name == "prototype")
        {
            self.emit_error(method.loc, ParseError::ClassStaticPrototype);
        }

        self.visit_expression(&mut method.key);

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
        let key_name = if prop.is_computed {
            None
        } else {
            match prop.key.as_ref() {
                Expression::String(name) => Some(name.value.as_str()),
                Expression::Id(id) => Some(id.name.as_str()),
                _ => None,
            }
        };

        match key_name {
            Some("constructor") => self.emit_error(prop.loc, ParseError::NonSimpleConstructor),
            Some("prototype") if prop.is_static => {
                self.emit_error(prop.loc, ParseError::ClassStaticPrototype)
            }
            _ => {}
        }

        self.visit_expression(&mut prop.key);

        // "arguments" is not allowed in initializer (but is allowed in key)
        let old_allow_arguments = self.allow_arguments;
        self.allow_arguments = false;

        visit_opt!(self, prop.value, visit_expression);

        self.allow_arguments = old_allow_arguments;
    }

    fn visit_variable_declaration_common(
        &mut self,
        var_decl: &mut VariableDeclaration,
        is_for_each_init: bool,
    ) {
        self.scope_builder.add_var_decl(var_decl);

        for declaration in &var_decl.declarations {
            if var_decl.kind == VarKind::Const && declaration.init.is_none() && !is_for_each_init {
                self.emit_error(declaration.loc, ParseError::ConstWithoutInitializer);
            }

            // Check for invalid names depending on context
            declaration.iter_bound_names(&mut |id| {
                if var_decl.kind != VarKind::Var && id.name == "let" {
                    self.emit_error(id.loc, ParseError::LetNameInLexicalDeclaration);
                }

                ().into()
            });

            // Add names to scope, checking for redeclarations
            declaration.iter_bound_names(&mut |id| {
                match var_decl.kind {
                    VarKind::Var => self.add_var_declared_id(id, NameKind::Var),
                    VarKind::Const => self.add_lex_declared_id(id, NameKind::Const),
                    VarKind::Let => self.add_lex_declared_id(id, NameKind::Let),
                }

                ().into()
            });
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
        labels.push((stmt.label.label.name.clone(), is_label_duplicate));

        let mut inner_stmt = stmt.body.as_mut();
        while let Statement::Labeled(stmt) = inner_stmt {
            let is_label_duplicate = self.visit_label_def(stmt, label_id);
            labels.push((stmt.label.label.name.clone(), is_label_duplicate));

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
        let label_name = &stmt.label.label.name;
        let is_duplicate = self.labels.contains_key(label_name);

        if is_duplicate {
            self.emit_error(stmt.label.label.loc, ParseError::DuplicateLabel);
        } else {
            self.labels
                .insert(label_name.clone(), LabelInfo { label_id, is_continue_target: false });
        }

        stmt.label.id = label_id;

        // Annex B: Always error on labeled function declarations in strict mode
        if self.is_in_strict_mode_context() {
            if let Statement::FuncDecl(_) = stmt.body.as_ref() {
                self.emit_error(stmt.label.label.loc, ParseError::InvalidLabeledFunction(true));
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
                    self.emit_error(
                        labeled.label.label.loc,
                        ParseError::InvalidLabeledFunction(false),
                    )
                }
            }
        }
    }

    fn visit_label_use(&mut self, label: Option<&mut Label>, is_continue: bool) {
        if let Some(label) = label {
            match self.labels.get(&label.label.name) {
                None => self.emit_error(label.label.loc, ParseError::LabelNotFound),
                Some(label_info) if is_continue && !label_info.is_continue_target => {
                    self.emit_error(label.label.loc, ParseError::LabelNotFound)
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
}

pub fn analyze(program: &mut Program, source: Rc<Source>) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source);
    analyzer.visit_program(program);

    if analyzer.errors.is_empty() {
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_for_eval(
    program: &mut Program,
    source: Rc<Source>,
    private_names: Option<HashMap<String, PrivateNameUsage>>,
    in_function: bool,
    in_method: bool,
    in_derived_constructor: bool,
    in_class_field_initializer: bool,
) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source);

    // Initialize private names from surrounding context if supplied
    if let Some(private_names) = private_names {
        analyzer
            .class_stack
            .push(ClassStackEntry { private_names, is_derived: false });
    }

    // If in function add a synthetic function entry
    if in_function {
        analyzer.function_stack.push(FunctionStackEntry {
            func: None,
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

    analyzer.visit_program(program);

    if analyzer.errors.is_empty() {
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}

pub fn analyze_function_for_function_constructor(
    func: &mut Function,
    source: Rc<Source>,
) -> Result<(), LocalizedParseErrors> {
    let mut analyzer = Analyzer::new(source);
    analyzer.visit_function_expression(func);

    if analyzer.errors.is_empty() {
        Ok(())
    } else {
        Err(LocalizedParseErrors::new(analyzer.errors))
    }
}
