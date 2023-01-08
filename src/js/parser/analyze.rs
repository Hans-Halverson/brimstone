use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{visit_opt, visit_vec};

use super::{
    ast::*, ast_visitor::*, loc::Loc, parser::LocalizedParseError, scope::ScopeBuilder,
    source::Source, LocalizedParseErrors, ParseError,
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
    labels: HashMap<String, LabelId>,
    // Number of labeled statements that the visitor is currently inside. Multiple labels on the
    // same statement all count as a single "label depth", and will receive the same label id.
    label_depth: LabelId,
    // Number of nested breakable statements the visitor is currently inside
    breakable_depth: usize,
    // Number of nested iterable statements the visitor is currently inside
    iterable_depth: usize,
    // Number of nested functions the visitor is currently inside
    function_depth: usize,
}

// Saved state from entering a function that can be restored from
struct FunctionSavedState {
    labels: HashMap<String, LabelId>,
    label_depth: LabelId,
    breakable_depth: usize,
    iterable_depth: usize,
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
            function_depth: 0,
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

    fn is_in_function(&self) -> bool {
        self.function_depth > 0
    }

    // Save state before visiting a function. This prevents labels and some context from leaking
    // into the inner function.
    fn save_function_state(&mut self) -> FunctionSavedState {
        let mut state = FunctionSavedState {
            labels: HashMap::new(),
            label_depth: self.label_depth,
            breakable_depth: self.breakable_depth,
            iterable_depth: self.iterable_depth,
        };

        std::mem::swap(&mut self.labels, &mut state.labels);
        self.label_depth = 0;
        self.breakable_depth = 0;
        self.iterable_depth = 0;

        state
    }

    // Restore state after visiting a function
    fn restore_function_state(&mut self, mut state: FunctionSavedState) {
        std::mem::swap(&mut self.labels, &mut state.labels);
        self.label_depth = state.label_depth;
        self.breakable_depth = state.breakable_depth;
        self.iterable_depth = state.iterable_depth;
    }
}

impl<'a> AstVisitor for Analyzer {
    fn visit_program(&mut self, program: &mut Program) {
        if program.has_use_strict_directive {
            self.enter_strict_mode_context();
        }

        self.scope_builder.enter_toplevel_scope(program);

        for toplevel in &mut program.toplevels {
            match toplevel {
                Toplevel::Statement(stmt) => self.visit_top_level_declaration_statement(stmt),
            }
        }

        self.scope_builder.exit_scope();

        if program.has_use_strict_directive {
            self.exit_strict_mode_context();
        }
    }

    fn visit_function_declaration(&mut self, func: &mut Function) {
        self.visit_function_declaration_common(func, true)
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.scope_builder.enter_block_scope(block);
        default_visit_block(self, block);
        self.scope_builder.exit_scope();
    }

    fn visit_switch_statement(&mut self, stmt: &mut SwitchStatement) {
        self.scope_builder.enter_switch_scope(stmt);
        self.inc_breakable_depth();

        default_visit_switch_statement(self, stmt);

        self.dec_breakable_depth();
        self.scope_builder.exit_scope();
    }

    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration) {
        self.scope_builder.add_var_decl(var_decl);
        default_visit_variable_declaration(self, var_decl);
    }

    fn visit_labeled_statement(&mut self, stmt: &mut LabeledStatement) {
        let (inner_stmt, label_stack) = self.push_all_labels(stmt);

        self.visit_statement(inner_stmt);

        self.pop_all_labels(label_stack);
    }

    fn visit_function_expression(&mut self, func: &mut Function) {
        self.visit_function_common(func)
    }

    fn visit_arrow_function(&mut self, func: &mut Function) {
        // Arrow functions do not provide an arguments object
        func.is_arguments_object_needed = false;

        self.visit_function_common(func);
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) {
        if !self.is_in_function() {
            self.emit_error(stmt.loc, ParseError::ReturnOutsideFunction);
        }

        default_visit_return_statement(self, stmt);
    }

    fn visit_break_statement(&mut self, stmt: &mut BreakStatement) {
        self.visit_label_use(stmt.label.as_mut());

        if stmt.label.is_none() && !self.is_in_breakable() {
            self.emit_error(stmt.loc, ParseError::UnlabeledBreakOutsideBreakable);
        }
    }

    fn visit_continue_statement(&mut self, stmt: &mut ContinueStatement) {
        self.visit_label_use(stmt.label.as_mut());

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
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_statement(self, stmt);

        self.dec_iterable_depth();
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement) {
        self.inc_iterable_depth();

        self.check_for_labeled_function(&stmt.body);
        default_visit_for_each_statement(self, stmt);

        self.dec_iterable_depth();
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

        self.visit_function_common(func);
    }

    fn visit_function_common(&mut self, func: &mut Function) {
        // Save analyzer context before descending into function
        let saved_state = self.save_function_state();

        visit_opt!(self, func.id, visit_identifier);
        visit_vec!(self, func.params, visit_pattern);

        // Enter strict mode context if applicable
        if func.is_strict_mode {
            self.enter_strict_mode_context();
        }

        // Visit body inside a new function scope
        self.scope_builder.enter_function_scope(func);
        self.function_depth += 1;

        match *func.body {
            FunctionBody::Block(ref mut block) => {
                for stmt in &mut block.body {
                    self.visit_top_level_declaration_statement(stmt)
                }
            }
            FunctionBody::Expression(ref mut expr) => self.visit_expression(expr),
        }

        // Static analysis of parameters and other function properties once body has been visited
        let mut has_parameter_expressions = false;
        let mut has_binding_patterns = false;
        let mut has_duplicate_parameters = false;
        // TODO: Initialize as false if function body does not contain "arguments" or "eval"
        let mut is_arguments_object_needed = func.is_arguments_object_needed && false;
        let mut parameter_names = HashSet::new();

        for param in &func.params {
            param.iter_patterns(&mut |patt| match patt {
                Pattern::Id(id) => {
                    if parameter_names.contains(&id.name) {
                        has_duplicate_parameters = true;
                    } else {
                        parameter_names.insert(&id.name);
                    }

                    // Arguments object is not needed if "arguments" is a bound name in the
                    // function parameters.
                    if id.name == "arguments" {
                        is_arguments_object_needed = false;
                    }
                }
                Pattern::Array(_) | Pattern::Object(_) => {
                    has_binding_patterns = true;
                }
                Pattern::Assign(_) => {
                    has_parameter_expressions = true;
                }
            });
        }

        // Arguments object is not needed if "arguments" appears in the lexically declared names, or
        // as a function var declared name.
        if is_arguments_object_needed {
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

        func.has_parameter_expressions = has_parameter_expressions;
        func.has_simple_parameter_list = !has_binding_patterns && !has_parameter_expressions;
        func.has_duplicate_parameters = has_duplicate_parameters;
        func.is_arguments_object_needed = is_arguments_object_needed;

        self.function_depth -= 1;
        self.scope_builder.exit_scope();

        if func.is_strict_mode {
            self.exit_strict_mode_context();
        }

        // Restore analyzer context after visiting function
        self.restore_function_state(saved_state);
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
        // Deep track of duplicate labels so that we don't pop the duplicate labels at the end
        let is_label_duplicate = self.visit_label_def(stmt, label_id);
        labels.push((stmt.label.label.name.clone(), is_label_duplicate));

        let mut inner_stmt = stmt.body.as_mut();
        while let Statement::Labeled(stmt) = inner_stmt {
            let is_label_duplicate = self.visit_label_def(stmt, label_id);
            labels.push((stmt.label.label.name.clone(), is_label_duplicate));

            inner_stmt = stmt.body.as_mut()
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
            self.labels.insert(label_name.clone(), label_id);
        }

        stmt.label.id = label_id;

        // Annex B: Always error on labeled function declarations in strict mode
        if self.is_in_strict_mode_context() {
            if let Statement::FuncDecl(_) = stmt.body.as_ref() {
                self.emit_error(
                    stmt.label.label.loc,
                    ParseError::InvalidLabeledFunction(true),
                );
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

    fn visit_label_use(&mut self, label: Option<&mut Label>) {
        if let Some(label) = label {
            match self.labels.get(&label.label.name) {
                None => self.emit_error(label.label.loc, ParseError::LabelNotFound),
                Some(label_id) => {
                    label.id = *label_id;
                }
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
