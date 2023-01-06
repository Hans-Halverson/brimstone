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
    scope_builder: ScopeBuilder,
    // Number of nested strict mode contexts the visitor is currently in
    strict_mode_context_depth: u64,
    // Set of labels defined where the visitor is currently in
    current_labels: HashMap<String, LabelId>,
    // Accumulator or errors reported during analysis
    errors: Vec<LocalizedParseError>,
}

impl<'a> Analyzer {
    pub fn new(source: Rc<Source>) -> Analyzer {
        Analyzer {
            source,
            scope_builder: ScopeBuilder::new(),
            strict_mode_context_depth: 0,
            current_labels: HashMap::new(),
            errors: Vec::new(),
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
        default_visit_switch_statement(self, stmt);
        self.scope_builder.exit_scope();
    }

    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration) {
        self.scope_builder.add_var_decl(var_decl);
        default_visit_variable_declaration(self, var_decl);
    }

    fn visit_labeled_statement(&mut self, stmt: &mut LabeledStatement) {
        let is_duplicate = self.enter_label_scope(stmt);
        default_visit_labeled_statement(self, stmt);
        self.exit_label_scope(&stmt.label.label.name, is_duplicate);
    }

    fn visit_function_expression(&mut self, func: &mut Function) {
        self.visit_function_common(func)
    }

    fn visit_arrow_function(&mut self, func: &mut Function) {
        // Arrow functions do not provide an arguments object
        func.is_arguments_object_needed = false;

        self.visit_function_common(func);
    }

    fn visit_break_statement(&mut self, stmt: &mut BreakStatement) {
        self.visit_label_use(stmt.label.as_mut())
    }

    fn visit_continue_statement(&mut self, stmt: &mut ContinueStatement) {
        self.visit_label_use(stmt.label.as_mut())
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
        self.check_for_labeled_function(&stmt.body);

        default_visit_while_statement(self, stmt);
    }

    fn visit_do_while_statement(&mut self, stmt: &mut DoWhileStatement) {
        self.check_for_labeled_function(&stmt.body);

        default_visit_do_while_statement(self, stmt);
    }

    fn visit_for_statement(&mut self, stmt: &mut ForStatement) {
        self.check_for_labeled_function(&stmt.body);

        default_visit_for_statement(self, stmt);
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement) {
        self.check_for_labeled_function(&stmt.body);

        default_visit_for_each_statement(self, stmt);
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
                // Enter label scopes and collect labels as we descend
                let mut labels = vec![];
                let is_label_duplicate = self.enter_label_scope(labeled_stmt);
                labels.push((labeled_stmt.label.label.name.clone(), is_label_duplicate));

                let mut inner_stmt = labeled_stmt.body.as_mut();
                while let Statement::Labeled(labeled_stmt) = inner_stmt {
                    let is_label_duplicate = self.enter_label_scope(labeled_stmt);
                    labels.push((labeled_stmt.label.label.name.clone(), is_label_duplicate));

                    inner_stmt = labeled_stmt.body.as_mut()
                }

                if let Statement::FuncDecl(func_decl) = inner_stmt {
                    self.visit_function_declaration_common(func_decl, false)
                }

                // Exit all label scopes in reverse order that they were entered
                for (label_name, is_duplicate) in labels.iter().rev() {
                    self.exit_label_scope(label_name, *is_duplicate)
                }
            }
            _ => self.visit_statement(stmt),
        }
    }

    fn visit_function_declaration_common(&mut self, func: &mut Function, is_lex_scoped_decl: bool) {
        self.scope_builder.add_func_decl(func, is_lex_scoped_decl);

        self.visit_function_common(func);
    }

    fn visit_function_common(&mut self, func: &mut Function) {
        visit_opt!(self, func.id, visit_identifier);
        visit_vec!(self, func.params, visit_pattern);

        // Enter strict mode context if applicable
        if func.has_use_strict_directive {
            self.enter_strict_mode_context();
        }
        func.is_strict_mode = self.is_in_strict_mode_context();

        // Visit body inside a new function scope
        self.scope_builder.enter_function_scope(func);

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

        self.scope_builder.exit_scope();

        if func.has_use_strict_directive {
            self.exit_strict_mode_context();
        }
    }

    fn enter_label_scope(&mut self, stmt: &mut LabeledStatement) -> bool {
        let label_name = &stmt.label.label.name;
        let is_duplicate = self.current_labels.contains_key(label_name);

        if is_duplicate {
            self.emit_error(stmt.label.label.loc, ParseError::DuplicateLabel);
        } else {
            let label_id = self.current_labels.len() as u32;
            self.current_labels.insert(label_name.clone(), label_id);

            stmt.label.id = label_id;
        }

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

    fn exit_label_scope(&mut self, label_name: &str, is_duplicate: bool) {
        if !is_duplicate {
            self.current_labels.remove(label_name);
        }
    }

    fn visit_label_use(&mut self, label: Option<&mut Label>) {
        if let Some(label) = label {
            match self.current_labels.get(&label.label.name) {
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
