use std::collections::HashSet;

use crate::{visit_opt, visit_vec};

use super::{ast::*, ast_visitor::*, scope::ScopeBuilder};

pub struct Analyzer {
    scope_builder: ScopeBuilder,
}

impl<'a> Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
            scope_builder: ScopeBuilder::new(),
        }
    }
}

impl<'a> AstVisitor for Analyzer {
    fn visit_program(&mut self, program: &mut Program) {
        self.scope_builder.enter_toplevel_scope(program);

        for toplevel in &mut program.toplevels {
            match toplevel {
                Toplevel::Statement(stmt) => self.visit_top_level_declaration_statement(stmt),
            }
        }

        self.scope_builder.exit_scope()
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
        self.visit_label_definition(stmt);
        default_visit_labeled_statement(self, stmt);
    }

    fn visit_function_expression(&mut self, func: &mut Function) {
        self.visit_function_common(func)
    }

    fn visit_arrow_function(&mut self, func: &mut Function) {
        // Arrow functions do not provide an arguments object
        func.is_arguments_object_needed = false;

        self.visit_function_common(func);
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
                self.visit_label_definition(labeled_stmt);

                let mut inner_stmt = labeled_stmt.body.as_mut();
                while let Statement::Labeled(labeled_stmt) = inner_stmt {
                    self.visit_label_definition(labeled_stmt);
                    inner_stmt = labeled_stmt.body.as_mut()
                }

                if let Statement::FuncDecl(func_decl) = inner_stmt {
                    self.visit_function_declaration_common(func_decl, false)
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

        self.scope_builder.exit_scope();

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
    }

    fn visit_label_definition(&mut self, _: &mut LabeledStatement) {
        // TODO: Analyze labels
    }
}

pub fn analyze(program: &mut Program) {
    let mut analyzer = Analyzer::new();
    analyzer.visit_program(program);
}
