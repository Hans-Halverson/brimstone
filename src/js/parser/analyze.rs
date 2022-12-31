use std::rc::Rc;

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

        self.scope_builder.enter_function_scope(func);

        match *func.body {
            FunctionBody::Block(ref mut block) => {
                for stmt in &mut block.body {
                    self.visit_top_level_declaration_statement(stmt)
                }
            }
            FunctionBody::Expression(ref mut expr) => self.visit_expression(expr),
        }

        self.scope_builder.exit_scope()
    }

    fn visit_label_definition(&mut self, _: &mut LabeledStatement) {
        // TODO: Analyze labels
    }
}

pub fn analyze(program: &mut Program) -> Rc<Analyzer> {
    let mut analyzer = Analyzer::new();
    analyzer.visit_program(program);

    Rc::new(analyzer)
}
