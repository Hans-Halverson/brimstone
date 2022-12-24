use std::rc::Rc;

use crate::{visit_opt, visit_vec};

use super::{ast::*, ast_visitor::*, facts::FactsCache, scope::ScopeBuilder};

pub struct Analyzer {
    scope_builder: ScopeBuilder,
    facts_cache: FactsCache,
}

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
            scope_builder: ScopeBuilder::new(),
            facts_cache: FactsCache::new(),
        }
    }

    pub fn facts_cache(&self) -> &FactsCache {
        &self.facts_cache
    }
}

impl<'a> AstVisitor<'a> for Analyzer {
    fn visit_program(&mut self, program: &Program) {
        // Always populate facts for the toplevel program
        self.facts_cache.get_or_create_facts(program.ast_id);

        self.scope_builder.enter_toplevel_scope(program);

        for toplevel in &program.toplevels {
            match toplevel {
                Toplevel::Statement(stmt) => self.visit_top_level_declaration_statement(stmt),
            }
        }

        self.scope_builder.exit_scope()
    }

    fn visit_function_declaration(&mut self, func: &Function) {
        self.visit_function_declaration_common(func, true)
    }

    fn visit_block(&mut self, block: &Block) {
        self.scope_builder.enter_block_scope(block.ast_id);
        default_visit_block(self, block);
        self.scope_builder.exit_scope();
    }

    fn visit_switch_statement(&mut self, stmt: &SwitchStatement) {
        self.scope_builder.enter_block_scope(stmt.ast_id);
        default_visit_switch_statement(self, stmt);
        self.scope_builder.exit_scope();
    }

    fn visit_variable_declaration(&mut self, var_decl: &VariableDeclaration) {
        self.scope_builder
            .add_var_decl(var_decl, &mut self.facts_cache);
        default_visit_variable_declaration(self, var_decl);
    }

    fn visit_labeled_statement(&mut self, stmt: &LabeledStatement) {
        self.visit_label_definition(stmt);
        default_visit_labeled_statement(self, stmt);
    }
}

impl Analyzer {
    fn visit_top_level_declaration_statement(&mut self, stmt: &Statement) {
        match stmt {
            // Toplevel function declarations are treated as var scoped decls
            Statement::FuncDecl(func_decl) => {
                self.visit_function_declaration_common(func_decl, false)
            }
            // Find statement under labels, if it is a function it is a var scoped decl
            Statement::Labeled(ref labeled_stmt) => {
                self.visit_label_definition(labeled_stmt);

                let mut inner_stmt = labeled_stmt.body.as_ref();
                while let Statement::Labeled(ref labeled_stmt) = inner_stmt {
                    self.visit_label_definition(labeled_stmt);
                    inner_stmt = labeled_stmt.body.as_ref()
                }

                if let Statement::FuncDecl(func_decl) = inner_stmt {
                    self.visit_function_declaration_common(func_decl, false)
                }
            }
            _ => self.visit_statement(stmt),
        }
    }

    fn visit_function_declaration_common(&mut self, func: &Function, is_lex_scoped_decl: bool) {
        self.scope_builder
            .add_func_decl(func, is_lex_scoped_decl, &mut self.facts_cache);

        self.visit_function_common(func);
    }

    fn visit_function_common(&mut self, func: &Function) {
        visit_opt!(self, func.id, visit_identifier);
        visit_vec!(self, func.params, visit_pattern);

        self.scope_builder.enter_function_scope(func);

        match *func.body {
            FunctionBody::Block(ref block) => {
                for stmt in &block.body {
                    self.visit_top_level_declaration_statement(stmt)
                }
            }
            FunctionBody::Expression(ref expr) => self.visit_expression(expr),
        }

        self.scope_builder.exit_scope()
    }

    fn visit_label_definition(&mut self, _: &LabeledStatement) {
        // TODO: Analyze labels
    }
}

pub fn analyze(program: &Program) -> Rc<Analyzer> {
    let mut analyzer = Analyzer::new();
    analyzer.visit_program(&program);

    Rc::new(analyzer)
}
