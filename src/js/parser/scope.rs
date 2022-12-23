use super::{
    ast::{self, AstId, AstPtr},
    facts::{FactsCache, LexDecl, VarDecl},
};

enum Scope {
    Toplevel(AstId),
    Function(AstId),
    // Block-like scope, also includes switch scope
    Block(AstId),
}

impl Scope {
    fn ast_id(&self) -> AstId {
        match self {
            Scope::Toplevel(ast_id) | Scope::Function(ast_id) | Scope::Block(ast_id) => *ast_id,
        }
    }
}

pub struct ScopeBuilder {
    scope_stack: Vec<Scope>,
}

impl ScopeBuilder {
    pub fn new() -> ScopeBuilder {
        ScopeBuilder {
            scope_stack: vec![],
        }
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn enter_toplevel_scope(&mut self, program: &ast::Program) {
        self.scope_stack.push(Scope::Toplevel(program.ast_id))
    }

    pub fn enter_function_scope(&mut self, func: &ast::Function) {
        self.scope_stack.push(Scope::Function(func.ast_id))
    }

    pub fn enter_block_scope(&mut self, ast_id: AstId) {
        self.scope_stack.push(Scope::Block(ast_id))
    }

    fn current_scope(&self) -> &Scope {
        self.scope_stack.last().unwrap()
    }

    pub fn add_var_decl(&mut self, var_decl: &ast::VariableDeclaration, facts: &mut FactsCache) {
        let is_lex_decl = match var_decl.kind {
            ast::VarKind::Var => false,
            ast::VarKind::Const | ast::VarKind::Let => true,
        };

        if is_lex_decl {
            self.insert_lex_scoped_decl(LexDecl::Var(AstPtr::from_ref(var_decl)), facts)
        } else {
            self.insert_var_scoped_decl(VarDecl::Var(AstPtr::from_ref(var_decl)), facts)
        }
    }

    pub fn add_func_decl(
        &mut self,
        func: &ast::Function,
        is_lex_scoped_decl: bool,
        facts: &mut FactsCache,
    ) {
        if is_lex_scoped_decl {
            self.insert_lex_scoped_decl(LexDecl::Func(AstPtr::from_ref(func)), facts)
        } else {
            self.insert_var_scoped_decl(VarDecl::Func(AstPtr::from_ref(func)), facts)
        }
    }

    fn insert_var_scoped_decl(&mut self, decl: VarDecl, facts: &mut FactsCache) {
        // Variables are hoisted to closest function or toplevel scope
        for scope in self.scope_stack.iter().rev() {
            if let Scope::Toplevel(_) | Scope::Function(_) = scope {
                let ast_id = scope.ast_id();
                let facts = facts.get_or_create_facts(ast_id);
                facts.add_var_decl(decl);

                return;
            }
        }
    }

    fn insert_lex_scoped_decl(&mut self, decl: LexDecl, facts: &mut FactsCache) {
        let ast_id = self.current_scope().ast_id();
        let facts = facts.get_or_create_facts(ast_id);
        facts.add_lex_decl(decl)
    }
}
