use super::ast::{self, AstPtr, LexDecl, VarDecl, WithDecls};

enum Scope {
    Toplevel(AstPtr<dyn WithDecls>),
    Function(AstPtr<dyn WithDecls>),
    // Block-like scope, also includes switch scope
    Block(AstPtr<dyn WithDecls>),
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
        self.scope_stack
            .push(Scope::Toplevel(AstPtr::from_ref(program)))
    }

    pub fn enter_function_scope(&mut self, func: &ast::Function) {
        self.scope_stack
            .push(Scope::Function(AstPtr::from_ref(func)))
    }

    pub fn enter_block_scope(&mut self, block: &ast::Block) {
        self.scope_stack.push(Scope::Block(AstPtr::from_ref(block)))
    }

    pub fn enter_switch_scope(&mut self, stmt: &ast::SwitchStatement) {
        self.scope_stack.push(Scope::Block(AstPtr::from_ref(stmt)))
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().unwrap()
    }

    pub fn add_var_decl(&mut self, var_decl: &ast::VariableDeclaration) {
        let is_lex_decl = match var_decl.kind {
            ast::VarKind::Var => false,
            ast::VarKind::Const | ast::VarKind::Let => true,
        };

        if is_lex_decl {
            self.insert_lex_scoped_decl(LexDecl::Var(AstPtr::from_ref(var_decl)))
        } else {
            self.insert_var_scoped_decl(VarDecl::Var(AstPtr::from_ref(var_decl)))
        }
    }

    pub fn add_func_decl(&mut self, func: &ast::Function, is_lex_scoped_decl: bool) {
        if is_lex_scoped_decl {
            self.insert_lex_scoped_decl(LexDecl::Func(AstPtr::from_ref(func)))
        } else {
            self.insert_var_scoped_decl(VarDecl::Func(AstPtr::from_ref(func)))
        }
    }

    fn insert_var_scoped_decl(&mut self, decl: VarDecl) {
        // Variables are hoisted to closest function or toplevel scope
        for scope in self.scope_stack.iter_mut().rev() {
            if let Scope::Toplevel(with_decls) | Scope::Function(with_decls) = scope {
                with_decls.as_mut().add_var_decl(decl);
                return;
            }
        }
    }

    fn insert_lex_scoped_decl(&mut self, decl: LexDecl) {
        match self.current_scope_mut() {
            Scope::Toplevel(with_decls)
            | Scope::Function(with_decls)
            | Scope::Block(with_decls) => with_decls.as_mut().add_lex_decl(decl),
        }
    }
}
