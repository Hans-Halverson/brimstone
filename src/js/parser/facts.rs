use std::collections::HashMap;

use crate::js::runtime::completion::AbstractResult;

use super::ast::{self, AstId, AstPtr};

pub struct Facts {
    // 8.1.7 VarScopedDeclarations
    var_decls: Vec<VarDecl>,
    // 8.1.5 LexicallyScopedDeclarations
    lex_decls: Vec<LexDecl>,
}

pub enum VarDecl {
    Func(AstPtr<ast::Function>),
    Var(AstPtr<ast::VariableDeclaration>),
}

pub enum LexDecl {
    Func(AstPtr<ast::Function>),
    Var(AstPtr<ast::VariableDeclaration>),
}

impl VarDecl {
    pub fn iter_bound_names<F: FnMut(&ast::Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        match &self {
            VarDecl::Func(func) => f(&func.as_ref().id.as_deref().unwrap()),
            VarDecl::Var(var_decl) => var_decl.as_ref().iter_bound_names(f),
        }
    }
}

impl LexDecl {
    pub fn iter_bound_names<F: FnMut(&ast::Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        match &self {
            LexDecl::Func(func) => f(&func.as_ref().id.as_deref().unwrap()),
            LexDecl::Var(var_decl) => var_decl.as_ref().iter_bound_names(f),
        }
    }
}

impl Facts {
    pub fn new() -> Facts {
        Facts {
            var_decls: vec![],
            lex_decls: vec![],
        }
    }

    pub fn var_decls(&self) -> &[VarDecl] {
        &self.var_decls
    }

    pub fn lex_decls(&self) -> &[LexDecl] {
        &self.lex_decls
    }

    pub fn add_var_decl(&mut self, var_decl: VarDecl) {
        self.var_decls.push(var_decl)
    }

    pub fn add_lex_decl(&mut self, lex_decl: LexDecl) {
        self.lex_decls.push(lex_decl)
    }
}

pub struct FactsCache {
    cache: HashMap<AstId, Facts>,
}

impl FactsCache {
    pub fn new() -> FactsCache {
        FactsCache {
            cache: HashMap::new(),
        }
    }

    pub fn get_facts(&self, ast_id: AstId) -> Option<&Facts> {
        self.cache.get(&ast_id)
    }

    pub fn get_or_create_facts(&mut self, ast_id: AstId) -> &mut Facts {
        self.cache.entry(ast_id).or_insert_with(|| Facts::new())
    }
}
