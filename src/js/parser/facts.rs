use std::collections::HashMap;

use super::ast::{self, AstId};

/// Reference to AST node without lifetime constraints. Only valid to use while AST is still live.
pub struct FactsPtr<T> {
    ptr: *const T,
}

impl<T> FactsPtr<T> {
    pub fn from_ref(value: &T) -> FactsPtr<T> {
        FactsPtr { ptr: value }
    }

    pub fn as_ref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

pub struct Facts {
    // 8.1.7 VarScopedDeclarations
    var_decls: Vec<VarDecl>,
    // 8.1.5 LexicallyScopedDeclarations
    lex_decls: Vec<LexDecl>,
}

pub enum VarDecl {
    Func(FactsPtr<ast::Function>),
    Var(FactsPtr<ast::VariableDeclaration>),
}

pub enum LexDecl {
    Func(FactsPtr<ast::Function>),
    Var(FactsPtr<ast::VariableDeclaration>),
}

impl Facts {
    pub fn new() -> Facts {
        Facts {
            var_decls: vec![],
            lex_decls: vec![],
        }
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
