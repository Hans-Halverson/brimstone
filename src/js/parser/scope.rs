use std::collections::HashMap;

use super::{
    ast::{self, AstPtr, LexDecl, VarDecl, WithDecls},
    loc::Loc,
    ParseError,
};

struct Scope {
    kind: ScopeKind,
    var_declared_names: HashMap<String, NameMetadata>,
    lex_declared_names: HashMap<String, NameMetadata>,
}

enum ScopeKind {
    Toplevel(AstPtr<dyn WithDecls>),
    Function(AstPtr<dyn WithDecls>),
    // Block-like scope, also includes switch scope
    Block(AstPtr<dyn WithDecls>),
    // Scope which does not contain any declarations of its own
    For,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Scope {
        Scope {
            kind,
            var_declared_names: HashMap::new(),
            lex_declared_names: HashMap::new(),
        }
    }
}

struct NameMetadata {
    kind: NameKind,
    loc: Loc,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NameKind {
    Var,
    Const,
    Let,
    Function,
    FunctionParameter,
    Class,
    CatchParameter,
}

pub struct ScopeBuilder {
    scope_stack: Vec<Scope>,
}

impl ScopeBuilder {
    pub fn new() -> ScopeBuilder {
        ScopeBuilder { scope_stack: vec![] }
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn enter_toplevel_scope(&mut self, program: &ast::Program) {
        self.scope_stack
            .push(Scope::new(ScopeKind::Toplevel(AstPtr::from_ref(program))))
    }

    pub fn enter_function_scope(&mut self, func: &ast::Function) {
        self.scope_stack
            .push(Scope::new(ScopeKind::Function(AstPtr::from_ref(func))))
    }

    pub fn enter_block_scope(&mut self, block: &ast::Block) {
        self.scope_stack
            .push(Scope::new(ScopeKind::Block(AstPtr::from_ref(block))))
    }

    pub fn enter_switch_scope(&mut self, stmt: &ast::SwitchStatement) {
        self.scope_stack
            .push(Scope::new(ScopeKind::Block(AstPtr::from_ref(stmt))))
    }

    pub fn enter_for_scope(&mut self) {
        self.scope_stack.push(Scope::new(ScopeKind::For))
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

    pub fn add_class_decl(&mut self, class: &ast::Class) {
        self.insert_lex_scoped_decl(LexDecl::Class(AstPtr::from_ref(class)))
    }

    fn insert_var_scoped_decl(&mut self, decl: VarDecl) {
        // Variables are hoisted to closest function or toplevel scope
        for scope in self.scope_stack.iter_mut().rev() {
            if let ScopeKind::Toplevel(with_decls) | ScopeKind::Function(with_decls) =
                &mut scope.kind
            {
                with_decls.as_mut().add_var_decl(decl);
                return;
            }
        }
    }

    fn insert_lex_scoped_decl(&mut self, decl: LexDecl) {
        match &mut self.current_scope_mut().kind {
            ScopeKind::Toplevel(with_decls)
            | ScopeKind::Function(with_decls)
            | ScopeKind::Block(with_decls) => with_decls.as_mut().add_lex_decl(decl),
            ScopeKind::For => {
                // Intentionally ignore, as the only lexical declarations that can be added in a
                // for scope are in the for-init.
            }
        }
    }

    // Add a lexically declared name in the current scope. Return an error if this name conflicts
    // with a lexically declared name in the current scope.
    pub fn add_lex_declared_name(
        &mut self,
        name: &String,
        loc: &Loc,
        kind: NameKind,
    ) -> Option<ParseError> {
        let scope = self.current_scope_mut();

        if let Some(existing_name) = scope.lex_declared_names.get(name) {
            return Some(ParseError::NameRedeclaration(name.clone(), existing_name.kind));
        }

        if let Some(existing_name) = scope.var_declared_names.get(name) {
            return Some(ParseError::NameRedeclaration(name.clone(), existing_name.kind));
        }

        scope
            .lex_declared_names
            .insert(name.clone(), NameMetadata { kind, loc: *loc });

        None
    }

    // Add a var declared name hoisted in the current scope. Return an error if this name conflicts
    // with a lexically declared name in any scope up to the scope the variable is hoisted to.
    pub fn add_var_declared_name(
        &mut self,
        name: &String,
        loc: &Loc,
        kind: NameKind,
    ) -> Option<ParseError> {
        // Variables are hoisted to closest function or toplevel scope, so add name to all scopes
        // up to the scope variable is hoisted into.
        for scope in self.scope_stack.iter_mut().rev() {
            if let Some(existing_name) = scope.lex_declared_names.get(name) {
                return Some(ParseError::NameRedeclaration(name.clone(), existing_name.kind));
            }

            scope
                .var_declared_names
                .insert(name.clone(), NameMetadata { kind, loc: *loc });

            if let ScopeKind::Toplevel(_) | ScopeKind::Function(_) = scope.kind {
                return None;
            }
        }

        None
    }
}
