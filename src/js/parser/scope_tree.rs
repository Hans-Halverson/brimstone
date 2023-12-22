use std::collections::HashSet;

use indexmap::IndexMap;

use super::{
    ast::{self, AstPtr, FunctionId},
    ParseError,
};

pub struct ScopeTree {
    _nodes: Vec<Box<ScopeNode>>,
}

pub struct ScopeTreeBuilder {
    nodes: Vec<Box<ScopeNode>>,
    current_node_id: ScopeNodeId,
}

pub struct SavedScopeTreeBuilderState {
    current_node_id: ScopeNodeId,
    current_node_num_bindings: usize,
    num_nodes: usize,
}

type AddBindingResult = Result<(), ParseError>;

impl ScopeTreeBuilder {
    pub fn new_global() -> ScopeTreeBuilder {
        ScopeTreeBuilder {
            nodes: vec![Box::new(ScopeNode::new(ScopeNodeKind::Global, None))],
            current_node_id: 0,
        }
    }

    pub fn finish(mut self) -> ScopeTree {
        // Re-sort bindings by index to properly order duplicates
        for node in &mut self.nodes {
            node.bindings.sort_by(|_, a, _, b| a.index.cmp(&b.index));
        }

        ScopeTree { _nodes: self.nodes }
    }

    pub fn save(&self) -> SavedScopeTreeBuilderState {
        SavedScopeTreeBuilderState {
            current_node_id: self.current_node_id,
            current_node_num_bindings: self.nodes[self.current_node_id].bindings.len(),
            num_nodes: self.nodes.len(),
        }
    }

    pub fn restore(&mut self, saved_state: &SavedScopeTreeBuilderState) {
        self.current_node_id = saved_state.current_node_id;
        self.nodes[self.current_node_id]
            .bindings
            .truncate(saved_state.current_node_num_bindings);
        self.nodes.truncate(saved_state.num_nodes);
    }

    pub fn enter_scope(&mut self, kind: ScopeNodeKind) -> AstPtr<ScopeNode> {
        let node_id = self.nodes.len();
        self.nodes
            .push(Box::new(ScopeNode::new(kind, Some(self.current_node_id))));
        self.current_node_id = node_id;

        self.get_node_ptr(node_id)
    }

    pub fn exit_scope(&mut self) {
        self.current_node_id = self.nodes[self.current_node_id].parent.unwrap();
    }

    fn get_node_mut(&mut self, node_id: ScopeNodeId) -> &mut ScopeNode {
        &mut self.nodes[node_id]
    }

    pub fn get_node_ptr(&self, node_id: ScopeNodeId) -> AstPtr<ScopeNode> {
        AstPtr::from_ref(self.nodes[node_id].as_ref())
    }

    pub fn add_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        if kind.is_lexically_scoped() {
            self.add_lexically_scoped_binding(name, kind)
        } else {
            self.add_var_scoped_binding(name, kind)
        }
    }

    fn add_lexically_scoped_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        let node = self.get_node_mut(self.current_node_id);

        // Error if there is already any binding with this name in the current scope, which is
        // guaranteed to detect conflicting lexically scoped bindings and var scoped bindings
        // declared in this scope.
        if let Some(binding) = node.bindings.get(name) {
            return Err(ParseError::NameRedeclaration(name.to_owned(), binding.kind));
        }

        // Then check for conflicting child var scoped bindings
        if node.child_var_names.contains(name) {
            return Err(ParseError::NameRedeclaration(
                name.to_owned(),
                // Guaranteed to conflict with a `var` binding, as the other var scoped names -
                // var scoped functions and function parameters - cannot appear in child scopes.
                BindingKind::Var,
            ));
        }

        node.add_binding(name, kind);

        Ok(())
    }

    fn add_var_scoped_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        // Walk up to the hoist target scope, checking for conflicting lexical bindings
        let mut node_id = self.current_node_id;
        loop {
            let node = self.get_node_mut(node_id);

            // Error if there is already a lexically scoped binding with this name. Existing var
            // scoped bindings with the same name are allowed.
            if let Some(binding) = node.bindings.get(name) {
                if binding.kind.is_lexically_scoped() {
                    return Err(ParseError::NameRedeclaration(name.to_owned(), binding.kind));
                }
            }

            if node.kind.is_hoist_target() {
                // Only override existing binding if it is a new function declaration
                if !node.bindings.contains_key(name) || kind.is_function() {
                    node.add_binding(name, kind);
                }

                return Ok(());
            } else {
                // Add var name to all scopes except for hoist target, so that later lexical
                // declarations can check for name conflicts.
                node.child_var_names.insert(name.to_owned());
            }

            node_id = node.parent.unwrap();
        }
    }
}

pub type ScopeNodeId = usize;

pub enum ScopeNodeKind {
    Global,
    Function(FunctionId),
    Block,
}

impl ScopeNodeKind {
    fn is_hoist_target(&self) -> bool {
        match self {
            ScopeNodeKind::Global | ScopeNodeKind::Function(_) => true,
            ScopeNodeKind::Block => false,
        }
    }
}

pub struct ScopeNode {
    parent: Option<ScopeNodeId>,
    kind: ScopeNodeKind,
    /// Bindings declared in this scope. Each name maps to the last binding with that name declared
    /// in the scope. Earlier var bindings may be overwritten by later var bindings, which is fine
    /// since they reference the same binding (though may contain different BindingKinds).
    bindings: IndexMap<String, Binding>,
    /// All var declared names that are in scope at this node but not included in the bindings map.
    /// This means the set of all var declared names in child scopes.
    child_var_names: HashSet<String>,
    /// Total number of bindings added to map, including duplicates.
    num_bindings: usize,
}

impl ScopeNode {
    fn add_binding(&mut self, name: &str, kind: BindingKind) {
        self.bindings
            .insert(name.to_owned(), Binding::new(kind, self.num_bindings));
        self.num_bindings += 1;
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BindingKind {
    Var,
    Const,
    Let,
    Function {
        /// Whether this is a LexicallyScopedDeclaration or a VarScopedDeclaration from the spec.
        /// Only is a VarScopedDeclaration if this is a toplevel function declaration within a
        /// script, function, or class static block.
        is_lexical: bool,
        /// Function AST node - needed for the tree walk interpreter.
        func_node: AstPtr<ast::Function>,
    },
    FunctionParameter,
    Class,
    CatchParameter,
}

impl BindingKind {
    /// Whether this is a LexicallyScopedDeclaration or a VarScopedDeclaration from the spec.
    pub fn is_lexically_scoped(&self) -> bool {
        match self {
            BindingKind::Var | BindingKind::FunctionParameter => false,
            BindingKind::Function { is_lexical, .. } => *is_lexical,
            BindingKind::Const
            | BindingKind::Let
            | BindingKind::Class
            | BindingKind::CatchParameter => true,
        }
    }

    pub fn is_const(&self) -> bool {
        *self == BindingKind::Const
    }

    pub fn is_function(&self) -> bool {
        match self {
            BindingKind::Function { .. } => true,
            _ => false,
        }
    }
}

pub struct Binding {
    kind: BindingKind,
    /// Index of the binding in the scope's bindings map. Used to determine the order of duplicate
    /// bindings.
    index: usize,
}

impl Binding {
    fn new(kind: BindingKind, index: usize) -> Binding {
        Binding { kind, index }
    }

    pub fn kind(&self) -> &BindingKind {
        &self.kind
    }

    pub fn is_const(&self) -> bool {
        self.kind.is_const()
    }
}

impl ScopeNode {
    fn new(kind: ScopeNodeKind, parent: Option<ScopeNodeId>) -> ScopeNode {
        ScopeNode {
            parent,
            kind,
            bindings: IndexMap::new(),
            child_var_names: HashSet::new(),
            num_bindings: 0,
        }
    }

    /// Iterator over all bindings in this scope, including funtion parameters and catch parameters.
    pub fn iter_bindings(&self) -> impl DoubleEndedIterator<Item = (&String, &Binding)> {
        self.bindings.iter()
    }

    /// Iterator over all VarScopedDeclarations in this scope. Note that this does not include
    /// function parameters, which are var scoped but do not count as a VarScopedDeclaration.
    pub fn iter_var_decls(&self) -> impl DoubleEndedIterator<Item = (&String, &Binding)> {
        self.bindings
            .iter()
            .filter(|(_, binding)| match binding.kind {
                BindingKind::Var | BindingKind::Function { is_lexical: false, .. } => true,
                _ => false,
            })
    }

    /// Iterator over all LexicallyScopedDeclarations in this scope. Note that this does not include
    /// catch parameters, which are handled separately.
    pub fn iter_lex_decls(&self) -> impl DoubleEndedIterator<Item = (&String, &Binding)> {
        self.bindings
            .iter()
            .filter(|(_, binding)| match binding.kind {
                BindingKind::Const
                | BindingKind::Let
                | BindingKind::Class
                | BindingKind::Function { is_lexical: true, .. } => true,
                _ => false,
            })
    }
}
