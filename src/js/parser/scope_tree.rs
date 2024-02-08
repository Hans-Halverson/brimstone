use std::{cell::Cell, collections::HashSet};

use indexmap::IndexMap;

use super::{
    ast::{self, AstPtr, FunctionId, Identifier},
    loc::Pos,
    ParseError,
};

pub struct ScopeTree {
    ast_nodes: Vec<Box<AstScopeNode>>,
    vm_nodes: Vec<VMScopeNode>,
    current_node_id: ScopeNodeId,
}

pub struct SavedScopeTreeState {
    current_node_id: ScopeNodeId,
    current_node_num_bindings: usize,
    num_nodes: usize,
}

type AddBindingResult = Result<AstPtr<AstScopeNode>, ParseError>;

/// Functions for constructing, mutating, and querying the AST scope tree.
impl ScopeTree {
    pub fn new_global() -> ScopeTree {
        let mut scope_tree = ScopeTree {
            ast_nodes: vec![],
            vm_nodes: vec![],
            current_node_id: GLOBAL_SCOPE_ID,
        };

        let global_scope_node =
            scope_tree.new_ast_scope_node(GLOBAL_SCOPE_ID, ScopeNodeKind::Global, None);
        scope_tree.ast_nodes.push(Box::new(global_scope_node));

        scope_tree
    }

    pub fn finish_ast_scope_tree(mut self) -> ScopeTree {
        for node in &mut self.ast_nodes {
            // Re-sort bindings by index to properly order duplicates
            if node.has_duplicates {
                node.bindings.sort_by(|_, a, _, b| a.index.cmp(&b.index));
            }
        }

        self
    }

    /// Save the current state of the AST scope tree, so that it can be restored later.
    pub fn save(&self) -> SavedScopeTreeState {
        SavedScopeTreeState {
            current_node_id: self.current_node_id,
            current_node_num_bindings: self.ast_nodes[self.current_node_id].bindings.len(),
            num_nodes: self.ast_nodes.len(),
        }
    }

    /// Restore from a saved state of the AST scope tree.
    pub fn restore(&mut self, saved_state: &SavedScopeTreeState) {
        self.current_node_id = saved_state.current_node_id;
        self.ast_nodes[self.current_node_id]
            .bindings
            .truncate(saved_state.current_node_num_bindings);
        self.ast_nodes.truncate(saved_state.num_nodes);
    }

    /// Enter a new AST scope node with the provided kind.
    pub fn enter_scope(&mut self, kind: ScopeNodeKind) -> AstPtr<AstScopeNode> {
        let node_id = self.ast_nodes.len();
        let ast_node = self.new_ast_scope_node(node_id, kind, Some(self.current_node_id));
        self.ast_nodes.push(Box::new(ast_node));

        self.current_node_id = node_id;

        self.get_ast_node_ptr(node_id)
    }

    /// Exit the current AST scope node.
    pub fn exit_scope(&mut self) {
        self.current_node_id = self.ast_nodes[self.current_node_id].parent.unwrap();
    }

    fn get_ast_node(&self, node_id: ScopeNodeId) -> &AstScopeNode {
        &self.ast_nodes[node_id]
    }

    fn get_ast_node_mut(&mut self, node_id: ScopeNodeId) -> &mut AstScopeNode {
        &mut self.ast_nodes[node_id]
    }

    pub fn get_ast_node_ptr(&self, node_id: ScopeNodeId) -> AstPtr<AstScopeNode> {
        AstPtr::from_ref(self.ast_nodes[node_id].as_ref())
    }

    fn new_ast_scope_node(
        &mut self,
        id: ScopeNodeId,
        kind: ScopeNodeKind,
        parent: Option<ScopeNodeId>,
    ) -> AstScopeNode {
        let enclosing_scope = if let ScopeNodeKind::Global | ScopeNodeKind::Function(_) = kind {
            // Global and function nodes are their own enclosing scope
            id
        } else {
            // Otherwise inherit enclosing scope from parent scope
            self.get_ast_node(parent.unwrap()).enclosing_scope
        };

        AstScopeNode {
            id,
            parent,
            kind,
            bindings: IndexMap::new(),
            child_var_names: HashSet::new(),
            num_bindings: 0,
            has_duplicates: false,
            force_vm_scope: false,
            enclosing_scope,
            enclosed_scopes: vec![],
            num_local_registers: 0,
            vm_scope: None,
        }
    }

    /// Add a binding to the AST scope tree, hoisting to a higher scope if necessary. On success
    /// return the AST scope node that the binding was added to.
    pub fn add_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        if kind.is_lexically_scoped() {
            self.add_lexically_scoped_binding(name, kind)
        } else {
            self.add_var_scoped_binding(name, kind)
        }
    }

    fn add_lexically_scoped_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        let node = self.get_ast_node_mut(self.current_node_id);

        // Error if there is already any binding with this name in the current scope, which is
        // guaranteed to detect conflicting lexically scoped bindings and var scoped bindings
        // declared in this scope.
        if let Some(binding) = node.bindings.get(name) {
            return Err(ParseError::NameRedeclaration(name.to_owned(), binding.kind.clone()));
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

        let mut vm_location = None;
        let mut needs_tdz_check = false;

        match node.kind {
            // Lexical bindings in the global scope are immediately known to be global
            ScopeNodeKind::Global => {
                vm_location = Some(VMLocation::Global);
            }
            // Lexical bindings in a switch always need a TDZ check since it hard to analyze when
            // the check can be elided. (e.g. due to out of order default case, or definitions and
            // uses in different cases).
            ScopeNodeKind::Block { is_switch: true } => {
                needs_tdz_check = true;
            }
            _ => {}
        }

        node.add_binding(name, kind, vm_location, needs_tdz_check);

        Ok(self.get_ast_node_ptr(self.current_node_id))
    }

    fn add_var_scoped_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        // Walk up to the hoist target scope, checking for conflicting lexical bindings
        let mut node_id = self.current_node_id;
        loop {
            let node = self.get_ast_node_mut(node_id);

            // Error if there is already a lexically scoped binding with this name. Existing var
            // scoped bindings with the same name are allowed.
            if let Some(binding) = node.bindings.get(name) {
                if binding.kind.is_lexically_scoped() {
                    return Err(ParseError::NameRedeclaration(
                        name.to_owned(),
                        binding.kind.clone(),
                    ));
                }
            }

            if node.kind.is_hoist_target() {
                // Only override existing binding if it is a new function declaration
                if !node.bindings.contains_key(name) || kind.is_function() {
                    // Var bindings in the global scope are immediately known to be global
                    let vm_location = if node.kind == ScopeNodeKind::Global {
                        Some(VMLocation::Global)
                    } else {
                        None
                    };

                    node.add_binding(name, kind, vm_location, /* needs_tdz_check */ false);
                }

                return Ok(self.get_ast_node_ptr(node_id));
            } else {
                // Add var name to all scopes except for hoist target, so that later lexical
                // declarations can check for name conflicts.
                node.child_var_names.insert(name.to_owned());
            }

            node_id = node.parent.unwrap();
        }
    }

    /// Resolve a use of the given name in the provided AST scope id to the scope where the binding
    /// with that name is defined. If the name could not be statically resolved to a definition then
    /// return None.
    ///
    /// Marks the binding as captured if it is used by a nested function.
    pub fn resolve_use(
        &mut self,
        use_scope_id: ScopeNodeId,
        id: &Identifier,
    ) -> Option<AstPtr<AstScopeNode>> {
        let mut scope_id = use_scope_id;
        loop {
            let scope = self.get_ast_node(scope_id);

            if scope.bindings.contains_key(&id.name) {
                // If the use is in a different function than the definition, mark the
                // binding as captured.
                if self.lookup_enclosing_function(scope_id)
                    != self.lookup_enclosing_function(use_scope_id)
                {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(&id.name).unwrap();
                    binding.is_captured = true;
                } else {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(&id.name).unwrap();

                    // For const and let bindings in the same scope, check if the use is before the
                    // end of the binding's initialization. If so we need to check for the TDZ.
                    if let BindingKind::Const { init_pos }
                    | BindingKind::Let { init_pos }
                    | BindingKind::CatchParameter { init_pos } = binding.kind()
                    {
                        if id.loc.start < init_pos.get() {
                            binding.needs_tdz_check = true;
                        }
                    }
                }

                return Some(self.get_ast_node_ptr(scope_id));
            }

            // If we have not yet found the binding when exiting a with scope, the use is not
            // statically resolvable.
            if scope.kind == ScopeNodeKind::With {
                return None;
            }

            if let Some(parent_id) = scope.parent {
                scope_id = parent_id;
            } else {
                return None;
            }
        }
    }

    /// Look up the enclosing scope and function id for an AST scope node (aka the most recent
    /// function scope ancestor). If this is the global scope then return None.
    fn lookup_enclosing_function(&self, scope_id: ScopeNodeId) -> Option<FunctionId> {
        let node = self.get_ast_node(scope_id);
        let enclosing_scope = self.get_ast_node(node.enclosing_scope);

        if let ScopeNodeKind::Function(func_id) = enclosing_scope.kind {
            Some(func_id)
        } else {
            None
        }
    }

    /// Force all scopes at the provided AST scope and above to use scope VM scope locations instead
    /// of local registers. e.g. taints the scope and all parent scopes for `eval` and `with`
    /// statements.
    pub fn force_vm_scope_for_visible_bindings(&mut self, scope_id: ScopeNodeId) {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_ast_node_mut(scope_id);

            scope.force_vm_scope = true;

            if let Some(parent_id) = scope.parent {
                scope_id = parent_id;
            } else {
                return;
            }
        }
    }
}

/// Functions for constructing the VM scope tree.
impl ScopeTree {
    pub fn finish_vm_scope_node(&mut self, ast_node_id: ScopeNodeId) {
        let vm_node_id = self.vm_nodes.len();
        let ast_node = self.get_ast_node_mut(ast_node_id);
        let enclosing_scope = ast_node.enclosing_scope;

        // Collect all bindings that must appear in a VM scope node, meaning all captured bindings
        // and all bindings in scope nodes that are forced to be VM scopes.
        let mut bindings = Vec::new();
        for (name, binding) in ast_node.bindings.iter_mut() {
            let is_global = matches!(binding.vm_location, Some(VMLocation::Global));
            if binding.is_captured || ast_node.force_vm_scope {
                // Bindings that are captured by another scope must be placed in a VM scope node
                if !is_global {
                    // Mark the VM location for the binding
                    binding.set_vm_location(VMLocation::Scope {
                        scope_id: vm_node_id,
                        index: bindings.len(),
                    });

                    bindings.push(name.clone());
                }

                // All bindings in VM scope nodes need a TDZ check as we do not analyze whether
                // they are guaranteed to be initialized before use.
                binding.needs_tdz_check = true;
            }

            // All globals need a TDZ check as they could be captured by any function
            if is_global {
                binding.needs_tdz_check = true;
            }
        }

        // Only if there are bindings to place in the scope do we need to create a VM scope node
        if !bindings.is_empty() {
            self.vm_nodes.push(VMScopeNode { _bindings: bindings });
            self.get_ast_node_mut(ast_node_id).vm_scope = Some(vm_node_id)
        }

        // Maintain a list of child AST scope nodes for each global or function scope (in reverse
        // order) so that locals can be calculated later.
        let enclosing_scope = self.get_ast_node_mut(enclosing_scope);
        enclosing_scope.enclosed_scopes.push(ast_node_id);
    }

    pub fn finish_vm_scope_tree(&mut self) {
        for i in 0..self.ast_nodes.len() {
            if self.ast_nodes[i].enclosed_scopes.is_empty() {
                continue;
            }

            let mut num_local_registers = 0;

            // Extract enclosed scopes
            let mut enclosed_scopes = vec![];
            std::mem::swap(&mut enclosed_scopes, &mut self.ast_nodes[i].enclosed_scopes);

            // All bindings without a location are placed in local registers in the enclosing scope
            for enclosed_scope in enclosed_scopes.iter().rev() {
                let enclosed_scope = self.get_ast_node_mut(*enclosed_scope);
                for (_, binding) in enclosed_scope.bindings.iter_mut() {
                    if binding.vm_location.is_none() {
                        binding.set_vm_location(VMLocation::LocalRegister(num_local_registers));
                        num_local_registers += 1;
                    }
                }
            }

            self.ast_nodes[i].num_local_registers = num_local_registers;
        }
    }
}

pub type ScopeNodeId = usize;

#[derive(Clone, Copy, PartialEq)]
pub enum ScopeNodeKind {
    Global,
    Function(FunctionId),
    Block { is_switch: bool },
    With,
}

impl ScopeNodeKind {
    pub fn block() -> ScopeNodeKind {
        ScopeNodeKind::Block { is_switch: false }
    }

    pub fn switch() -> ScopeNodeKind {
        ScopeNodeKind::Block { is_switch: true }
    }

    fn is_hoist_target(&self) -> bool {
        match self {
            ScopeNodeKind::Global | ScopeNodeKind::Function(_) => true,
            ScopeNodeKind::Block { .. } | ScopeNodeKind::With => false,
        }
    }
}

pub struct AstScopeNode {
    id: ScopeNodeId,
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
    /// Whether at least one duplicate binding was added to map, replacing an earlier binding.
    has_duplicates: bool,
    /// Whether all bindings in this scope must have a VM scope location instead of being in local
    /// registers.
    force_vm_scope: bool,
    /// The most recent ancestor global or function AST scope node, meaning the node that this
    /// scope's locals will be placed in. For global and function nodes this points to itself.
    enclosing_scope: ScopeNodeId,
    /// The set of all descendant AST scope nodes that are enclosed by this AST scope node. Meaning
    /// the set of all scope nodes directly owned by each global and function node. Includes self.
    enclosed_scopes: Vec<ScopeNodeId>,
    /// If this is global or function AST scope node, the total number of local registers needed for
    /// the locals in all enclosed scopes.
    num_local_registers: usize,
    /// Concrete VM scope node id for this AST scope node, if any bindings of this AST scope node
    /// need to be located in a VM scope node.
    vm_scope: Option<ScopeNodeId>,
}

const GLOBAL_SCOPE_ID: ScopeNodeId = 0;

impl AstScopeNode {
    pub fn id(&self) -> ScopeNodeId {
        self.id
    }

    pub fn num_local_registers(&self) -> usize {
        self.num_local_registers
    }

    fn add_binding(
        &mut self,
        name: &str,
        kind: BindingKind,
        vm_location: Option<VMLocation>,
        needs_tdz_check: bool,
    ) {
        let insert_result = self.bindings.insert(
            name.to_owned(),
            Binding::new(kind, self.num_bindings, vm_location, needs_tdz_check),
        );

        if insert_result.is_some() {
            self.has_duplicates = true;
        }

        self.num_bindings += 1;
    }

    pub fn get_binding(&self, name: &str) -> &Binding {
        self.bindings.get(name).unwrap()
    }

    pub fn get_binding_mut(&mut self, name: &str) -> &mut Binding {
        self.bindings.get_mut(name).unwrap()
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
                BindingKind::Const { .. }
                | BindingKind::Let { .. }
                | BindingKind::Class
                | BindingKind::Function { is_lexical: true, .. } => true,
                _ => false,
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BindingKind {
    Var,
    Const {
        /// The source position after which this constant has been initialized, inclusive.
        init_pos: Cell<Pos>,
    },
    Let {
        /// The source position after which this let has been initialized, inclusive.
        init_pos: Cell<Pos>,
    },
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
    CatchParameter {
        /// The source position after which this parameter has been initialized, inclusive.
        init_pos: Cell<Pos>,
    },
}

impl BindingKind {
    /// Whether this is a LexicallyScopedDeclaration or a VarScopedDeclaration from the spec.
    pub fn is_lexically_scoped(&self) -> bool {
        match self {
            BindingKind::Var | BindingKind::FunctionParameter => false,
            BindingKind::Function { is_lexical, .. } => *is_lexical,
            BindingKind::Const { .. }
            | BindingKind::Let { .. }
            | BindingKind::Class
            | BindingKind::CatchParameter { .. } => true,
        }
    }

    pub fn is_const(&self) -> bool {
        matches!(self, BindingKind::Const { .. })
    }

    pub fn is_function(&self) -> bool {
        matches!(self, BindingKind::Function { .. })
    }

    pub fn has_tdz(&self) -> bool {
        matches!(
            self,
            BindingKind::Const { .. }
                | BindingKind::Let { .. }
                | BindingKind::Class
                | BindingKind::CatchParameter { .. }
        )
    }
}

pub struct Binding {
    kind: BindingKind,
    /// Index of the binding in the scope's bindings map. Used to determine the order of duplicate
    /// bindings.
    index: usize,
    /// Whether this binding is captured by a nested function. Set during use analysis.
    is_captured: bool,
    /// If this is a const or let declaration, whether there is some use that requires a TDZ check.
    needs_tdz_check: bool,
    /// Location of the binding in the VM, must be set before bytecode generation.
    vm_location: Option<VMLocation>,
}

impl Binding {
    fn new(
        kind: BindingKind,
        index: usize,
        vm_location: Option<VMLocation>,
        needs_tdz_check: bool,
    ) -> Binding {
        Binding {
            kind,
            index,
            is_captured: false,
            needs_tdz_check,
            vm_location,
        }
    }

    pub fn kind(&self) -> &BindingKind {
        &self.kind
    }

    pub fn is_const(&self) -> bool {
        self.kind.is_const()
    }

    pub fn vm_location(&self) -> Option<VMLocation> {
        self.vm_location
    }

    pub fn set_vm_location(&mut self, location: VMLocation) {
        self.vm_location = Some(location);
    }

    pub fn needs_tdz_check(&self) -> bool {
        self.needs_tdz_check
    }
}

#[derive(Clone, Copy)]
pub enum VMLocation {
    /// Binding is in the global scope.
    Global,
    /// Argument with the given index in the current function.
    Argument(usize),
    /// Local with the given index in the current function.
    LocalRegister(usize),
    /// Captured binding with the given index in a VM scope.
    Scope { scope_id: usize, index: usize },
}

pub struct VMScopeNode {
    _bindings: Vec<String>,
}
