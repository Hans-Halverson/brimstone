use std::{cell::Cell, collections::HashSet};

use indexmap::IndexMap;

use super::{
    ast::{self, AstPtr, FunctionId, TaggedResolvedScope},
    loc::{Loc, Pos},
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

        // All global scopes start with an implicit `this` binding
        scope_tree
            .add_binding("this", BindingKind::ImplicitThis { is_global: true })
            .unwrap();

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

    pub fn get_vm_node(&self, node_id: ScopeNodeId) -> &VMScopeNode {
        &self.vm_nodes[node_id]
    }

    fn new_ast_scope_node(
        &mut self,
        id: ScopeNodeId,
        kind: ScopeNodeKind,
        parent: Option<ScopeNodeId>,
    ) -> AstScopeNode {
        let enclosing_scope = if let ScopeNodeKind::Global | ScopeNodeKind::Function { .. } = kind {
            // Global and function nodes are their own enclosing scope
            id
        } else {
            // Otherwise inherit enclosing scope from parent scope
            self.get_ast_node(parent.unwrap()).enclosing_scope
        };

        let mut extra_var_names = HashSet::new();

        // Function body scopes must know the names of all parameter names in the parent function
        // scope (to check for conflicts against lexical declarations).
        if kind == ScopeNodeKind::FunctionBody {
            let func_scope = self.get_ast_node(parent.unwrap());
            // All bindings in function scope are gaurantueed to be function parameters if a
            // function body scope is created.
            for (name, _) in func_scope.iter_bindings() {
                extra_var_names.insert(name.clone());
            }
        }

        // With scope nodes always have dynamic bindings
        let supports_dynamic_bindings = kind == ScopeNodeKind::With;

        AstScopeNode {
            id,
            parent,
            kind,
            bindings: IndexMap::new(),
            extra_var_names,
            num_bindings: 0,
            has_duplicates: false,
            supports_dynamic_access: false,
            supports_dynamic_bindings,
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

        // Then check for other conflicting var scoped bindings, e.g. in child scopes
        if node.extra_var_names.contains(name) {
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
            ScopeNodeKind::Switch => {
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
                    let vm_location =
                        if node.kind == ScopeNodeKind::Global && !kind.is_implicit_this() {
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
                node.extra_var_names.insert(name.to_owned());
            }

            node_id = node.parent.unwrap();
        }
    }

    /// Resolve a use of the given name in the provided AST scope id to the scope where the binding
    /// with that name is defined. If the name could not be statically resolved to a definition then
    /// return None.
    ///
    /// Marks the binding as captured if it is used by a nested function. Returns the resolved scope
    /// and whether the use is a capture.
    pub fn resolve_use(
        &mut self,
        use_scope_id: ScopeNodeId,
        name: &str,
        name_loc: Loc,
    ) -> (TaggedResolvedScope, bool) {
        let mut scope_id = use_scope_id;
        loop {
            let scope = self.get_ast_node(scope_id);
            let scope_parent = scope.parent;
            let scope_supports_dynamic_bindings = scope.supports_dynamic_bindings;

            let mut found_def = scope.bindings.contains_key(name);

            // Arguments object bindings are lazily added when we determine that one is needed.
            // We know that one is needed if we are looking up "arguments" and it has not been
            // resolved to a binding by the time we reach the function scope.
            if !found_def && name == "arguments" && self.needs_implicit_arguments_object(scope_id) {
                self.add_implicit_arguments_object(scope_id);
                found_def = true;
            }

            if found_def {
                let mut is_capture = false;

                // If the use is in a different function than the definition, mark the
                // binding as captured.
                if self.lookup_enclosing_function(scope_id)
                    != self.lookup_enclosing_function(use_scope_id)
                {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(name).unwrap();
                    binding.is_captured = true;
                    is_capture = true;
                } else {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(name).unwrap();

                    // For const and let bindings in the same scope, check if the use is before the
                    // end of the binding's initialization. If so we need to check for the TDZ.
                    if let BindingKind::Const { init_pos }
                    | BindingKind::Let { init_pos }
                    | BindingKind::CatchParameter { init_pos } = binding.kind()
                    {
                        if name_loc.start < init_pos.get() {
                            binding.needs_tdz_check = true;
                        }
                    }
                }

                let resolved_scope = TaggedResolvedScope::resolved(self.get_ast_node_ptr(scope_id));

                return (resolved_scope, is_capture);
            }

            // If we have not yet found the binding when exiting a with scope, the use is not
            // statically resolvable.
            //
            // Note that `this` bindings can still be searched for past dynamic scopes.
            if scope_supports_dynamic_bindings && name != "this" {
                return (TaggedResolvedScope::unresolved_dynamic(), false);
            }

            if let Some(parent_id) = scope_parent {
                scope_id = parent_id;
            } else {
                return (TaggedResolvedScope::unresolved_global(), false);
            }
        }
    }

    /// Look up the enclosing scope and function id for an AST scope node (aka the most recent
    /// function scope ancestor). If this is the global scope then return None.
    fn lookup_enclosing_function(&self, scope_id: ScopeNodeId) -> Option<FunctionId> {
        let node = self.get_ast_node(scope_id);
        let enclosing_scope = self.get_ast_node(node.enclosing_scope);

        if let ScopeNodeKind::Function { id: func_id, .. } = enclosing_scope.kind {
            Some(func_id)
        } else {
            None
        }
    }

    /// Mark all scopes at the provided AST scope and above as being dynamically accessed, e.g. due
    /// to the presence of `eval` or `with`.
    pub fn support_dynamic_access_in_visible_bindings(&mut self, scope_id: ScopeNodeId) {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_ast_node_mut(scope_id);
            let scope_parent = scope.parent;

            if !scope.supports_dynamic_access {
                scope.supports_dynamic_access = true;

                // Any function scope that may be dynamically accessed must have an arguments object
                if self.needs_implicit_arguments_object(scope_id) {
                    self.add_implicit_arguments_object(scope_id);
                }
            }

            if let Some(parent_id) = scope_parent {
                scope_id = parent_id;
            } else {
                return;
            }
        }
    }

    /// Handle the presence of a sloppy direct eval in a scope. This means the var hoist target of
    /// the scope must support dynamic bindings, since the sloppy direct eval could introduce var
    /// bindings into that hoist target scope.
    pub fn mark_sloppy_direct_eval_in_scope(&mut self, scope_id: ScopeNodeId) {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_ast_node_mut(scope_id);

            match scope.kind {
                // No need to mark the global scope as supporting dynamic bindings
                ScopeNodeKind::Global => return,
                // Function var scope must suport dynamic bindings
                ScopeNodeKind::Function { .. } | ScopeNodeKind::FunctionBody => {
                    scope.supports_dynamic_bindings = true;
                    return;
                }
                _ => {
                    if let Some(parent_id) = scope.parent {
                        scope_id = parent_id;
                    } else {
                        panic!("could not find hoist target to mark as dynamic");
                    }
                }
            }
        }
    }

    fn needs_implicit_arguments_object(&mut self, scope_id: ScopeNodeId) -> bool {
        let scope = self.get_ast_node(scope_id);
        matches!(scope.kind, ScopeNodeKind::Function { is_arrow: false, .. })
            && !scope.bindings.contains_key("arguments")
    }

    fn add_implicit_arguments_object(&mut self, scope_id: ScopeNodeId) {
        self.get_ast_node_mut(scope_id).add_binding(
            "arguments",
            BindingKind::ImplicitArguments,
            None,
            false,
        );
    }
}

/// Functions for constructing the VM scope tree.
impl ScopeTree {
    /// Complete an AST scope node and create a corresponding VM scope node if necessary.
    ///
    /// Optionally provide the number of arguments for a mapped arguments object if one needs to be
    /// created.
    pub fn finish_vm_scope_node(
        &mut self,
        ast_node_id: ScopeNodeId,
        arguments_object_length: Option<usize>,
    ) {
        let vm_node_id = self.vm_nodes.len();
        let ast_node = self.get_ast_node_mut(ast_node_id);
        let enclosing_scope = ast_node.enclosing_scope;

        let mut bindings = vec![];

        // A mapped arguments object requires that all arguments be placed in the VM scope node
        // in order.
        let has_mapped_arguments_object = arguments_object_length.is_some();
        if has_mapped_arguments_object {
            // Bindings start out with unresolvable "%private" for each argument name, which will be
            // overriden by the actual binding name for accessible arguments. Some arguments cannot
            // be accessed by name (since they are shadowed by another binding) and will remain
            // unresolvable.
            bindings = vec![SHADOWED_SCOPE_SLOT_NAME.to_owned(); arguments_object_length.unwrap()];

            for (name, binding) in ast_node.bindings.iter_mut() {
                if binding.kind() != &BindingKind::FunctionParameter {
                    continue;
                }

                let arg_index = if let Some(VMLocation::Argument(arg_index)) = binding.vm_location {
                    arg_index
                } else {
                    unreachable!(
                        "function param bindings in simple params must have argument location"
                    )
                };

                binding
                    .set_vm_location(VMLocation::Scope { scope_id: vm_node_id, index: arg_index });

                bindings[arg_index] = name.clone();
            }
        }

        // Collect all bindings that must appear in a VM scope node
        for (name, binding) in ast_node.bindings.iter_mut() {
            // Function parameters for mapped arguments object have already been placed in the scope
            if has_mapped_arguments_object && binding.kind() == &BindingKind::FunctionParameter {
                continue;
            }

            let is_global = matches!(binding.vm_location, Some(VMLocation::Global));

            let needs_vm_scope =
                // All captured bindings must be placed in a VM scope
                binding.is_captured ||
                // We sometimes force bindings in VM scopes due to dynamic accesses.
                // e.g. in the presence of `eval` or `with`
                //
                // No need to place the global `this` in a VM scope unless it is captured.
                (ast_node.supports_dynamic_access &&
                    (name != "this" || ast_node.kind != ScopeNodeKind::Global));

            if needs_vm_scope {
                // Global bindings are always accessible so no need to place in VM scope. The only
                // exception is the global `this` which must be accessed through a VM scope.
                if !is_global || binding.is_implicit_this() {
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

        // Only if there are bindings to place in the scope do we need to create a VM scope node.
        // With scope also always need a VM scope node for their target object.
        if !bindings.is_empty() || ast_node.kind() == ScopeNodeKind::With {
            self.vm_nodes.push(VMScopeNode { bindings });
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

            // All bindings without a location are placed in local registers in the enclosing scope.
            // (Except for implicit this, which is either captured or loaded from the this slot).
            for enclosed_scope in enclosed_scopes.iter().rev() {
                let enclosed_scope = self.get_ast_node_mut(*enclosed_scope);
                for (_, binding) in enclosed_scope.bindings.iter_mut() {
                    if binding.vm_location.is_none() && !binding.is_implicit_this() {
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
    Function {
        /// A unique identifier for the function. Currently the function's starting source position.
        id: FunctionId,
        /// Whether this is an arrow function
        is_arrow: bool,
    },
    /// A function body scope only appears when the function has parameter expressions. In this case
    /// the function body is the hoist target for all declarations in the body, keeping them
    /// separate from the parameters.
    FunctionBody,
    Block,
    Switch,
    With,
}

impl ScopeNodeKind {
    fn is_hoist_target(&self) -> bool {
        match self {
            ScopeNodeKind::Global
            | ScopeNodeKind::Function { .. }
            | ScopeNodeKind::FunctionBody => true,
            ScopeNodeKind::Block | ScopeNodeKind::Switch | ScopeNodeKind::With => false,
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
    /// - Includes the set of all var declared names in child scopes
    /// - For function body scopes, includes the parameter names
    extra_var_names: HashSet<String>,
    /// Total number of bindings added to map, including duplicates.
    num_bindings: usize,
    /// Whether at least one duplicate binding was added to map, replacing an earlier binding.
    has_duplicates: bool,
    /// Whether this scope may be dynamically accessed (meaning there may be runtime lookups by
    /// name, e.g. due to the presence of `eval` or `with`). This means all bindings in this scope
    /// must be placed in a VM scope node so they can be accessed at runtime.
    supports_dynamic_access: bool,
    /// Whether this scope may have bindings dynamically added to it at runtime, preventing static
    /// analysis e.g. due to the presence of `eval` or `with`.
    supports_dynamic_bindings: bool,
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

    pub fn kind(&self) -> ScopeNodeKind {
        self.kind
    }

    pub fn num_local_registers(&self) -> usize {
        self.num_local_registers
    }

    pub fn vm_scope_id(&self) -> Option<ScopeNodeId> {
        self.vm_scope
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

    pub fn get_binding_opt(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }

    pub fn get_binding_mut(&mut self, name: &str) -> &mut Binding {
        self.bindings.get_mut(name).unwrap()
    }

    /// Iterator over all bindings in this scope, including funtion parameters and catch parameters.
    pub fn iter_bindings(&self) -> impl DoubleEndedIterator<Item = (&String, &Binding)> {
        self.bindings.iter()
    }

    /// Iterator over all VarScopedDeclarations in this scope. Note that this does not include
    /// function parameters or implicit function bindings, which are var scoped but do not count as
    /// VarScopedDeclarations.
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
    /// An implicit `this` binding introduced in a function or global scope.
    ImplicitThis {
        /// Whether this is `this` in the global scope.
        is_global: bool,
    },
    /// An implicit `arguments` binding introduced in a function. Note that any var or var function
    /// is treated as an "explicit" `arguments` binding and will require an arguments object.
    ImplicitArguments,
}

impl BindingKind {
    /// Whether this is a LexicallyScopedDeclaration from the spec.
    pub fn is_lexically_scoped(&self) -> bool {
        match self {
            BindingKind::Var
            | BindingKind::FunctionParameter
            | BindingKind::ImplicitThis { .. }
            | BindingKind::ImplicitArguments => false,
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

    pub fn is_implicit_this(&self) -> bool {
        matches!(self, BindingKind::ImplicitThis { .. })
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

    /// An binding with the name `arguments` requires that an arguments object be created if:
    /// - It is an implicit `arguments` binding (i.e. a use of `arguments` which reaches a function
    ///   when resolved to a binding).
    /// - It is an explicit `arguments` binding (i.e. a var or var function declaration with the
    ///   name `arguments`).
    pub fn is_valid_arguments_kind(&self) -> bool {
        matches!(
            self,
            BindingKind::Var
                | BindingKind::Function { is_lexical: false, .. }
                | BindingKind::ImplicitArguments
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

    pub fn is_implicit_this(&self) -> bool {
        self.kind.is_implicit_this()
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
    bindings: Vec<String>,
}

impl VMScopeNode {
    #[inline]
    pub fn bindings(&self) -> &[String] {
        &self.bindings
    }
}

pub const SHADOWED_SCOPE_SLOT_NAME: &str = "%shadowed";
