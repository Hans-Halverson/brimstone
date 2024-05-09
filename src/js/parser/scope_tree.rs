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
    fn new_with_root(kind: ScopeNodeKind) -> ScopeTree {
        let mut scope_tree = ScopeTree {
            ast_nodes: vec![],
            vm_nodes: vec![],
            current_node_id: INITIAL_SCOPE_ID,
        };

        let global_scope_node = scope_tree.new_ast_scope_node(INITIAL_SCOPE_ID, kind, None);
        scope_tree.ast_nodes.push(Box::new(global_scope_node));

        // All root scopes start with an implicit `this` binding
        scope_tree
            .add_binding("this", BindingKind::ImplicitThis { in_derived_constructor: false })
            .unwrap();

        scope_tree
    }

    pub fn new_global() -> ScopeTree {
        Self::new_with_root(ScopeNodeKind::Global)
    }

    pub fn new_eval(is_direct: bool) -> ScopeTree {
        // Start off without setting the strict flag. This flag will be right away during parsing.
        Self::new_with_root(ScopeNodeKind::Eval { is_direct, is_strict: false })
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

    pub fn current_scope(&self) -> AstPtr<AstScopeNode> {
        AstPtr::from_ref(self.ast_nodes[self.current_node_id].as_ref())
    }

    pub fn set_current_scope(&mut self, node_id: ScopeNodeId) {
        self.current_node_id = node_id;
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
        let enclosing_scope = if let ScopeNodeKind::Global
        | ScopeNodeKind::Function { .. }
        | ScopeNodeKind::Eval { .. } = kind
        {
            // Global, function, and eval nodes are their own enclosing scope
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
            allow_empty_vm_node: false,
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

        if let Some(err) = node.error_if_lexical_name_already_declared(name) {
            return Err(err);
        }

        // Lexical bindings in a switch always need a TDZ check since it hard to analyze when
        // the check can be elided. (e.g. due to out of order default case, or definitions and
        // uses in different cases).
        let needs_tdz_check = node.kind == ScopeNodeKind::Switch;

        node.add_binding(name, kind, None, needs_tdz_check);

        // Lexically scoped bindings in the global scope must be placed in a VM scope node so that
        // they can be accessed by different scripts.
        if node.kind == ScopeNodeKind::Global {
            let binding = node.bindings.get_mut(name).unwrap();
            binding.force_vm_scope = true;
        }

        Ok(self.get_ast_node_ptr(self.current_node_id))
    }

    fn add_var_scoped_binding(&mut self, name: &str, kind: BindingKind) -> AddBindingResult {
        // Walk up to the hoist target scope, checking for conflicting lexical bindings
        let mut node_id = self.current_node_id;
        let mut in_with_statement = false;

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

            // Track if the var binding is within a with statement
            if node.kind == ScopeNodeKind::With {
                in_with_statement = true;
            }

            if node.kind.is_hoist_target() {
                let existing_binding = node.bindings.get(name);
                let can_overwrite =
                    // Function declarations overwrite all other bindings
                    kind.is_function()
                    // Function expression names are implicitly added and can always be overwritten
                    || existing_binding
                        .map(|b| b.kind().is_function_expression_name())
                        .unwrap_or(false);

                if existing_binding.is_none() || can_overwrite {
                    let vm_location = if in_with_statement {
                        // Var bindings in a with statement require a dynamic lookup at runtime as
                        // they may reference a true var or a property of the with object.
                        Some(VMLocation::WithVar)
                    } else if node.kind == ScopeNodeKind::Global && !kind.is_implicit_this() {
                        // Var bindings in the global scope are immediately known to be global
                        Some(VMLocation::Global)
                    } else if matches!(node.kind, ScopeNodeKind::Eval { is_strict: false, .. })
                        && !kind.is_implicit_this()
                    {
                        // Var bindings in a sloppy eval are dynamically added to their parent
                        // var scope, and require a dynamic lookup at runtime.
                        Some(VMLocation::EvalVar)
                    } else {
                        None
                    };

                    node.add_binding(name, kind, vm_location, /* needs_tdz_check */ false);
                } else if existing_binding.is_some() && in_with_statement {
                    // Should treat as a WithVar binding if any var binding is within a with
                    node.bindings
                        .get_mut(name)
                        .unwrap()
                        .set_vm_location(VMLocation::WithVar)
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

    /// Add a binding to the current scope in the AST scope tree.
    pub fn add_binding_to_current_node(&mut self, name: &str, kind: BindingKind) {
        self.get_ast_node_mut(self.current_node_id)
            .add_binding(name, kind, /* vm_location */ None, /* needs_tdz_check */ false);
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
            let scope_kind = scope.kind;
            let is_sloppy_eval_scope =
                matches!(scope_kind, ScopeNodeKind::Eval { is_strict: false, .. });
            let scope_supports_dynamic_bindings = scope.supports_dynamic_bindings;

            let mut found_def = scope.bindings.contains_key(name);

            // Arguments object bindings and new.target bindings are lazily added when we determine
            // that one is needed. We know that one is needed if we are looking up e.g. "arguments"
            // and it has not been resolved to a binding by the time we reach the function scope.
            if !found_def && name == "arguments" && self.needs_implicit_arguments_object(scope_id) {
                self.add_implicit_arguments_object(scope_id);
                found_def = true;
            }

            if !found_def
                && name == NEW_TARGET_BINDING_NAME
                && self.needs_implicit_new_target(scope_id)
            {
                self.add_implicit_new_target(scope_id);
                found_def = true;
            }

            if found_def {
                let mut is_capture = false;

                // If the use is in a different function than the definition, mark the
                // binding as captured.
                let binding = if self.lookup_enclosing_function(scope_id)
                    != self.lookup_enclosing_function(use_scope_id)
                {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(name).unwrap();
                    binding.is_captured = true;
                    is_capture = true;

                    binding
                } else {
                    let bindings = &mut self.get_ast_node_mut(scope_id).bindings;
                    let binding = bindings.get_mut(name).unwrap();

                    // For const and let bindings in the same scope, check if the use is before the
                    // end of the binding's initialization. If so we need to check for the TDZ.
                    if let BindingKind::Const { init_pos }
                    | BindingKind::Let { init_pos }
                    | BindingKind::Class { init_pos, in_body_scope: false }
                    | BindingKind::CatchParameter { init_pos }
                    | BindingKind::FunctionParameter { init_pos, .. } = binding.kind()
                    {
                        if name_loc.start < init_pos.get() {
                            binding.needs_tdz_check = true;
                        }
                    }

                    // Home object bindings and class names in the scope body must always be stored
                    // in a VM scope. Do not need to set this flag in other case since binding will
                    // have been captured.
                    if matches!(
                        binding.kind(),
                        BindingKind::Class { in_body_scope: true, .. } | BindingKind::HomeObject
                    ) {
                        binding.force_vm_scope = true;
                    }

                    binding
                };

                // Var bindings in sloppy direct evals are added to the parent var scope, which
                // requires a dynamic lookup at runtime.
                if is_sloppy_eval_scope && !binding.kind().is_lexically_scoped() {
                    return (TaggedResolvedScope::unresolved_dynamic(), false);
                }

                let resolved_scope = TaggedResolvedScope::resolved(self.get_ast_node_ptr(scope_id));
                return (resolved_scope, is_capture);
            }

            // If we have not yet found the binding when exiting a scope that may contain dynamic
            // bindings, then the use is not statically resolvable.
            //
            // Note that `this` bindings can still be searched for past dynamic scopes.
            if scope_supports_dynamic_bindings && name != "this" {
                return (TaggedResolvedScope::unresolved_dynamic(), false);
            }

            // If we have not yet found the binding when exiting an eval scope, then the use is not
            // statically resolvable. Direct evals look up in their environment, while indirect
            // evals are known to be in the global scope so they can look up globally.
            if let ScopeNodeKind::Eval { is_direct, .. } = scope_kind {
                if is_direct {
                    return (TaggedResolvedScope::unresolved_dynamic(), false);
                } else {
                    return (TaggedResolvedScope::unresolved_global(), false);
                }
            }

            if let Some(parent_id) = scope_parent {
                scope_id = parent_id;
            } else {
                return (TaggedResolvedScope::unresolved_global(), false);
            }
        }
    }

    /// Look up the enclosing scope and function id for an AST scope node (aka the most recent
    /// function scope ancestor). If this is the global or eval scope then return None.
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
                // and new.target since any binding could be looked up.
                //
                // IMPROVEMENT: new.target does not need to be added above `with` scopes
                if self.needs_implicit_arguments_object(scope_id) {
                    self.add_implicit_arguments_object(scope_id);
                }

                if self.needs_implicit_new_target(scope_id) {
                    self.add_implicit_new_target(scope_id);
                }
            }

            if let Some(parent_id) = scope_parent {
                scope_id = parent_id;
            } else {
                return;
            }
        }
    }

    /// Handle the presence of a sloppy direct eval in the current scope. This means the var hoist
    /// target of the current scope must support dynamic bindings, since the sloppy direct eval
    /// could introduce var bindings into that hoist target scope.
    pub fn mark_sloppy_direct_eval(&mut self) {
        let mut scope_id = self.current_node_id;
        loop {
            let scope = self.get_ast_node_mut(scope_id);

            match scope.kind {
                // No need to mark the global scope as supporting dynamic bindings
                ScopeNodeKind::Global => return,
                // Functions must be marked to support dynamic bindings
                ScopeNodeKind::Function { .. } | ScopeNodeKind::FunctionBody => {
                    scope.supports_dynamic_bindings = true;
                    return;
                }
                // Only strict eval needs to support dynamic bindings. In sloppy eval bindings are
                // added to the parent var scope.
                ScopeNodeKind::Eval { is_strict, .. } => {
                    if is_strict {
                        scope.supports_dynamic_bindings = true;
                    }

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

    fn needs_implicit_new_target(&mut self, scope_id: ScopeNodeId) -> bool {
        let scope = self.get_ast_node(scope_id);
        matches!(scope.kind, ScopeNodeKind::Function { is_arrow: false, .. })
            && !scope.bindings.contains_key(NEW_TARGET_BINDING_NAME)
    }

    fn add_implicit_new_target(&mut self, scope_id: ScopeNodeId) {
        self.get_ast_node_mut(scope_id).add_binding(
            NEW_TARGET_BINDING_NAME,
            BindingKind::ImplicitNewTarget,
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
        num_extra_slots: Option<usize>,
    ) {
        let vm_node_id = self.vm_nodes.len();
        let ast_node = self.get_ast_node_mut(ast_node_id);
        let enclosing_scope = ast_node.enclosing_scope;

        let mut bindings = vec![];
        let has_mapped_arguments_object =
            num_extra_slots.is_some() && matches!(ast_node.kind(), ScopeNodeKind::Function { .. });

        // The first slot in a global scope always contains the realm
        if ast_node.kind() == ScopeNodeKind::Global {
            bindings.push(REALM_SCOPE_SLOT_NAME.to_owned());
        } else if has_mapped_arguments_object {
            // A mapped arguments object requires that all arguments be placed in the VM scope node
            // in order.

            // Bindings start out with unresolvable "%private" for each argument name, which will be
            // overriden by the actual binding name for accessible arguments. Some arguments cannot
            // be accessed by name (since they are shadowed by another binding) and will remain
            // unresolvable.
            bindings = vec![SHADOWED_SCOPE_SLOT_NAME.to_owned(); num_extra_slots.unwrap()];

            for (name, binding) in ast_node.bindings.iter_mut() {
                let arg_index = if let BindingKind::FunctionParameter { index, .. } = binding.kind()
                {
                    *index as usize
                } else {
                    continue;
                };

                // Do not overwrite a WithVar, which will be dynamically accessed
                if !matches!(binding.vm_location(), Some(VMLocation::WithVar)) {
                    binding.set_vm_location(VMLocation::Scope {
                        scope_id: vm_node_id,
                        index: arg_index,
                    });
                }

                bindings[arg_index] = name.clone();
            }
        } else if num_extra_slots.is_some() && ast_node.kind() == ScopeNodeKind::Class {
            // Extra slots for a class scope will hold computed and private field names
            bindings = vec![CLASS_FIELD_SLOT_NAME.to_owned(); num_extra_slots.unwrap()];
        }

        // Collect all bindings that must appear in a VM scope node
        for (name, binding) in ast_node.bindings.iter_mut() {
            // Function parameters for mapped arguments object have already been placed in the scope
            if has_mapped_arguments_object
                && matches!(binding.kind(), BindingKind::FunctionParameter { .. })
            {
                continue;
            }

            let is_global = matches!(binding.vm_location, Some(VMLocation::Global));
            let is_eval_var = matches!(binding.vm_location, Some(VMLocation::EvalVar));
            let is_with_var = matches!(binding.vm_location, Some(VMLocation::WithVar));

            let needs_vm_scope = (
                // All captured bindings must be placed in a VM scope
                binding.is_captured ||
                binding.force_vm_scope ||
                // All private names must be placed in a VM scope
                matches!(binding.kind(), BindingKind::PrivateName) ||
                // We sometimes force bindings in VM scopes due to dynamic accesses.
                // e.g. in the presence of `eval` or `with`
                //
                // No need to place the global `this` in a VM scope unless it is captured.
                (ast_node.supports_dynamic_access &&
                    (name != "this" || ast_node.kind != ScopeNodeKind::Global))
            ) && (
                // Eval var bindings will already be placed in VM scope objects during eval
                // declaration instantiation.
                !is_eval_var
            ) && (
                // With var bindings will be placed in VM scope objects due to dynamic loads and
                // stores.
                !is_with_var
            );

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

                // Lexical bindings in VM scope nodes need a TDZ check as we do not analyze whether
                // they are guaranteed to be initialized before use.
                if !matches!(binding.kind(), BindingKind::FunctionParameter { .. }) {
                    binding.needs_tdz_check = true;
                }
            }

            // With vars still need to be added to the VM scope (except in the global scope, where
            // it will be added to the global object).
            if is_with_var && ast_node.kind != ScopeNodeKind::Global {
                bindings.push(name.clone());
            }

            // All globals need a TDZ check as they could be captured by any function
            if is_global {
                binding.needs_tdz_check = true;
            }
        }

        // Only if there are bindings to place in the scope do we need to create a VM scope node.
        // Any scopes that could contain dynamic bindings must also have a VM scope node to hold the
        // dynamic object. This includes all with scopes.
        if !bindings.is_empty()
            || ast_node.supports_dynamic_bindings
            || ast_node.allow_empty_vm_node
        {
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
            //  - Except for implicit this, which is either captured or loaded from the this slot.
            //  - Except for class name binding in body scope, which is always stored in scope
            for enclosed_scope in enclosed_scopes.iter().rev() {
                let enclosed_scope = self.get_ast_node_mut(*enclosed_scope);
                for (_, binding) in enclosed_scope.bindings.iter_mut() {
                    if binding.vm_location.is_none()
                        && !matches!(
                            binding.kind(),
                            BindingKind::ImplicitThis { .. }
                                | BindingKind::HomeObject
                                | BindingKind::Class { in_body_scope: true, .. }
                        )
                    {
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
        /// Whether this is a function expression
        is_expression: bool,
    },
    /// A function body scope only appears when the function has parameter expressions. In this case
    /// the function body is the hoist target for all declarations in the body, keeping them
    /// separate from the parameters.
    FunctionBody,
    Block,
    Switch,
    /// Body of a class declaration.
    Class,
    With,
    /// Scope created within an eval. Var bindings of a sloppy direct eval are added to the parent
    /// var scope.
    Eval {
        /// Whether this is a direct or indirect eval.
        is_direct: bool,
        /// Whether the eval body is in strict mode.
        is_strict: bool,
    },
    /// Scope for a static initializer block of a class. Acts as a var hoist target.
    StaticInitializer,
}

impl ScopeNodeKind {
    fn is_hoist_target(&self) -> bool {
        match self {
            ScopeNodeKind::Global
            | ScopeNodeKind::Function { .. }
            | ScopeNodeKind::FunctionBody
            // Sloppy eval will add var bindings to the parent var scope. However we still consider
            // the eval scope to be a hoist target during analysis.
            | ScopeNodeKind::Eval { .. }
            | ScopeNodeKind::StaticInitializer => true,
            ScopeNodeKind::Block | ScopeNodeKind::Switch | ScopeNodeKind::Class | ScopeNodeKind::With => false,
        }
    }

    pub fn is_function_expression(&self) -> bool {
        matches!(self, ScopeNodeKind::Function { is_expression: true, .. })
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
    /// Whether to create a VM node when this AST scope node has no bindings.
    allow_empty_vm_node: bool,
    /// The most recent ancestor global, function, or eval AST scope node, meaning the node that
    /// this scope's locals will be placed in. For global, function, and eval nodes this points to
    /// itself.
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

const INITIAL_SCOPE_ID: ScopeNodeId = 0;

impl AstScopeNode {
    pub fn id(&self) -> ScopeNodeId {
        self.id
    }

    pub fn kind(&self) -> ScopeNodeKind {
        self.kind
    }

    pub fn set_kind(&mut self, kind: ScopeNodeKind) {
        self.kind = kind;
    }

    pub fn num_local_registers(&self) -> usize {
        self.num_local_registers
    }

    pub fn vm_scope_id(&self) -> Option<ScopeNodeId> {
        self.vm_scope
    }

    pub fn supports_dynamic_bindings(&self) -> bool {
        self.supports_dynamic_bindings
    }

    pub fn set_allow_empty_vm_node(&mut self, value: bool) {
        self.allow_empty_vm_node = value;
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

    pub fn has_binding(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
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
                | BindingKind::Class { .. }
                | BindingKind::Function { is_lexical: true, .. } => true,
                _ => false,
            })
    }

    /// Error if the provided lexical name is already declared in this scope.
    pub fn error_if_lexical_name_already_declared(&self, name: &str) -> Option<ParseError> {
        // Error if there is already any binding with this name in the current scope, which is
        // guaranteed to detect conflicting lexically scoped bindings and var scoped bindings
        // declared in this scope.
        if let Some(binding) = self.bindings.get(name) {
            // Function expression name bindings can always be overriden
            if !binding.kind().is_function_expression_name() {
                return Some(ParseError::NameRedeclaration(name.to_owned(), binding.kind.clone()));
            }
        }

        // Then check for other conflicting var scoped bindings, e.g. in child scopes
        if self.extra_var_names.contains(name) {
            return Some(ParseError::NameRedeclaration(
                name.to_owned(),
                // Guaranteed to conflict with a `var` binding, as the other var scoped names -
                // var scoped functions and function parameters - cannot appear in child scopes.
                BindingKind::Var,
            ));
        }

        None
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
        /// Whether this is the name of a function expression.
        is_expression: bool,
        /// Function AST node - needed for the tree walk interpreter.
        func_node: AstPtr<ast::Function>,
    },
    FunctionParameter {
        /// The source position after which this parameter has been initialized, inclusive.
        init_pos: Cell<Pos>,
        /// Index of the parameter in the function's parameter list.
        index: u32,
    },
    Class {
        /// Whether this is the class name binding inside the class body, as opposed to in the
        /// surrounding scope.
        in_body_scope: bool,
        /// The source position after which this class has been initialized, inclusive. Only set
        /// for the class name binding in the parent scope, not the binding in the class body.
        init_pos: Cell<Pos>,
    },
    CatchParameter {
        /// The source position after which this parameter has been initialized, inclusive.
        init_pos: Cell<Pos>,
    },
    /// An implicit `this` binding introduced in a function or root scope.
    ImplicitThis {
        /// Whether this is the `this` value for a derived constructor.
        in_derived_constructor: bool,
    },
    /// An implicit `arguments` binding introduced in a function. Note that any var or var function
    /// is treated as an "explicit" `arguments` binding and will require an arguments object.
    ImplicitArguments,
    /// new.target is treated as a binding.
    ImplicitNewTarget,
    /// The constructor itself is treated as a binding in derived constructors, since super calls
    /// need to look up the constructor.
    DerivedConstructor,
    /// The home object for methods defined on an object literal or class.
    HomeObject,
    /// A private name introduced in a class. Includes the "#" prefix.
    PrivateName,
}

impl BindingKind {
    pub fn new_function_parameter(index: u32) -> BindingKind {
        BindingKind::FunctionParameter { init_pos: Cell::new(0), index }
    }

    /// Whether this is a LexicallyScopedDeclaration from the spec.
    pub fn is_lexically_scoped(&self) -> bool {
        match self {
            BindingKind::Var
            | BindingKind::FunctionParameter { .. }
            | BindingKind::ImplicitThis { .. }
            | BindingKind::ImplicitArguments
            | BindingKind::ImplicitNewTarget
            | BindingKind::DerivedConstructor
            | BindingKind::HomeObject
            | BindingKind::PrivateName => false,
            BindingKind::Function { is_lexical, .. } => *is_lexical,
            BindingKind::Const { .. }
            | BindingKind::Let { .. }
            | BindingKind::Class { .. }
            | BindingKind::CatchParameter { .. } => true,
        }
    }

    pub fn is_immutable(&self) -> bool {
        matches!(self, BindingKind::Const { .. })
            | matches!(self, BindingKind::Class { in_body_scope: true, .. })
    }

    pub fn is_function(&self) -> bool {
        matches!(self, BindingKind::Function { .. })
    }

    pub fn is_function_expression_name(&self) -> bool {
        matches!(self, BindingKind::Function { is_expression: true, .. })
    }

    pub fn is_implicit_this(&self) -> bool {
        matches!(self, BindingKind::ImplicitThis { .. })
    }

    pub fn is_implicit_arguments(&self) -> bool {
        matches!(self, BindingKind::ImplicitArguments)
    }

    pub fn is_function_parameter(&self) -> bool {
        matches!(self, BindingKind::FunctionParameter { .. })
    }

    pub fn is_private_name(&self) -> bool {
        matches!(self, BindingKind::PrivateName)
    }

    fn has_tdz(&self) -> bool {
        matches!(
            self,
            BindingKind::Const { .. }
                | BindingKind::Let { .. }
                | BindingKind::Class { .. }
                | BindingKind::CatchParameter { .. }
                | BindingKind::FunctionParameter { .. }
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
    /// Whether to force this binding to be in a VM scope. Set during use analysis.
    force_vm_scope: bool,
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
            force_vm_scope: false,
            needs_tdz_check,
            vm_location,
        }
    }

    pub fn kind(&self) -> &BindingKind {
        &self.kind
    }

    pub fn is_immutable(&self) -> bool {
        self.kind.is_immutable()
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
        self.kind().has_tdz() && self.needs_tdz_check
    }
}

#[derive(Clone, Copy)]
pub enum VMLocation {
    /// Var binding in the global scope.
    Global,
    /// Argument with the given index in the current function.
    Argument(usize),
    /// Local with the given index in the current function.
    LocalRegister(usize),
    /// Captured binding with the given index in a VM scope.
    Scope { scope_id: usize, index: usize },
    /// In sloppy evals vars are added to the parent var scope at runtime. This means they must be
    /// dynamically looked up by name at runtime, and are defined via special instructions.
    EvalVar,
    /// Vars defined in a with statement must be dynamically loaded and stored since they may be a
    /// normal var or they may be a property of the with object.
    WithVar,
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
pub const REALM_SCOPE_SLOT_NAME: &str = "%realm";
pub const NEW_TARGET_BINDING_NAME: &str = "%new.target";
pub const DERIVED_CONSTRUCTOR_BINDING_NAME: &str = "%constructor";
pub const HOME_OBJECT_BINDING_NAME: &str = "%homeObject";
pub const STATIC_HOME_OBJECT_BINDING_NAME: &str = "%staticHomeObject";
const CLASS_FIELD_SLOT_NAME: &str = "%classField";
