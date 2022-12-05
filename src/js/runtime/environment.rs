use std::{collections::HashMap, rc::Rc};

use crate::must_;

use super::{
    completion::AbstractResult,
    error::{err_uninitialized_, reference_error_, type_error_},
    value::{ObjectValue, Value},
};

// 8.1 Lexical Environment
pub struct LexicalEnvironment {
    env: Rc<dyn Environment>,
    // Optional reference to the outer (parent) environment. If None this is the global environment.
    outer: Option<Rc<LexicalEnvironment>>,
}

impl LexicalEnvironment {
    // 8.1.2.5 NewGlobalEnvironment
    pub fn new_global_environment(
        global_obj: Rc<ObjectValue>,
        global_this_val: Rc<ObjectValue>,
    ) -> LexicalEnvironment {
        let obj_env = ObjectEnvironment {
            bindings: HashMap::new(),
            binding_obj: global_obj,
        };
        let decl_env = DeclarativeEnvironment {
            bindings: HashMap::new(),
        };

        let global_env = GlobalEnvironment {
            bindings: HashMap::new(),
            object_env: Rc::new(obj_env),
            global_this_val,
            decl_env,
            var_names: vec![],
        };

        let env = LexicalEnvironment {
            env: Rc::new(global_env),
            outer: None,
        };

        env
    }

    /// Create a placeholder, "undefined" value that should not be used
    pub fn placeholder() -> LexicalEnvironment {
        LexicalEnvironment {
            env: Rc::new(DeclarativeEnvironment {
                bindings: HashMap::new(),
            }),
            outer: None,
        }
    }
}

// 8.1.1 Environment Record
pub trait Environment {
    fn has_binding(&self, name: &str) -> AbstractResult<bool>;
    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()>;
    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()>;
    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()>;
    fn set_mutable_binding(
        &mut self,
        name: String,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()>;
    fn get_binding_value(&self, name: &str, _is_strict: bool) -> AbstractResult<Value>;
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Value;
}

struct Binding {
    value: Value,
    is_mutable: bool,
    is_initialized: bool,
    is_strict: bool,
    can_delete: bool,
}

impl Binding {
    fn new(is_mutable: bool, is_strict: bool, can_delete: bool) -> Binding {
        // Value starts uninitialized
        Binding {
            value: Value::Undefined,
            is_mutable,
            is_initialized: false,
            is_strict,
            can_delete,
        }
    }
}

// 8.1.1.1 Declarative Environment Record
pub struct DeclarativeEnvironment {
    bindings: HashMap<String, Binding>,
}

impl Environment for DeclarativeEnvironment {
    // 8.1.1.1.1 HasBinding
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        self.bindings.contains_key(name).into()
    }

    // 8.1.1.1.2 CreateMutableBinding
    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        let binding = Binding::new(true, false, can_delete);
        self.bindings.insert(name.to_string(), binding);
        ().into()
    }

    // 8.1.1.1.3 CreateImmutableBinding
    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        let binding = Binding::new(false, is_strict, false);
        self.bindings.insert(name.to_string(), binding);
        ().into()
    }

    // 8.1.1.1.4 InitializeBinding
    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        let binding = self.bindings.get_mut(name).unwrap();
        binding.value = value;
        binding.is_initialized = true;
        ().into()
    }

    // 8.1.1.1.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        name: String,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        match self.bindings.get_mut(&name) {
            None if is_strict => reference_error_(&format!("{} is not defined", name)),
            None => {
                self.create_mutable_binding(name.to_string(), true);
                self.initialize_binding(&name, value);
                ().into()
            }
            Some(binding) => {
                let s = if binding.is_strict { true } else { is_strict };

                if !binding.is_initialized {
                    return err_uninitialized_(&name);
                }

                if binding.is_mutable {
                    binding.value = value;
                } else if s {
                    return type_error_(&format!("{} is immutable", name));
                }

                ().into()
            }
        }
    }

    // 8.1.1.1.6 GetBindingValue
    fn get_binding_value(&self, name: &str, _is_strict: bool) -> AbstractResult<Value> {
        let binding = self.bindings.get(name).unwrap();
        if !binding.is_initialized {
            return err_uninitialized_(name);
        }

        binding.value.clone().into()
    }

    // 8.1.1.1.7 DeleteBinding
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        let binding = self.bindings.get(name).unwrap();
        if !binding.can_delete {
            return false.into();
        }

        self.bindings.remove(name);

        true.into()
    }

    // 8.1.1.1.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        false
    }

    // 8.1.1.1.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 8.1.1.1.10 WithBaseObject
    fn with_base_object(&self) -> Value {
        Value::Undefined
    }
}

// 8.1.1.2 Declarative Environment Record
pub struct ObjectEnvironment {
    bindings: HashMap<String, Value>,
    binding_obj: Rc<ObjectValue>,
}

impl Environment for ObjectEnvironment {
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        unimplemented!()
    }

    fn set_mutable_binding(
        &mut self,
        name: String,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        unimplemented!()
    }

    fn get_binding_value(&self, name: &str, _is_strict: bool) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn has_this_binding(&self) -> bool {
        unimplemented!()
    }

    fn has_super_binding(&self) -> bool {
        unimplemented!()
    }

    fn with_base_object(&self) -> Value {
        unimplemented!()
    }
}

// 8.1.1.4 Global Environment Record
pub struct GlobalEnvironment {
    bindings: HashMap<String, Value>,

    // The global object. [[ObjectRecord]] in spec.
    object_env: Rc<ObjectEnvironment>,

    global_this_val: Rc<ObjectValue>,

    // Declarative environment. [[DeclarativeRecord]] in spec.
    decl_env: DeclarativeEnvironment,

    var_names: Vec<String>,
}

impl Environment for GlobalEnvironment {
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(name)) {
            return true.into();
        }

        self.object_env.has_binding(name)
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        unimplemented!()
    }

    fn set_mutable_binding(
        &mut self,
        name: String,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        unimplemented!()
    }

    fn get_binding_value(&self, name: &str, _is_strict: bool) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn has_this_binding(&self) -> bool {
        unimplemented!()
    }

    fn has_super_binding(&self) -> bool {
        unimplemented!()
    }

    fn with_base_object(&self) -> Value {
        unimplemented!()
    }
}
