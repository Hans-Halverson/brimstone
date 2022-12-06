use super::environment::Environment;

use crate::js::runtime::{
    completion::AbstractResult,
    error::{err_not_defined_, err_uninitialized_, type_error_},
    value::Value,
};

use std::collections::HashMap;

pub struct Binding {
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
    pub bindings: HashMap<String, Binding>,
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
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        match self.bindings.get_mut(name) {
            None if is_strict => err_not_defined_(name),
            None => {
                self.create_mutable_binding(name.to_string(), true);
                self.initialize_binding(name, value);
                ().into()
            }
            Some(binding) => {
                let s = if binding.is_strict { true } else { is_strict };

                if !binding.is_initialized {
                    return err_uninitialized_(name);
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
