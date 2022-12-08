use std::rc::Rc;

use crate::js::runtime::{
    abstract_operations::{define_property_or_throw, get, has_property, set, to_boolean},
    completion::AbstractResult,
    error::err_not_defined_,
    gc::Gc,
    value::{ObjectValue, Value},
    Context,
};
use crate::maybe_;

use super::environment::{Environment, LexicalEnvironment};

// 8.1.1.2 Object Environment Record
pub struct ObjectEnvironment {
    pub binding_object: Gc<ObjectValue>,
    pub with_environment: bool,
}

impl ObjectEnvironment {
    // 8.1.2.3 NewObjectEnvironment
    fn new(binding_object: Gc<ObjectValue>, outer: Rc<LexicalEnvironment>) -> LexicalEnvironment {
        let obj_env = ObjectEnvironment {
            binding_object,
            with_environment: false,
        };

        LexicalEnvironment {
            env: Rc::new(obj_env),
            outer: Some(outer),
        }
    }
}

impl Environment for ObjectEnvironment {
    // 8.1.1.2.1 HasBinding
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        if !maybe_!(has_property(self.binding_object, name)) {
            return false.into();
        } else if !self.with_environment {
            return true.into();
        }

        // Ignore properties in @@unscopables
        let unscopables = maybe_!(get(self.binding_object, "@@unscopables"));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe_!(get(unscopables, name));
            let blocked = to_boolean(&value);
            if blocked {
                return false.into();
            }
        }

        true.into()
    }

    // 8.1.1.2.1 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> AbstractResult<()> {
        let prop_desc = ObjectValue::new_with_value_4(
            "value".to_string(),
            Value::undefined(),
            "writable".to_string(),
            true.into(),
            "enumerable".to_string(),
            true.into(),
            "configurable".into(),
            can_delete.into(),
        );

        define_property_or_throw(self.binding_object, &name, prop_desc)
    }

    // 8.1.1.2.3 CreateImmutableBinding
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> AbstractResult<()> {
        unreachable!("ObjectEnvironment::create_immutable_binding is never used in spec")
    }

    // 8.1.1.2.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
    ) -> AbstractResult<()> {
        self.set_mutable_binding(cx, name, value, false)
    }

    // 8.1.1.2.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        _: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        maybe_!(set(self.binding_object, name, value, is_strict));
        ().into()
    }

    // 8.1.1.2.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> AbstractResult<Value> {
        if !maybe_!(has_property(self.binding_object, name)) {
            return if !is_strict {
                Value::undefined().into()
            } else {
                err_not_defined_(cx, name)
            };
        }

        get(self.binding_object, name)
    }

    // 8.1.1.2.7 DeleteBinding
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        self.binding_object.as_mut().delete(name)
    }

    // 8.1.1.2.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        false
    }

    // 8.1.1.2.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 8.1.1.2.10 WithBaseObject
    fn with_base_object(&self) -> Value {
        if self.with_environment {
            return self.binding_object.into();
        }

        Value::undefined().into()
    }
}
