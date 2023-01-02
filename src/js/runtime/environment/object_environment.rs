use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, get, has_property, set},
        completion::EvalResult,
        error::err_not_defined_,
        gc::Gc,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        type_utilities::to_boolean,
        value::Value,
        Context,
    },
    maybe,
};

use super::environment::Environment;

// 9.1.1.2 Object Environment Record
pub struct ObjectEnvironment {
    pub binding_object: Gc<ObjectValue>,
    pub is_with_environment: bool,
    pub outer: Option<Gc<dyn Environment>>,
}

impl ObjectEnvironment {
    // 9.1.2.3 NewObjectEnvironment
    pub fn new(
        binding_object: Gc<ObjectValue>,
        is_with_environment: bool,
        outer: Option<Gc<dyn Environment>>,
    ) -> ObjectEnvironment {
        ObjectEnvironment {
            binding_object,
            is_with_environment,
            outer,
        }
    }
}

impl Environment for ObjectEnvironment {
    // 9.1.1.2.1 HasBinding
    fn has_binding(&self, cx: &mut Context, name: &str) -> EvalResult<bool> {
        if !maybe!(has_property(self.binding_object, name)) {
            return false.into();
        } else if !self.is_with_environment {
            return true.into();
        }

        // TODO: Change to symbol once symbols are implemented
        // Ignore properties in @@unscopables
        let unscopables = maybe!(get(cx, self.binding_object, "@@unscopables"));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe!(get(cx, unscopables, name));
            let blocked = to_boolean(value);
            if blocked {
                return false.into();
            }
        }

        true.into()
    }

    // 9.1.1.2.1 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> EvalResult<()> {
        let prop_desc = PropertyDescriptor::data(Value::undefined(), true, true, can_delete);
        define_property_or_throw(cx, self.binding_object, &name, prop_desc)
    }

    // 9.1.1.2.3 CreateImmutableBinding
    fn create_immutable_binding(&mut self, _: &mut Context, _: String, _: bool) -> EvalResult<()> {
        unreachable!("ObjectEnvironment::create_immutable_binding is never used in spec")
    }

    // 9.1.1.2.4 InitializeBinding
    fn initialize_binding(&mut self, cx: &mut Context, name: &str, value: Value) -> EvalResult<()> {
        self.set_mutable_binding(cx, name, value, false)
    }

    // 9.1.1.2.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()> {
        let still_exists = maybe!(has_property(self.binding_object, name));
        if !still_exists && is_strict {
            return err_not_defined_(cx, name);
        }

        maybe!(set(cx, self.binding_object, name, value, is_strict));
        ().into()
    }

    // 9.1.1.2.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> EvalResult<Value> {
        if !maybe!(has_property(self.binding_object, name)) {
            return if !is_strict {
                Value::undefined().into()
            } else {
                err_not_defined_(cx, name)
            };
        }

        get(cx, self.binding_object, name)
    }

    // 9.1.1.2.7 DeleteBinding
    fn delete_binding(&mut self, _: &mut Context, name: &str) -> EvalResult<bool> {
        self.binding_object.delete(name)
    }

    // 9.1.1.2.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        false
    }

    // 9.1.1.2.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 9.1.1.2.10 WithBaseObject
    fn with_base_object(&self) -> Option<Gc<ObjectValue>> {
        if self.is_with_environment {
            return Some(self.binding_object);
        }

        None
    }

    fn get_this_binding(&self, _: &mut Context) -> EvalResult<Value> {
        panic!("ObjectEnvironment::get_this_binding is never called in spec")
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.outer
    }
}
