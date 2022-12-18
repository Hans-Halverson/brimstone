use crate::js::runtime::{
    abstract_operations::{define_property_or_throw, get, has_property, set},
    completion::AbstractResult,
    error::err_not_defined_,
    gc::Gc,
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    type_utilities::to_boolean,
    value::Value,
    Context,
};
use crate::maybe_;

use super::environment::Environment;

// 8.1.1.2 Object Environment Record
pub struct ObjectEnvironment {
    pub binding_object: Gc<ObjectValue>,
    pub with_environment: bool,
    pub outer: Option<Gc<dyn Environment>>,
}

impl ObjectEnvironment {
    // 8.1.2.3 NewObjectEnvironment
    fn new(
        cx: &mut Context,
        binding_object: Gc<ObjectValue>,
        outer: Gc<dyn Environment>,
    ) -> Gc<ObjectEnvironment> {
        cx.heap.alloc(ObjectEnvironment {
            binding_object,
            with_environment: false,
            outer: Some(outer),
        })
    }
}

impl Environment for ObjectEnvironment {
    // 8.1.1.2.1 HasBinding
    fn has_binding(&self, cx: &mut Context, name: &str) -> AbstractResult<bool> {
        if !maybe_!(has_property(self.binding_object, name)) {
            return false.into();
        } else if !self.with_environment {
            return true.into();
        }

        // Ignore properties in @@unscopables
        let unscopables = maybe_!(get(cx, self.binding_object, "@@unscopables"));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe_!(get(cx, unscopables, name));
            let blocked = to_boolean(value);
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
        let prop_desc = PropertyDescriptor::data(Value::undefined(), true, true, can_delete);
        define_property_or_throw(cx, self.binding_object, &name, prop_desc)
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
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        maybe_!(set(cx, self.binding_object, name, value, is_strict));
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

        get(cx, self.binding_object, name)
    }

    // 8.1.1.2.7 DeleteBinding
    fn delete_binding(&mut self, _: &mut Context, name: &str) -> AbstractResult<bool> {
        self.binding_object.delete(name)
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

    fn get_this_binding(&self, _: &mut Context) -> AbstractResult<Value> {
        panic!("ObjectEnvironment::get_this_binding is never called in spec")
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.outer
    }
}
