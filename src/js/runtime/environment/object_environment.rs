use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, get, has_property, set},
        completion::EvalResult,
        error::err_not_defined_,
        gc::{Gc, GcDeref, Handle},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        string_value::StringValue,
        type_utilities::to_boolean,
        value::Value,
        Context,
    },
    maybe, set_uninit,
};

use super::environment::{DynEnvironment, Environment, HeapDynEnvironment};

// 9.1.1.2 Object Environment Record
#[repr(C)]
pub struct ObjectEnvironment {
    descriptor: Gc<ObjectDescriptor>,
    binding_object: Gc<ObjectValue>,
    outer: Option<HeapDynEnvironment>,
    is_with_environment: bool,
}

impl GcDeref for ObjectEnvironment {}

impl ObjectEnvironment {
    // 9.1.2.3 NewObjectEnvironment
    pub fn new(
        cx: &mut Context,
        binding_object: Handle<ObjectValue>,
        is_with_environment: bool,
        outer: Option<DynEnvironment>,
    ) -> Handle<ObjectEnvironment> {
        let mut env = cx.heap.alloc_uninit::<ObjectEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::ObjectEnvironment));
        set_uninit!(env.binding_object, binding_object.get_());
        set_uninit!(env.is_with_environment, is_with_environment);
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));

        env
    }

    #[inline]
    pub fn binding_object(&self) -> Gc<ObjectValue> {
        self.binding_object
    }
}

impl Environment for Gc<ObjectEnvironment> {
    fn as_object_environment(&mut self) -> Option<Gc<ObjectEnvironment>> {
        Some(*self)
    }

    // 9.1.1.2.1 HasBinding
    fn has_binding(&self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool> {
        let name_key = PropertyKey::string(cx, name);
        if !maybe!(has_property(cx, self.binding_object, name_key)) {
            return false.into();
        } else if !self.is_with_environment {
            return true.into();
        }

        // Ignore properties in @@unscopables
        let unscopables_key = PropertyKey::symbol(cx.well_known_symbols.unscopables);
        let unscopables = maybe!(get(cx, self.binding_object, unscopables_key));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe!(get(cx, unscopables, name_key));
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
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let prop_desc = PropertyDescriptor::data(Value::undefined(), true, true, can_delete);
        let name_key = PropertyKey::string(cx, name);
        define_property_or_throw(cx, self.binding_object, name_key, prop_desc)
    }

    // 9.1.1.2.3 CreateImmutableBinding
    fn create_immutable_binding(
        &mut self,
        _: &mut Context,
        _: Gc<StringValue>,
        _: bool,
    ) -> EvalResult<()> {
        unreachable!("ObjectEnvironment::create_immutable_binding is never used in spec")
    }

    // 9.1.1.2.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()> {
        self.set_mutable_binding(cx, name, value, false)
    }

    // 9.1.1.2.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()> {
        let name_key = PropertyKey::string(cx, name);
        let still_exists = maybe!(has_property(cx, self.binding_object, name_key));
        if !still_exists && is_strict {
            return err_not_defined_(cx, name);
        }

        maybe!(set(cx, self.binding_object, name_key, value, is_strict));
        ().into()
    }

    // 9.1.1.2.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Value> {
        let name_key = PropertyKey::string(cx, name);
        if !maybe!(has_property(cx, self.binding_object, name_key)) {
            return if !is_strict {
                Value::undefined().into()
            } else {
                err_not_defined_(cx, name)
            };
        }

        get(cx, self.binding_object, name_key)
    }

    // 9.1.1.2.7 DeleteBinding
    fn delete_binding(&mut self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool> {
        let name_key = PropertyKey::string(cx, name);
        self.binding_object.delete(cx, name_key)
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

    fn outer(&self) -> Option<DynEnvironment> {
        self.outer.as_ref().map(DynEnvironment::from_heap)
    }
}
