use std::mem::size_of;

use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, get, has_property, set},
        completion::EvalResult,
        error::err_not_defined_,
        gc::{Handle, HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        string_value::StringValue,
        type_utilities::to_boolean,
        Context, HeapPtr, Value,
    },
    maybe, set_uninit,
};

use super::environment::{DynEnvironment, Environment, HeapDynEnvironment};

// 9.1.1.2 Object Environment Record
#[repr(C)]
pub struct ObjectEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    binding_object: HeapPtr<ObjectValue>,
    outer: Option<HeapDynEnvironment>,
    is_with_environment: bool,
}

impl ObjectEnvironment {
    // 9.1.2.3 NewObjectEnvironment
    pub fn new(
        cx: Context,
        binding_object: Handle<ObjectValue>,
        is_with_environment: bool,
        outer: Option<DynEnvironment>,
    ) -> Handle<ObjectEnvironment> {
        let mut env = cx.alloc_uninit::<ObjectEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::ObjectEnvironment));
        set_uninit!(env.binding_object, binding_object.get_());
        set_uninit!(env.is_with_environment, is_with_environment);
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));

        env.to_handle()
    }

    #[inline]
    pub fn binding_object(&self) -> Handle<ObjectValue> {
        self.binding_object.to_handle()
    }
}

impl Environment for Handle<ObjectEnvironment> {
    fn as_object_environment(&mut self) -> Option<Handle<ObjectEnvironment>> {
        Some(*self)
    }

    // 9.1.1.2.1 HasBinding
    fn has_binding(&self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        let binding_object = self.binding_object();
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        if !maybe!(has_property(cx, binding_object, name_key)) {
            return false.into();
        } else if !self.is_with_environment {
            return true.into();
        }

        // Ignore properties in @@unscopables
        let unscopables_key = cx.well_known_symbols.unscopables();
        let unscopables = maybe!(get(cx, binding_object, unscopables_key));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe!(get(cx, unscopables, name_key));
            let blocked = to_boolean(value.get());
            if blocked {
                return false.into();
            }
        }

        true.into()
    }

    // 9.1.1.2.1 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let prop_desc = PropertyDescriptor::data(cx.undefined(), true, true, can_delete);
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        define_property_or_throw(cx, self.binding_object(), name_key, prop_desc)
    }

    // 9.1.1.2.3 CreateImmutableBinding
    fn create_immutable_binding(
        &mut self,
        _: Context,
        _: Handle<StringValue>,
        _: bool,
    ) -> EvalResult<()> {
        unreachable!("ObjectEnvironment::create_immutable_binding is never used in spec")
    }

    // 9.1.1.2.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()> {
        self.set_mutable_binding(cx, name, value, false)
    }

    // 9.1.1.2.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()> {
        let binding_object = self.binding_object();
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        let still_exists = maybe!(has_property(cx, binding_object, name_key));
        if !still_exists && is_strict {
            return err_not_defined_(cx, name);
        }

        maybe!(set(cx, binding_object, name_key, value, is_strict));
        ().into()
    }

    // 9.1.1.2.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Handle<Value>> {
        let binding_object = self.binding_object();
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        if !maybe!(has_property(cx, binding_object, name_key)) {
            return if !is_strict {
                cx.undefined().into()
            } else {
                err_not_defined_(cx, name)
            };
        }

        get(cx, binding_object, name_key)
    }

    // 9.1.1.2.7 DeleteBinding
    fn delete_binding(&mut self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        self.binding_object().delete(cx, name_key)
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
    fn with_base_object(&self) -> Option<Handle<ObjectValue>> {
        if self.is_with_environment {
            return Some(self.binding_object());
        }

        None
    }

    fn get_this_binding(&self, _: Context) -> EvalResult<Handle<Value>> {
        panic!("ObjectEnvironment::get_this_binding is never called in spec")
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.outer.as_ref().map(DynEnvironment::from_heap)
    }
}

impl HeapObject for HeapPtr<ObjectEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<ObjectEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.binding_object);
        self.outer.as_mut().map(|o| o.visit_pointers(visitor));
    }
}
