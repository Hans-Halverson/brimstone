use std::mem::size_of;

use super::environment::{DynEnvironment, Environment, HeapDynEnvironment};

use crate::{
    js::runtime::{
        collections::{BsHashMap, BsHashMapField},
        completion::EvalResult,
        error::{err_not_defined_, err_uninitialized_, type_error_},
        gc::{Handle, HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        string_value::{FlatString, StringValue},
        value::Value,
        Context, HeapPtr,
    },
    must, set_uninit,
};

#[derive(Clone, Copy)]
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
            value: Value::undefined(),
            is_mutable,
            is_initialized: false,
            is_strict,
            can_delete,
        }
    }
}

// 9.1.1.1 Declarative Environment Record
#[repr(C)]
pub struct DeclarativeEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    bindings: HeapPtr<BindingsMap>,
    outer: Option<HeapDynEnvironment>,
}

type BindingsMap = BsHashMap<HeapPtr<FlatString>, Binding>;

impl DeclarativeEnvironment {
    // 9.1.2.2 NewDeclarativeEnvironment
    pub fn new(cx: Context, outer: Option<DynEnvironment>) -> Handle<DeclarativeEnvironment> {
        // Allocate and place behind handle before allocating environment
        let bindings_map = Self::new_bindings_map(cx);

        let mut env = cx.alloc_uninit::<DeclarativeEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::DeclarativeEnvironment));
        set_uninit!(env.bindings, bindings_map.get_());
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));

        env.to_handle()
    }

    pub fn init_as_base(
        cx: Context,
        env: &mut DeclarativeEnvironment,
        kind: ObjectKind,
        bindings: Handle<BindingsMap>,
        outer: Option<DynEnvironment>,
    ) {
        set_uninit!(env.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(env.bindings, bindings.get_());
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));
    }

    pub fn uninit(cx: Context) -> Handle<DeclarativeEnvironment> {
        let mut env = cx.alloc_uninit::<DeclarativeEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::DeclarativeEnvironment));
        set_uninit!(env.bindings, HeapPtr::uninit());
        set_uninit!(env.outer, None);

        env.to_handle()
    }

    pub fn new_bindings_map(cx: Context) -> Handle<BindingsMap> {
        BindingsMap::new_initial(cx, ObjectKind::DeclarativeEnvironmentBindingsMap).to_handle()
    }
}

impl Handle<DeclarativeEnvironment> {
    fn bindings_field(&self) -> DeclarativeEnvironmentBindingsMapField {
        DeclarativeEnvironmentBindingsMapField(*self)
    }
}

impl Environment for Handle<DeclarativeEnvironment> {
    // 9.1.1.1.1 HasBinding
    fn has_binding(&self, _: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        self.bindings.contains_key(&name.flatten().get_()).into()
    }

    // 9.1.1.1.2 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let binding = Binding::new(true, false, can_delete);
        let flat_name = name.flatten();
        self.bindings_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(flat_name.get_(), binding);
        ().into()
    }

    // 9.1.1.1.3 CreateImmutableBinding
    fn create_immutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        let binding = Binding::new(false, is_strict, false);
        let flat_name = name.flatten();
        self.bindings_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(flat_name.get_(), binding);
        ().into()
    }

    // 9.1.1.1.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        _: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()> {
        let binding = self.bindings.get_mut(&name.flatten().get_()).unwrap();
        binding.value = value.get();
        binding.is_initialized = true;
        ().into()
    }

    // 9.1.1.1.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()> {
        match self.bindings.get_mut(&name.flatten().get_()) {
            None if is_strict => err_not_defined_(cx, name),
            None => {
                must!(self.create_mutable_binding(cx, name, true));
                must!(self.initialize_binding(cx, name, value));
                ().into()
            }
            Some(binding) => {
                let s = if binding.is_strict { true } else { is_strict };

                if !binding.is_initialized {
                    return err_uninitialized_(cx, name);
                }

                if binding.is_mutable {
                    binding.value = value.get();
                } else if s {
                    return type_error_(cx, &format!("{} is immutable", name));
                }

                ().into()
            }
        }
    }

    // 9.1.1.1.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: Context,
        name: Handle<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<Handle<Value>> {
        let binding = self.bindings.get(&name.flatten().get_()).unwrap();
        if !binding.is_initialized {
            return err_uninitialized_(cx, name);
        }

        binding.value.to_handle(cx).into()
    }

    // 9.1.1.1.7 DeleteBinding
    fn delete_binding(&mut self, _: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        let name = name.flatten().get_();
        let binding = self.bindings.get(&name).unwrap();
        if !binding.can_delete {
            return false.into();
        }

        self.bindings.remove(&name);

        true.into()
    }

    // 9.1.1.1.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        false
    }

    // 9.1.1.1.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 9.1.1.1.10 WithBaseObject
    fn with_base_object(&self) -> Option<Handle<ObjectValue>> {
        None
    }

    fn get_this_binding(&self, _: Context) -> EvalResult<Handle<Value>> {
        panic!("DeclarativeEnvironment::get_this_binding is never called in spec")
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.outer.as_ref().map(DynEnvironment::from_heap)
    }
}

pub struct DeclarativeEnvironmentBindingsMapField(Handle<DeclarativeEnvironment>);

impl BsHashMapField<HeapPtr<FlatString>, Binding> for DeclarativeEnvironmentBindingsMapField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<BindingsMap> {
        BindingsMap::new(cx, ObjectKind::DeclarativeEnvironmentBindingsMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<BindingsMap> {
        self.0.bindings
    }

    fn set(&mut self, _: Context, map: HeapPtr<BindingsMap>) {
        self.0.bindings = map;
    }
}

impl HeapObject for HeapPtr<DeclarativeEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<DeclarativeEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.bindings);
        self.outer.as_mut().map(|o| o.visit_pointers(visitor));
    }
}

impl DeclarativeEnvironmentBindingsMapField {
    pub fn byte_size(map: &HeapPtr<BindingsMap>) -> usize {
        BindingsMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<BindingsMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(key);
            visitor.visit_value(&mut value.value);
        }
    }
}
