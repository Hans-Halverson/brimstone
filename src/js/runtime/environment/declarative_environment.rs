use super::environment::{DynEnvironment, Environment, HeapDynEnvironment};

use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{err_not_defined_, err_uninitialized_, type_error_},
        gc::{GcDeref, Handle, HandleValue, Heap},
        object_descriptor::{BaseDescriptors, ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        string_value::StringValue,
        value::Value,
        Context, HeapPtr,
    },
    set_uninit,
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
    bindings: HashMap<HeapPtr<StringValue>, Binding>,
    outer: Option<HeapDynEnvironment>,
}

impl GcDeref for DeclarativeEnvironment {}

impl DeclarativeEnvironment {
    // 9.1.2.2 NewDeclarativeEnvironment
    pub fn new(cx: &mut Context, outer: Option<DynEnvironment>) -> Handle<DeclarativeEnvironment> {
        let mut env = cx.heap.alloc_uninit::<DeclarativeEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::DeclarativeEnvironment));
        set_uninit!(env.bindings, HashMap::new());
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));

        env
    }

    pub fn init_as_base(
        cx: &mut Context,
        env: &mut DeclarativeEnvironment,
        kind: ObjectKind,
        outer: Option<DynEnvironment>,
    ) {
        set_uninit!(env.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(env.bindings, HashMap::new());
        set_uninit!(env.outer, outer.as_ref().map(DynEnvironment::to_heap));
    }

    pub fn uninit(
        heap: &mut Heap,
        base_descriptors: &BaseDescriptors,
    ) -> Handle<DeclarativeEnvironment> {
        let mut env = heap.alloc_uninit::<DeclarativeEnvironment>();

        set_uninit!(env.descriptor, base_descriptors.get(ObjectKind::DeclarativeEnvironment));
        set_uninit!(env.bindings, HashMap::new());
        set_uninit!(env.outer, None);

        env
    }
}

impl Environment for Handle<DeclarativeEnvironment> {
    // 9.1.1.1.1 HasBinding
    fn has_binding(&self, _: &mut Context, name: Handle<StringValue>) -> EvalResult<bool> {
        self.bindings.contains_key(&name.get_()).into()
    }

    // 9.1.1.1.2 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        _: &mut Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let binding = Binding::new(true, false, can_delete);
        self.bindings.insert(name.get_(), binding);
        ().into()
    }

    // 9.1.1.1.3 CreateImmutableBinding
    fn create_immutable_binding(
        &mut self,
        _: &mut Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        let binding = Binding::new(false, is_strict, false);
        self.bindings.insert(name.get_(), binding);
        ().into()
    }

    // 9.1.1.1.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        _: &mut Context,
        name: Handle<StringValue>,
        value: HandleValue,
    ) -> EvalResult<()> {
        let binding = self.bindings.get_mut(&name.get_()).unwrap();
        binding.value = value.get();
        binding.is_initialized = true;
        ().into()
    }

    // 9.1.1.1.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Handle<StringValue>,
        value: HandleValue,
        is_strict: bool,
    ) -> EvalResult<()> {
        match self.bindings.get_mut(&name.get_()) {
            None if is_strict => err_not_defined_(cx, name),
            None => {
                self.create_mutable_binding(cx, name, true);
                self.initialize_binding(cx, name, value);
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
        cx: &mut Context,
        name: Handle<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<HandleValue> {
        let binding = self.bindings.get(&name.get_()).unwrap();
        if !binding.is_initialized {
            return err_uninitialized_(cx, name);
        }

        HandleValue::from_value(cx, binding.value).into()
    }

    // 9.1.1.1.7 DeleteBinding
    fn delete_binding(&mut self, _: &mut Context, name: Handle<StringValue>) -> EvalResult<bool> {
        let name = name.get_();
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

    fn get_this_binding(&self, _: &mut Context) -> EvalResult<HandleValue> {
        panic!("DeclarativeEnvironment::get_this_binding is never called in spec")
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.outer.as_ref().map(DynEnvironment::from_heap)
    }
}
