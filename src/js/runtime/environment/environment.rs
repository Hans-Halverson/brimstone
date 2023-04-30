use std::ops::{Deref, DerefMut};

use crate::{
    js::runtime::{
        completion::EvalResult, gc::Gc, object_value::ObjectValue, reference::Reference,
        string_value::StringValue, value::Value, Context,
    },
    maybe,
};

use super::{
    function_environment::FunctionEnvironment, global_environment::GlobalEnvironment,
    object_environment::ObjectEnvironment,
};

// 9.1 Environment Record
pub trait Environment {
    // Environment functions from spec
    fn has_binding(&self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool>;
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()>;
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()>;
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: Gc<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<Value>;
    fn delete_binding(&mut self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Option<Gc<ObjectValue>>;

    fn get_this_binding(&self, cx: &mut Context) -> EvalResult<Value>;

    // Optional reference to the outer (parent) environment. If None this is the global environment.
    // Implements section 8.1 Lexical Environment, but embedded in each environment record.
    fn outer(&self) -> Option<DynEnvironment>;

    // Downcasts
    fn as_function_environment(&mut self) -> Option<Gc<FunctionEnvironment>> {
        None
    }

    fn as_global_environment(&mut self) -> Option<Gc<GlobalEnvironment>> {
        None
    }

    fn as_object_environment(&mut self) -> Option<Gc<ObjectEnvironment>> {
        None
    }
}

// 8.1.2.1 GetIdentifierReference
pub fn get_identifier_reference(
    cx: &mut Context,
    env: Option<DynEnvironment>,
    name: Gc<StringValue>,
    is_strict: bool,
) -> EvalResult<Reference> {
    match env {
        None => Reference::new_unresolvable(name, is_strict).into(),
        Some(env) => {
            if maybe!(env.has_binding(cx, name)) {
                Reference::new_env(env, name, is_strict).into()
            } else {
                get_identifier_reference(cx, env.outer(), name, is_strict)
            }
        }
    }
}

/// An environment trait object, containing both a pointer to the environment object on the heap
/// along with the environment object's vtable.
///
/// Differs from a true rust trait object in that the data pointer contains the receiver value
/// directly instead of a pointer to the receiver.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct DynEnvironment {
    data: *const (),
    vtable: *const (),
}

pub struct HeapDynEnvironment {
    inner: DynEnvironment,
}

impl<T> Gc<T>
where
    Gc<T>: Environment,
{
    // Convert an environment GC reference to a trait object
    #[inline]
    pub fn into_dyn(&self) -> DynEnvironment
    where
        Self: Sized,
    {
        let vtable = extract_environment_vtable::<Self>();
        DynEnvironment { data: self.as_ptr() as *const (), vtable }
    }
}

impl DynEnvironment {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.data == other.data
    }

    #[inline]
    pub fn to_heap(&self) -> HeapDynEnvironment {
        HeapDynEnvironment { inner: *self }
    }

    #[inline]
    pub fn from_heap(heap_dyn_environment: &HeapDynEnvironment) -> DynEnvironment {
        heap_dyn_environment.inner
    }
}

// Implicitly deref to a true rust trait object by constructing a true trait object with a pointer
// to the receiver value, with the same vtable.
impl Deref for DynEnvironment {
    type Target = dyn Environment;

    fn deref(&self) -> &Self::Target {
        let data = &self.data as *const _ as *const ();
        let trait_object = DynEnvironment { data, vtable: self.vtable };
        unsafe { std::mem::transmute::<DynEnvironment, &dyn Environment>(trait_object) }
    }
}

impl DerefMut for DynEnvironment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let data = &self.data as *const _ as *const ();
        let trait_object = DynEnvironment { data, vtable: self.vtable };
        unsafe { std::mem::transmute::<DynEnvironment, &mut dyn Environment>(trait_object) }
    }
}

/// Compile time shenanigans to extract the trait object vtable for a particular type that
/// implements Object so that we can construct our own trait objects manually.
const fn extract_environment_vtable<T: Environment>() -> *const () {
    unsafe {
        let example_ptr: *const T = std::ptr::null();
        let example_trait_object: *const dyn Environment = example_ptr;
        let trait_object =
            std::mem::transmute::<*const dyn Environment, DynEnvironment>(example_trait_object);

        trait_object.vtable
    }
}
