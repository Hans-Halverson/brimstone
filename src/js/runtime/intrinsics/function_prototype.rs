use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        completion::AbstractResult,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::OrdinaryObject,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        value::Value,
        Context,
    },
};

use super::intrinsics::Intrinsic;

#[repr(C)]
pub struct FunctionPrototype {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl GcDeref for FunctionPrototype {}

impl_gc_into!(FunctionPrototype, ObjectValue);

impl FunctionPrototype {
    const VTABLE: *const () = extract_object_vtable::<FunctionPrototype>();

    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Gc<FunctionPrototype> {
        let func_prototype = FunctionPrototype {
            _vtable: Self::VTABLE,
            object: OrdinaryObject::new_uninit(),
        };

        cx.heap.alloc(func_prototype)
    }

    pub fn initialize(&mut self, realm: Gc<Realm>) {
        self.object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for FunctionPrototype {
    fn call(
        &self,
        _: &mut Context,
        _this_argument: Value,
        _arguments: Vec<Value>,
    ) -> AbstractResult<Value> {
        // 20.2.3 Properties of the Function Prototype Object
        // Accepts any arguments and returns undefined when invoked
        Value::undefined().into()
    }

    fn is_callable(&self) -> bool {
        true
    }
}
