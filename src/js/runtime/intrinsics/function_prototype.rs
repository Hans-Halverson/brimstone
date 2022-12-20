use std::{cell::RefCell, rc::Rc};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        completion::AbstractResult,
        gc::Gc,
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

const VTABLE: *const () = extract_object_vtable::<FunctionPrototype>();

impl_gc_into!(FunctionPrototype, ObjectValue);

impl FunctionPrototype {
    pub fn new(cx: &mut Context, realm: Rc<RefCell<Realm>>) -> Gc<ObjectValue> {
        let ordinary_object = OrdinaryObject::new(
            Some(realm.borrow().get_intrinsic(Intrinsic::ObjectPrototype)),
            true,
        );
        let func_prototype = FunctionPrototype {
            _vtable: VTABLE,
            object: ordinary_object,
        };

        cx.heap.alloc(func_prototype).into()
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
        // 19.2.3 Properties of the Function Prototype Object
        // Accepts any arguments and returns undefined when invoked
        Value::undefined().into()
    }
}
