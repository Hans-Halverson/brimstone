use std::{cell::RefCell, rc::Rc};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        completion::AbstractResult,
        gc::Gc,
        object_value::{
            extract_object_vtable, set_immutable_prototype, Object, ObjectValue, ObjectValueVtable,
        },
        ordinary_object::OrdinaryObject,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        value::Value,
        Context,
    },
};

#[repr(C)]
pub struct ObjectPrototype {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

const VTABLE: *const () = extract_object_vtable::<ObjectPrototype>();

impl ObjectPrototype {
    pub fn new(cx: &mut Context, _realm: Rc<RefCell<Realm>>) -> Gc<ObjectValue> {
        let ordinary_object = OrdinaryObject::new(None, true);
        let object_prototype = ObjectPrototype {
            _vtable: VTABLE,
            object: ordinary_object,
        };

        cx.heap.alloc(object_prototype).into()
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
impl Object for ObjectPrototype {
    fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> AbstractResult<bool> {
        set_immutable_prototype(self.into(), proto)
    }
}

impl_gc_into!(ObjectPrototype, ObjectValue);
