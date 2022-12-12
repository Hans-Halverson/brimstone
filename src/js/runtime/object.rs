use super::{
    completion::AbstractResult,
    gc::Gc,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    property_descriptor::PropertyDescriptor,
    value::Value,
    Context,
};

// An ordinary object
#[repr(C)]
pub struct OrdinaryObject {
    _vtable: ObjectValueVtable,
}

const ORDINARY_OBJECT_VTABLE: *const () = extract_object_vtable::<OrdinaryObject>();

impl OrdinaryObject {
    pub fn new() -> OrdinaryObject {
        OrdinaryObject {
            _vtable: ORDINARY_OBJECT_VTABLE,
        }
    }

    pub fn create_data_property_or_throw(
        &mut self,
        name: &str,
        value: Value,
    ) -> AbstractResult<()> {
        unimplemented!()
    }
}

impl Object for OrdinaryObject {
    fn get_prototype_of(&self) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn is_extensible(&self) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn prevent_extensions(&self) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn get_own_property(&self, key: &str) -> AbstractResult<Option<PropertyDescriptor>> {
        unimplemented!()
    }

    fn define_own_property(&mut self, key: &str, desc: PropertyDescriptor) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn has_property(&self, key: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn get(&self, key: &str, receiver: Value) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn set(&mut self, key: &str, value: Value, receiver: Value) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn delete(&mut self, key: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn own_property_keys(&self) -> AbstractResult<Vec<Value>> {
        unimplemented!()
    }
}

impl<'a> Into<&'a ObjectValue> for &'a OrdinaryObject {
    fn into(self) -> &'a ObjectValue {
        unsafe { &*((self as *const _) as *const ObjectValue) }
    }
}

impl Into<Gc<ObjectValue>> for Gc<OrdinaryObject> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.as_ref() as *const _ as *mut ObjectValue)
    }
}

impl Into<Gc<ObjectValue>> for &OrdinaryObject {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}

pub fn ordinary_object_create(cx: &mut Context, proto: &str) -> Gc<OrdinaryObject> {
    unimplemented!()
}
