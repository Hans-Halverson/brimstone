use super::{
    completion::AbstractResult,
    environment::environment::Environment,
    gc::{Gc, GcDeref},
    object::OrdinaryObject,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    property_descriptor::PropertyDescriptor,
    value::Value,
};

#[derive(PartialEq)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

#[repr(C)]
pub struct Function {
    _vtable: ObjectValueVtable,
    is_strict: bool,
    pub this_mode: ThisMode,
    // Object properties of this function
    object: OrdinaryObject,
    pub environment: Gc<dyn Environment>,
    pub home_object: Option<Gc<ObjectValue>>,
}

impl GcDeref for Function {}

const FUNCTION_VTABLE: *const () = extract_object_vtable::<Function>();

impl Object for Function {
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

impl<'a> Into<&'a ObjectValue> for &'a Function {
    fn into(self) -> &'a ObjectValue {
        unsafe { &*((self as *const _) as *const ObjectValue) }
    }
}

impl Into<Gc<ObjectValue>> for Gc<Function> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.as_ref() as *const _ as *mut ObjectValue)
    }
}
