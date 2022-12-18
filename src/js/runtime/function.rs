use wrap_ordinary_object::wrap_ordinary_object;

use super::{
    completion::AbstractResult,
    environment::environment::Environment,
    gc::{Gc, GcDeref},
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::OrdinaryObject,
    property_descriptor::PropertyDescriptor,
    value::Value,
    Context,
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

impl Function {
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
impl Object for Function {
    fn call(
        &self,
        _: &mut Context,
        _this_argument: Value,
        _arguments: Vec<Value>,
    ) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn construct(
        &self,
        _: &mut Context,
        _arguments: Vec<Value>,
        _new_target: Gc<ObjectValue>,
    ) -> AbstractResult<Gc<ObjectValue>> {
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
