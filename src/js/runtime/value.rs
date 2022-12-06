use std::{cell::RefCell, rc::Rc};

use super::completion::AbstractResult;

#[derive(Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Number(f64),
    Object(Rc<RefCell<ObjectValue>>),
    String(Box<String>),
}

impl From<bool> for Value {
    fn from(value: bool) -> Value {
        Value::Bool(value)
    }
}

pub struct ObjectValue {}

impl ObjectValue {
    pub const EMPTY: ObjectValue = ObjectValue {};

    pub fn new_with_value_1(prop1: String, value1: Value) -> ObjectValue {
        unimplemented!()
    }

    pub fn new_with_value_4(
        prop1: String,
        value1: Value,
        prop2: String,
        value2: Value,
        prop3: String,
        value3: Value,
        prop4: String,
        value4: Value,
    ) -> ObjectValue {
        unimplemented!()
    }

    // Returns either a PropertyDescriptor or undefined
    pub fn get_own_property(&self, prop: &str) -> AbstractResult<Value> {
        unimplemented!()
    }

    pub fn get(&self, prop: &str) -> Value {
        unimplemented!()
    }

    pub fn delete(&mut self, prop: &str) -> AbstractResult<bool> {
        unimplemented!()
    }
}

pub struct PropertyDescriptor;

impl PropertyDescriptor {
    pub fn is_configurable(prop: &ObjectValue) -> bool {
        unimplemented!()
    }

    pub fn is_writable(prop: &ObjectValue) -> bool {
        unimplemented!()
    }

    pub fn is_enumerable(prop: &ObjectValue) -> bool {
        unimplemented!()
    }

    pub fn is_data_descriptor(prop: &ObjectValue) -> bool {
        unimplemented!()
    }
}
