use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    completion::AbstractResult,
    environment::Environment,
    value::{ObjectValue, Value},
};

pub struct Binding {
    pub is_initialized: bool,
}

// 8.1.1.2 Object Environment Record
pub struct ObjectEnvironment {
    pub bindings: HashMap<String, Binding>,
    pub binding_obj: Rc<RefCell<ObjectValue>>,
}

impl Environment for ObjectEnvironment {
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        unimplemented!()
    }

    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        unimplemented!()
    }

    fn set_mutable_binding(
        &mut self,
        name: String,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        unimplemented!()
    }

    fn get_binding_value(&self, name: &str, _is_strict: bool) -> AbstractResult<Value> {
        unimplemented!()
    }

    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        unimplemented!()
    }

    fn has_this_binding(&self) -> bool {
        unimplemented!()
    }

    fn has_super_binding(&self) -> bool {
        unimplemented!()
    }

    fn with_base_object(&self) -> Value {
        unimplemented!()
    }
}
