use crate::js::runtime::{
    completion::EvalResult,
    gc::Gc,
    object_value::ObjectValue,
    value::{StringValue, Value},
    Context,
};

use super::{declarative_environment::DeclarativeEnvironment, environment::Environment};

pub struct ModuleEnvironment {
    env: DeclarativeEnvironment,
}

// 9.1.1.5 Module Environment Record
impl ModuleEnvironment {
    // 9.1.2.6 NewModuleEnvironment
    fn new(cx: &mut Context, outer: Gc<dyn Environment>) -> Gc<ModuleEnvironment> {
        // Inner decl env contains the outer environment pointer
        cx.heap
            .alloc(ModuleEnvironment { env: DeclarativeEnvironment::new(Some(outer)) })
    }
}

impl Environment for ModuleEnvironment {
    // 9.1.1.5.1 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Value> {
        unimplemented!()
    }

    // 9.1.1.5.2 DeleteBinding
    fn delete_binding(&mut self, _: &mut Context, _name: Gc<StringValue>) -> EvalResult<bool> {
        unreachable!("ModuleEnvironment::delete_binding is never called according to the spec")
    }

    // 9.1.1.5.3 HasThisBinding
    fn has_this_binding(&self) -> bool {
        true
    }

    // 9.1.1.5.4 GetThisBinding
    fn get_this_binding(&self, _: &mut Context) -> EvalResult<Value> {
        Value::undefined().into()
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool> {
        self.env.has_binding(cx, name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        self.env.create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env.create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()> {
        self.env.initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env.set_mutable_binding(cx, name, value, is_strict)
    }

    fn has_super_binding(&self) -> bool {
        self.env.has_super_binding()
    }

    fn with_base_object(&self) -> Option<Gc<ObjectValue>> {
        self.env.with_base_object()
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.env.outer()
    }
}

impl ModuleEnvironment {
    // 9.1.1.5.5 CreateImportBinding
    fn create_import_binding(&self) -> EvalResult<Value> {
        unimplemented!()
    }
}
