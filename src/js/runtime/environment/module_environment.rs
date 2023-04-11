use crate::js::runtime::{
    completion::EvalResult,
    gc::{Gc, GcDeref},
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    string_value::StringValue,
    value::Value,
    Context,
};

use super::{
    declarative_environment::DeclarativeEnvironment,
    environment::{DynEnvironment, Environment},
};

#[repr(C)]
pub struct ModuleEnvironment {
    env: DeclarativeEnvironment,
}

impl GcDeref for ModuleEnvironment {}

// 9.1.1.5 Module Environment Record
impl ModuleEnvironment {
    // 9.1.2.6 NewModuleEnvironment
    fn new(cx: &mut Context, outer: DynEnvironment) -> Gc<ModuleEnvironment> {
        let env =
            DeclarativeEnvironment::new_as_env_base(cx, ObjectKind::ModuleEnvironment, Some(outer));
        cx.heap.alloc(ModuleEnvironment { env })
    }
}

impl Gc<ModuleEnvironment> {
    #[inline]
    fn env(&self) -> Gc<DeclarativeEnvironment> {
        self.cast()
    }
}

impl Environment for Gc<ModuleEnvironment> {
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
        self.env().has_binding(cx, name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        self.env().create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()> {
        self.env().initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().set_mutable_binding(cx, name, value, is_strict)
    }

    fn has_super_binding(&self) -> bool {
        self.env().has_super_binding()
    }

    fn with_base_object(&self) -> Option<Gc<ObjectValue>> {
        self.env().with_base_object()
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.env().outer()
    }
}

impl ModuleEnvironment {
    // 9.1.1.5.5 CreateImportBinding
    fn create_import_binding(&self) -> EvalResult<Value> {
        unimplemented!()
    }
}
