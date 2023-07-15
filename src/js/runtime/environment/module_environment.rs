use crate::js::runtime::{
    completion::EvalResult,
    gc::{Handle, IsHeapObject},
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    string_value::StringValue,
    Context, Value,
};

use super::{
    declarative_environment::DeclarativeEnvironment,
    environment::{DynEnvironment, Environment},
};

#[repr(C)]
pub struct ModuleEnvironment {
    env: DeclarativeEnvironment,
}

impl IsHeapObject for ModuleEnvironment {}

// 9.1.1.5 Module Environment Record
impl ModuleEnvironment {
    // 9.1.2.6 NewModuleEnvironment
    fn _new(cx: &mut Context, outer: DynEnvironment) -> Handle<ModuleEnvironment> {
        // Allocate and put behind handles before allocating module environment
        let bindings = DeclarativeEnvironment::new_bindings_map(cx).to_handle();

        let mut env = cx.heap.alloc_uninit::<ModuleEnvironment>();

        DeclarativeEnvironment::init_as_base(
            cx,
            &mut env.env,
            ObjectKind::ModuleEnvironment,
            bindings,
            Some(outer),
        );

        env.to_handle()
    }
}

impl Handle<ModuleEnvironment> {
    #[inline]
    fn env(&self) -> Handle<DeclarativeEnvironment> {
        self.cast()
    }
}

impl Environment for Handle<ModuleEnvironment> {
    // 9.1.1.5.1 GetBindingValue
    fn get_binding_value(
        &self,
        _cx: &mut Context,
        _name: Handle<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<Handle<Value>> {
        unimplemented!()
    }

    // 9.1.1.5.2 DeleteBinding
    fn delete_binding(&mut self, _: &mut Context, _name: Handle<StringValue>) -> EvalResult<bool> {
        unreachable!("ModuleEnvironment::delete_binding is never called according to the spec")
    }

    // 9.1.1.5.3 HasThisBinding
    fn has_this_binding(&self) -> bool {
        true
    }

    // 9.1.1.5.4 GetThisBinding
    fn get_this_binding(&self, cx: &mut Context) -> EvalResult<Handle<Value>> {
        cx.undefined().into()
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, cx: &mut Context, name: Handle<StringValue>) -> EvalResult<bool> {
        self.env().has_binding(cx, name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        self.env().create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()> {
        self.env().initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().set_mutable_binding(cx, name, value, is_strict)
    }

    fn has_super_binding(&self) -> bool {
        self.env().has_super_binding()
    }

    fn with_base_object(&self) -> Option<Handle<ObjectValue>> {
        self.env().with_base_object()
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.env().outer()
    }
}

impl ModuleEnvironment {
    // 9.1.1.5.5 CreateImportBinding
    fn _create_import_binding(&self) -> EvalResult<Handle<Value>> {
        unimplemented!()
    }
}
