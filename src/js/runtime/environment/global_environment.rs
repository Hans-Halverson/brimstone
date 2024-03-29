use std::mem::size_of;

use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, has_own_property, is_extensible, set},
        collections::{BsHashSet, BsHashSetField},
        completion::EvalResult,
        error::type_error_,
        gc::{Handle, HeapObject, HeapVisitor},
        global_names::{can_declare_global_function, can_declare_global_var},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        string_value::{FlatString, StringValue},
        Context, HeapPtr, Value,
    },
    set_uninit,
};
use crate::{maybe, must};

use super::{
    declarative_environment::DeclarativeEnvironment,
    environment::{DynEnvironment, Environment},
    object_environment::ObjectEnvironment,
};

// 9.1.1.4 Global Environment Record
#[repr(C)]
pub struct GlobalEnvironment {
    // Declarative environment. [[DeclarativeRecord]] in spec.
    decl_env: DeclarativeEnvironment,

    // The global object. [[ObjectRecord]] in spec.
    object_env: HeapPtr<ObjectEnvironment>,

    global_this_value: HeapPtr<ObjectValue>,

    var_names: HeapPtr<VarNamesSet>,
}

type VarNamesSet = BsHashSet<HeapPtr<FlatString>>;

impl Handle<GlobalEnvironment> {
    #[inline]
    fn decl_env(&self) -> Handle<DeclarativeEnvironment> {
        self.cast()
    }
}

impl GlobalEnvironment {
    // 9.1.2.5 NewGlobalEnvironment
    pub fn new(
        cx: Context,
        global_object: Handle<ObjectValue>,
        global_this_value: Handle<ObjectValue>,
    ) -> Handle<GlobalEnvironment> {
        // Allocate and place behind handle before allocating environment
        let object_env = ObjectEnvironment::new(cx, global_object, false, None);
        let bindings = DeclarativeEnvironment::new_bindings_map(cx);
        let var_names = Self::new_var_names_set(cx).to_handle();

        let mut env = cx.alloc_uninit::<GlobalEnvironment>();

        // Declarative environment contains outer environment
        DeclarativeEnvironment::init_as_base(
            cx,
            &mut env.decl_env,
            ObjectKind::GlobalEnvironment,
            bindings,
            None,
        );

        set_uninit!(env.object_env, object_env.get_());
        set_uninit!(env.global_this_value, global_this_value.get_());
        set_uninit!(env.var_names, var_names.get_());

        env.to_handle()
    }

    pub fn new_var_names_set(cx: Context) -> HeapPtr<VarNamesSet> {
        GlobalEnvironmentVarNamesField::new(cx, VarNamesSet::MIN_CAPACITY)
    }

    #[inline]
    pub fn object_env(&self) -> Handle<ObjectEnvironment> {
        self.object_env.to_handle()
    }

    #[inline]
    pub fn global_this_value(&self) -> Handle<ObjectValue> {
        self.global_this_value.to_handle()
    }
}

impl Environment for Handle<GlobalEnvironment> {
    fn as_global_environment(&mut self) -> Option<Handle<GlobalEnvironment>> {
        Some(*self)
    }

    // 9.1.1.4.1 HasBinding
    fn has_binding(&self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return true.into();
        }

        self.object_env().has_binding(cx, name)
    }

    // 9.1.1.4.2 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env().create_mutable_binding(cx, name, can_delete)
    }

    // 9.1.1.4.3 CreateImutableBinding
    fn create_immutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env()
            .create_immutable_binding(cx, name, is_strict)
    }

    // 9.1.1.4.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return self.decl_env().initialize_binding(cx, name, value);
        }

        self.object_env().initialize_binding(cx, name, value)
    }

    // 9.1.1.4.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return self
                .decl_env()
                .set_mutable_binding(cx, name, value, is_strict);
        }

        self.object_env()
            .set_mutable_binding(cx, name, value, is_strict)
    }

    // 9.1.1.4.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Handle<Value>> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return self.decl_env().get_binding_value(cx, name, is_strict);
        }

        self.object_env().get_binding_value(cx, name, is_strict)
    }

    // 9.1.1.4.7 DeleteBinding
    fn delete_binding(&mut self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        if must!(self.decl_env().has_binding(cx, name)) {
            return self.decl_env().delete_binding(cx, name);
        }

        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        let mut object_env = self.object_env();

        if maybe!(has_own_property(cx, object_env.binding_object(), name_key)) {
            let status = maybe!(object_env.delete_binding(cx, name));
            if status {
                self.var_names.remove(&name.flatten().get_());
            }

            return status.into();
        }

        return true.into();
    }

    // 9.1.1.4.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        true
    }

    // 9.1.1.4.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 9.1.1.4.10 WithBaseObject
    fn with_base_object(&self) -> Option<Handle<ObjectValue>> {
        None
    }

    // 9.1.1.4.11 GetThisBinding
    fn get_this_binding(&self, _: Context) -> EvalResult<Handle<Value>> {
        self.global_this_value().into()
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.decl_env().outer()
    }
}

impl GlobalEnvironment {
    // 9.1.1.4.12 HasVarDeclaration
    pub fn has_var_declaration(&self, name: Handle<StringValue>) -> bool {
        self.var_names.contains(&name.flatten().get_())
    }

    // 9.1.1.4.14 HasRestrictedGlobalProperty
    pub fn has_restricted_global_property(
        &self,
        cx: Context,
        name: Handle<StringValue>,
    ) -> EvalResult<bool> {
        // GC safe since self is never referenced after this point
        let global_object = self.object_env.binding_object();

        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        let existing_prop = maybe!(global_object.get_own_property(cx, name_key));

        match existing_prop {
            None => false.into(),
            Some(existing_prop) => (!existing_prop.is_configurable()).into(),
        }
    }

    // 9.1.1.4.15 CanDeclareGlobalVar
    pub fn can_declare_global_var(
        &self,
        cx: Context,
        name: Handle<StringValue>,
    ) -> EvalResult<bool> {
        // GC safe since self is never referenced after this point
        let global_object = self.object_env.binding_object();
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        can_declare_global_var(cx, global_object, name_key)
    }

    // 9.1.1.4.16 CanDeclareGlobalFunction
    pub fn can_declare_global_function(
        &self,
        cx: Context,
        name: Handle<StringValue>,
    ) -> EvalResult<bool> {
        // GC safe since self is never referenced after this point
        let global_object = self.object_env.binding_object();
        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        can_declare_global_function(cx, global_object, name_key)
    }
}

impl Handle<GlobalEnvironment> {
    fn var_names_field(&self) -> GlobalEnvironmentVarNamesField {
        GlobalEnvironmentVarNamesField(*self)
    }

    // 9.1.1.4.13 HasLexicalDeclaration
    pub fn has_lexical_declaration(
        &self,
        cx: Context,
        name: Handle<StringValue>,
    ) -> EvalResult<bool> {
        self.decl_env().has_binding(cx, name)
    }

    // 9.1.1.4.17 CreateGlobalVarBinding
    pub fn create_global_var_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let global_object = self.object_env.binding_object();

        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        let has_property = maybe!(has_own_property(cx, global_object, name_key));
        let is_extensible = maybe!(is_extensible(cx, global_object));

        if !has_property && is_extensible {
            let mut object_env = self.object_env();
            maybe!(object_env.create_mutable_binding(cx, name, can_delete));
            maybe!(object_env.initialize_binding(cx, name, cx.undefined()));
        }

        let name = name.flatten();
        if !self.var_names.contains(&name) {
            self.var_names_field()
                .maybe_grow_for_insertion(cx)
                .insert_without_growing(name.get_());
        }

        ().into()
    }

    // 9.1.1.4.18 CreateGlobalFunctionBinding
    pub fn create_global_function_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        can_delete: bool,
    ) -> EvalResult<()> {
        let global_object = self.object_env.binding_object();

        let name_key = PropertyKey::string(cx, name).to_handle(cx);
        let existing_prop = maybe!(global_object.get_own_property(cx, name_key));

        let is_writable = match existing_prop {
            None => true,
            Some(existing_prop) => existing_prop.is_configurable(),
        };

        let prop_desc = if is_writable {
            PropertyDescriptor::data(value, true, true, can_delete)
        } else {
            PropertyDescriptor::data_value_only(value)
        };

        maybe!(define_property_or_throw(cx, global_object, name_key, prop_desc));
        maybe!(set(cx, global_object, name_key, value, false));

        let name = name.flatten();
        if !(self.var_names.contains(&name)) {
            self.var_names_field()
                .maybe_grow_for_insertion(cx)
                .insert_without_growing(name.get_());
        }

        ().into()
    }
}

#[derive(Clone)]
pub struct GlobalEnvironmentVarNamesField(Handle<GlobalEnvironment>);

impl BsHashSetField<HeapPtr<FlatString>> for GlobalEnvironmentVarNamesField {
    fn new(cx: Context, capacity: usize) -> HeapPtr<VarNamesSet> {
        VarNamesSet::new(cx, ObjectKind::GlobalEnvironmentNameSet, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<VarNamesSet> {
        self.0.var_names
    }

    fn set(&mut self, _: Context, set: HeapPtr<VarNamesSet>) {
        self.0.var_names = set;
    }
}

impl HeapObject for HeapPtr<GlobalEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<GlobalEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<DeclarativeEnvironment>()
            .visit_pointers(visitor);
        visitor.visit_pointer(&mut self.object_env);
        visitor.visit_pointer(&mut self.global_this_value);
        visitor.visit_pointer(&mut self.var_names);
    }
}

impl GlobalEnvironmentVarNamesField {
    pub fn byte_size(set: &HeapPtr<VarNamesSet>) -> usize {
        VarNamesSet::calculate_size_in_bytes(set.capacity())
    }

    pub fn visit_pointers(set: &mut HeapPtr<VarNamesSet>, visitor: &mut impl HeapVisitor) {
        set.visit_pointers(visitor);

        for element in set.iter_mut_gc_unsafe() {
            visitor.visit_pointer(element);
        }
    }
}
