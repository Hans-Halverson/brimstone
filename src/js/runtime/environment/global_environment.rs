use std::collections::HashSet;

use crate::js::runtime::{
    abstract_operations::{define_property_or_throw, has_own_property, is_extensible, set},
    completion::AbstractResult,
    error::type_error_,
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    value::Value,
    Context,
};
use crate::{maybe_, must_};

use super::{
    declarative_environment::DeclarativeEnvironment, environment::Environment,
    object_environment::ObjectEnvironment,
};

// 9.1.1.4 Global Environment Record
pub struct GlobalEnvironment {
    // The global object. [[ObjectRecord]] in spec.
    pub object_env: ObjectEnvironment,

    // Declarative environment. [[DeclarativeRecord]] in spec.
    pub decl_env: DeclarativeEnvironment,

    pub global_this_value: Gc<ObjectValue>,

    pub var_names: HashSet<String>,
}

impl GcDeref for GlobalEnvironment {}

impl GlobalEnvironment {
    // 9.1.2.5 NewGlobalEnvironment
    pub fn new(
        cx: &mut Context,
        global_object: Gc<ObjectValue>,
        global_this_value: Gc<ObjectValue>,
    ) -> Gc<GlobalEnvironment> {
        let object_env = ObjectEnvironment::new(global_object, false, None);
        // Declarative environment contains outer environment
        let decl_env = DeclarativeEnvironment::new(None);

        cx.heap.alloc(GlobalEnvironment {
            object_env,
            global_this_value,
            decl_env,
            var_names: HashSet::new(),
        })
    }
}

impl Environment for GlobalEnvironment {
    // 9.1.1.4.1 HasBinding
    fn has_binding(&self, cx: &mut Context, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(cx, name)) {
            return true.into();
        }

        self.object_env.has_binding(cx, name)
    }

    // 9.1.1.4.2 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(cx, &name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env.create_mutable_binding(cx, name, can_delete)
    }

    // 9.1.1.4.3 CreateImutableBinding
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(cx, &name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env.create_immutable_binding(cx, name, is_strict)
    }

    // 9.1.1.4.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(cx, name)) {
            return self.decl_env.initialize_binding(cx, name, value);
        }

        self.object_env.initialize_binding(cx, name, value)
    }

    // 9.1.1.4.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(cx, name)) {
            return self
                .decl_env
                .set_mutable_binding(cx, name, value, is_strict);
        }

        self.object_env
            .set_mutable_binding(cx, name, value, is_strict)
    }

    // 9.1.1.4.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> AbstractResult<Value> {
        if must_!(self.decl_env.has_binding(cx, &name)) {
            return self.decl_env.get_binding_value(cx, name, is_strict);
        }

        self.object_env.get_binding_value(cx, name, is_strict)
    }

    // 9.1.1.4.7 DeleteBinding
    fn delete_binding(&mut self, cx: &mut Context, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(cx, &name)) {
            return self.decl_env.delete_binding(cx, name);
        }

        if maybe_!(has_own_property(self.object_env.binding_object, name)) {
            let status = maybe_!(self.object_env.delete_binding(cx, name));
            if status {
                self.var_names.remove(name);
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
    fn with_base_object(&self) -> Option<Gc<ObjectValue>> {
        None
    }

    // 9.1.1.4.11 GetThisBinding
    fn get_this_binding(&self, _: &mut Context) -> AbstractResult<Value> {
        self.global_this_value.into()
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.decl_env.outer()
    }
}

impl GlobalEnvironment {
    // 9.1.1.4.12 HasVarDeclaration
    pub fn has_var_declaration(&self, name: &str) -> bool {
        self.var_names.contains(name)
    }

    // 9.1.1.4.13 HasLexicalDeclaration
    pub fn has_lexical_declaration(&self, cx: &mut Context, name: &str) -> AbstractResult<bool> {
        self.decl_env.has_binding(cx, name)
    }

    // 9.1.1.4.14 HasRestrictedGlobalProperty
    pub fn has_restricted_global_property(&self, name: &str) -> AbstractResult<bool> {
        let global_object = &self.object_env.binding_object;
        let existing_prop = maybe_!(global_object.get_own_property(name));

        match existing_prop {
            None => false.into(),
            Some(existing_prop) => (!existing_prop.is_configurable()).into(),
        }
    }

    // 9.1.1.4.15 CanDeclareGlobalVar
    pub fn can_declare_global_var(&self, name: &str) -> AbstractResult<bool> {
        let global_object = self.object_env.binding_object;
        if maybe_!(has_own_property(global_object, name)) {
            return true.into();
        }

        is_extensible(global_object)
    }

    // 9.1.1.4.16 CanDeclareGlobalFunction
    pub fn can_declare_global_function(&self, name: &str) -> AbstractResult<bool> {
        let global_object = self.object_env.binding_object;
        let existing_prop = maybe_!(global_object.get_own_property(name));

        match existing_prop {
            None => is_extensible(global_object),
            Some(existing_prop) => {
                if existing_prop.is_configurable() {
                    return true.into();
                }

                let result = existing_prop.is_data_descriptor()
                    && existing_prop.is_writable()
                    && existing_prop.is_enumerable();

                result.into()
            }
        }
    }

    // 9.1.1.4.17 CreateGlobalVarBinding
    pub fn create_global_var_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        can_delete: bool,
    ) -> AbstractResult<()> {
        let global_object = self.object_env.binding_object;
        let has_property = maybe_!(has_own_property(global_object, name));
        let is_extensible = maybe_!(is_extensible(global_object));

        if !has_property && is_extensible {
            maybe_!(self
                .object_env
                .create_mutable_binding(cx, name.to_string(), can_delete));
            maybe_!(self
                .object_env
                .initialize_binding(cx, name, Value::undefined()));
        }

        if !self.var_names.contains(name) {
            self.var_names.insert(name.to_string());
        }

        ().into()
    }

    // 9.1.1.4.18 CreateGlobalFunctionBinding
    pub fn create_global_function_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        value: Value,
        can_delete: bool,
    ) -> AbstractResult<()> {
        let global_object = self.object_env.binding_object;
        let existing_prop = maybe_!(global_object.get_own_property(&name));

        let is_writable = match existing_prop {
            None => true,
            Some(existing_prop) => existing_prop.is_configurable(),
        };

        let prop_desc = if is_writable {
            PropertyDescriptor::data(value, true, true, can_delete)
        } else {
            PropertyDescriptor::data_value_only(value)
        };

        maybe_!(define_property_or_throw(
            cx,
            global_object,
            &name,
            prop_desc
        ));
        maybe_!(set(cx, global_object, &name, value, false));

        if !(self.var_names.contains(&name)) {
            self.var_names.insert(name.to_string());
        }

        ().into()
    }
}
