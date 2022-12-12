use std::collections::HashSet;

use crate::js::runtime::{
    abstract_operations::{define_property_or_throw, has_own_property, is_extensible, set},
    completion::{AbstractResult, Completion},
    error::type_error_,
    gc::Gc,
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    value::Value,
    Context,
};
use crate::{maybe_, maybe__, must_};

use super::{
    declarative_environment::DeclarativeEnvironment, environment::Environment,
    object_environment::ObjectEnvironment,
};

// 8.1.1.4 Global Environment Record
pub struct GlobalEnvironment {
    // The global object. [[ObjectRecord]] in spec.
    pub object_env: ObjectEnvironment,

    // Declarative environment. [[DeclarativeRecord]] in spec.
    pub decl_env: DeclarativeEnvironment,

    pub global_this_value: Gc<ObjectValue>,

    pub var_names: HashSet<String>,
}

impl GlobalEnvironment {
    // 8.1.2.5 NewGlobalEnvironment
    pub fn new(
        cx: &mut Context,
        global_object: Gc<ObjectValue>,
        global_this_value: Gc<ObjectValue>,
    ) -> Gc<GlobalEnvironment> {
        let object_env = ObjectEnvironment {
            binding_object: global_object,
            with_environment: false,
            outer: None,
        };
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
    // 8.1.1.4.1 HasBinding
    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(name)) {
            return true.into();
        }

        self.object_env.has_binding(name)
    }

    // 8.1.1.4.2 CreateMutableBinding
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(&name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env.create_mutable_binding(cx, name, can_delete)
    }

    // 8.1.1.4.3 CreateImutableBinding
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(&name)) {
            return type_error_(cx, &format!("Redeclaration of {}", name));
        }

        self.decl_env.create_immutable_binding(cx, name, is_strict)
    }

    // 8.1.1.4.4 InitializeBinding
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(name)) {
            return self.decl_env.initialize_binding(cx, name, value);
        }

        self.object_env.initialize_binding(cx, name, value)
    }

    // 8.1.1.4.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(name)) {
            return self
                .decl_env
                .set_mutable_binding(cx, name, value, is_strict);
        }

        self.object_env
            .set_mutable_binding(cx, name, value, is_strict)
    }

    // 8.1.1.4.6 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> AbstractResult<Value> {
        if must_!(self.decl_env.has_binding(&name)) {
            return self.decl_env.get_binding_value(cx, name, is_strict);
        }

        self.object_env.get_binding_value(cx, name, is_strict)
    }

    // 8.1.1.4.7 DeleteBinding
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(&name)) {
            return self.decl_env.delete_binding(name);
        }

        if maybe_!(has_own_property(self.object_env.binding_object, name)) {
            let status = maybe_!(self.object_env.delete_binding(name));
            if status {
                self.var_names.remove(name);
            }

            return status.into();
        }

        return true.into();
    }

    // 8.1.1.4.8 HasThisBinding
    fn has_this_binding(&self) -> bool {
        true
    }

    // 8.1.1.4.9 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        false
    }

    // 8.1.1.4.10 WithBaseObject
    fn with_base_object(&self) -> Value {
        Value::undefined()
    }

    // 8.1.1.4.11 GetThisBinding
    fn get_this_binding(&self, _: &mut Context) -> AbstractResult<Value> {
        self.global_this_value.into()
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.decl_env.outer()
    }
}

impl GlobalEnvironment {
    // 8.1.1.4.12 HasVarDeclaration
    fn has_var_declaration(&self, name: &str) -> bool {
        self.var_names.contains(name)
    }

    // 8.1.1.4.13 HasLexicalDeclaration
    fn has_lexical_declaration(&self, name: &str) -> AbstractResult<bool> {
        self.decl_env.has_binding(name)
    }

    // 8.1.1.4.14 HasRestrictedGlobalProperty
    fn has_restricted_global_property(&self, name: &str) -> AbstractResult<bool> {
        let global_object = &self.object_env.binding_object;
        let existing_prop = maybe_!(global_object.get_own_property(name));

        match existing_prop {
            None => false.into(),
            Some(existing_prop) => (!existing_prop.is_configurable()).into(),
        }
    }

    // 8.1.1.4.15 CanDeclareGlobalVar
    fn can_declare_global_var(&self, name: &str) -> AbstractResult<bool> {
        let global_object = self.object_env.binding_object;
        if maybe_!(has_own_property(global_object, name)) {
            return true.into();
        }

        is_extensible(global_object).into()
    }

    // 8.1.1.4.16 CanDeclareGlobalFunction
    fn can_declare_global_function(&self, name: &str) -> AbstractResult<bool> {
        let global_object = self.object_env.binding_object;
        let existing_prop = maybe_!(global_object.get_own_property(name));

        match existing_prop {
            None => is_extensible(global_object).into(),
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

    // 8.1.1.4.17 CreateGlobalVarBinding
    fn create_global_var_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        can_delete: bool,
    ) -> Completion {
        let global_object = self.object_env.binding_object;
        let has_property = maybe__!(has_own_property(global_object, name));
        let is_extensible = is_extensible(global_object);

        if !has_property && is_extensible {
            maybe__!(self
                .object_env
                .create_mutable_binding(cx, name.to_string(), can_delete));
            maybe__!(self
                .object_env
                .initialize_binding(cx, name, Value::undefined()));
        }

        if !self.var_names.contains(name) {
            self.var_names.insert(name.to_string());
        }

        Completion::empty()
    }

    // 8.1.1.4.18 CreateGlobalFunctionBinding
    fn create_global_function_binding(
        &mut self,
        name: String,
        value: Value,
        can_delete: bool,
    ) -> Completion {
        let global_object = self.object_env.binding_object;
        let existing_prop = maybe__!(global_object.get_own_property(&name));

        let is_writable = match existing_prop {
            None => true,
            Some(existing_prop) => existing_prop.is_configurable(),
        };

        let prop_desc = if is_writable {
            PropertyDescriptor::data(value, true, true, can_delete)
        } else {
            PropertyDescriptor::data_value_only(value)
        };

        maybe__!(define_property_or_throw(global_object, &name, prop_desc));
        maybe__!(set(global_object, &name, value, false));

        if !(self.var_names.contains(&name)) {
            self.var_names.insert(name.to_string());
        }

        Completion::empty()
    }
}
