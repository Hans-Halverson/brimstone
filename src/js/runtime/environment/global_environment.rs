use std::{cell::RefCell, collections::HashSet, rc::Rc};

use crate::{maybe_, maybe__, must_};

use crate::js::runtime::{
    abstract_operations::{define_property_or_throw, has_own_property, is_extensible, set},
    completion::{AbstractResult, Completion},
    error::type_error_,
    value::{ObjectValue, PropertyDescriptor, Value},
};

use super::{
    declarative_environment::DeclarativeEnvironment, environment::Environment,
    object_environment::ObjectEnvironment,
};

// 8.1.1.4 Global Environment Record
pub struct GlobalEnvironment {
    // The global object. [[ObjectRecord]] in spec.
    pub object_env: ObjectEnvironment,

    pub global_this_val: Rc<RefCell<ObjectValue>>,

    // Declarative environment. [[DeclarativeRecord]] in spec.
    pub decl_env: DeclarativeEnvironment,

    pub var_names: HashSet<String>,
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
    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(&name)) {
            return type_error_(&format!("Redeclaration of {}", name));
        }

        self.decl_env.create_mutable_binding(name, can_delete)
    }

    // 8.1.1.4.3 CreateImutableBinding
    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(&name)) {
            return type_error_(&format!("Redeclaration of {}", name));
        }

        self.decl_env.create_immutable_binding(name, is_strict)
    }

    // 8.1.1.4.4 InitializeBinding
    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(name)) {
            return self.decl_env.initialize_binding(name, value);
        }

        self.object_env.initialize_binding(name, value)
    }

    // 8.1.1.4.5 SetMutableBinding
    fn set_mutable_binding(
        &mut self,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        if must_!(self.decl_env.has_binding(name)) {
            return self.decl_env.set_mutable_binding(name, value, is_strict);
        }

        self.object_env.set_mutable_binding(name, value, is_strict)
    }

    // 8.1.1.4.6 GetBindingValue
    fn get_binding_value(&self, name: &str, is_strict: bool) -> AbstractResult<Value> {
        if must_!(self.decl_env.has_binding(&name)) {
            return self.decl_env.get_binding_value(name, is_strict);
        }

        self.object_env.get_binding_value(name, is_strict)
    }

    // 8.1.1.4.7 DeleteBinding
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        if must_!(self.decl_env.has_binding(&name)) {
            return self.decl_env.delete_binding(name);
        }

        if maybe_!(has_own_property(
            &self.object_env.binding_obj.borrow(),
            name
        )) {
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
        Value::Undefined
    }
}

impl GlobalEnvironment {
    // 8.1.1.4.11 GetThisBinding
    fn get_this_binding(&self) -> Rc<RefCell<ObjectValue>> {
        self.global_this_val.clone()
    }

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
        let global_object = &self.object_env.binding_obj;
        let existing_prop = maybe_!(global_object.borrow().get_own_property(name));

        return match existing_prop {
            Value::Undefined => false.into(),
            Value::Object(prop_val) => {
                (!PropertyDescriptor::is_configurable(&prop_val.borrow())).into()
            }
            _ => unreachable!("GetOwnProperty returns only property descriptor or undefined"),
        };
    }

    // 8.1.1.4.15 CanDeclareGlobalVar
    fn can_declare_global_var(&self, name: &str) -> AbstractResult<bool> {
        let global_object = &self.object_env.binding_obj;
        if maybe_!(has_own_property(&global_object.borrow(), name)) {
            return true.into();
        }

        is_extensible(&global_object.borrow()).into()
    }

    // 8.1.1.4.16 CanDeclareGlobalFunction
    fn can_declare_global_function(&self, name: &str) -> AbstractResult<bool> {
        let global_object = &self.object_env.binding_obj;
        let existing_prop = maybe_!(global_object.borrow().get_own_property(name));

        match existing_prop {
            Value::Undefined => return is_extensible(&global_object.borrow()).into(),
            Value::Object(ref prop_val) => {
                let prop_val = &prop_val.borrow();
                if PropertyDescriptor::is_configurable(prop_val) {
                    return true.into();
                }

                let result = PropertyDescriptor::is_data_descriptor(prop_val)
                    && PropertyDescriptor::is_writable(prop_val)
                    && PropertyDescriptor::is_enumerable(prop_val);

                return result.into();
            }
            _ => unreachable!("GetOwnProperty returns only property descriptor or undefined"),
        }
    }

    // 8.1.1.4.17 CreateGlobalVarBinding
    fn create_global_var_binding(&mut self, name: &str, can_delete: bool) -> Completion {
        let global_object = &self.object_env.binding_obj;
        let has_property = maybe__!(has_own_property(&global_object.borrow(), name));
        let is_extensible = is_extensible(&global_object.borrow());

        if !has_property && is_extensible {
            maybe__!(self
                .object_env
                .create_mutable_binding(name.to_string(), can_delete));
            maybe__!(self.object_env.initialize_binding(name, Value::Undefined));
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
        let global_object = &self.object_env.binding_obj;
        let existing_prop = maybe__!(global_object.borrow().get_own_property(&name));
        let is_complex_prop = match existing_prop {
            Value::Undefined => true,
            Value::Object(object_val) => PropertyDescriptor::is_configurable(&object_val.borrow()),
            _ => unreachable!("GetOwnProperty returns only property descriptor or undefined"),
        };

        let prop_desc = if is_complex_prop {
            ObjectValue::new_with_value_4(
                "value".to_string(),
                value.clone(),
                "writable".to_string(),
                true.into(),
                "enumerable".to_string(),
                true.into(),
                "configurable".to_string(),
                can_delete.into(),
            )
        } else {
            ObjectValue::new_with_value_1("value".to_string(), value.clone())
        };

        maybe__!(define_property_or_throw(
            &mut global_object.borrow_mut(),
            &name,
            prop_desc
        ));

        self.object_env
            .bindings
            .get_mut(&name)
            .unwrap()
            .is_initialized = true;

        maybe__!(set(&mut global_object.borrow_mut(), &name, value, false));

        if !(self.var_names.contains(&name)) {
            self.var_names.insert(name.to_string());
        }

        Completion::empty()
    }
}
