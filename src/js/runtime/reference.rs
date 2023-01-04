use crate::maybe;

use super::{
    abstract_operations::{private_get, private_set, set},
    completion::EvalResult,
    environment::{environment::Environment, private_environment::PrivateNameId},
    error::{reference_error_, type_error_},
    execution_context::get_global_object,
    gc::Gc,
    type_utilities::to_object,
    value::Value,
    Context,
};

// 6.2.4 Reference Record
pub struct Reference {
    base: ReferenceBase,
    name: String,
    is_strict: bool,
    this_value: Option<Value>,
    private_name: Option<PrivateNameId>,
}

pub enum ReferenceBase {
    Unresolvable,
    Value(Value),
    Env(Gc<dyn Environment>),
}

impl Reference {
    pub fn new_unresolvable(name: String, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Unresolvable,
            name,
            is_strict,
            this_value: None,
            private_name: None,
        }
    }

    pub fn new_value(value: Value, name: String, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Value(value),
            name,
            is_strict,
            this_value: None,
            private_name: None,
        }
    }

    pub fn new_env(env: Gc<dyn Environment>, name: String, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Env(env),
            name,
            is_strict,
            this_value: None,
            private_name: None,
        }
    }

    pub fn base(&self) -> &ReferenceBase {
        &self.base
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_strict(&self) -> bool {
        self.is_strict
    }

    // 6.2.4.2 IsUnresolvableReference
    pub fn is_unresolvable_reference(&self) -> bool {
        match self.base {
            ReferenceBase::Unresolvable => true,
            _ => false,
        }
    }

    // 6.2.4.3 IsSuperReference
    #[inline]
    pub fn is_super_reference(&self) -> bool {
        self.this_value.is_some()
    }

    // 6.2.4.4 IsPrivateReference
    pub fn is_private_reference(&self) -> bool {
        self.private_name.is_some()
    }

    // 6.2.4.5 GetValue
    pub fn get_value(&self, cx: &mut Context) -> EvalResult<Value> {
        match self.base {
            ReferenceBase::Unresolvable => {
                reference_error_(cx, &format!("Could not resolve {}", self.name))
            }
            ReferenceBase::Value(value) => {
                let base = maybe!(to_object(cx, value));
                if self.is_private_reference() {
                    return private_get(base, &self.name);
                }

                base.get(cx, &self.name, self.get_this_value())
            }
            ReferenceBase::Env(env) => env.get_binding_value(cx, &self.name, self.is_strict),
        }
    }

    // 6.2.4.6 PutValue
    pub fn put_value(&mut self, cx: &mut Context, value: Value) -> EvalResult<()> {
        match self.base {
            ReferenceBase::Unresolvable => {
                if self.is_strict {
                    return reference_error_(cx, &format!("Could not resolve {}", self.name));
                }

                let global_obj = get_global_object(cx);
                maybe!(set(cx, global_obj, &self.name, value, false));

                return ().into();
            }
            ReferenceBase::Value(base_value) => {
                let mut base = maybe!(to_object(cx, base_value));
                if self.is_private_reference() {
                    return private_set(base, &self.name, value);
                }

                let succeeded = maybe!(base.set(cx, &self.name, value, self.get_this_value()));
                if !succeeded && self.is_strict {
                    return type_error_(cx, &format!("Can't assign property {}", self.name));
                }

                return ().into();
            }
            ReferenceBase::Env(mut env) => {
                env.set_mutable_binding(cx, &self.name, value, self.is_strict)
            }
        }
    }

    // 6.2.4.7 GetThisValue
    pub fn get_this_value(&self) -> Value {
        match self.this_value {
            Some(value) => value,
            None => match self.base {
                ReferenceBase::Value(value) => value,
                ReferenceBase::Env(_) | ReferenceBase::Unresolvable => {
                    unreachable!("Only called for property references")
                }
            },
        }
    }

    // 6.2.4.8 InitializeReferencedBinding
    pub fn initialize_referenced_binding(
        &mut self,
        cx: &mut Context,
        value: Value,
    ) -> EvalResult<()> {
        match self.base {
            ReferenceBase::Env(mut env) => env.initialize_binding(cx, &self.name, value),
            ReferenceBase::Value(_) | ReferenceBase::Unresolvable => {
                unreachable!("Only called for environment references")
            }
        }
    }

    // 6.2.4.9 MakePrivateReference
    pub fn make_private_reference(
        cx: &mut Context,
        base_value: Value,
        private_identifier: String,
    ) -> Reference {
        let private_env = cx.current_execution_context().private_env.unwrap();
        let private_name = private_env.resolve_private_identifier(&private_identifier);

        Reference {
            base: ReferenceBase::Value(base_value),
            name: private_identifier,
            is_strict: true,
            this_value: None,
            private_name: Some(private_name),
        }
    }
}
