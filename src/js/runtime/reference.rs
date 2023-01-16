use crate::maybe;

use super::{
    abstract_operations::{private_get, private_set, set},
    completion::EvalResult,
    environment::{environment::Environment, private_environment::PrivateNameId},
    error::{reference_error_, type_error_},
    execution_context::get_global_object,
    gc::Gc,
    property_key::PropertyKey,
    type_utilities::to_object,
    value::{StringValue, Value},
    Context,
};

// 6.2.4 Reference Record
pub struct Reference {
    base: ReferenceBase,
    is_strict: bool,
    this_value: Option<Value>,
}

pub enum ReferenceBase {
    Unresolvable {
        name: Gc<StringValue>,
    },
    Property {
        object: Value,
        property: PropertyKey,
        private_id: Option<PrivateNameId>,
    },
    Env {
        env: Gc<dyn Environment>,
        name: Gc<StringValue>,
    },
}

impl Reference {
    pub fn new_unresolvable(name: Gc<StringValue>, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Unresolvable { name },
            is_strict,
            this_value: None,
        }
    }

    pub fn new_property(object: Value, property: PropertyKey, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Property { object, property, private_id: None },
            is_strict,
            this_value: None,
        }
    }

    pub fn new_property_with_this(
        object: Value,
        property: PropertyKey,
        is_strict: bool,
        this_value: Value,
    ) -> Reference {
        Reference {
            base: ReferenceBase::Property { object, property, private_id: None },
            is_strict,
            this_value: Some(this_value),
        }
    }

    pub fn new_env(env: Gc<dyn Environment>, name: Gc<StringValue>, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Env { env, name },
            is_strict,
            this_value: None,
        }
    }

    pub fn base(&self) -> &ReferenceBase {
        &self.base
    }

    pub fn name_as_property_key(&self) -> PropertyKey {
        match self.base {
            ReferenceBase::Unresolvable { name, .. } | ReferenceBase::Env { name, .. } => {
                PropertyKey::string(name)
            }
            ReferenceBase::Property { ref property, .. } => property.clone(),
        }
    }

    pub fn is_strict(&self) -> bool {
        self.is_strict
    }

    // 6.2.4.2 IsUnresolvableReference
    pub fn is_unresolvable_reference(&self) -> bool {
        match self.base {
            ReferenceBase::Unresolvable { .. } => true,
            _ => false,
        }
    }

    // 6.2.4.3 IsSuperReference
    #[inline]
    pub fn is_super_reference(&self) -> bool {
        self.this_value.is_some()
    }

    // 6.2.4.5 GetValue
    pub fn get_value(&self, cx: &mut Context) -> EvalResult<Value> {
        match self.base {
            ReferenceBase::Unresolvable { name } => {
                reference_error_(cx, &format!("Could not resolve {}", name.str()))
            }
            ReferenceBase::Property { object, ref property, private_id } => {
                let base = maybe!(to_object(cx, object));
                match private_id {
                    Some(private_id) => return private_get(cx, base, private_id),
                    None => base.get(cx, &property, self.get_this_value()),
                }
            }
            ReferenceBase::Env { env, name } => env.get_binding_value(cx, name, self.is_strict),
        }
    }

    // 6.2.4.6 PutValue
    pub fn put_value(&mut self, cx: &mut Context, value: Value) -> EvalResult<()> {
        match self.base {
            ReferenceBase::Unresolvable { name } => {
                if self.is_strict {
                    return reference_error_(cx, &format!("Could not resolve {}", name.str()));
                }

                let global_obj = get_global_object(cx);
                maybe!(set(cx, global_obj, &PropertyKey::string(name), value, false));

                return ().into();
            }
            ReferenceBase::Property { object, ref property, private_id } => {
                let mut base = maybe!(to_object(cx, object));
                if let Some(private_id) = private_id {
                    return private_set(cx, base, private_id, value);
                }

                let succeeded = maybe!(base.set(cx, property, value, self.get_this_value()));
                if !succeeded && self.is_strict {
                    return type_error_(cx, &format!("Can't assign property {}", property));
                }

                return ().into();
            }
            ReferenceBase::Env { mut env, name } => {
                env.set_mutable_binding(cx, name, value, self.is_strict)
            }
        }
    }

    // 6.2.4.7 GetThisValue
    pub fn get_this_value(&self) -> Value {
        match self.this_value {
            Some(value) => value,
            None => match self.base {
                ReferenceBase::Property { object, .. } => object,
                ReferenceBase::Env { .. } | ReferenceBase::Unresolvable { .. } => {
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
            ReferenceBase::Env { mut env, name, .. } => env.initialize_binding(cx, name, value),
            ReferenceBase::Property { .. } | ReferenceBase::Unresolvable { .. } => {
                unreachable!("Only called for environment references")
            }
        }
    }

    // 6.2.4.9 MakePrivateReference
    pub fn make_private_reference(
        cx: &mut Context,
        base_value: Value,
        private_name: Gc<StringValue>,
    ) -> Reference {
        let private_env = cx.current_execution_context().private_env.unwrap();
        let private_id = private_env.resolve_private_identifier(private_name.str());

        Reference {
            base: ReferenceBase::Property {
                object: base_value,
                property: PropertyKey::string(private_name),
                private_id: Some(private_id),
            },
            is_strict: true,
            this_value: None,
        }
    }
}
