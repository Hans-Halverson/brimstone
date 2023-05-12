use crate::maybe;

use super::{
    abstract_operations::{private_get, private_set, set},
    completion::EvalResult,
    environment::{environment::DynEnvironment, private_environment::PrivateName},
    error::{reference_error_, type_error_},
    gc::Handle,
    interned_strings::InternedStrings,
    property_key::PropertyKey,
    string_value::StringValue,
    type_utilities::to_object,
    Context, Value,
};

// 6.2.4 Reference Record
pub struct Reference {
    base: ReferenceBase,
    is_strict: bool,
    this_value: Option<Handle<Value>>,
}

pub enum ReferenceBase {
    Unresolvable {
        name: Handle<StringValue>,
    },
    Property {
        object: Handle<Value>,
        property: Handle<PropertyKey>,
        private_name: Option<PrivateName>,
    },
    Env {
        env: DynEnvironment,
        name: Handle<StringValue>,
    },
}

impl Reference {
    // An empty reference that will never be used
    pub const EMPTY: Reference = Reference {
        base: ReferenceBase::Property {
            object: Handle::<Value>::dangling(),
            property: Handle::<PropertyKey>::dangling(),
            private_name: None,
        },
        is_strict: false,
        this_value: None,
    };

    pub fn new_unresolvable(name: Handle<StringValue>, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Unresolvable { name },
            is_strict,
            this_value: None,
        }
    }

    pub fn new_property(
        object: Handle<Value>,
        property: Handle<PropertyKey>,
        is_strict: bool,
    ) -> Reference {
        Reference {
            base: ReferenceBase::Property { object, property, private_name: None },
            is_strict,
            this_value: None,
        }
    }

    pub fn new_property_with_this(
        object: Handle<Value>,
        property: Handle<PropertyKey>,
        is_strict: bool,
        this_value: Handle<Value>,
    ) -> Reference {
        Reference {
            base: ReferenceBase::Property { object, property, private_name: None },
            is_strict,
            this_value: Some(this_value),
        }
    }

    pub fn new_env(env: DynEnvironment, name: Handle<StringValue>, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Env { env, name },
            is_strict,
            this_value: None,
        }
    }

    pub fn base(&self) -> &ReferenceBase {
        &self.base
    }

    pub fn name_as_property_key(&self, cx: &mut Context) -> Handle<PropertyKey> {
        match self.base {
            ReferenceBase::Unresolvable { name, .. } | ReferenceBase::Env { name, .. } => {
                PropertyKey::string(cx, name).to_handle(cx)
            }
            ReferenceBase::Property { property, .. } => property,
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
    pub fn get_value(&self, cx: &mut Context) -> EvalResult<Handle<Value>> {
        match self.base {
            ReferenceBase::Unresolvable { name } => {
                reference_error_(cx, &format!("Could not resolve {}", name))
            }
            ReferenceBase::Property { object, property, private_name } => {
                let base = maybe!(to_object(cx, object));
                match private_name {
                    Some(private_name) => return private_get(cx, base, private_name),
                    None => base.get(cx, property, self.get_this_value()),
                }
            }
            ReferenceBase::Env { env, name } => env.get_binding_value(cx, name, self.is_strict),
        }
    }

    // 6.2.4.6 PutValue
    pub fn put_value(&mut self, cx: &mut Context, value: Handle<Value>) -> EvalResult<()> {
        match self.base {
            ReferenceBase::Unresolvable { name } => {
                if self.is_strict {
                    return reference_error_(cx, &format!("Could not resolve {}", name));
                }

                let global_obj = cx.get_global_object();
                let property_key = PropertyKey::string(cx, name).to_handle(cx);
                maybe!(set(cx, global_obj, property_key, value, false));

                return ().into();
            }
            ReferenceBase::Property { object, property, private_name } => {
                let mut base = maybe!(to_object(cx, object));
                if let Some(private_name) = private_name {
                    return private_set(cx, base, private_name, value);
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
    pub fn get_this_value(&self) -> Handle<Value> {
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
        value: Handle<Value>,
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
        base_value: Handle<Value>,
        private_name_str: &str,
    ) -> Reference {
        let private_name = cx
            .current_execution_context_ptr()
            .private_env_ptr()
            .unwrap()
            .resolve_private_identifier(private_name_str);

        let private_name_string_value = InternedStrings::get_str(cx, private_name_str);
        let property_key = PropertyKey::string(cx, private_name_string_value).to_handle(cx);

        Reference {
            base: ReferenceBase::Property {
                object: base_value,
                property: property_key,
                private_name: Some(private_name),
            },
            is_strict: true,
            this_value: None,
        }
    }
}
