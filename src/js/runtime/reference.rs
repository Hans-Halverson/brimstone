use crate::{maybe_, must_};

use super::{
    abstract_operations::set,
    completion::AbstractResult,
    environment::environment::Environment,
    error::{reference_error_, type_error_},
    execution_context::get_global_object,
    gc::Gc,
    type_utilities::to_object,
    value::Value,
    Context,
};

pub struct Reference {
    base: ReferenceBase,
    name: String,
    is_strict: bool,
    this_value: Option<Value>,
}

pub enum ReferenceBase {
    // Can only be undefined, an Object, a Boolean, a String, a Symbol, a Number, or a BigInt
    Value(Value),
    Env(Gc<dyn Environment>),
}

impl Reference {
    pub fn new_value(value: Value, name: String, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Value(value),
            name,
            is_strict,
            this_value: None,
        }
    }

    pub fn new_env(env: Gc<dyn Environment>, name: String, is_strict: bool) -> Reference {
        Reference {
            base: ReferenceBase::Env(env),
            name,
            is_strict,
            this_value: None,
        }
    }

    // 6.2.4.1 GetBase
    pub fn get_base(&self) -> &ReferenceBase {
        &self.base
    }

    // 6.2.4.2 GetReferencedName
    pub fn get_referenced_name(&self) -> &str {
        &self.name
    }

    // 6.2.4.3 IsStrictReference
    pub fn is_strict_reference(&self) -> bool {
        self.is_strict
    }

    // 6.2.4.5 IsPropertyReference
    // Can likely be removed by inlining directly at call sites.
    pub fn is_property_reference(&self) -> bool {
        match self.base {
            ReferenceBase::Env(_) => false,
            ReferenceBase::Value(value) if value.is_undefined() => false,
            ReferenceBase::Value(_) => true,
        }
    }

    // 6.2.4.6 IsUnresolvableReference
    pub fn is_unresolvable_reference(&self) -> bool {
        match self.base {
            ReferenceBase::Value(value) if value.is_undefined() => true,
            _ => false,
        }
    }

    // 6.2.4.7 IsSuperReference
    #[inline]
    pub fn is_super_reference(&self) -> bool {
        self.this_value.is_some()
    }

    // 6.2.4.8 GetValue
    pub fn get_value(&self, cx: &mut Context) -> AbstractResult<Value> {
        if self.is_unresolvable_reference() {
            return reference_error_(cx, &format!("Could not resolve {}", self.name));
        }

        match self.base {
            ReferenceBase::Value(value) => {
                let base = if value.is_object() {
                    value.as_object()
                } else {
                    // Primitive value case. Elided redundant HasPrimitiveBase check.
                    must_!(to_object(value))
                };

                base.get(self.get_referenced_name(), self.get_this_value())
            }
            ReferenceBase::Env(env) => {
                env.get_binding_value(cx, self.get_referenced_name(), self.is_strict_reference())
            }
        }
    }

    // 6.2.4.9 PutValue
    pub fn put_value(&mut self, cx: &mut Context, value: Value) -> AbstractResult<()> {
        if self.is_unresolvable_reference() {
            if self.is_strict_reference() {
                return reference_error_(cx, &format!("Could not resolve {}", self.name));
            }

            let global_obj = get_global_object(cx);
            maybe_!(set(global_obj, self.get_referenced_name(), value, false));

            return ().into();
        }

        match self.base {
            ReferenceBase::Value(value) => {
                let mut base = if value.is_object() {
                    value.as_object()
                } else {
                    // Primitive value case. Elided redundant HasPrimitiveBase check.
                    must_!(to_object(value))
                };

                let succeeded =
                    maybe_!(base.set(cx, self.get_referenced_name(), value, self.get_this_value()));
                if !succeeded && self.is_strict_reference() {
                    return type_error_(cx, &format!("Can't assign property {}", self.name));
                }

                return ().into();
            }
            ReferenceBase::Env(mut env) => env.set_mutable_binding(
                cx,
                self.get_referenced_name(),
                value,
                self.is_strict_reference(),
            ),
        }
    }

    // 6.2.4.10 GetThisValue
    pub fn get_this_value(&self) -> Value {
        match self.this_value {
            Some(value) => value,
            None => match self.base {
                ReferenceBase::Value(value) => value,
                ReferenceBase::Env(_) => unreachable!("Only called for property references"),
            },
        }
    }

    // 6.2.4.11 InitializeReferencedBinding
    pub fn initialize_referenced_binding(
        &mut self,
        cx: &mut Context,
        value: Value,
    ) -> AbstractResult<()> {
        match self.base {
            ReferenceBase::Env(mut env) => {
                env.initialize_binding(cx, self.get_referenced_name(), value)
            }
            ReferenceBase::Value(_) => unreachable!("Only called for environment references"),
        }
    }
}
