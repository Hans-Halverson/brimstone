use std::collections::HashSet;

use crate::{
    js::{
        parser::ast,
        runtime::{
            abstract_operations::{copy_data_properties, get_v},
            completion::EvalResult,
            environment::environment::Environment,
            eval::{
                expression::{eval_expression, eval_property_name},
                statement::eval_named_anonymous_function_or_expression,
            },
            execution_context::resolve_binding,
            gc::Gc,
            intrinsics::intrinsics::Intrinsic,
            object_value::ObjectValue,
            ordinary_object::ordinary_object_create,
            property_key::PropertyKey,
            type_utilities::require_object_coercible,
            value::{StringValue, Value},
            Context,
        },
    },
    maybe, must,
};

// 8.5.2 BindingInitialization
pub fn binding_initialization(
    cx: &mut Context,
    patt: &ast::Pattern,
    value: Value,
    env: Option<Gc<dyn Environment>>,
) -> EvalResult<()> {
    match patt {
        ast::Pattern::Id(id) => {
            let name_value = id_string_value(cx, id);
            initialize_bound_name(cx, name_value, value, env)
        }
        ast::Pattern::Array(_) => unimplemented!("array patterns"),
        ast::Pattern::Object(object) => object_binding_initialization(cx, object, value, env),
        ast::Pattern::Assign(_) => unreachable!(),
    }
}

// Combination of object binding initialization sections from spec:
//
// 8.5.2 BindingInitialization
// 14.3.3.1 PropertyBindingInitialization,
// 14.3.3.2 RestBindingInitialization
// 14.3.3.3 KeyedBindingInitialization.
fn object_binding_initialization(
    cx: &mut Context,
    object: &ast::ObjectPattern,
    object_value: Value,
    env: Option<Gc<dyn Environment>>,
) -> EvalResult<()> {
    maybe!(require_object_coercible(cx, object_value));

    let mut bound_names = HashSet::new();

    for property in &object.properties {
        if property.is_rest {
            // 14.3.3.2 RestBindingInitialization
            let name_value = id_string_value(cx, property.value.to_id());
            let mut reference = maybe!(resolve_binding(cx, name_value, env));

            let object_proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
            let rest_object: Gc<ObjectValue> =
                cx.heap.alloc(ordinary_object_create(object_proto)).into();

            maybe!(copy_data_properties(cx, rest_object, object_value, &bound_names));

            if env.is_none() {
                maybe!(reference.put_value(cx, rest_object.into()))
            } else {
                maybe!(reference.initialize_referenced_binding(cx, rest_object.into()))
            }
        } else {
            // Initializers are represented as an assignment pattern as the value
            let (binding_value, initializer) = match property.value.as_ref() {
                ast::Pattern::Assign(ast::AssignmentPattern { left, right, .. }) => {
                    (left.as_ref(), Some(right.as_ref()))
                }
                other_pattern => (other_pattern, None),
            };

            // 14.3.3.1 PropertyBindingInitialization
            let name_key = match &property.key {
                None => id_property_key(cx, binding_value.to_id()),
                Some(expr) => maybe!(eval_property_name(cx, expr, property.is_computed)),
            };

            // 14.3.3.3 KeyedBindingInitialization
            match binding_value {
                ast::Pattern::Id(id) => {
                    let binding_name = id_string_value(cx, id);
                    let mut reference = maybe!(resolve_binding(cx, binding_name, env));

                    let mut property_value = maybe!(get_v(cx, object_value, &name_key));

                    bound_names.insert(PropertyKey::string(binding_name));

                    if let Some(init) = initializer {
                        if property_value.is_undefined() {
                            property_value = maybe!(eval_named_anonymous_function_or_expression(
                                cx, init, &name_key
                            ));
                        }
                    }

                    if env.is_none() {
                        maybe!(reference.put_value(cx, property_value))
                    } else {
                        maybe!(reference.initialize_referenced_binding(cx, property_value))
                    }
                }
                binding_pattern => {
                    let mut property_value = maybe!(get_v(cx, object_value, &name_key));

                    if let Some(init) = initializer {
                        if property_value.is_undefined() {
                            property_value = maybe!(eval_expression(cx, init));
                        }
                    }

                    maybe!(binding_initialization(cx, binding_pattern, property_value, env));
                }
            }
        }
    }

    ().into()
}

// 8.5.2.1 InitializeBoundName
pub fn initialize_bound_name(
    cx: &mut Context,
    name: Gc<StringValue>,
    value: Value,
    env: Option<Gc<dyn Environment>>,
) -> EvalResult<()> {
    match env {
        Some(mut env) => {
            must!(env.initialize_binding(cx, name, value));
            ().into()
        }
        None => {
            let mut reference = maybe!(resolve_binding(cx, name, env));
            reference.put_value(cx, value)
        }
    }
}

#[inline]
pub fn id_string_value(cx: &mut Context, id: &ast::Identifier) -> Gc<StringValue> {
    cx.get_interned_string(&id.name)
}

#[inline]
pub fn id_property_key(cx: &mut Context, id: &ast::Identifier) -> PropertyKey {
    PropertyKey::string(cx.get_interned_string(&id.name))
}
