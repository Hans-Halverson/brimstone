use std::collections::HashSet;

use crate::{
    js::{
        parser::ast,
        runtime::{
            abstract_operations::{copy_data_properties, get_v},
            array_object::array_create,
            completion::EvalResult,
            environment::environment::DynEnvironment,
            eval::{
                expression::{
                    eval_expression, eval_member_expression_to_reference, eval_property_name,
                    eval_super_member_expression_to_reference,
                },
                statement::eval_named_anonymous_function_or_expression_if,
            },
            execution_context::resolve_binding,
            gc::Gc,
            interned_strings::InternedStrings,
            iterator::{
                get_iterator, iterator_close, iterator_step, iterator_value, Iterator, IteratorHint,
            },
            ordinary_object::ordinary_object_create,
            property::Property,
            property_key::PropertyKey,
            reference::Reference,
            string_value::StringValue,
            type_utilities::require_object_coercible,
            value::Value,
            Completion, Context,
        },
    },
    maybe, must,
};

// 8.5.2 BindingInitialization
//
// Combined with 13.15.5.2 DestructuringAssignmentEvaluation from spec, since implementations are
// very close.
pub fn binding_initialization(
    cx: &mut Context,
    patt: &ast::Pattern,
    value: Value,
    env: Option<DynEnvironment>,
) -> EvalResult<()> {
    match patt {
        ast::Pattern::Id(id) => {
            let name_value = id_string_value(cx, id);
            initialize_bound_name(cx, name_value, value, env)
        }
        ast::Pattern::Array(array) => {
            let mut iterator = maybe!(get_iterator(cx, value, IteratorHint::Sync, None));
            let result = iterator_binding_initialization(cx, array, &mut iterator, env);

            if !iterator.is_done {
                let completion = if let EvalResult::Throw(thrown_value) = result {
                    Completion::throw(thrown_value)
                } else {
                    Completion::empty()
                };

                let close_completion = iterator_close(cx, &iterator, completion);
                maybe!(close_completion.into_eval_result());
            }

            result
        }
        ast::Pattern::Object(object) => object_binding_initialization(cx, object, value, env),
        ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
            unreachable!("invalid pattern for BindingInitialization")
        }
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
    env: Option<DynEnvironment>,
) -> EvalResult<()> {
    maybe!(require_object_coercible(cx, object_value));

    let mut bound_names = HashSet::new();

    for property in &object.properties {
        if property.is_rest {
            // 14.3.3.2 RestBindingInitialization
            // Evaluate property to reference
            let mut reference = match property.value.as_ref() {
                ast::Pattern::Id(id) => {
                    let name_value = id_string_value(cx, id);
                    maybe!(resolve_binding(cx, name_value, env))
                }
                ast::Pattern::Reference(ast::Expression::Member(expr)) => {
                    maybe!(eval_member_expression_to_reference(cx, &expr))
                }
                ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
                    maybe!(eval_super_member_expression_to_reference(cx, &expr))
                }
                _ => unreachable!("invalid rest property pattern"),
            };

            let rest_object = ordinary_object_create(cx);

            maybe!(copy_data_properties(cx, rest_object, object_value, &bound_names));

            if env.is_none() {
                maybe!(reference.put_value(cx, rest_object.into()))
            } else {
                maybe!(reference.initialize_referenced_binding(cx, rest_object.into()))
            }
        } else {
            let (binding_value, initializer) = maybe_extract_initializer(property.value.as_ref());

            // 14.3.3.1 PropertyBindingInitialization
            let name_key = match &property.key {
                None => id_property_key(cx, binding_value.to_id()),
                Some(expr) => maybe!(eval_property_name(cx, expr, property.is_computed)),
            };

            bound_names.insert(name_key.clone());

            // 14.3.3.3 KeyedBindingInitialization
            let mut reference = match binding_value {
                ast::Pattern::Id(id) => {
                    let binding_name = id_string_value(cx, id);
                    maybe!(resolve_binding(cx, binding_name, env))
                }
                ast::Pattern::Reference(ast::Expression::Member(expr)) => {
                    maybe!(eval_member_expression_to_reference(cx, &expr))
                }
                ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
                    maybe!(eval_super_member_expression_to_reference(cx, &expr))
                }
                binding_pattern @ (ast::Pattern::Object(_) | ast::Pattern::Array(_)) => {
                    let mut property_value = maybe!(get_v(cx, object_value, name_key));

                    if let Some(init) = initializer {
                        if property_value.is_undefined() {
                            property_value = maybe!(eval_expression(cx, init));
                        }
                    }

                    return binding_initialization(cx, binding_pattern, property_value, env);
                }
                ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
                    unreachable!("invalid object property pattern")
                }
            };

            let mut property_value = maybe!(get_v(cx, object_value, name_key));

            if let Some(init) = initializer {
                if property_value.is_undefined() {
                    // Only perform named evaluation if the binding is an identifier pattern
                    if binding_value.is_id() {
                        let binding_value_id = binding_value.to_id();
                        let value_key = id_property_key(cx, binding_value_id);
                        property_value = maybe!(eval_named_anonymous_function_or_expression_if(
                            cx,
                            init,
                            value_key,
                            || {
                                // Only perform named evaluation if id was not parenthesized
                                binding_value_id.loc.start == property.value.to_assign().loc.start
                            }
                        ));
                    } else {
                        property_value = maybe!(eval_expression(cx, init));
                    }
                }
            }

            if env.is_none() {
                maybe!(reference.put_value(cx, property_value))
            } else {
                maybe!(reference.initialize_referenced_binding(cx, property_value))
            }
        }
    }

    ().into()
}

enum ReferenceOrBindingPattern<'a> {
    Reference(Reference, Option<&'a ast::Expression>),
    Pattern(&'a ast::Pattern, Option<&'a ast::Expression>),
}

// 8.5.3 IteratorBindingInitialization
fn iterator_binding_initialization(
    cx: &mut Context,
    array_pattern: &ast::ArrayPattern,
    iterator: &mut Iterator,
    env: Option<DynEnvironment>,
) -> EvalResult<()> {
    for element in &array_pattern.elements {
        // Binding identifiers must be resolved before later steps
        let reference_or_binding_pattern = match element {
            ast::ArrayPatternElement::Pattern(pattern) => {
                let (binding_value, initializer) = maybe_extract_initializer(pattern);
                match binding_value {
                    ast::Pattern::Id(id) => {
                        let name_value = id_string_value(cx, id);
                        let reference = maybe!(resolve_binding(cx, name_value, env));
                        ReferenceOrBindingPattern::Reference(reference, initializer)
                    }
                    ast::Pattern::Reference(ast::Expression::Member(expr)) => {
                        let reference = maybe!(eval_member_expression_to_reference(cx, &expr));
                        ReferenceOrBindingPattern::Reference(reference, initializer)
                    }
                    ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
                        let reference =
                            maybe!(eval_super_member_expression_to_reference(cx, &expr));
                        ReferenceOrBindingPattern::Reference(reference, initializer)
                    }
                    binding_pattern @ (ast::Pattern::Object(_) | ast::Pattern::Array(_)) => {
                        ReferenceOrBindingPattern::Pattern(binding_pattern, initializer)
                    }
                    ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
                        unreachable!("invalid array element pattern")
                    }
                }
            }
            ast::ArrayPatternElement::Rest(ast::RestElement { argument, .. }) => {
                match argument.as_ref() {
                    ast::Pattern::Id(id) => {
                        let name_value = id_string_value(cx, id);
                        let reference = maybe!(resolve_binding(cx, name_value, env));
                        ReferenceOrBindingPattern::Reference(reference, None)
                    }
                    ast::Pattern::Reference(ast::Expression::Member(expr)) => {
                        let reference = maybe!(eval_member_expression_to_reference(cx, &expr));
                        ReferenceOrBindingPattern::Reference(reference, None)
                    }
                    ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
                        let reference =
                            maybe!(eval_super_member_expression_to_reference(cx, &expr));
                        ReferenceOrBindingPattern::Reference(reference, None)
                    }
                    binding_pattern @ (ast::Pattern::Object(_) | ast::Pattern::Array(_)) => {
                        ReferenceOrBindingPattern::Pattern(binding_pattern, None)
                    }
                    ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
                        unreachable!("invalid array element pattern")
                    }
                }
            }
            // Handle holes immediately as they ignore the rest of evaluation for this element
            ast::ArrayPatternElement::Hole => {
                if !iterator.is_done {
                    let next = iterator_step(cx, &iterator);

                    match next {
                        EvalResult::Throw(thrown_value) => {
                            iterator.is_done = true;
                            return EvalResult::Throw(thrown_value);
                        }
                        EvalResult::Ok(None) => {
                            iterator.is_done = true;
                        }
                        EvalResult::Ok(Some(_)) => {}
                    }
                }

                continue;
            }
        };

        match element {
            ast::ArrayPatternElement::Pattern(pattern) => {
                let mut value = Value::undefined();

                // Perform a step of the iterator if it is not complete
                if !iterator.is_done {
                    let next_result = iterator_step(cx, iterator);

                    match next_result {
                        EvalResult::Throw(thrown_value) => {
                            iterator.is_done = true;
                            return EvalResult::Throw(thrown_value);
                        }
                        EvalResult::Ok(None) => {
                            iterator.is_done = true;
                        }
                        EvalResult::Ok(Some(next)) => {
                            let next_value_result = iterator_value(cx, next);

                            match next_value_result {
                                EvalResult::Throw(thrown_value) => {
                                    iterator.is_done = true;
                                    return EvalResult::Throw(thrown_value);
                                }
                                EvalResult::Ok(next_value) => {
                                    value = next_value;
                                }
                            }
                        }
                    }
                }

                // Evaluate and use initializer if value is undefined and initializer is present,
                // then bind pattern.
                match reference_or_binding_pattern {
                    ReferenceOrBindingPattern::Reference(mut reference, initializer) => {
                        if value.is_undefined() {
                            if let Some(initializer) = initializer {
                                let name_key = reference.name_as_property_key(cx);
                                value = maybe!(eval_named_anonymous_function_or_expression_if(
                                    cx,
                                    initializer,
                                    name_key,
                                    || {
                                        // Only perform named evaluation if pattern is unparenthesized id
                                        let assign_pattern = pattern.to_assign();
                                        match assign_pattern.left.as_ref() {
                                            ast::Pattern::Id(id)
                                                if id.loc.start == assign_pattern.loc.start =>
                                            {
                                                true
                                            }
                                            _ => false,
                                        }
                                    }
                                ));
                            }
                        }

                        if env.is_none() {
                            maybe!(reference.put_value(cx, value));
                        } else {
                            maybe!(reference.initialize_referenced_binding(cx, value));
                        }
                    }
                    ReferenceOrBindingPattern::Pattern(pattern, initializer) => {
                        if value.is_undefined() {
                            if let Some(initializer) = initializer {
                                value = maybe!(eval_expression(cx, initializer));
                            }
                        }

                        maybe!(binding_initialization(cx, pattern, value, env));
                    }
                }
            }
            ast::ArrayPatternElement::Rest(_) => {
                let array = must!(array_create(cx, 0, None));
                let mut index = 0;

                loop {
                    // Perform a step of the iterator if it is not complete
                    let mut next_value = None;
                    if !iterator.is_done {
                        let next_result = iterator_step(cx, iterator);

                        match next_result {
                            EvalResult::Throw(thrown_value) => {
                                iterator.is_done = true;
                                return EvalResult::Throw(thrown_value);
                            }
                            EvalResult::Ok(next) => {
                                if next.is_none() {
                                    iterator.is_done = true;
                                }

                                next_value = next;
                            }
                        }
                    }

                    // Rest element is complete, bind it and return since rest must be the last element
                    if iterator.is_done {
                        return match reference_or_binding_pattern {
                            ReferenceOrBindingPattern::Reference(mut reference, _)
                                if env.is_none() =>
                            {
                                reference.put_value(cx, array.into())
                            }
                            ReferenceOrBindingPattern::Reference(mut reference, _) => {
                                reference.initialize_referenced_binding(cx, array.into())
                            }
                            ReferenceOrBindingPattern::Pattern(pattern, _) => {
                                binding_initialization(cx, pattern, array.into(), env)
                            }
                        };
                    }

                    // Get next value from iterator and append to rest array
                    let next_value = iterator_value(cx, next_value.unwrap());

                    match next_value {
                        EvalResult::Throw(thrown_value) => {
                            iterator.is_done = true;
                            return EvalResult::Throw(thrown_value);
                        }
                        EvalResult::Ok(next_value) => {
                            let property_key = PropertyKey::array_index(cx, index);
                            let property = Property::data(next_value, true, true, true);
                            array.object().set_property(cx, property_key, property);
                            index += 1;
                        }
                    }
                }
            }
            ast::ArrayPatternElement::Hole => unreachable!(),
        }
    }

    ().into()
}

// 8.5.2.1 InitializeBoundName
pub fn initialize_bound_name(
    cx: &mut Context,
    name: Gc<StringValue>,
    value: Value,
    env: Option<DynEnvironment>,
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

// 13.15.5.2 DestructuringAssignmentEvaluation
pub fn destructuring_assignment_evaluation(
    cx: &mut Context,
    patt: &ast::Pattern,
    value: Value,
) -> EvalResult<()> {
    binding_initialization(cx, patt, value, None)
}

#[inline]
pub fn id_string_value(cx: &mut Context, id: &ast::Identifier) -> Gc<StringValue> {
    InternedStrings::get_str(cx, &id.name)
}

#[inline]
pub fn id_property_key(cx: &mut Context, id: &ast::Identifier) -> PropertyKey {
    let string_value = InternedStrings::get_str(cx, &id.name);
    PropertyKey::string(cx, string_value)
}

#[inline]
pub fn private_id_property_key(cx: &mut Context, id: &ast::Identifier) -> PropertyKey {
    let private_string_value = cx.alloc_string(format!("#{}", id.name));
    PropertyKey::string(cx, private_string_value)
}

// Initializers are represented as an assignment pattern as the value, wrapping the binding pattern.
// Unwrap this structure into the inner binding pattern and the initializer, if one exists.
pub fn maybe_extract_initializer(
    pattern: &ast::Pattern,
) -> (&ast::Pattern, Option<&ast::Expression>) {
    match pattern {
        ast::Pattern::Assign(ast::AssignmentPattern { left, right, .. }) => {
            (left.as_ref(), Some(right.as_ref()))
        }
        other_pattern => (other_pattern, None),
    }
}
