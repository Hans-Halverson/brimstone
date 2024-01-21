use std::{collections::HashSet, convert::TryInto};

use num_bigint::{BigInt, Sign};

use crate::{
    js::{
        parser::ast::{self, AstPtr, UpdateOperator},
        runtime::{
            abstract_operations::{
                call, call_object, construct, copy_data_properties, create_data_property_or_throw,
                define_property_or_throw, get_method, has_property, initialize_instance_elements,
                ordinary_has_instance, private_get, set_integrity_level, IntegrityLevel,
            },
            array_object::array_create,
            completion::EvalResult,
            environment::environment::Environment,
            error::{range_error_, reference_error_, type_error_},
            eval::{
                pattern::destructuring_assignment_evaluation,
                statement::eval_named_anonymous_function_or_expression_if,
            },
            execution_context::{
                get_new_target, get_this_environment, resolve_binding, resolve_this_binding,
            },
            interned_strings::InternedStrings,
            intrinsics::{intrinsics::Intrinsic, regexp_constructor::RegExpObject},
            iterator::iter_iterator_values,
            numeric_operations::number_exponentiate,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::ordinary_object_create,
            property::Property,
            property_descriptor::PropertyDescriptor,
            property_key::PropertyKey,
            reference::{Reference, ReferenceBase},
            string_value::StringValue,
            type_utilities::{
                is_callable, is_constructor, is_less_than, is_loosely_equal, is_strictly_equal,
                number_to_string, same_object_value, to_boolean, to_int32, to_number, to_numeric,
                to_object, to_primitive, to_property_key, to_string, to_uint32,
                ToPrimitivePreferredType,
            },
            value::{BigIntValue, Value, BOOL_TAG, NULL_TAG, UNDEFINED_TAG},
            Context, Handle,
        },
    },
    maybe, must,
};

use super::{
    class::eval_class_expression,
    eval::perform_eval,
    function::{
        instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
        method_definition_evaluation,
    },
    pattern::{id_property_key, id_string_value},
    statement::eval_named_anonymous_function_or_expression,
};

pub fn eval_expression(cx: Context, expr: &ast::Expression) -> EvalResult<Handle<Value>> {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
        ast::Expression::Null(_) => eval_null_literal(cx),
        ast::Expression::Boolean(lit) => eval_boolean_literal(cx, lit),
        ast::Expression::Number(lit) => eval_number_literal(cx, lit),
        ast::Expression::String(lit) => eval_string_literal(cx, lit),
        ast::Expression::BigInt(lit) => eval_bigint_literal(cx, lit),
        ast::Expression::RegExp(lit) => eval_regexp_literal(cx, lit),
        ast::Expression::Unary(expr) => match expr.operator {
            ast::UnaryOperator::Plus => eval_unary_plus(cx, expr),
            ast::UnaryOperator::Minus => eval_unary_minus(cx, expr),
            ast::UnaryOperator::LogicalNot => eval_logical_not_expression(cx, expr),
            ast::UnaryOperator::BitwiseNot => eval_bitwise_not(cx, expr),
            ast::UnaryOperator::TypeOf => eval_typeof_expression(cx, expr),
            ast::UnaryOperator::Delete => eval_delete_expression(cx, expr),
            ast::UnaryOperator::Void => eval_void_expression(cx, expr),
        },
        ast::Expression::Binary(expr) => eval_binary_expression(cx, expr),
        ast::Expression::Logical(expr) => eval_logical_expression(cx, expr),
        ast::Expression::Assign(expr) => eval_assignment_expression(cx, expr),
        ast::Expression::Update(expr) => eval_update_expression(cx, expr),
        ast::Expression::Member(expr) => eval_member_expression(cx, expr),
        ast::Expression::Chain(expr) => eval_chain_expression(cx, expr),
        ast::Expression::Conditional(expr) => eval_conditional_expression(cx, expr),
        ast::Expression::Call(expr) => eval_call_expression(cx, expr),
        ast::Expression::New(expr) => eval_new_expression(cx, expr),
        ast::Expression::Sequence(expr) => eval_sequence_expression(cx, expr),
        ast::Expression::Array(expr) => eval_array_expression(cx, expr),
        ast::Expression::Object(expr) => eval_object_expression(cx, expr),
        ast::Expression::Function(func) => eval_function_expression(cx, func),
        ast::Expression::ArrowFunction(func) => eval_arrow_function(cx, func),
        ast::Expression::Class(class) => eval_class_expression(cx, class),
        ast::Expression::This(_) => eval_this_expression(cx),
        ast::Expression::Await(_) => unimplemented!("await expression"),
        ast::Expression::Yield(_) => unimplemented!("yield expression"),
        ast::Expression::SuperMember(expr) => eval_super_member_expression(cx, expr),
        ast::Expression::SuperCall(expr) => eval_super_call_expression(cx, expr),
        ast::Expression::Template(lit) => eval_template_literal(cx, lit),
        ast::Expression::TaggedTemplate(expr) => eval_tagged_template_expression(cx, expr),
        ast::Expression::MetaProperty(expr) => match expr.kind {
            ast::MetaPropertyKind::NewTarget => eval_new_target(cx),
            ast::MetaPropertyKind::ImportMeta => unimplemented!("import.meta"),
        },
        ast::Expression::Import(_) => unimplemented!("import expression"),
    }
}

// 13.1.3 Identifier Evaluation
pub fn eval_identifier(cx: Context, id: &ast::Identifier) -> EvalResult<Handle<Value>> {
    let name_value = id_string_value(cx, id);
    let reference = maybe!(resolve_binding(cx, name_value, None));

    // Unlike the spec, greedily call GetValue here as all eval functions evaluate to a value.
    reference.get_value(cx)
}

// Same as eval_identifier, but returns a reference instead of a value
pub fn eval_identifier_to_reference(cx: Context, id: &ast::Identifier) -> EvalResult<Reference> {
    let name_value = id_string_value(cx, id);
    resolve_binding(cx, name_value, None)
}

// 13.2.1.1 This Expression Evaluation
fn eval_this_expression(cx: Context) -> EvalResult<Handle<Value>> {
    resolve_this_binding(cx)
}

// 13.2.3.1 Literal Evaluation
fn eval_null_literal(cx: Context) -> EvalResult<Handle<Value>> {
    cx.null().into()
}

fn eval_boolean_literal(cx: Context, lit: &ast::BooleanLiteral) -> EvalResult<Handle<Value>> {
    cx.bool(lit.value).into()
}

fn eval_number_literal(cx: Context, lit: &ast::NumberLiteral) -> EvalResult<Handle<Value>> {
    Value::number(lit.value).to_handle(cx).into()
}

fn eval_string_literal(cx: Context, lit: &ast::StringLiteral) -> EvalResult<Handle<Value>> {
    let interned_value = InternedStrings::get_wtf8_str(cx, &lit.value);
    interned_value.into()
}

fn eval_bigint_literal(cx: Context, lit: &ast::BigIntLiteral) -> EvalResult<Handle<Value>> {
    BigIntValue::new(cx, lit.value.clone()).to_handle().into()
}

// 13.2.4.2 Array Initializer Evaluation
// 13.2.4.1 ArrayAccumulation
fn eval_array_expression(cx: Context, expr: &ast::ArrayExpression) -> EvalResult<Handle<Value>> {
    let array = must!(array_create(cx, 0, None));

    // Property key is shared between iterations
    let mut key = PropertyKey::uninit().to_handle(cx);

    let mut index = 0;
    for element in expr.elements.iter() {
        match element {
            ast::ArrayElement::Hole => {
                key.replace(PropertyKey::array_index(cx, index));
                let desc = Property::data(cx.empty(), true, true, true);

                array.object().set_property(cx, key, desc);
                index += 1;
            }
            ast::ArrayElement::Expression(expr) => {
                key.replace(PropertyKey::array_index(cx, index));
                let element_value = maybe!(eval_expression(cx, expr));
                let desc = Property::data(element_value, true, true, true);

                array.object().set_property(cx, key, desc);
                index += 1;
            }
            ast::ArrayElement::Spread(spread) => {
                let iterable = maybe!(eval_expression(cx, &spread.argument));
                let completion = iter_iterator_values(cx, iterable, &mut |cx, value| {
                    key.replace(PropertyKey::array_index(cx, index));
                    let desc = Property::data(value, true, true, true);

                    array.object().set_property(cx, key, desc);
                    index += 1;

                    None
                });

                maybe!(completion.into_eval_result());
            }
        }
    }

    array.into()
}

// 13.2.5.4 Object Initializer Evaluation
fn eval_object_expression(cx: Context, expr: &ast::ObjectExpression) -> EvalResult<Handle<Value>> {
    let mut object = ordinary_object_create(cx);

    for property in &expr.properties {
        match property.value.as_ref() {
            // Spread element
            _ if {
                if let ast::PropertyKind::Spread(_) = property.kind {
                    true
                } else {
                    false
                }
            } =>
            {
                let from_value = maybe!(eval_expression(cx, &property.key));
                maybe!(copy_data_properties(cx, object, from_value, &HashSet::new()));
            }
            // Identifier shorthand property
            None => {
                let id = property.key.as_ref().to_id();
                let prop_value = maybe!(eval_identifier(cx, id));
                let prop_key = id_property_key(cx, id);
                must!(create_data_property_or_throw(cx, object, prop_key, prop_value));
            }
            Some(_) if property.is_method => {
                let property_key =
                    maybe!(eval_property_name(cx, &property.key, property.is_computed));
                let func = if let Some(ast::Expression::Function(func)) = property.value.as_deref()
                {
                    func
                } else {
                    unreachable!()
                };

                maybe!(method_definition_evaluation(
                    cx,
                    object,
                    func,
                    property_key,
                    &property.kind,
                    /* is_enumerable */ true,
                ))
            }
            Some(value) => {
                let property_key =
                    maybe!(eval_property_name(cx, &property.key, property.is_computed));

                let is_proto_setter =
                    property_key.get() == cx.names.__proto__ && !property.is_computed;
                if is_proto_setter {
                    let prop_value = maybe!(eval_expression(cx, value));
                    if prop_value.is_object() {
                        must!(object.set_prototype_of(cx, Some(prop_value.as_object())));
                    } else if prop_value.is_null() {
                        must!(object.set_prototype_of(cx, None));
                    }
                } else {
                    let prop_value = maybe!(eval_named_anonymous_function_or_expression(
                        cx,
                        value,
                        property_key
                    ));

                    must!(create_data_property_or_throw(cx, object, property_key, prop_value));
                }
            }
        }
    }

    object.into()
}

pub fn eval_property_name<'a>(
    mut cx: Context,
    key: &ast::Expression,
    is_computed: bool,
) -> EvalResult<Handle<PropertyKey>> {
    let property_key = if is_computed {
        let property_key_value = maybe!(eval_expression(cx, key));
        maybe!(to_property_key(cx, property_key_value))
    } else {
        match key {
            ast::Expression::Id(id) => id_property_key(cx, id),
            ast::Expression::String(lit) => {
                let string_value = InternedStrings::get_wtf8_str(cx, &lit.value);
                PropertyKey::string(cx, string_value).to_handle(cx)
            }
            ast::Expression::Number(lit) => {
                let key_value = Value::number(lit.value);
                if key_value.is_smi() {
                    let smi_value = key_value.as_smi();
                    if smi_value >= 0 {
                        return PropertyKey::array_index(cx, smi_value as u32)
                            .to_handle(cx)
                            .into();
                    }
                }

                let string_value = cx.alloc_string(&number_to_string(key_value.as_double()));
                PropertyKey::string(cx, string_value).to_handle(cx)
            }
            ast::Expression::BigInt(lit) => {
                let string_value = cx.alloc_string(&lit.value.to_string());
                PropertyKey::string(cx, string_value).to_handle(cx)
            }
            _ => unreachable!(),
        }
    };

    property_key.into()
}

// 13.2.7.3 RegExp Literal Evaluation
fn eval_regexp_literal(cx: Context, lit: &ast::RegExpLiteral) -> EvalResult<Handle<Value>> {
    maybe!(RegExpObject::new_from_literal(cx, lit)).into()
}

// 13.2.8.5 Template Literal Evaluation
fn eval_template_literal(cx: Context, lit: &ast::TemplateLiteral) -> EvalResult<Handle<Value>> {
    let mut string_parts = Vec::with_capacity(lit.quasis.len() * 2 - 1);

    let first_quasi_part =
        InternedStrings::get_wtf8_str(cx, &lit.quasis[0].cooked.as_ref().unwrap());
    string_parts.push(first_quasi_part);

    for i in 1..lit.quasis.len() {
        let expr_value = maybe!(eval_expression(cx, &lit.expressions[i - 1]));
        let expr_string = maybe!(to_string(cx, expr_value));

        string_parts.push(expr_string);

        let quasi_part = InternedStrings::get_wtf8_str(cx, &lit.quasis[i].cooked.as_ref().unwrap());
        string_parts.push(quasi_part);
    }

    let mut concat_string = cx.names.empty_string().as_string();
    for string_part in string_parts {
        concat_string = StringValue::concat(cx, concat_string, string_part);
    }

    concat_string.into()
}

// 13.3.2.1 Member Expression Evaluation
// Standard eval functions greedily call GetValue instead of returning a reference. This function
// inlines the GetValue call.
fn eval_member_expression(cx: Context, expr: &ast::MemberExpression) -> EvalResult<Handle<Value>> {
    let base_value = maybe!(eval_expression(cx, &expr.object));

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = maybe!(to_property_key(cx, property_name_value));

        let base = maybe!(to_object(cx, base_value));
        base.get(cx, property_key, base_value)
    } else if expr.is_private {
        let base = maybe!(to_object(cx, base_value));
        let private_env = cx
            .current_execution_context_ptr()
            .private_env_ptr()
            .unwrap();
        let private_name_string = id_string_value(cx, expr.property.to_id());
        let private_name = private_env.resolve_private_identifier(private_name_string);

        private_get(cx, base, private_name)
    } else {
        let property_key = id_property_key(cx, expr.property.to_id());
        let base = maybe!(to_object(cx, base_value));

        base.get(cx, property_key, base_value)
    }
}

// Same as eval_member_expression, but returns a reference instead of a value
pub fn eval_member_expression_to_reference(
    cx: Context,
    expr: &ast::MemberExpression,
) -> EvalResult<Reference> {
    let base_value = maybe!(eval_expression(cx, &expr.object));
    eval_member_expression_to_reference_with_base(cx, expr, base_value)
}

fn eval_member_expression_to_reference_with_base(
    cx: Context,
    expr: &ast::MemberExpression,
    base_value: Handle<Value>,
) -> EvalResult<Reference> {
    let is_strict = cx.current_execution_context_ptr().is_strict_mode();

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = maybe!(to_property_key(cx, property_name_value));

        Reference::new_property(base_value, property_key, is_strict).into()
    } else if expr.is_private {
        let property_name = id_string_value(cx, expr.property.to_id());
        Reference::make_private_reference(cx, base_value, property_name).into()
    } else {
        let property_key = id_property_key(cx, expr.property.to_id());
        Reference::new_property(base_value, property_key, is_strict).into()
    }
}

// 13.3.5.1 New Expression Evaluation
// 13.3.5.1.1 EvaluateNew
fn eval_new_expression(cx: Context, expr: &ast::NewExpression) -> EvalResult<Handle<Value>> {
    let constructor = maybe!(eval_expression(cx, &expr.callee));
    let arg_values = maybe!(eval_argument_list(cx, &expr.arguments));

    if !is_constructor(constructor) {
        return type_error_(cx, "value is not a constructor");
    }

    maybe!(construct(cx, constructor.as_object(), &arg_values, None)).into()
}

#[inline]
fn maybe_eval_expression_to_reference(
    cx: Context,
    expr: &ast::Expression,
) -> EvalResult<Option<Reference>> {
    match expr {
        ast::Expression::Id(id) => Some(maybe!(eval_identifier_to_reference(cx, &id))).into(),
        ast::Expression::Member(expr) => {
            Some(maybe!(eval_member_expression_to_reference(cx, &expr))).into()
        }
        ast::Expression::SuperMember(expr) => {
            Some(maybe!(eval_super_member_expression_to_reference(cx, &expr))).into()
        }
        ast::Expression::Chain(expr) => {
            Some(maybe!(eval_chain_expression_to_reference(cx, expr))).into()
        }
        _ => None.into(),
    }
}

// 13.3.6.1 Call Expression Evaluation
fn eval_call_expression(cx: Context, expr: &ast::CallExpression) -> EvalResult<Handle<Value>> {
    let callee_reference = maybe!(maybe_eval_expression_to_reference(cx, expr.callee.as_ref()));

    let (func_value, this_value) = match callee_reference {
        Some(reference) => {
            let func_value = maybe!(reference.get_value(cx));

            // Check for direct call to eval
            let eval_func_ptr = cx.get_intrinsic_ptr(Intrinsic::Eval);
            if func_value.is_object()
                && same_object_value(func_value.as_object().get_(), eval_func_ptr)
            {
                let is_non_property_eval_reference = match reference.base() {
                    ReferenceBase::Property { .. } => false,
                    ReferenceBase::Unresolvable { name } | ReferenceBase::Env { name, .. } => {
                        name.eq(&cx.names.eval().as_string())
                    }
                };

                if is_non_property_eval_reference {
                    let arg_values = maybe!(eval_argument_list(cx, &expr.arguments));
                    if arg_values.is_empty() {
                        return cx.undefined().into();
                    }

                    let eval_arg = arg_values[0];
                    let is_strict_caller = cx.current_execution_context_ptr().is_strict_mode();

                    return perform_eval(cx, eval_arg, is_strict_caller, true);
                }
            }

            let this_value = match reference.base() {
                ReferenceBase::Property { .. } => reference.get_this_value(),
                ReferenceBase::Env { env, .. } => match env.with_base_object() {
                    Some(base_object) => base_object.into(),
                    None => cx.undefined(),
                },
                _ => unreachable!(),
            };

            (func_value, this_value)
        }
        None => {
            let func_value = maybe!(eval_expression(cx, &expr.callee));
            (func_value, cx.undefined())
        }
    };

    eval_call(cx, func_value, this_value, &expr.arguments)
}

// 13.3.6.2 EvaluateCall
// Modified to take the function value and this value instead of a reference.
fn eval_call(
    cx: Context,
    func_value: Handle<Value>,
    this_value: Handle<Value>,
    arguments: &[ast::CallArgument],
) -> EvalResult<Handle<Value>> {
    let arg_values = maybe!(eval_argument_list(cx, arguments));
    if !is_callable(func_value) {
        return type_error_(cx, "value is not a function");
    }

    call(cx, func_value, this_value, &arg_values)
}

// 13.3.7.1 SuperProperty Evaluation
fn eval_super_member_expression(
    cx: Context,
    expr: &ast::SuperMemberExpression,
) -> EvalResult<Handle<Value>> {
    let reference = maybe!(eval_super_member_expression_to_reference(cx, expr));
    reference.get_value(cx)
}

// Same as eval_super_member_expression, but returns a reference instead of a value
pub fn eval_super_member_expression_to_reference(
    cx: Context,
    expr: &ast::SuperMemberExpression,
) -> EvalResult<Reference> {
    let mut env = get_this_environment(cx);
    let env = env.as_function_environment().unwrap();

    let actual_this = maybe!(env.get_this_binding(cx));

    let property_key = if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = maybe!(to_property_key(cx, property_name_value));
        property_key
    } else {
        id_property_key(cx, expr.property.as_ref().to_id())
    };

    // 13.3.7.3 MakeSuperPropertyReference inlined
    let is_strict = cx.current_execution_context_ptr().is_strict_mode();
    let base_value = maybe!(env.get_super_base(cx));

    Reference::new_property_with_this(base_value, property_key, is_strict, actual_this).into()
}

// 13.3.7.1 SuperCall Evaluation
fn eval_super_call_expression(
    cx: Context,
    expr: &ast::SuperCallExpression,
) -> EvalResult<Handle<Value>> {
    let new_target = get_new_target(cx);

    // 13.3.7.2 GetSuperConstructor inlined
    let mut this_env = get_this_environment(cx);
    let mut this_env = if let Some(func_env) = this_env.as_function_environment() {
        func_env
    } else {
        unreachable!()
    };

    let func = must!(this_env
        .function_object()
        .cast::<ObjectValue>()
        .get_prototype_of(cx));
    let arg_list = maybe!(eval_argument_list(cx, &expr.arguments));

    let func = match func {
        Some(func) if func.is_constructor() => func,
        _ => return type_error_(cx, "super must be a constructor"),
    };

    let result = maybe!(construct(cx, func, &arg_list, new_target));
    maybe!(this_env.bind_this_value(cx, result.into()));
    maybe!(initialize_instance_elements(cx, result, this_env.function_object()));

    result.into()
}

// 13.3.8.1 ArgumentListEvaluation
fn eval_argument_list(
    cx: Context,
    arguments: &[ast::CallArgument],
) -> EvalResult<Vec<Handle<Value>>> {
    let mut arg_values = vec![];

    for arg in arguments {
        match arg {
            ast::CallArgument::Expression(expr) => {
                let arg_value = maybe!(eval_expression(cx, expr));
                arg_values.push(arg_value)
            }
            ast::CallArgument::Spread(spread) => {
                let iterable = maybe!(eval_expression(cx, &spread.argument));
                let completion = iter_iterator_values(cx, iterable, &mut |_, value| {
                    arg_values.push(value);

                    None
                });
                maybe!(completion.into_eval_result());
            }
        }
    }

    arg_values.into()
}

// 13.3.9.1 Optional Chain Evaluation
fn eval_chain_expression(cx: Context, expr: &ast::ChainExpression) -> EvalResult<Handle<Value>> {
    maybe!(eval_chain_expression_part(cx, &expr.expression))
        .0
        .into()
}

fn eval_chain_expression_to_reference(
    cx: Context,
    expr: &ast::ChainExpression,
) -> EvalResult<Reference> {
    maybe!(eval_chain_expression_part(cx, &expr.expression))
        .1
        .into()
}

fn eval_chain_expression_part(
    cx: Context,
    expr: &ast::Expression,
) -> EvalResult<(Handle<Value>, Reference)> {
    match expr {
        ast::Expression::Member(member_expr) => {
            let base_value = maybe!(eval_chain_expression_part(cx, &member_expr.object)).0;
            if base_value.is_nullish() {
                return (cx.undefined(), Reference::EMPTY).into();
            }

            let reference =
                maybe!(eval_member_expression_to_reference_with_base(cx, member_expr, base_value));
            let value = maybe!(reference.get_value(cx));

            (value, reference).into()
        }
        // Logic partially duplicated from eval_call_expression, returns early for nullish functions
        ast::Expression::Call(call_expr) => {
            let callee_reference: Option<Reference> = match call_expr.callee.as_ref() {
                // Check for a member callee so that we can short circuit
                ast::Expression::Member(expr) => {
                    let base_value = maybe!(eval_chain_expression_part(cx, &expr.object)).0;
                    if base_value.is_nullish() {
                        return (cx.undefined(), Reference::EMPTY).into();
                    }

                    Some(maybe!(eval_member_expression_to_reference_with_base(
                        cx, &expr, base_value
                    )))
                    .into()
                }
                other_expr => {
                    maybe!(maybe_eval_expression_to_reference(cx, &other_expr))
                }
            };

            // Evaluate callee and find this value, returns early if callee is nullish
            let (func_value, this_value) = match callee_reference {
                Some(reference) => {
                    let func_value = maybe!(reference.get_value(cx));
                    if func_value.is_nullish() {
                        return (cx.undefined(), Reference::EMPTY).into();
                    }

                    let this_value = match reference.base() {
                        ReferenceBase::Property { .. } => reference.get_this_value(),
                        ReferenceBase::Env { env, .. } => match env.with_base_object() {
                            Some(base_object) => base_object.into(),
                            None => cx.undefined(),
                        },
                        _ => unreachable!(),
                    };

                    (func_value, this_value)
                }
                None => {
                    let func_value = maybe!(eval_chain_expression_part(cx, &call_expr.callee)).0;
                    if func_value.is_nullish() {
                        return (cx.undefined(), Reference::EMPTY).into();
                    }

                    (func_value, cx.undefined())
                }
            };

            let value = maybe!(eval_call(cx, func_value, this_value, &call_expr.arguments));

            (value, Reference::EMPTY).into()
        }
        other_expr => (maybe!(eval_expression(cx, other_expr)), Reference::EMPTY).into(),
    }
}

// 13.3.11.1 Tagged Template Evaluation
fn eval_tagged_template_expression(
    cx: Context,
    expr: &ast::TaggedTemplateExpression,
) -> EvalResult<Handle<Value>> {
    let (func_value, this_value) = match maybe!(maybe_eval_expression_to_reference(cx, &expr.tag)) {
        Some(reference) => {
            let func_value = maybe!(reference.get_value(cx));

            let this_value = match reference.base() {
                ReferenceBase::Property { .. } => reference.get_this_value(),
                ReferenceBase::Env { env, .. } => match env.with_base_object() {
                    Some(base_object) => base_object.into(),
                    None => cx.undefined(),
                },
                _ => unreachable!(),
            };

            (func_value, this_value)
        }
        None => {
            let func_value = maybe!(eval_expression(cx, &expr.tag));
            (func_value, cx.undefined())
        }
    };

    let template_object = get_template_object(cx, &expr.quasi).into();

    let mut arg_values = vec![template_object];
    for expr in &expr.quasi.expressions {
        arg_values.push(maybe!(eval_expression(cx, expr)));
    }

    if !is_callable(func_value) {
        return type_error_(cx, "value is not a function");
    }

    call(cx, func_value, this_value, &arg_values)
}

// 13.2.8.3 GetTemplateObject
fn get_template_object(cx: Context, lit: &ast::TemplateLiteral) -> Handle<ObjectValue> {
    // Template object is cached in realm's template registery
    let mut realm = cx.current_realm();
    if let Some(template_object) = realm.get_template_object(AstPtr::from_ref(lit)) {
        return template_object;
    }

    let num_strings = lit.quasis.len();
    let template_object: Handle<ObjectValue> =
        must!(array_create(cx, num_strings as u64, None)).into();
    let raw_object: Handle<ObjectValue> = must!(array_create(cx, num_strings as u64, None)).into();

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    for (i, quasi) in lit.quasis.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32));

        let cooked_value = match &quasi.cooked {
            None => cx.undefined(),
            Some(cooked) => InternedStrings::get_wtf8_str(cx, cooked).into(),
        };
        let cooked_desc = PropertyDescriptor::data(cooked_value, false, true, false);
        must!(define_property_or_throw(cx, template_object, index_key, cooked_desc));

        let raw_value = InternedStrings::get_wtf8_str(cx, &quasi.raw);
        let raw_desc = PropertyDescriptor::data(raw_value.into(), false, true, false);
        must!(define_property_or_throw(cx, raw_object, index_key, raw_desc));
    }

    must!(set_integrity_level(cx, raw_object.into(), IntegrityLevel::Frozen));

    let raw_object_desc = PropertyDescriptor::data(raw_object.into(), false, false, false);
    must!(define_property_or_throw(cx, template_object, cx.names.raw(), raw_object_desc));

    must!(set_integrity_level(cx, template_object, IntegrityLevel::Frozen));

    realm.add_template_object(cx, AstPtr::from_ref(lit), template_object);

    template_object
}

// 13.3.12.1 NewTarget Evaluation
fn eval_new_target(cx: Context) -> EvalResult<Handle<Value>> {
    match get_new_target(cx) {
        None => cx.undefined().into(),
        Some(new_target) => new_target.into(),
    }
}

// 13.4.2.1 Postfix Increment Evaluation
// 13.4.3.1 Postfix Decrement Evaluation
// 13.4.4.1 Prefix Increment Evaluation
// 13.4.5.1 Prefix Decrement Evaluation
fn eval_update_expression(cx: Context, expr: &ast::UpdateExpression) -> EvalResult<Handle<Value>> {
    let mut argument_reference =
        match maybe!(maybe_eval_expression_to_reference(cx, expr.argument.as_ref())) {
            Some(reference) => reference,
            _ => return reference_error_(cx, "expected a reference"),
        };

    let old_value = maybe!(argument_reference.get_value(cx));
    let old_value = maybe!(to_numeric(cx, old_value));

    let new_value = match expr.operator {
        UpdateOperator::Increment => {
            if old_value.is_bigint() {
                let inc_value = old_value.as_bigint().bigint() + 1;
                BigIntValue::new(cx, inc_value).into()
            } else {
                Value::number(old_value.as_number() + 1.0).to_handle(cx)
            }
        }
        UpdateOperator::Decrement => {
            if old_value.is_bigint() {
                let dec_value = old_value.as_bigint().bigint() - 1;
                BigIntValue::new(cx, dec_value).into()
            } else {
                Value::number(old_value.as_number() - 1.0).to_handle(cx)
            }
        }
    };

    maybe!(argument_reference.put_value(cx, new_value));

    if expr.is_prefix {
        new_value.into()
    } else {
        old_value.into()
    }
}

// 13.5.1.2 Delete Expression Evaluation
fn eval_delete_expression(cx: Context, expr: &ast::UnaryExpression) -> EvalResult<Handle<Value>> {
    let reference = match maybe!(maybe_eval_expression_to_reference(cx, expr.argument.as_ref())) {
        Some(reference) => reference,
        None => {
            maybe!(eval_expression(cx, expr.argument.as_ref()));
            return cx.bool(true).into();
        }
    };

    match reference.base() {
        ReferenceBase::Unresolvable { .. } => cx.bool(true).into(),
        ReferenceBase::Property { object, property, .. } => {
            if reference.is_super_reference() {
                return reference_error_(cx, "cannot delete super");
            }

            let mut base_object = maybe!(to_object(cx, *object));
            let delete_status = maybe!(base_object.delete(cx, *property));
            if !delete_status && reference.is_strict() {
                return type_error_(cx, "cannot delete property");
            }

            cx.bool(delete_status).into()
        }
        ReferenceBase::Env { mut env, name } => {
            let delete_status = maybe!((*env).delete_binding(cx, *name));
            cx.bool(delete_status).into()
        }
    }
}

// 13.5.2.1 Void Expression Evaluation
fn eval_void_expression(cx: Context, expr: &ast::UnaryExpression) -> EvalResult<Handle<Value>> {
    maybe!(eval_expression(cx, &expr.argument));
    cx.undefined().into()
}

// 13.5.3.1 TypeOf Expression Evaluation
fn eval_typeof_expression(
    mut cx: Context,
    expr: &ast::UnaryExpression,
) -> EvalResult<Handle<Value>> {
    let value = match maybe!(maybe_eval_expression_to_reference(cx, expr.argument.as_ref())) {
        Some(reference) => {
            if reference.is_unresolvable_reference() {
                return cx.alloc_string("undefined").into();
            }

            maybe!(reference.get_value(cx))
        }
        None => maybe!(eval_expression(cx, expr.argument.as_ref())),
    };

    eval_typeof(cx, value).into()
}

pub fn eval_typeof(mut cx: Context, value: Handle<Value>) -> Handle<StringValue> {
    let type_string = if value.is_pointer() {
        let kind = value.as_pointer().descriptor().kind();
        match kind {
            ObjectKind::String => "string",
            ObjectKind::Symbol => "symbol",
            ObjectKind::BigInt => "bigint",
            // All other pointer values must be an object
            _ => {
                if value.as_object().is_callable() {
                    "function"
                } else {
                    "object"
                }
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => "object",
            UNDEFINED_TAG => "undefined",
            BOOL_TAG => "boolean",
            // Otherwise must be a number - either a double or smi
            _ => "number",
        }
    };

    cx.alloc_string(type_string)
}

// 13.5.4.1 Unary Plus Evaluation
fn eval_unary_plus(cx: Context, expr: &ast::UnaryExpression) -> EvalResult<Handle<Value>> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    to_number(cx, value)
}

// 13.5.5.1 Unary Minus Evaluation
fn eval_unary_minus(cx: Context, expr: &ast::UnaryExpression) -> EvalResult<Handle<Value>> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    eval_negate(cx, value)
}

pub fn eval_negate(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let neg_bignum = -value.as_bigint().bigint();
        BigIntValue::new(cx, neg_bignum).into()
    } else {
        Value::number(-value.as_number()).to_handle(cx).into()
    }
}

// 13.5.6.1 Bitwise Not Evaluation
fn eval_bitwise_not(cx: Context, expr: &ast::UnaryExpression) -> EvalResult<Handle<Value>> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let not_bignum = !value.as_bigint().bigint();
        BigIntValue::new(cx, not_bignum).into()
    } else {
        let value = must!(to_int32(cx, value));
        Value::smi(!value).to_handle(cx).into()
    }
}

// 13.5.7.1 Logical Not Evaluation
fn eval_logical_not_expression(
    cx: Context,
    expr: &ast::UnaryExpression,
) -> EvalResult<Handle<Value>> {
    let expr_value = maybe!(eval_expression(cx, &expr.argument));
    cx.bool(!to_boolean(expr_value.get())).into()
}

fn eval_binary_expression(cx: Context, expr: &ast::BinaryExpression) -> EvalResult<Handle<Value>> {
    match expr.operator {
        ast::BinaryOperator::Add => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_add(cx, left_value, right_value)
        }
        ast::BinaryOperator::Subtract => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_subtract(cx, left_value, right_value)
        }
        ast::BinaryOperator::Multiply => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_multiply(cx, left_value, right_value)
        }
        ast::BinaryOperator::Divide => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_divide(cx, left_value, right_value)
        }
        ast::BinaryOperator::Remainder => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_remainder(cx, left_value, right_value)
        }
        ast::BinaryOperator::Exponent => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_exponentiation(cx, left_value, right_value)
        }
        ast::BinaryOperator::EqEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            let is_equal = maybe!(is_loosely_equal(cx, left_value, right_value));
            cx.bool(is_equal).into()
        }
        ast::BinaryOperator::NotEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            let is_equal = maybe!(is_loosely_equal(cx, left_value, right_value));
            cx.bool(!is_equal).into()
        }
        ast::BinaryOperator::EqEqEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            let is_equal = is_strictly_equal(left_value, right_value);
            cx.bool(is_equal).into()
        }
        ast::BinaryOperator::NotEqEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            let is_equal = is_strictly_equal(left_value, right_value);
            cx.bool(!is_equal).into()
        }
        ast::BinaryOperator::LessThan => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_less_than(cx, left_value, right_value)
        }
        ast::BinaryOperator::LessThanOrEqual => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_less_than_or_equal(cx, left_value, right_value)
        }
        ast::BinaryOperator::GreaterThan => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_greater_than(cx, left_value, right_value)
        }
        ast::BinaryOperator::GreaterThanOrEqual => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_greater_than_or_equal(cx, left_value, right_value)
        }
        ast::BinaryOperator::And => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_bitwise_and(cx, left_value, right_value)
        }
        ast::BinaryOperator::Or => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_bitwise_or(cx, left_value, right_value)
        }
        ast::BinaryOperator::Xor => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_bitwise_xor(cx, left_value, right_value)
        }
        ast::BinaryOperator::ShiftLeft => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_shift_left(cx, left_value, right_value)
        }
        ast::BinaryOperator::ShiftRightArithmetic => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_shift_right_arithmetic(cx, left_value, right_value)
        }
        ast::BinaryOperator::ShiftRightLogical => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_shift_right_logical(cx, left_value, right_value)
        }
        ast::BinaryOperator::InstanceOf => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            let is_instance_of = maybe!(eval_instanceof_expression(cx, left_value, right_value));
            cx.bool(is_instance_of).into()
        }
        ast::BinaryOperator::In => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_in_expression(cx, left_value, right_value)
        }
        ast::BinaryOperator::InPrivate => {
            let right_value = maybe!(eval_expression(cx, &expr.right));
            eval_private_in_expression(cx, expr.left.to_id(), right_value)
        }
    }
}

pub fn eval_add(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_prim = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::None));
    let right_prim = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::None));
    if left_prim.is_string() || right_prim.is_string() {
        let left_string = maybe!(to_string(cx, left_prim));
        let right_string = maybe!(to_string(cx, right_prim));

        return StringValue::concat(cx, left_string, right_string).into();
    }

    let left_num = maybe!(to_numeric(cx, left_prim));
    let right_num = maybe!(to_numeric(cx, right_prim));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() + right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() + right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_subtract(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() - right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() - right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_multiply(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() * right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() * right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_divide(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let bigint_right = right_num.as_bigint().bigint();
        if bigint_right.eq(&BigInt::default()) {
            return range_error_(cx, "BigInt division by zero");
        }

        let result = left_num.as_bigint().bigint() / bigint_right;
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() / right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_remainder(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let bigint_right = right_num.as_bigint().bigint();
        if bigint_right.eq(&BigInt::default()) {
            return range_error_(cx, "BigInt division by zero");
        }

        let bigint_left = left_num.as_bigint().bigint();
        if bigint_left.eq(&BigInt::default()) {
            return BigIntValue::new(cx, BigInt::default()).into();
        }

        let result = bigint_left % bigint_right;
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() % right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_exponentiation(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let base_bignum = left_num.as_bigint().bigint();
        let exponent_bignum = right_num.as_bigint().bigint();

        if exponent_bignum.lt(&BigInt::default()) {
            return range_error_(cx, "BigInt negative exponent");
        } else if exponent_bignum.eq(&BigInt::default()) && base_bignum.eq(&BigInt::default()) {
            return BigIntValue::new(cx, 1.into()).into();
        }

        if let Ok(exponent_u32) = exponent_bignum.try_into() {
            let result = base_bignum.pow(exponent_u32);
            return BigIntValue::new(cx, result).into();
        } else {
            // This guarantees a bigint that is too large
            return range_error_(cx, "BigInt is too large");
        }
    } else {
        return Value::number(number_exponentiate(left_num.as_number(), right_num.as_number()))
            .to_handle(cx)
            .into();
    }
}

pub fn eval_less_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    if result.is_undefined() {
        cx.bool(false).into()
    } else {
        cx.bool(result.as_bool()).into()
    }
}

pub fn eval_greater_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    if result.is_undefined() {
        cx.bool(false).into()
    } else {
        cx.bool(result.as_bool()).into()
    }
}

pub fn eval_less_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    cx.bool(result.is_false()).into()
}

pub fn eval_greater_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    cx.bool(result.is_false()).into()
}

fn eval_bitwise_and(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() & right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi & right_smi).to_handle(cx).into();
    }
}

fn eval_bitwise_or(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() | right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi | right_smi).to_handle(cx).into();
    }
}

fn eval_bitwise_xor(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() ^ right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi ^ right_smi).to_handle(cx).into();
    }
}

fn eval_shift_left(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &right_num.as_bigint().bigint()
        ));

        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi << shift).to_handle(cx).into();
    }
}

fn eval_shift_right_arithmetic(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &-right_num.as_bigint().bigint()
        ));

        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi >> shift).to_handle(cx).into();
    }
}

// 6.1.6.2.9 BigInt::leftShift
fn eval_bigint_left_shift(cx: Context, left: &BigInt, right: &BigInt) -> EvalResult<BigInt> {
    let bigint_2: BigInt = 2.into();

    if right.lt(&BigInt::default()) {
        let exponent: u32 = match (-right).try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is zero, since no bigints that can be represented
            // that would be large enough for the result to be non-zero.
            Err(_) => return BigInt::default().into(),
        };

        let pow_of_2 = bigint_2.pow(exponent);

        // Division result must be rounded down, even for negative numbers. Detect this case and
        // force rounding down for negative numbers.
        if left.sign() == Sign::Minus {
            let result: BigInt = (left - &pow_of_2 + 1) / &pow_of_2;
            result.into()
        } else {
            (left / pow_of_2).into()
        }
    } else {
        let exponent: u32 = match right.try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is too large
            Err(_) => {
                return range_error_(cx, "BigInt is too large");
            }
        };

        (left * bigint_2.pow(exponent)).into()
    }
}

fn eval_shift_right_logical(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint || right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    let left_smi = must!(to_uint32(cx, left_value));
    let right_u32 = must!(to_uint32(cx, right_value));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;

    return Value::from(left_smi >> shift).to_handle(cx).into();
}

// 13.10.2 InstanceofOperator
pub fn eval_instanceof_expression(
    cx: Context,
    value: Handle<Value>,
    target: Handle<Value>,
) -> EvalResult<bool> {
    if !target.is_object() {
        return type_error_(cx, "invalid instanceof operand");
    }

    let has_instance_key = cx.well_known_symbols.has_instance();
    let instance_of_handler = maybe!(get_method(cx, target, has_instance_key));
    if let Some(instance_of_handler) = instance_of_handler {
        let result = maybe!(call_object(cx, instance_of_handler, target, &[value]));
        return to_boolean(result.get()).into();
    }

    let target_object = target.as_object();
    if !target_object.is_callable() {
        return type_error_(cx, "invalid 'instanceof' operand");
    }

    let has_instance = maybe!(ordinary_has_instance(cx, target, value));
    has_instance.into()
}

fn eval_in_expression(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    if !right_value.is_object() {
        return type_error_(cx, "right side of 'in' must be an object");
    }

    let property_key = maybe!(to_property_key(cx, left_value));

    let has_property = maybe!(has_property(cx, right_value.as_object(), property_key));
    cx.bool(has_property).into()
}

fn eval_private_in_expression(
    cx: Context,
    private_property: &ast::Identifier,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    if !right_value.is_object() {
        return type_error_(cx, "right side of 'in' must be an object");
    }

    let private_name_string = id_string_value(cx, private_property);
    let private_name = cx
        .current_execution_context_ptr()
        .private_env_ptr()
        .unwrap()
        .resolve_private_identifier(private_name_string);

    let has_private_property = right_value.as_object().has_private_element(private_name);
    cx.bool(has_private_property).into()
}

// 13.13.1 Logical Expression Evaluation
fn eval_logical_expression(
    cx: Context,
    expr: &ast::LogicalExpression,
) -> EvalResult<Handle<Value>> {
    match expr.operator {
        ast::LogicalOperator::And => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            if !to_boolean(left_value.get()) {
                left_value.into()
            } else {
                eval_expression(cx, &expr.right)
            }
        }
        ast::LogicalOperator::Or => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            if to_boolean(left_value.get()) {
                left_value.into()
            } else {
                eval_expression(cx, &expr.right)
            }
        }
        ast::LogicalOperator::NullishCoalesce => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            if left_value.is_nullish() {
                eval_expression(cx, &expr.right)
            } else {
                left_value.into()
            }
        }
    }
}

// 13.14.1 Conditional Expression Evaluation
fn eval_conditional_expression(
    cx: Context,
    expr: &ast::ConditionalExpression,
) -> EvalResult<Handle<Value>> {
    let test_value = maybe!(eval_expression(cx, &expr.test));
    if to_boolean(test_value.get()) {
        eval_expression(cx, &expr.conseq)
    } else {
        eval_expression(cx, &expr.altern)
    }
}

// 13.15.2 Assignment Expression Evaluation
fn eval_assignment_expression(
    cx: Context,
    expr: &ast::AssignmentExpression,
) -> EvalResult<Handle<Value>> {
    let mut reference = match expr.left.as_ref() {
        ast::Pattern::Id(id) => maybe!(eval_identifier_to_reference(cx, &id)),
        ast::Pattern::Reference(ast::Expression::Member(expr)) => {
            maybe!(eval_member_expression_to_reference(cx, &expr))
        }
        ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
            maybe!(eval_super_member_expression_to_reference(cx, &expr))
        }
        ast::Pattern::Object(_) | ast::Pattern::Array(_) => {
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(destructuring_assignment_evaluation(cx, &expr.left, right_value));

            return right_value.into();
        }
        ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
            unreachable!("invalid assigment left hand side")
        }
    };

    let is_non_parenthesized_id_predicate = || match expr.left.as_ref() {
        ast::Pattern::Id(id) if id.loc.start == expr.loc.start => true,
        _ => false,
    };

    let result_value = match expr.operator {
        ast::AssignmentOperator::Equals => {
            let name_key = reference.name_as_property_key(cx);
            maybe!(eval_named_anonymous_function_or_expression_if(
                cx,
                &expr.right,
                name_key,
                is_non_parenthesized_id_predicate
            ))
        }
        ast::AssignmentOperator::Add => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_add(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Subtract => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_subtract(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Multiply => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_multiply(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Divide => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_divide(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Remainder => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_remainder(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Exponent => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_exponentiation(cx, left_value, right_value))
        }
        ast::AssignmentOperator::And => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_bitwise_and(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Or => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_bitwise_or(cx, left_value, right_value))
        }
        ast::AssignmentOperator::Xor => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_bitwise_xor(cx, left_value, right_value))
        }
        ast::AssignmentOperator::ShiftLeft => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_shift_left(cx, left_value, right_value))
        }
        ast::AssignmentOperator::ShiftRightArithmetic => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_shift_right_arithmetic(cx, left_value, right_value))
        }
        ast::AssignmentOperator::ShiftRightLogical => {
            let left_value = maybe!(reference.get_value(cx));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            maybe!(eval_shift_right_logical(cx, left_value, right_value))
        }
        ast::AssignmentOperator::LogicalAnd => {
            let left_value = maybe!(reference.get_value(cx));
            if !to_boolean(left_value.get()) {
                return left_value.into();
            }

            let name_key = reference.name_as_property_key(cx);
            maybe!(eval_named_anonymous_function_or_expression_if(
                cx,
                &expr.right,
                name_key,
                is_non_parenthesized_id_predicate,
            ))
        }
        ast::AssignmentOperator::LogicalOr => {
            let left_value = maybe!(reference.get_value(cx));
            if to_boolean(left_value.get()) {
                return left_value.into();
            }

            let name_key = reference.name_as_property_key(cx);
            maybe!(eval_named_anonymous_function_or_expression_if(
                cx,
                &expr.right,
                name_key,
                is_non_parenthesized_id_predicate
            ))
        }
        ast::AssignmentOperator::NullishCoalesce => {
            let left_value = maybe!(reference.get_value(cx));
            if !left_value.is_nullish() {
                return left_value.into();
            }

            let name_key = reference.name_as_property_key(cx);
            maybe!(eval_named_anonymous_function_or_expression_if(
                cx,
                &expr.right,
                name_key,
                is_non_parenthesized_id_predicate
            ))
        }
    };

    maybe!(reference.put_value(cx, result_value));

    result_value.into()
}

// 13.16.1 Sequence Expression Evaluation
fn eval_sequence_expression(
    cx: Context,
    expr: &ast::SequenceExpression,
) -> EvalResult<Handle<Value>> {
    let mut value = cx.empty();

    for expr in &expr.expressions {
        value = maybe!(eval_expression(cx, expr))
    }

    value.into()
}

// 15.2.6 Function Expression Evaluation
fn eval_function_expression(cx: Context, func: &ast::Function) -> EvalResult<Handle<Value>> {
    instantiate_ordinary_function_expression(cx, func, None).into()
}

// 15.3.5 Arrow Function Evaluation
fn eval_arrow_function(cx: Context, func: &ast::Function) -> EvalResult<Handle<Value>> {
    instantiate_arrow_function_expression(cx, func, None).into()
}
