use std::{collections::HashSet, convert::TryInto};

use num_bigint::{BigInt, Sign};

use crate::{
    js::{
        parser::ast::{self, UpdateOperator},
        runtime::{
            abstract_operations::{
                call, call_object, construct, copy_data_properties, create_data_property_or_throw,
                get_method, has_property, initialize_instance_elements, ordinary_has_instance,
                private_get,
            },
            array_object::array_create,
            completion::EvalResult,
            environment::environment::Environment,
            error::{range_error_, reference_error_, type_error_},
            execution_context::{
                get_new_target, get_this_environment, resolve_binding, resolve_this_binding,
            },
            intrinsics::intrinsics::Intrinsic,
            iterator::iter_iterator_values,
            numeric_operations::number_exponentiate,
            object_value::{Object, ObjectValue},
            ordinary_object::ordinary_object_create,
            property::Property,
            property_key::PropertyKey,
            reference::{Reference, ReferenceBase},
            type_utilities::{
                is_callable, is_constructor, is_less_than, is_loosely_equal, is_strictly_equal,
                same_object_value, to_boolean, to_int32, to_number, to_numeric, to_object,
                to_primitive, to_property_key, to_string, to_uint32, ToPrimitivePreferredType,
            },
            value::{
                Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, OBJECT_TAG, STRING_TAG, SYMBOL_TAG,
                UNDEFINED_TAG,
            },
            Context, Gc,
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

pub fn eval_expression(cx: &mut Context, expr: &ast::Expression) -> EvalResult<Value> {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
        ast::Expression::Null(_) => eval_null_literal(),
        ast::Expression::Boolean(lit) => eval_boolean_literal(lit),
        ast::Expression::Number(lit) => eval_number_literal(lit),
        ast::Expression::String(lit) => eval_string_literal(cx, lit),
        ast::Expression::BigInt(lit) => eval_bigint_literal(cx, lit),
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
    }
}

// 13.1.3 Identifier Evaluation
pub fn eval_identifier(cx: &mut Context, id: &ast::Identifier) -> EvalResult<Value> {
    let name_value = id_string_value(cx, id);
    let reference = maybe!(resolve_binding(cx, name_value, None));

    // Unlike the spec, greedily call GetValue here as all eval functions evaluate to a value.
    reference.get_value(cx)
}

// Same as eval_identifier, but returns a reference instead of a value
pub fn eval_identifier_to_reference(
    cx: &mut Context,
    id: &ast::Identifier,
) -> EvalResult<Reference> {
    let name_value = id_string_value(cx, id);
    resolve_binding(cx, name_value, None)
}

// 13.2.1.1 This Expression Evaluation
fn eval_this_expression(cx: &mut Context) -> EvalResult<Value> {
    resolve_this_binding(cx)
}

// 13.2.3.1 Literal Evaluation
fn eval_null_literal() -> EvalResult<Value> {
    Value::null().into()
}

fn eval_boolean_literal(lit: &ast::BooleanLiteral) -> EvalResult<Value> {
    Value::bool(lit.value).into()
}

fn eval_number_literal(lit: &ast::NumberLiteral) -> EvalResult<Value> {
    Value::number(lit.value).into()
}

fn eval_string_literal(cx: &mut Context, lit: &ast::StringLiteral) -> EvalResult<Value> {
    let interned_value = cx.get_interned_string(&lit.value);
    interned_value.into()
}

fn eval_bigint_literal(cx: &mut Context, lit: &ast::BigIntLiteral) -> EvalResult<Value> {
    Value::bigint(cx.heap.alloc_bigint(lit.value.clone())).into()
}

// 13.2.4.2 Array Initializer Evaluation
// 13.2.4.1 ArrayAccumulation
fn eval_array_expression(cx: &mut Context, expr: &ast::ArrayExpression) -> EvalResult<Value> {
    let mut array = must!(array_create(cx, 0, None));

    let mut index = 0;
    for element in expr.elements.iter() {
        match element {
            ast::ArrayElement::Hole => {
                let key = PropertyKey::array_index(index);
                let desc = Property::data(Value::empty(), true, true, true);

                array.object.set_property(&key, desc);
                index += 1;
            }
            ast::ArrayElement::Expression(expr) => {
                let key = PropertyKey::array_index(index);
                let element_value = maybe!(eval_expression(cx, expr));
                let desc = Property::data(element_value, true, true, true);

                array.object.set_property(&key, desc);
                index += 1;
            }
            ast::ArrayElement::Spread(spread) => {
                let iterable = maybe!(eval_expression(cx, &spread.argument));
                let completion = iter_iterator_values(cx, iterable, &mut |_, value| {
                    let key = PropertyKey::array_index(index);
                    let desc = Property::data(value, true, true, true);

                    array.object.set_property(&key, desc);
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
fn eval_object_expression(cx: &mut Context, expr: &ast::ObjectExpression) -> EvalResult<Value> {
    let proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
    let mut object: Gc<ObjectValue> = cx.heap.alloc(ordinary_object_create(proto)).into();

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
                must!(create_data_property_or_throw(cx, object, &prop_key, prop_value));
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
                    &property_key,
                    &property.kind,
                    /* is_enumerable */ true,
                ))
            }
            Some(value) => {
                let property_key =
                    maybe!(eval_property_name(cx, &property.key, property.is_computed));

                // TODO: Check if in JSON.parse
                let is_proto_setter = property_key == cx.names.__proto__ && !property.is_computed;
                if is_proto_setter {
                    let prop_value = maybe!(eval_expression(cx, value));
                    match prop_value.get_tag() {
                        OBJECT_TAG => {
                            must!(object.set_prototype_of(Some(prop_value.as_object())));
                        }
                        NULL_TAG => {
                            must!(object.set_prototype_of(None));
                        }
                        _ => {}
                    }
                } else {
                    let prop_value = maybe!(eval_named_anonymous_function_or_expression(
                        cx,
                        value,
                        &property_key
                    ));

                    must!(create_data_property_or_throw(cx, object, &property_key, prop_value));
                }
            }
        }
    }

    object.into()
}

pub fn eval_property_name<'a>(
    cx: &mut Context,
    key: &ast::Expression,
    is_computed: bool,
) -> EvalResult<PropertyKey> {
    let property_key = if is_computed {
        let property_key_value = maybe!(eval_expression(cx, key));
        maybe!(to_property_key(cx, property_key_value))
    } else {
        match key {
            ast::Expression::Id(id) => id_property_key(cx, id),
            ast::Expression::String(lit) => {
                let string_value = cx.get_interned_string(lit.value.as_str());
                PropertyKey::string(string_value)
            }
            ast::Expression::Number(lit) => {
                let key_value = Value::number(lit.value);
                if key_value.is_smi() {
                    let smi_value = key_value.as_smi();
                    if smi_value >= 0 {
                        return PropertyKey::array_index(smi_value as u32).into();
                    }
                }

                // TODO: Implement Number::toString from spec
                let string_value = cx.heap.alloc_string(key_value.as_double().to_string());
                PropertyKey::string(string_value)
            }
            _ => unreachable!(),
        }
    };

    property_key.into()
}

// 13.3.2.1 Member Expression Evaluation
// Standard eval functions greedily call GetValue instead of returning a reference. This function
// inlines the GetValue call.
fn eval_member_expression(cx: &mut Context, expr: &ast::MemberExpression) -> EvalResult<Value> {
    let base_value = maybe!(eval_expression(cx, &expr.object));

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = maybe!(to_property_key(cx, property_name_value));

        let base = maybe!(to_object(cx, base_value));
        base.get(cx, &property_key, base.into())
    } else if expr.is_private {
        let base = maybe!(to_object(cx, base_value));
        let private_env = cx.current_execution_context().private_env.unwrap();
        let private_name = expr.property.to_id().name.as_str();
        let private_id = private_env.resolve_private_identifier(private_name);

        private_get(cx, base, private_id)
    } else {
        let property_key = id_property_key(cx, expr.property.to_id());
        let base = maybe!(to_object(cx, base_value));

        base.get(cx, &property_key, base.into())
    }
}

// Same as eval_member_expression, but returns a reference instead of a value
fn eval_member_expression_to_reference(
    cx: &mut Context,
    expr: &ast::MemberExpression,
) -> EvalResult<Reference> {
    let base_value = maybe!(eval_expression(cx, &expr.object));

    let is_strict = cx.current_execution_context().is_strict_mode;

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
fn eval_new_expression(cx: &mut Context, expr: &ast::NewExpression) -> EvalResult<Value> {
    let constructor = maybe!(eval_expression(cx, &expr.callee));
    let arg_values = maybe!(eval_argument_list(cx, &expr.arguments));

    if !is_constructor(constructor) {
        return type_error_(cx, "value is not a constructor");
    }

    maybe!(construct(cx, constructor.as_object(), &arg_values, None)).into()
}

// 13.3.6.1 Call Expression Evaluation
fn eval_call_expression(cx: &mut Context, expr: &ast::CallExpression) -> EvalResult<Value> {
    let callee_reference = match expr.callee.as_ref() {
        ast::Expression::Id(id) => Some(maybe!(eval_identifier_to_reference(cx, &id))),
        ast::Expression::Member(expr) => {
            Some(maybe!(eval_member_expression_to_reference(cx, &expr)))
        }
        ast::Expression::SuperMember(expr) => {
            Some(maybe!(eval_super_member_expression_to_reference(cx, &expr)))
        }
        _ => None,
    };

    let (func_value, this_value) = match callee_reference {
        Some(reference) => {
            let func_value = maybe!(reference.get_value(cx));

            // Check for direct call to eval
            let eval_func = cx.current_realm().get_intrinsic(Intrinsic::Eval);
            if func_value.is_object() && same_object_value(func_value.as_object(), eval_func) {
                let is_non_property_eval_reference = match reference.base() {
                    ReferenceBase::Property { .. } => false,
                    ReferenceBase::Unresolvable { name } | ReferenceBase::Env { name, .. } => {
                        name.str() == "eval"
                    }
                };

                if is_non_property_eval_reference {
                    let arg_values = maybe!(eval_argument_list(cx, &expr.arguments));
                    if arg_values.is_empty() {
                        return Value::undefined().into();
                    }

                    let eval_arg = &arg_values[0];
                    let is_strict_caller = cx.current_execution_context().is_strict_mode;

                    return perform_eval(cx, eval_arg.clone(), is_strict_caller, true);
                }
            }

            let this_value = match reference.base() {
                ReferenceBase::Property { .. } => reference.get_this_value(),
                ReferenceBase::Env { env, .. } => match env.with_base_object() {
                    Some(base_object) => base_object.into(),
                    None => Value::undefined(),
                },
                _ => unreachable!(),
            };

            (func_value, this_value)
        }
        None => {
            let func_value = maybe!(eval_expression(cx, &expr.callee));
            (func_value, Value::undefined())
        }
    };

    eval_call(cx, func_value, this_value, &expr.arguments)
}

// 13.3.6.2 EvaluateCall
// Modified to take the function value and this value instead of a reference.
fn eval_call(
    cx: &mut Context,
    func_value: Value,
    this_value: Value,
    arguments: &[ast::CallArgument],
) -> EvalResult<Value> {
    let arg_values = maybe!(eval_argument_list(cx, arguments));
    if !is_callable(func_value) {
        return type_error_(cx, "value is not a function");
    }

    call(cx, func_value, this_value, &arg_values)
}

// 13.3.7.1 SuperProperty Evaluation
fn eval_super_member_expression(
    cx: &mut Context,
    expr: &ast::SuperMemberExpression,
) -> EvalResult<Value> {
    let reference = maybe!(eval_super_member_expression_to_reference(cx, expr));
    reference.get_value(cx)
}

// Same as eval_super_member_expression, but returns a reference instead of a value
fn eval_super_member_expression_to_reference(
    cx: &mut Context,
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
    let is_strict = cx.current_execution_context().is_strict_mode;
    let base_value = maybe!(env.get_super_base());

    Reference::new_property_with_this(base_value, property_key, is_strict, actual_this).into()
}

// 13.3.7.1 SuperCall Evaluation
fn eval_super_call_expression(
    cx: &mut Context,
    expr: &ast::SuperCallExpression,
) -> EvalResult<Value> {
    let new_target = get_new_target(cx);

    // 13.3.7.2 GetSuperConstructor inlined
    let mut this_env = get_this_environment(cx);
    let this_env = if let Some(func_env) = this_env.as_function_environment() {
        func_env
    } else {
        unreachable!()
    };

    let func = must!(this_env.function_object.get_prototype_of());
    let arg_list = maybe!(eval_argument_list(cx, &expr.arguments));

    let func = match func {
        Some(func) if func.is_constructor() => func,
        _ => return type_error_(cx, "super must be a constructor"),
    };

    let result = maybe!(construct(cx, func, &arg_list, new_target));
    maybe!(this_env.bind_this_value(cx, result.into()));
    maybe!(initialize_instance_elements(cx, result, this_env.function_object));

    result.into()
}

// 13.3.8.1 ArgumentListEvaluation
fn eval_argument_list(cx: &mut Context, arguments: &[ast::CallArgument]) -> EvalResult<Vec<Value>> {
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

// 13.4.2.1 Postfix Increment Evaluation
// 13.4.3.1 Postfix Decrement Evaluation
// 13.4.4.1 Prefix Increment Evaluation
// 13.4.5.1 Prefix Decrement Evaluation
fn eval_update_expression(cx: &mut Context, expr: &ast::UpdateExpression) -> EvalResult<Value> {
    let mut argument_reference = match expr.argument.as_ref() {
        ast::Expression::Id(id) => maybe!(eval_identifier_to_reference(cx, &id)),
        ast::Expression::Member(expr) => maybe!(eval_member_expression_to_reference(cx, &expr)),
        ast::Expression::SuperMember(expr) => {
            maybe!(eval_super_member_expression_to_reference(cx, &expr))
        }
        _ => return reference_error_(cx, "expected a reference"),
    };
    let old_value = maybe!(argument_reference.get_value(cx));
    let old_value = maybe!(to_numeric(cx, old_value));

    let new_value = match expr.operator {
        UpdateOperator::Increment => {
            if old_value.is_bigint() {
                let inc_value = old_value.as_bigint().bigint() + 1;
                Value::bigint(cx.heap.alloc_bigint(inc_value))
            } else {
                Value::number(old_value.as_number() + 1.0)
            }
        }
        UpdateOperator::Decrement => {
            if old_value.is_bigint() {
                let dec_value = old_value.as_bigint().bigint() - 1;
                Value::bigint(cx.heap.alloc_bigint(dec_value))
            } else {
                Value::number(old_value.as_number() - 1.0)
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
fn eval_delete_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let reference = match expr.argument.as_ref() {
        ast::Expression::Id(id) => maybe!(eval_identifier_to_reference(cx, &id)),
        ast::Expression::Member(expr) => maybe!(eval_member_expression_to_reference(cx, &expr)),
        ast::Expression::SuperMember(expr) => {
            maybe!(eval_super_member_expression_to_reference(cx, &expr))
        }
        other => {
            maybe!(eval_expression(cx, other));
            return true.into();
        }
    };

    match reference.base() {
        ReferenceBase::Unresolvable { .. } => true.into(),
        ReferenceBase::Property { object, property, .. } => {
            if reference.is_super_reference() {
                return reference_error_(cx, "cannot delete super");
            }

            let base_object = maybe!(to_object(cx, *object));
            let delete_status = maybe!(base_object.clone().delete(property));
            if !delete_status && reference.is_strict() {
                return type_error_(cx, "cannot delete property");
            }

            delete_status.into()
        }
        ReferenceBase::Env { env, name } => {
            let delete_status = maybe!(env.clone().delete_binding(cx, *name));
            delete_status.into()
        }
    }
}

// 13.5.2.1 Void Expression Evaluation
fn eval_void_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    maybe!(eval_expression(cx, &expr.argument));
    Value::undefined().into()
}

// 13.5.3.1 TypeOf Expression Evaluation
fn eval_typeof_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let value = match expr.argument.as_ref() {
        ast::Expression::Id(id) => {
            let reference = maybe!(eval_identifier_to_reference(cx, &id));
            if reference.is_unresolvable_reference() {
                return cx.heap.alloc_string(String::from("undefined")).into();
            }

            maybe!(reference.get_value(cx))
        }
        ast::Expression::Member(expr) => {
            let reference = maybe!(eval_member_expression_to_reference(cx, &expr));
            if reference.is_unresolvable_reference() {
                return cx.heap.alloc_string(String::from("undefined")).into();
            }

            maybe!(reference.get_value(cx))
        }
        ast::Expression::SuperMember(expr) => {
            let reference = maybe!(eval_super_member_expression_to_reference(cx, &expr));
            if reference.is_unresolvable_reference() {
                return cx.heap.alloc_string(String::from("undefined")).into();
            }

            maybe!(reference.get_value(cx))
        }
        other => maybe!(eval_expression(cx, other)),
    };

    let type_string = match value.get_tag() {
        NULL_TAG => "object",
        UNDEFINED_TAG => "undefined",
        BOOL_TAG => "boolean",
        STRING_TAG => "string",
        OBJECT_TAG => {
            if value.as_object().is_callable() {
                "function"
            } else {
                "object"
            }
        }
        SYMBOL_TAG => "symbol",
        BIGINT_TAG => "bigint",
        _ => "number",
    };

    cx.heap.alloc_string(String::from(type_string)).into()
}

// 13.5.4.1 Unary Plus Evaluation
fn eval_unary_plus(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    to_number(cx, value)
}

// 13.5.5.1 Unary Minus Evaluation
fn eval_unary_minus(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let neg_bignum = -value.as_bigint().bigint();
        Value::bigint(cx.heap.alloc_bigint(neg_bignum)).into()
    } else {
        Value::number(-value.as_number()).into()
    }
}

// 13.5.6.1 Bitwise Not Evaluation
fn eval_bitwise_not(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let value = maybe!(eval_expression(cx, &expr.argument));
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let not_bignum = !value.as_bigint().bigint();
        Value::bigint(cx.heap.alloc_bigint(not_bignum)).into()
    } else {
        let value = must!(to_int32(cx, value));
        Value::smi(!value).into()
    }
}

// 13.5.7.1 Logical Not Evaluation
fn eval_logical_not_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let expr_value = maybe!(eval_expression(cx, &expr.argument));
    (!to_boolean(expr_value)).into()
}

fn eval_binary_expression(cx: &mut Context, expr: &ast::BinaryExpression) -> EvalResult<Value> {
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
            maybe!(is_loosely_equal(cx, left_value, right_value)).into()
        }
        ast::BinaryOperator::NotEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            (!maybe!(is_loosely_equal(cx, left_value, right_value))).into()
        }
        ast::BinaryOperator::EqEqEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            is_strictly_equal(left_value, right_value).into()
        }
        ast::BinaryOperator::NotEqEq => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            let right_value = maybe!(eval_expression(cx, &expr.right));
            (!is_strictly_equal(left_value, right_value)).into()
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
            eval_instanceof_expression(cx, left_value, right_value)
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

fn eval_add(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_prim = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::None));
    let right_prim = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::None));
    if left_prim.is_string() || right_prim.is_string() {
        let left_string = maybe!(to_string(cx, left_prim));
        let right_string = maybe!(to_string(cx, right_prim));

        return cx
            .heap
            .alloc_string(format!("{}{}", left_string.str(), right_string.str()))
            .into();
    }

    let left_num = maybe!(to_numeric(cx, left_prim));
    let right_num = maybe!(to_numeric(cx, right_prim));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() + right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        return Value::number(left_num.as_number() + right_num.as_number()).into();
    }
}

fn eval_subtract(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() - right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        return Value::number(left_num.as_number() - right_num.as_number()).into();
    }
}

fn eval_multiply(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() * right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        return Value::number(left_num.as_number() * right_num.as_number()).into();
    }
}

fn eval_divide(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
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
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        return Value::number(left_num.as_number() / right_num.as_number()).into();
    }
}

fn eval_remainder(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
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
            return Value::bigint(cx.heap.alloc_bigint(BigInt::default())).into();
        }

        let result = bigint_left % bigint_right;
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        return Value::number(left_num.as_number() % right_num.as_number()).into();
    }
}

fn eval_exponentiation(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
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
            return Value::bigint(cx.heap.alloc_bigint(1.into())).into();
        }

        if let Ok(exponent_u32) = exponent_bignum.try_into() {
            let result = base_bignum.pow(exponent_u32);
            return Value::bigint(cx.heap.alloc_bigint(result)).into();
        } else {
            // This guarantees a bigint that is too large
            return range_error_(cx, "BigInt is too large");
        }
    } else {
        return Value::number(number_exponentiate(left_num.as_number(), right_num.as_number()))
            .into();
    }
}

fn eval_less_than(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    if result.is_undefined() {
        false.into()
    } else {
        result.into()
    }
}

fn eval_greater_than(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    if result.is_undefined() {
        false.into()
    } else {
        result.into()
    }
}

fn eval_less_than_or_equal(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    (result.is_false()).into()
}

fn eval_greater_than_or_equal(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    (result.is_false()).into()
}

fn eval_bitwise_and(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() & right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi & right_smi).into();
    }
}

fn eval_bitwise_or(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() | right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi | right_smi).into();
    }
}

fn eval_bitwise_xor(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() ^ right_num.as_bigint().bigint();
        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi ^ right_smi).into();
    }
}

fn eval_shift_left(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            left_num.as_bigint().bigint(),
            right_num.as_bigint().bigint()
        ));

        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi << shift).into();
    }
}

fn eval_shift_right_arithmetic(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            left_num.as_bigint().bigint(),
            &-right_num.as_bigint().bigint()
        ));

        return Value::bigint(cx.heap.alloc_bigint(result)).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi >> shift).into();
    }
}

// 6.1.6.2.9 BigInt::leftShift
fn eval_bigint_left_shift(cx: &mut Context, left: &BigInt, right: &BigInt) -> EvalResult<BigInt> {
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
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
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

    return Value::from_u64((left_smi >> shift) as u64).into();
}

// 13.10.2 InstanceofOperator
fn eval_instanceof_expression(cx: &mut Context, value: Value, target: Value) -> EvalResult<Value> {
    if !target.is_object() {
        return type_error_(cx, "invalid instanceof operand");
    }

    let has_instance_key = PropertyKey::symbol(cx.well_known_symbols.has_instance);
    let instance_of_handler = maybe!(get_method(cx, target, &has_instance_key));
    if let Some(instance_of_handler) = instance_of_handler {
        let result = maybe!(call_object(cx, instance_of_handler, target, &[value]));
        return to_boolean(result).into();
    }

    let target_object = target.as_object();
    if !target_object.is_callable() {
        return type_error_(cx, "invalid 'instanceof' operand");
    }

    let has_instance = maybe!(ordinary_has_instance(cx, target_object, value));
    has_instance.into()
}

fn eval_in_expression(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
    if !right_value.is_object() {
        return type_error_(cx, "right side of 'in' must be an object");
    }

    let property_key = maybe!(to_property_key(cx, left_value));

    let has_property = maybe!(has_property(right_value.as_object(), &property_key));
    has_property.into()
}

fn eval_private_in_expression(
    cx: &mut Context,
    private_property: &ast::Identifier,
    right_value: Value,
) -> EvalResult<Value> {
    if !right_value.is_object() {
        return type_error_(cx, "right side of 'in' must be an object");
    }

    let private_id = cx
        .current_execution_context()
        .private_env
        .unwrap()
        .resolve_private_identifier(&private_property.name);

    right_value
        .as_object()
        .private_element_find(private_id)
        .is_some()
        .into()
}

// 13.13.1 Logical Expression Evaluation
fn eval_logical_expression(cx: &mut Context, expr: &ast::LogicalExpression) -> EvalResult<Value> {
    match expr.operator {
        ast::LogicalOperator::And => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            if !to_boolean(left_value) {
                left_value.into()
            } else {
                eval_expression(cx, &expr.right)
            }
        }
        ast::LogicalOperator::Or => {
            let left_value = maybe!(eval_expression(cx, &expr.left));
            if to_boolean(left_value) {
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
    cx: &mut Context,
    expr: &ast::ConditionalExpression,
) -> EvalResult<Value> {
    let test_value = maybe!(eval_expression(cx, &expr.test));
    if to_boolean(test_value) {
        eval_expression(cx, &expr.conseq)
    } else {
        eval_expression(cx, &expr.altern)
    }
}

// 13.15.2 Assignment Expression Evaluation
fn eval_assignment_expression(
    cx: &mut Context,
    expr: &ast::AssignmentExpression,
) -> EvalResult<Value> {
    let mut reference = match expr.left.as_ref() {
        ast::Pattern::Id(id) => maybe!(eval_identifier_to_reference(cx, &id)),
        ast::Pattern::Reference(ast::Expression::Member(expr)) => {
            maybe!(eval_member_expression_to_reference(cx, &expr))
        }
        ast::Pattern::Reference(ast::Expression::SuperMember(expr)) => {
            maybe!(eval_super_member_expression_to_reference(cx, &expr))
        }
        ast::Pattern::Object(_) => unimplemented!("object patterns"),
        ast::Pattern::Array(_) => unimplemented!("array patterns"),
        _ => unreachable!("invalid assigment left hand side"),
    };

    let result_value = match expr.operator {
        ast::AssignmentOperator::Equals => {
            maybe!(eval_named_anonymous_function_or_expression(
                cx,
                &expr.right,
                &reference.name_as_property_key()
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
            if !to_boolean(left_value) {
                return left_value.into();
            }

            maybe!(eval_named_anonymous_function_or_expression(
                cx,
                &expr.right,
                &reference.name_as_property_key()
            ))
        }
        ast::AssignmentOperator::LogicalOr => {
            let left_value = maybe!(reference.get_value(cx));
            if to_boolean(left_value) {
                return left_value.into();
            }

            maybe!(eval_named_anonymous_function_or_expression(
                cx,
                &expr.right,
                &reference.name_as_property_key()
            ))
        }
        ast::AssignmentOperator::NullishCoalesce => {
            let left_value = maybe!(reference.get_value(cx));
            if !left_value.is_nullish() {
                return left_value.into();
            }

            maybe!(eval_named_anonymous_function_or_expression(
                cx,
                &expr.right,
                &reference.name_as_property_key()
            ))
        }
    };

    maybe!(reference.put_value(cx, result_value));

    result_value.into()
}

// 13.16.1 Sequence Expression Evaluation
fn eval_sequence_expression(cx: &mut Context, expr: &ast::SequenceExpression) -> EvalResult<Value> {
    let mut value = Value::empty();

    for expr in &expr.expressions {
        value = maybe!(eval_expression(cx, expr))
    }

    value.into()
}

// 15.2.6 Function Expression Evaluation
fn eval_function_expression(cx: &mut Context, func: &ast::Function) -> EvalResult<Value> {
    instantiate_ordinary_function_expression(cx, func, None).into()
}

// 15.3.5 Arrow Function Evaluation
fn eval_arrow_function(cx: &mut Context, func: &ast::Function) -> EvalResult<Value> {
    instantiate_arrow_function_expression(cx, func, None).into()
}
