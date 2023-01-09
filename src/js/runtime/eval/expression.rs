use crate::{
    js::{
        parser::ast::{self, UpdateOperator},
        runtime::{
            abstract_operations::{
                call, call_object, construct, create_data_property_or_throw, get_method,
                has_property, ordinary_has_instance,
            },
            completion::EvalResult,
            error::{reference_error_, type_error_},
            execution_context::{resolve_binding, resolve_this_binding},
            intrinsics::intrinsics::Intrinsic,
            object_value::ObjectValue,
            ordinary_object::ordinary_object_create,
            reference::{Reference, ReferenceBase},
            type_utilities::{
                is_callable, is_constructor, is_less_than, is_loosely_equal, is_strictly_equal,
                same_object_value, to_boolean, to_number, to_numeric, to_object, to_primitive,
                to_property_key, to_string, ToPrimitivePreferredType,
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
    statement::eval_named_anonymous_function_or_expression,
};

pub fn eval_expression(cx: &mut Context, expr: &ast::Expression) -> EvalResult<Value> {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
        ast::Expression::Null(_) => eval_null_literal(),
        ast::Expression::Boolean(lit) => eval_boolean_literal(lit),
        ast::Expression::Number(lit) => eval_number_literal(lit),
        ast::Expression::String(lit) => eval_string_literal(cx, lit),
        ast::Expression::Unary(expr) => match expr.operator {
            ast::UnaryOperator::Plus => eval_unary_plus(cx, expr),
            ast::UnaryOperator::Minus => eval_unary_minus(cx, expr),
            ast::UnaryOperator::LogicalNot => eval_logical_not_expression(cx, expr),
            ast::UnaryOperator::BitwiseNot => unimplemented!("bitwise not expression"),
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
        ast::Expression::Array(_) => unimplemented!("array expression"),
        ast::Expression::Object(expr) => eval_object_expression(cx, expr),
        ast::Expression::Function(func) => eval_function_expression(cx, func),
        ast::Expression::ArrowFunction(func) => eval_arrow_function(cx, func),
        ast::Expression::Class(class) => eval_class_expression(cx, class),
        ast::Expression::This(_) => eval_this_expression(cx),
        ast::Expression::Await(_) => unimplemented!("await expression"),
        ast::Expression::Yield(_) => unimplemented!("yield expression"),
    }
}

// 13.1.3 Identifier Evaluation
pub fn eval_identifier(cx: &mut Context, id: &ast::Identifier) -> EvalResult<Value> {
    let reference = maybe!(resolve_binding(cx, &id.name, None));

    // Unlike the spec, greedily call GetValue here as all eval functions evaluate to a value.
    // TOOD: If a reference is needed provide another method to evaluate to a reference instead
    reference.get_value(cx)
}

// Same as eval_identifier, but returns a reference instead of a value
pub fn eval_identifier_to_reference(
    cx: &mut Context,
    id: &ast::Identifier,
) -> EvalResult<Reference> {
    resolve_binding(cx, &id.name, None)
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
    Value::string(cx.heap.alloc_string(lit.value.clone())).into()
}

// 13.2.5.4 Object Initializer Evaluation
fn eval_object_expression(cx: &mut Context, expr: &ast::ObjectExpression) -> EvalResult<Value> {
    let proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
    let mut object: Gc<ObjectValue> = cx.heap.alloc(ordinary_object_create(proto)).into();

    for property in &expr.properties {
        match property.value.as_ref() {
            // Identifier shorthand property
            None => {
                if let ast::Expression::Id(id) = property.key.as_ref() {
                    let prop_value = maybe!(eval_identifier(cx, id));
                    must!(create_data_property_or_throw(
                        cx, object, &id.name, prop_value
                    ));
                } else {
                    unreachable!()
                };
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
                    property.kind,
                    /* is_enumerable */ true,
                ))
            }
            Some(value) => {
                let property_key =
                    maybe!(eval_property_name(cx, &property.key, property.is_computed));

                // TODO: Check if in JSON.parse
                let is_proto_setter = property_key == "__proto__" && !property.is_computed;
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
                        property_key
                    ));

                    must!(create_data_property_or_throw(
                        cx,
                        object,
                        property_key,
                        prop_value
                    ));
                }
            }
        }
    }

    object.into()
}

pub fn eval_property_name<'a>(
    cx: &mut Context,
    key: &'a ast::Expression,
    is_computed: bool,
) -> EvalResult<&'a str> {
    let property_key = if is_computed {
        let property_key_value = maybe!(eval_expression(cx, key));
        to_property_key(property_key_value).str()
    } else {
        match key {
            ast::Expression::Id(id) => id.name.as_str(),
            ast::Expression::String(lit) => lit.value.as_str(),
            ast::Expression::Number(_) => unimplemented!("numeric property keys"),
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
        let property_key = to_property_key(property_name_value);

        let base = maybe!(to_object(cx, base_value));
        base.get(cx, property_key.str(), base.into())
    } else {
        let property_name = match *expr.property {
            ast::Expression::Id(ref id) => &id.name,
            _ => unreachable!(),
        };

        let base = maybe!(to_object(cx, base_value));
        base.get(cx, property_name, base.into())
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
        let property_key = to_property_key(property_name_value);

        Reference::new_value(base_value, String::from(property_key.str()), is_strict).into()
    } else {
        let property_name = match *expr.property {
            ast::Expression::Id(ref id) => &id.name,
            _ => unreachable!(),
        };

        Reference::new_value(base_value, property_name.to_owned(), is_strict).into()
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
        _ => None,
    };

    let (func_value, this_value) = match callee_reference {
        Some(reference) => {
            let func_value = maybe!(reference.get_value(cx));

            // Check for direct call to eval
            let eval_func = cx.current_realm().get_intrinsic(Intrinsic::Eval);
            if func_value.is_object()
                && same_object_value(func_value.as_object(), eval_func)
                && !reference.is_property_reference()
                && reference.name() == "eval"
            {
                let arg_values = maybe!(eval_argument_list(cx, &expr.arguments));
                if arg_values.is_empty() {
                    return Value::undefined().into();
                }

                let eval_arg = &arg_values[0];
                let is_strict_caller = cx.current_execution_context().is_strict_mode;

                return perform_eval(cx, eval_arg.clone(), is_strict_caller, true);
            }

            let this_value = match reference.base() {
                ReferenceBase::Value(_) => reference.get_this_value(),
                ReferenceBase::Env(env) => match env.with_base_object() {
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
    arguments: &[ast::Expression],
) -> EvalResult<Value> {
    let arg_values = maybe!(eval_argument_list(cx, arguments));
    if !is_callable(func_value) {
        return type_error_(cx, "value is not a function");
    }

    call(cx, func_value, this_value, &arg_values)
}

// 13.3.8.1 ArgumentListEvaluation
fn eval_argument_list(cx: &mut Context, arguments: &[ast::Expression]) -> EvalResult<Vec<Value>> {
    let mut arg_values = vec![];

    for arg in arguments {
        let arg_value = maybe!(eval_expression(cx, arg));
        arg_values.push(arg_value)
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
        _ => return reference_error_(cx, "expected a reference"),
    };
    let old_value = maybe!(argument_reference.get_value(cx));
    let old_value = maybe!(to_numeric(cx, old_value));

    let new_value = match expr.operator {
        UpdateOperator::Increment => {
            if old_value.is_bigint() {
                unimplemented!("BigInts")
            } else {
                Value::number(old_value.as_number() + 1.0)
            }
        }
        UpdateOperator::Decrement => {
            if old_value.is_bigint() {
                unimplemented!("BigInts")
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
        other => {
            maybe!(eval_expression(cx, other));
            return true.into();
        }
    };

    match reference.base() {
        ReferenceBase::Unresolvable => true.into(),
        ReferenceBase::Value(base_value) => {
            if reference.is_super_reference() {
                return reference_error_(cx, "cannot delete super");
            }

            let base_object = maybe!(to_object(cx, base_value.clone()));
            let delete_status = maybe!(base_object.clone().delete(reference.name()));
            if !delete_status && reference.is_strict() {
                return type_error_(cx, "cannot delete property");
            }

            delete_status.into()
        }
        ReferenceBase::Env(env) => {
            let delete_status = maybe!(env.clone().delete_binding(cx, reference.name()));
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
        unimplemented!("BigInt")
    } else {
        Value::number(-value.as_number()).into()
    }
}

// 13.5.7.1 Logical Not Evaluation
fn eval_logical_not_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    let expr_value = maybe!(eval_expression(cx, &expr.argument));
    (!to_boolean(expr_value)).into()
}

fn eval_binary_expression(cx: &mut Context, expr: &ast::BinaryExpression) -> EvalResult<Value> {
    let left_value = maybe!(eval_expression(cx, &expr.left));
    let right_value = maybe!(eval_expression(cx, &expr.right));

    match expr.operator {
        ast::BinaryOperator::Add => eval_add(cx, left_value, right_value),
        ast::BinaryOperator::Subtract => eval_subtract(cx, left_value, right_value),
        ast::BinaryOperator::Multiply => eval_multiply(cx, left_value, right_value),
        ast::BinaryOperator::Divide => eval_divide(cx, left_value, right_value),
        ast::BinaryOperator::Remainder => eval_remainder(cx, left_value, right_value),
        ast::BinaryOperator::Exponent => unimplemented!("exponent expression"),
        ast::BinaryOperator::EqEq => maybe!(is_loosely_equal(cx, left_value, right_value)).into(),
        ast::BinaryOperator::NotEq => {
            (!maybe!(is_loosely_equal(cx, left_value, right_value))).into()
        }
        ast::BinaryOperator::EqEqEq => is_strictly_equal(left_value, right_value).into(),
        ast::BinaryOperator::NotEqEq => (!is_strictly_equal(left_value, right_value)).into(),
        ast::BinaryOperator::LessThan => eval_less_than(cx, left_value, right_value),
        ast::BinaryOperator::LessThanOrEqual => {
            eval_less_than_or_equal(cx, left_value, right_value)
        }
        ast::BinaryOperator::GreaterThan => eval_greater_than(cx, left_value, right_value),
        ast::BinaryOperator::GreaterThanOrEqual => {
            eval_greater_than_or_equal(cx, left_value, right_value)
        }
        ast::BinaryOperator::And => unimplemented!("bitwise and expression"),
        ast::BinaryOperator::Or => unimplemented!("bitwise or expression"),
        ast::BinaryOperator::Xor => unimplemented!("bitwise xor expression"),
        ast::BinaryOperator::ShiftLeft => unimplemented!("left shift expression"),
        ast::BinaryOperator::ShiftRightArithmetic => {
            unimplemented!("arithmetic right shift expression")
        }
        ast::BinaryOperator::ShiftRightLogical => unimplemented!("logical right shift expression"),
        ast::BinaryOperator::InstanceOf => eval_instanceof_expression(cx, left_value, right_value),
        ast::BinaryOperator::In => eval_in_expression(cx, left_value, right_value),
    }
}

fn eval_add(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left_prim = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::None));
    let right_prim = maybe!(to_primitive(
        cx,
        right_value,
        ToPrimitivePreferredType::None
    ));
    if left_prim.is_string() || right_prim.is_string() {
        let left_string = maybe!(to_string(cx, left_prim));
        let right_string = maybe!(to_string(cx, right_prim));

        return cx
            .heap
            .alloc_string(format!("{}{}", left_string.str(), right_string.str()))
            .into();
    }

    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error_(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        unimplemented!("BigInt")
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
        unimplemented!("BigInt")
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
        unimplemented!("BigInt")
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
        unimplemented!("BigInt")
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
        unimplemented!("BigInt")
    } else {
        return Value::number(left_num.as_number() % right_num.as_number()).into();
    }
}

fn eval_less_than(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left = maybe!(to_primitive(
        cx,
        left_value,
        ToPrimitivePreferredType::Number
    ));
    let right = maybe!(to_primitive(
        cx,
        right_value,
        ToPrimitivePreferredType::Number
    ));

    let result = maybe!(is_less_than(cx, left, right));
    if result.is_undefined() {
        false.into()
    } else {
        result.into()
    }
}

fn eval_greater_than(cx: &mut Context, left_value: Value, right_value: Value) -> EvalResult<Value> {
    let left = maybe!(to_primitive(
        cx,
        left_value,
        ToPrimitivePreferredType::Number
    ));
    let right = maybe!(to_primitive(
        cx,
        right_value,
        ToPrimitivePreferredType::Number
    ));

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
    let left = maybe!(to_primitive(
        cx,
        left_value,
        ToPrimitivePreferredType::Number
    ));
    let right = maybe!(to_primitive(
        cx,
        right_value,
        ToPrimitivePreferredType::Number
    ));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    (result.is_false()).into()
}

fn eval_greater_than_or_equal(
    cx: &mut Context,
    left_value: Value,
    right_value: Value,
) -> EvalResult<Value> {
    let left = maybe!(to_primitive(
        cx,
        left_value,
        ToPrimitivePreferredType::Number
    ));
    let right = maybe!(to_primitive(
        cx,
        right_value,
        ToPrimitivePreferredType::Number
    ));

    let result = maybe!(is_less_than(cx, left, right));
    (result.is_false()).into()
}

// 13.10.2 InstanceofOperator
fn eval_instanceof_expression(cx: &mut Context, value: Value, target: Value) -> EvalResult<Value> {
    if !target.is_object() {
        return type_error_(cx, "invalid instanceof operand");
    }

    // TODO: Change to symbol once symbols are implemented
    let instance_of_handler = maybe!(get_method(cx, target, "@@hasInstance"));
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

    // TODO: Handle private identifiers on left hand side
    let property_key = to_property_key(left_value);

    let has_property = maybe!(has_property(right_value.as_object(), property_key.str()));
    has_property.into()
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
        ast::Expression::Id(id) => maybe!(eval_identifier_to_reference(cx, &id)),
        ast::Expression::Member(expr) => maybe!(eval_member_expression_to_reference(cx, &expr)),
        ast::Expression::Object(_) => unimplemented!("object patterns"),
        ast::Expression::Array(_) => unimplemented!("array patterns"),
        _ => unreachable!("invalid assigment left hand side"),
    };

    let result_value = match expr.operator {
        ast::AssignmentOperator::Equals => {
            maybe!(eval_named_anonymous_function_or_expression(
                cx,
                &expr.right,
                reference.name()
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
        _ => unimplemented!("assignment operators"),
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
