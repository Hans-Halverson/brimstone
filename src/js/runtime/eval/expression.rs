use crate::{
    js::{
        parser::ast,
        runtime::{
            abstract_operations::{call, construct},
            completion::EvalResult,
            error::type_error_,
            execution_context::{resolve_binding, resolve_this_binding},
            reference::{Reference, ReferenceBase},
            type_utilities::{
                is_callable, is_constructor, is_loosely_equal, is_strictly_equal, to_boolean,
                to_number, to_numeric, to_object, to_primitive, to_property_key, to_string,
                ToPrimitivePreferredType,
            },
            value::Value,
            Context,
        },
    },
    maybe,
};

use super::{
    function::{instantiate_arrow_function_expression, instantiate_ordinary_function_expression},
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
            ast::UnaryOperator::Void => eval_void_expression(cx, expr),
            ast::UnaryOperator::LogicalNot => eval_logical_not_expression(cx, expr),
            _ => unimplemented!("unary expression evaluation"),
        },
        ast::Expression::Binary(expr) => eval_binary_expression(cx, expr),
        ast::Expression::Logical(expr) => eval_logical_expression(cx, expr),
        ast::Expression::Assign(expr) => eval_assignment_expression(cx, expr),
        ast::Expression::Member(expr) => eval_member_expression(cx, expr),
        ast::Expression::Conditional(expr) => eval_conditional_expression(cx, expr),
        ast::Expression::Call(expr) => eval_call_expression(cx, expr),
        ast::Expression::New(expr) => eval_new_expression(cx, expr),
        ast::Expression::Sequence(expr) => eval_sequence_expression(cx, expr),
        ast::Expression::Function(func) => eval_function_expression(cx, func),
        ast::Expression::ArrowFunction(func) => eval_arrow_function(cx, func),
        ast::Expression::This(_) => eval_this_expression(cx),
        _ => unimplemented!("expression evaluation"),
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
fn eval_identifier_to_reference(cx: &mut Context, id: &ast::Identifier) -> EvalResult<Reference> {
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

// 13.3.2.1 Member Expression Evaluation
// Standard eval functions greedily call GetValue instead of returning a reference. This function
// inlines the GetValue call.
fn eval_member_expression(cx: &mut Context, expr: &ast::MemberExpression) -> EvalResult<Value> {
    let base_value = maybe!(eval_expression(cx, &expr.object));

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = to_property_key(property_name_value);

        let base = maybe!(to_object(cx, base_value));
        base.get(cx, &property_key, base.into())
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

    // TODO: Check if we are currently in strict mode
    let is_strict = false;

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = to_property_key(property_name_value);

        Reference::new_value(base_value, property_key, is_strict).into()
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

            // TODO: Check for direct call to eval

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

// 13.5.2.1 Void Expression Evaluation
fn eval_void_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> EvalResult<Value> {
    maybe!(eval_expression(cx, &expr.argument));
    Value::undefined().into()
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

// 13.5.7.1 Logical Not Expression Evaluation
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
        ast::BinaryOperator::EqEq => maybe!(is_loosely_equal(cx, left_value, right_value)).into(),
        ast::BinaryOperator::NotEq => {
            (!maybe!(is_loosely_equal(cx, left_value, right_value))).into()
        }
        ast::BinaryOperator::EqEqEq => is_strictly_equal(left_value, right_value).into(),
        ast::BinaryOperator::NotEqEq => (!is_strictly_equal(left_value, right_value)).into(),
        _ => unimplemented!("binary operator"),
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