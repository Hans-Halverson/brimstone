use crate::{
    js::{
        parser::ast,
        runtime::{
            abstract_operations::{call, construct},
            completion::{AbstractResult, Completion, CompletionKind},
            error::type_error,
            execution_context::{resolve_binding, resolve_this_binding},
            reference::{Reference, ReferenceBase},
            type_utilities::{is_callable, is_constructor, to_boolean, to_object, to_property_key},
            value::Value,
            Context,
        },
    },
    maybe, maybe__,
};

use super::{
    function::{instantiate_arrow_function_expression, instantiate_ordinary_function_expression},
    statement::eval_named_anonymous_function_or_expression,
};

pub fn eval_expression(cx: &mut Context, expr: &ast::Expression) -> Completion {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
        ast::Expression::Null(_) => eval_null_literal(),
        ast::Expression::Boolean(lit) => eval_boolean_literal(lit),
        ast::Expression::Number(lit) => eval_number_literal(lit),
        ast::Expression::String(lit) => eval_string_literal(cx, lit),
        ast::Expression::Unary(expr) => match expr.operator {
            ast::UnaryOperator::Void => eval_void_expression(cx, expr),
            ast::UnaryOperator::LogicalNot => eval_logical_not_expression(cx, expr),
            _ => unimplemented!("unary expression evaluation"),
        },
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
pub fn eval_identifier(cx: &mut Context, id: &ast::Identifier) -> Completion {
    let reference = maybe__!(resolve_binding(cx, &id.name, None));

    // Unlike the spec, greedily call GetValue here as all eval functions evaluate to a value.
    // TOOD: If a reference is needed provide another method to evaluate to a reference instead
    reference.get_value(cx).into()
}

// Same as eval_identifier, but returns a reference instead of a value
fn eval_identifier_to_reference(
    cx: &mut Context,
    id: &ast::Identifier,
) -> AbstractResult<Reference> {
    resolve_binding(cx, &id.name, None)
}

// 13.2.1.1 This Expression Evaluation
fn eval_this_expression(cx: &mut Context) -> Completion {
    resolve_this_binding(cx).into()
}

// 13.2.3.1 Literal Evaluation
fn eval_null_literal() -> Completion {
    Value::null().into()
}

fn eval_boolean_literal(lit: &ast::BooleanLiteral) -> Completion {
    Value::bool(lit.value).into()
}

fn eval_number_literal(lit: &ast::NumberLiteral) -> Completion {
    Value::number(lit.value).into()
}

fn eval_string_literal(cx: &mut Context, lit: &ast::StringLiteral) -> Completion {
    Value::string(cx.heap.alloc_string(lit.value.clone())).into()
}

// 13.3.2.1 Member Expression Evaluation
// Standard eval functions greedily call GetValue instead of returning a reference. This function
// inlines the GetValue call.
fn eval_member_expression(cx: &mut Context, expr: &ast::MemberExpression) -> Completion {
    let base_value = maybe!(eval_expression(cx, &expr.object));

    if expr.is_computed {
        let property_name_value = maybe!(eval_expression(cx, &expr.property));
        let property_key = to_property_key(property_name_value);

        let base = maybe__!(to_object(cx, base_value));
        base.get(cx, &property_key, base.into()).into()
    } else {
        let property_name = match *expr.property {
            ast::Expression::Id(ref id) => &id.name,
            _ => unreachable!(),
        };

        let base = maybe__!(to_object(cx, base_value));
        base.get(cx, property_name, base.into()).into()
    }
}

// Same as eval_member_expression, but returns a reference instead of a value
fn eval_member_expression_to_reference(
    cx: &mut Context,
    expr: &ast::MemberExpression,
) -> AbstractResult<Reference> {
    let base_completion = eval_expression(cx, &expr.object);
    let base_value = match base_completion.kind() {
        CompletionKind::Normal => base_completion.value(),
        CompletionKind::Throw => return AbstractResult::Throw(base_completion.value()),
        CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
            unreachable!("expression cannot have non-throw abnormal completions")
        }
    };

    // TODO: Check if we are currently in strict mode
    let is_strict = false;

    if expr.is_computed {
        let property_name_completion = eval_expression(cx, &expr.property);
        let property_name_value = match property_name_completion.kind() {
            CompletionKind::Normal => property_name_completion.value(),
            CompletionKind::Throw => {
                return AbstractResult::Throw(property_name_completion.value())
            }
            CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
                unreachable!("expression cannot have non-throw abnormal completions")
            }
        };

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
fn eval_new_expression(cx: &mut Context, expr: &ast::NewExpression) -> Completion {
    let constructor = maybe!(eval_expression(cx, &expr.callee));
    let arg_values = maybe__!(eval_argument_list(cx, &expr.arguments));

    if !is_constructor(constructor) {
        return type_error(cx, "value is not a constructor");
    }

    construct(cx, constructor.as_object(), arg_values, None).into()
}

// 13.3.6.1 Call Expression Evaluation
fn eval_call_expression(cx: &mut Context, expr: &ast::CallExpression) -> Completion {
    let callee_reference = match expr.callee.as_ref() {
        ast::Expression::Id(id) => Some(maybe__!(eval_identifier_to_reference(cx, &id))),
        ast::Expression::Member(expr) => {
            Some(maybe__!(eval_member_expression_to_reference(cx, &expr)))
        }
        _ => None,
    };

    let (func_value, this_value) = match callee_reference {
        Some(reference) => {
            let func_value = maybe__!(reference.get_value(cx));

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
) -> Completion {
    let arg_values = maybe__!(eval_argument_list(cx, arguments));
    if !is_callable(func_value) {
        return type_error(cx, "value is not a function");
    }

    call(cx, func_value, this_value, arg_values).into()
}

// 13.3.8.1 ArgumentListEvaluation
fn eval_argument_list(
    cx: &mut Context,
    arguments: &[ast::Expression],
) -> AbstractResult<Vec<Value>> {
    let mut arg_values = vec![];

    for arg in arguments {
        let arg_completion = eval_expression(cx, arg);
        match arg_completion.kind() {
            CompletionKind::Normal => arg_values.push(arg_completion.value()),
            CompletionKind::Throw => return AbstractResult::Throw(arg_completion.value()),
            CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
                unreachable!("expression cannot have non-throw abnormal completions")
            }
        };
    }

    arg_values.into()
}

// 13.5.2.1 Void Expression Evaluation
fn eval_void_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> Completion {
    maybe!(eval_expression(cx, &expr.argument));
    Value::undefined().into()
}

// 13.5.7.1 Logical Not Expression Evaluation
fn eval_logical_not_expression(cx: &mut Context, expr: &ast::UnaryExpression) -> Completion {
    let expr_value = maybe!(eval_expression(cx, &expr.argument));
    (!to_boolean(expr_value)).into()
}

// 13.13.1 Logical Expression Evaluation
fn eval_logical_expression(cx: &mut Context, expr: &ast::LogicalExpression) -> Completion {
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
fn eval_conditional_expression(cx: &mut Context, expr: &ast::ConditionalExpression) -> Completion {
    let test_value = maybe!(eval_expression(cx, &expr.test));
    if to_boolean(test_value) {
        eval_expression(cx, &expr.conseq)
    } else {
        eval_expression(cx, &expr.altern)
    }
}

// 13.15.2 Assignment Expression Evaluation
fn eval_assignment_expression(cx: &mut Context, expr: &ast::AssignmentExpression) -> Completion {
    let mut reference = match expr.left.as_ref() {
        ast::Expression::Id(id) => maybe__!(eval_identifier_to_reference(cx, &id)),
        ast::Expression::Member(expr) => maybe__!(eval_member_expression_to_reference(cx, &expr)),
        ast::Expression::Object(_) => unimplemented!("object patterns"),
        ast::Expression::Array(_) => unimplemented!("array patterns"),
        _ => unreachable!("invalid assigment left hand side"),
    };

    match expr.operator {
        ast::AssignmentOperator::Equals => {}
        _ => unimplemented!("assignment operators"),
    }

    let right_value = maybe!(eval_named_anonymous_function_or_expression(
        cx,
        &expr.right,
        reference.name()
    ));

    maybe__!(reference.put_value(cx, right_value));

    right_value.into()
}

// 13.16.1 Sequence Expression Evaluation
fn eval_sequence_expression(cx: &mut Context, expr: &ast::SequenceExpression) -> Completion {
    let mut value = Value::empty();

    for expr in &expr.expressions {
        value = maybe!(eval_expression(cx, expr))
    }

    value.into()
}

// 15.2.6 Function Expression Evaluation
fn eval_function_expression(cx: &mut Context, func: &ast::Function) -> Completion {
    instantiate_ordinary_function_expression(cx, func, None).into()
}

// 15.3.5 Arrow Function Evaluation
fn eval_arrow_function(cx: &mut Context, func: &ast::Function) -> Completion {
    instantiate_arrow_function_expression(cx, func, None).into()
}
