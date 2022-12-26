use crate::{
    js::{
        parser::ast,
        runtime::{
            completion::{AbstractResult, Completion},
            execution_context::{resolve_binding, resolve_this_binding},
            type_utilities::to_boolean,
            value::Value,
            Context,
        },
    },
    maybe, maybe__,
};

use super::function::{
    instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
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
        ast::Expression::Conditional(expr) => eval_conditional_expression(cx, expr),
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
