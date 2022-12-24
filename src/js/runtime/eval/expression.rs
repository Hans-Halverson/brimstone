use crate::{
    js::{
        parser::ast,
        runtime::{
            completion::{AbstractResult, Completion},
            execution_context::resolve_binding,
            Context,
        },
    },
    maybe__,
};

use super::function::{
    instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
};

pub fn eval_expression(cx: &mut Context, expr: &ast::Expression) -> Completion {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
        ast::Expression::Function(func) => eval_function_expression(cx, func),
        ast::Expression::ArrowFunction(func) => eval_arrow_function(cx, func),
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

// 15.2.6 Function Expression Evaluation
fn eval_function_expression(cx: &mut Context, func: &ast::Function) -> Completion {
    instantiate_ordinary_function_expression(cx, func, None).into()
}

// 15.3.5 Arrow Function Evaluation
fn eval_arrow_function(cx: &mut Context, func: &ast::Function) -> Completion {
    instantiate_arrow_function_expression(cx, func, None).into()
}
