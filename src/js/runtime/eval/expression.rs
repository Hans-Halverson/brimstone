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

pub fn eval_expression(cx: &mut Context, expr: &ast::Expression) -> Completion {
    match expr {
        ast::Expression::Id(id) => eval_identifier(cx, id),
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
