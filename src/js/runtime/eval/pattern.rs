use crate::{
    js::{
        parser::ast,
        runtime::{
            completion::AbstractResult, environment::environment::Environment,
            execution_context::resolve_binding, gc::Gc, value::Value, Context,
        },
    },
    maybe_, must_,
};

// 8.5.2 BindingInitialization
pub fn binding_initialization(
    cx: &mut Context,
    patt: &ast::Pattern,
    value: Value,
    env: Option<Gc<dyn Environment>>,
) -> AbstractResult<()> {
    match patt {
        ast::Pattern::Id(id) => initialize_bound_name(cx, &id.name, value, env),
        ast::Pattern::Array(_) => unimplemented!("array patterns"),
        ast::Pattern::Object(_) => unimplemented!("object patterns"),
        ast::Pattern::Assign(_) => unreachable!(),
    }
}

// 8.5.2.1 InitializeBoundName
fn initialize_bound_name(
    cx: &mut Context,
    name: &str,
    value: Value,
    env: Option<Gc<dyn Environment>>,
) -> AbstractResult<()> {
    match env {
        Some(mut env) => {
            must_!(env.initialize_binding(cx, name, value));
            ().into()
        }
        None => {
            let mut reference = maybe_!(resolve_binding(cx, name, env));
            reference.put_value(cx, value)
        }
    }
}
