use crate::{
    js::{
        parser::ast,
        runtime::{
            completion::EvalResult,
            environment::environment::Environment,
            execution_context::resolve_binding,
            gc::Gc,
            property_key::PropertyKey,
            value::{StringValue, Value},
            Context,
        },
    },
    maybe, must,
};

// 8.5.2 BindingInitialization
pub fn binding_initialization(
    cx: &mut Context,
    patt: &ast::Pattern,
    value: Value,
    env: Option<Gc<dyn Environment>>,
) -> EvalResult<()> {
    match patt {
        ast::Pattern::Id(id) => {
            let name_value = id_string_value(cx, id);
            initialize_bound_name(cx, name_value, value, env)
        }
        ast::Pattern::Array(_) => unimplemented!("array patterns"),
        ast::Pattern::Object(_) => unimplemented!("object patterns"),
        ast::Pattern::Assign(_) => unreachable!(),
    }
}

// 8.5.2.1 InitializeBoundName
pub fn initialize_bound_name(
    cx: &mut Context,
    name: Gc<StringValue>,
    value: Value,
    env: Option<Gc<dyn Environment>>,
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

#[inline]
pub fn id_string_value(cx: &mut Context, id: &ast::Identifier) -> Gc<StringValue> {
    cx.get_interned_string(&id.name)
}

#[inline]
pub fn id_property_key(cx: &mut Context, id: &ast::Identifier) -> PropertyKey {
    PropertyKey::String(cx.get_interned_string(&id.name))
}
