use crate::{
    js::{
        parser::ast,
        runtime::{
            completion::AbstractResult,
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
                private_environment::PrivateEnvironment,
            },
            function::{make_constructor, ordinary_function_create, set_function_name, Function},
            gc::Gc,
            intrinsics::intrinsics::Intrinsic,
            Context,
        },
    },
    must_,
};

// 15.2.4 InstantiateOrdinaryFunctionObject
pub fn instantiate_ordinary_function_object(
    cx: &mut Context,
    func_node: &ast::Function,
    env: Gc<dyn Environment>,
    private_env: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    let name = match &func_node.id {
        None => "default",
        Some(id) => &id.name,
    };

    let function_prototype = cx
        .current_realm()
        .get_intrinsic(Intrinsic::FunctionPrototype);
    let function_object =
        ordinary_function_create(cx, function_prototype, func_node, false, env, private_env);

    set_function_name(cx, function_object.into(), name, None);
    make_constructor(cx, function_object, None, None);

    function_object
}

// 15.2.5 InstantiateOrdinaryFunctionExpression
pub fn instantiate_ordinary_function_expression(
    cx: &mut Context,
    func_node: &ast::Function,
    name: Option<&str>,
) -> Gc<Function> {
    let current_context = cx.current_execution_context();
    let function_prototype = cx
        .current_realm()
        .get_intrinsic(Intrinsic::FunctionPrototype);

    match &func_node.id {
        None => {
            let name = name.unwrap_or("");
            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                current_context.lexical_env,
                current_context.private_env,
            );

            set_function_name(cx, closure.into(), name, None);
            make_constructor(cx, closure, None, None);

            closure
        }
        Some(id) => {
            let name = &id.name;
            let mut func_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(
                current_context.lexical_env,
            )));
            must_!(func_env.create_immutable_binding(cx, name.to_string(), false));

            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                to_trait_object(func_env),
                current_context.private_env,
            );

            set_function_name(cx, closure.into(), name, None);
            make_constructor(cx, closure, None, None);

            must_!(func_env.initialize_binding(cx, name, closure.into()));

            closure
        }
    }
}

// 15.3.4 InstantiateArrowFunctionExpression
pub fn instantiate_arrow_function_expression(
    cx: &mut Context,
    func_node: &ast::Function,
    name: Option<&str>,
) -> Gc<Function> {
    let name = name.unwrap_or("");
    let current_context = cx.current_execution_context();

    let function_prototype = cx
        .current_realm()
        .get_intrinsic(Intrinsic::FunctionPrototype);
    let closure = ordinary_function_create(
        cx,
        function_prototype,
        func_node,
        true,
        current_context.lexical_env,
        current_context.private_env,
    );
    set_function_name(cx, closure.into(), name, None);

    closure
}
