use std::collections::HashSet;

use crate::{
    js::{
        parser::ast::{self, LexDecl, VarDecl, WithDecls},
        runtime::{
            abstract_operations::define_property_or_throw,
            arguments_object::{create_mapped_arguments_object, create_unmapped_arguments_object},
            array_object::array_create,
            completion::{Completion, EvalResult},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
                private_environment::PrivateEnvironment,
            },
            eval::pattern::id_property_key,
            execution_context::resolve_binding,
            function::{
                define_method_property, instantiate_function_object, make_constructor, make_method,
                ordinary_function_create, set_function_name, FuncKind, Function,
            },
            gc::Gc,
            intrinsics::intrinsics::Intrinsic,
            object_value::{Object, ObjectValue},
            property::{PrivateProperty, Property},
            property_descriptor::PropertyDescriptor,
            property_key::PropertyKey,
            value::Value,
            Context,
        },
    },
    maybe__, must,
};

use super::{
    expression::eval_expression,
    pattern::{binding_initialization, id_string_value, maybe_extract_initializer},
    statement::eval_named_anonymous_function_or_expression,
};

// 10.2.11 FunctionDeclarationInstantiation
pub fn function_declaration_instantiation(
    cx: &mut Context,
    func: &Function,
    arguments: &[Value],
) -> Completion {
    let func_node = if let FuncKind::Function(func_node) = &func.func_node {
        func_node.as_ref()
    } else {
        unreachable!()
    };

    let mut function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for var_decl in func_node.var_decls().iter().rev() {
        if let VarDecl::Func(func_ptr) = var_decl {
            let func = func_ptr.as_ref();
            let name = &func.id.as_deref().unwrap().name;

            function_names.insert(name);
            functions_to_initialize.push(func);
        }
    }

    let is_strict = func_node.is_strict_mode;

    let mut callee_context = cx.current_execution_context();

    // A new environment is needed so that direct eval calls in parameter expressions are outside
    // the environment where parameters are declared.
    let mut env = if is_strict || !func_node.has_parameter_expressions {
        callee_context.lexical_env
    } else {
        let new_env = DeclarativeEnvironment::new(Some(callee_context.lexical_env));
        callee_context.lexical_env = to_trait_object(cx.heap.alloc(new_env));
        callee_context.lexical_env
    };

    let mut parameter_names: HashSet<&str> = HashSet::new();

    for param in &func_node.params {
        must!(param.iter_bound_names(&mut |id| {
            parameter_names.insert(&id.name);

            let name_value = id_string_value(cx, id);

            let already_declared = must!(env.has_binding(cx, name_value));
            if !already_declared {
                must!(env.create_mutable_binding(cx, name_value, false));
                if func_node.has_duplicate_parameters {
                    must!(env.initialize_binding(cx, name_value, Value::undefined()));
                }
            }

            ().into()
        }));
    }

    // Set up arguments object if needed
    if func_node.is_arguments_object_needed {
        let arguments_object = if is_strict || !func_node.has_simple_parameter_list {
            create_unmapped_arguments_object(cx, &arguments)
        } else {
            create_mapped_arguments_object(cx, func.into(), &func_node.params, &arguments, env)
        };

        let arguments_name_value = cx.get_interned_string("arguments");

        if is_strict {
            must!(env.create_immutable_binding(cx, arguments_name_value, false))
        } else {
            must!(env.create_mutable_binding(cx, arguments_name_value, false))
        }

        must!(env.initialize_binding(cx, arguments_name_value, arguments_object));

        parameter_names.insert("arguments");
    }

    let binding_init_env = if func_node.has_duplicate_parameters {
        None
    } else {
        Some(env)
    };

    // Perform inlined IteratorBindingInitialization instead of creating an actual iterator object
    let mut arg_index = 0;
    for param in &func_node.params {
        let mut value = Value::undefined();

        // For rest parameters create an array holding all the values, then evaluate pattern as
        // normal with this new array as the value.
        let pattern = match param {
            ast::FunctionParam::Pattern(pattern) => {
                if arg_index < arguments.len() {
                    let argument = arguments[arg_index];
                    arg_index += 1;
                    value = argument;
                }

                pattern
            }
            ast::FunctionParam::Rest(rest) => {
                let mut rest_array = must!(array_create(cx, 0, None));

                for (array_index, arg_index) in (arg_index..arguments.len()).enumerate() {
                    let array_key = PropertyKey::array_index(array_index as u32);
                    let array_property = Property::data(arguments[arg_index], true, true, true);
                    rest_array.object.set_property(&array_key, array_property);
                }

                value = rest_array.into();

                rest.argument.as_ref()
            }
        };

        let (patt, init) = maybe_extract_initializer(pattern);

        match patt {
            ast::Pattern::Id(id) => {
                let name_value = id_string_value(cx, id);
                let mut reference = maybe__!(resolve_binding(cx, name_value, binding_init_env));

                if let Some(init) = init {
                    if value.is_undefined() {
                        value = maybe__!(eval_named_anonymous_function_or_expression(
                            cx,
                            init,
                            &PropertyKey::string(name_value)
                        ));
                    }
                }

                if binding_init_env.is_none() {
                    maybe__!(reference.put_value(cx, value));
                } else {
                    maybe__!(reference.initialize_referenced_binding(cx, value));
                }
            }
            binding_pattern => {
                if let Some(init) = init {
                    if value.is_undefined() {
                        value = maybe__!(eval_expression(cx, init,));
                    }
                }

                maybe__!(binding_initialization(cx, binding_pattern, value, binding_init_env))
            }
        }
    }

    // Create bindings for var decls in function body
    let mut var_env = if !func_node.has_parameter_expressions {
        let mut instantiated_var_names = parameter_names;

        for var_decl in func_node.var_decls() {
            must!(var_decl.iter_bound_names(&mut |id| {
                if instantiated_var_names.insert(&id.name) {
                    let name_value = id_string_value(cx, id);
                    must!(env.create_mutable_binding(cx, name_value, false));
                    must!(env.initialize_binding(cx, name_value, Value::undefined()));
                }

                ().into()
            }));
        }

        env
    } else {
        // A separate Environment Record is needed to ensure that closures created by expressions in
        // the formal parameter list do not have visibility of declarations in the function body.
        callee_context.variable_env =
            to_trait_object(cx.heap.alloc(DeclarativeEnvironment::new(Some(env))));
        let mut var_env = callee_context.variable_env;

        let mut instantiated_var_names = HashSet::new();

        for var_decl in func_node.var_decls() {
            must!(var_decl.iter_bound_names(&mut |id| {
                if instantiated_var_names.insert(&id.name) {
                    let name_value = id_string_value(cx, id);

                    must!(env.create_mutable_binding(cx, name_value, false));

                    let initial_value = if !parameter_names.contains(id.name.as_str())
                        || function_names.contains(&id.name)
                    {
                        Value::undefined()
                    } else {
                        must!(env.get_binding_value(cx, name_value, false))
                    };

                    must!(var_env.initialize_binding(cx, name_value, initial_value));

                    ().into()
                }

                ().into()
            }));
        }

        var_env
    };

    let lex_env = if !is_strict {
        // Non-strict functions use a separate Environment Record for top-level lexical declarations
        // so that a direct eval can determine whether any var scoped declarations introduced by the
        // eval code conflict with pre-existing top-level lexically scoped declarations.
        to_trait_object(cx.heap.alloc(DeclarativeEnvironment::new(Some(var_env))))
    } else {
        var_env
    };

    callee_context.lexical_env = lex_env;

    // Create bindings for lex decls in function body
    for lex_decl in func_node.lex_decls() {
        match lex_decl {
            LexDecl::Var(var_decl) if var_decl.as_ref().kind == ast::VarKind::Const => {
                must!(lex_decl.iter_bound_names(&mut |id| {
                    let name_value = id_string_value(cx, id);
                    env.create_immutable_binding(cx, name_value, true)
                }))
            }
            _ => must!(lex_decl.iter_bound_names(&mut |id| {
                let name_value = id_string_value(cx, id);
                env.create_mutable_binding(cx, name_value, false)
            })),
        }
    }

    // Initialize toplevel function objects
    let private_env = callee_context.private_env;
    for func in functions_to_initialize {
        let func_name = id_string_value(cx, func.id.as_deref().unwrap());
        let func_object = instantiate_function_object(cx, func, lex_env, private_env);
        must!(var_env.set_mutable_binding(cx, func_name, func_object.into(), false));
    }

    Completion::empty()
}

// 15.2.4 InstantiateOrdinaryFunctionObject
pub fn instantiate_ordinary_function_object(
    cx: &mut Context,
    func_node: &ast::Function,
    env: Gc<dyn Environment>,
    private_env: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    let name_key = match &func_node.id {
        None => cx.names.default(),
        Some(id) => id_property_key(cx, id),
    };

    let function_prototype = cx
        .current_realm()
        .get_intrinsic(Intrinsic::FunctionPrototype);
    let function_object =
        ordinary_function_create(cx, function_prototype, func_node, false, env, private_env);

    set_function_name(cx, function_object.into(), &name_key, None);
    make_constructor(cx, function_object, None, None);

    function_object
}

// 15.2.5 InstantiateOrdinaryFunctionExpression
pub fn instantiate_ordinary_function_expression(
    cx: &mut Context,
    func_node: &ast::Function,
    name: Option<&PropertyKey>,
) -> Gc<Function> {
    let current_context = cx.current_execution_context();
    let function_prototype = cx
        .current_realm()
        .get_intrinsic(Intrinsic::FunctionPrototype);

    match &func_node.id {
        None => {
            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                current_context.lexical_env,
                current_context.private_env,
            );

            match name {
                None => set_function_name(cx, closure.into(), &cx.names.empty_string(), None),
                Some(name) => set_function_name(cx, closure.into(), name, None),
            }

            make_constructor(cx, closure, None, None);

            closure
        }
        Some(id) => {
            let mut func_env = cx
                .heap
                .alloc(DeclarativeEnvironment::new(Some(current_context.lexical_env)));

            let name_value = id_string_value(cx, id);
            must!(func_env.create_immutable_binding(cx, name_value, false));

            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                to_trait_object(func_env),
                current_context.private_env,
            );

            set_function_name(cx, closure.into(), &PropertyKey::string(name_value), None);
            make_constructor(cx, closure, None, None);

            must!(func_env.initialize_binding(cx, name_value, closure.into()));

            closure
        }
    }
}

// 15.3.4 InstantiateArrowFunctionExpression
pub fn instantiate_arrow_function_expression(
    cx: &mut Context,
    func_node: &ast::Function,
    name: Option<&PropertyKey>,
) -> Gc<Function> {
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

    match name {
        None => set_function_name(cx, closure.into(), &cx.names.empty_string(), None),
        Some(name) => set_function_name(cx, closure.into(), name, None),
    }

    closure
}

// 15.4.4 DefineMethod
pub fn define_method(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    func_node: &ast::Function,
    function_prototype: Option<Gc<ObjectValue>>,
) -> Gc<Function> {
    let current_execution_context = cx.current_execution_context();
    let env = current_execution_context.lexical_env;
    let private_env = current_execution_context.private_env;

    let prototype = match function_prototype {
        Some(prototype) => prototype,
        None => current_execution_context
            .realm
            .get_intrinsic(Intrinsic::FunctionPrototype),
    };

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    closure
}

// 15.4.5 MethodDefinitionEvaluation is split into normal and private copies
pub fn method_definition_evaluation(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    func_node: &ast::Function,
    property_key: &PropertyKey,
    property_kind: &ast::PropertyKind,
    is_enumerable: bool,
) -> EvalResult<()> {
    if func_node.is_async || func_node.is_generator {
        unimplemented!("async and generator functions")
    }

    // Handle regular method definitions
    if let ast::PropertyKind::Init = property_kind {
        let closure = define_method(cx, object, func_node, None);
        set_function_name(cx, closure.into(), property_key, None);
        define_method_property(cx, object, property_key, closure, is_enumerable);

        return ().into();
    }

    // Otherwise is a getter or setter
    let current_execution_context = cx.current_execution_context();
    let env = current_execution_context.lexical_env;
    let private_env = current_execution_context.private_env;

    let prototype = current_execution_context
        .realm
        .get_intrinsic(Intrinsic::FunctionPrototype);

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    match property_kind {
        ast::PropertyKind::Get => {
            set_function_name(cx, closure.into(), property_key, Some("get"));

            // TOOD: Check if property_key is private name
            let desc =
                PropertyDescriptor::accessor(Some(closure.into()), None, is_enumerable, true);
            define_property_or_throw(cx, object, property_key, desc)
        }
        ast::PropertyKind::Set => {
            set_function_name(cx, closure.into(), property_key, Some("set"));

            // TOOD: Check if property_key is private name
            let desc =
                PropertyDescriptor::accessor(None, Some(closure.into()), is_enumerable, true);
            define_property_or_throw(cx, object, property_key, desc)
        }
        _ => unreachable!(),
    }
}

// 15.4.5 MethodDefinitionEvaluation is split into normal and private copies
pub fn private_method_definition_evaluation(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    func_node: &ast::Function,
    property_name: &PropertyKey,
    method_kind: ast::ClassMethodKind,
) -> PrivateProperty {
    if func_node.is_async || func_node.is_generator {
        unimplemented!("async and generator functions")
    }

    // Handle regular method definitions
    if method_kind == ast::ClassMethodKind::Method {
        let closure = define_method(cx, object, func_node, None);
        set_function_name(cx, closure.into(), property_name, None);

        return PrivateProperty::method(closure.into());
    }

    // Otherwise is a getter or setter
    let current_execution_context = cx.current_execution_context();
    let env = current_execution_context.lexical_env;
    let private_env = current_execution_context.private_env;

    let prototype = current_execution_context
        .realm
        .get_intrinsic(Intrinsic::FunctionPrototype);

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    match method_kind {
        ast::ClassMethodKind::Get => {
            set_function_name(cx, closure.into(), property_name, Some("get"));
            PrivateProperty::getter(cx, closure.into())
        }
        ast::ClassMethodKind::Set => {
            set_function_name(cx, closure.into(), property_name, Some("set"));
            PrivateProperty::setter(cx, closure.into())
        }
        _ => unreachable!(),
    }
}
