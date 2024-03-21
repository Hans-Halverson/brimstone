use std::{collections::HashSet, rc::Rc};

use crate::{
    js::{
        common::wtf_8::Wtf8String,
        parser::{
            analyze::analyze_function_for_function_constructor,
            ast,
            parser::{
                parse_function_body_for_function_constructor,
                parse_function_for_function_constructor,
                parse_function_params_for_function_constructor,
            },
            scope_tree::{Binding, BindingKind},
            source::Source,
        },
        runtime::{
            abstract_operations::define_property_or_throw,
            arguments_object::{create_mapped_arguments_object, create_unmapped_arguments_object},
            array_object::array_create,
            bytecode::{
                function::{dump_bytecode_function, Closure},
                generator::BytecodeProgramGenerator,
            },
            completion::{Completion, EvalResult},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{DynEnvironment, Environment},
                private_environment::PrivateEnvironment,
            },
            error::syntax_error_,
            eval::pattern::id_property_key,
            execution_context::resolve_binding,
            function::{
                define_method_property, instantiate_function_object, make_constructor, make_method,
                ordinary_function_create, set_function_name, Function,
            },
            interned_strings::InternedStrings,
            intrinsics::intrinsics::Intrinsic,
            object_value::ObjectValue,
            ordinary_object::get_prototype_from_constructor,
            property::Property,
            property_descriptor::PropertyDescriptor,
            property_key::PropertyKey,
            to_string, Context, Handle, Value,
        },
    },
    maybe, maybe__, must,
};

use super::{
    expression::eval_expression,
    pattern::{binding_initialization, id_string_value, maybe_extract_initializer},
    statement::eval_named_anonymous_function_or_expression,
};

// 10.2.11 FunctionDeclarationInstantiation
pub fn function_declaration_instantiation(
    cx: Context,
    func: Handle<Function>,
    arguments: &[Handle<Value>],
) -> Completion {
    let func_node = if let Some(func_node) = func.func_ast_node() {
        func_node
    } else {
        unreachable!()
    };
    let func_node = func_node.as_ref();

    let mut function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    iter_var_decls(func_node, |(name, binding)| {
        if let BindingKind::Function { func_node, .. } = binding.kind() {
            if function_names.insert(name) {
                functions_to_initialize.push(func_node.as_ref());
            }
        }
    });

    let is_strict = func_node.is_strict_mode();

    let mut callee_context = cx.current_execution_context();

    // A new environment is needed so that direct eval calls in parameter expressions are outside
    // the environment where parameters are declared.
    let mut env = if is_strict || !func_node.has_parameter_expressions() {
        callee_context.lexical_env()
    } else {
        let new_env = DeclarativeEnvironment::new(cx, Some(callee_context.lexical_env()));
        callee_context.set_lexical_env(new_env.into_dyn_env());
        callee_context.lexical_env()
    };

    let mut parameter_names: HashSet<&str> = HashSet::new();

    for param in &func_node.params {
        must!(param.iter_bound_names(&mut |id| {
            parameter_names.insert(&id.name);

            let name_value = id_string_value(cx, id);

            let already_declared = must!(env.has_binding(cx, name_value));
            if !already_declared {
                must!(env.create_mutable_binding(cx, name_value, false));
                if func_node.has_duplicate_parameters() {
                    must!(env.initialize_binding(cx, name_value, cx.undefined()));
                }
            }

            ().into()
        }));
    }

    // Set up arguments object if needed
    if func_node.is_arguments_object_needed() {
        let arguments_object = if func_node.needs_mapped_arguments_object() {
            create_mapped_arguments_object(cx, func, &func_node.params, &arguments, env)
        } else {
            create_unmapped_arguments_object(cx, &arguments)
        };

        let arguments_name_value = InternedStrings::get_str(cx, "arguments");

        if is_strict {
            must!(env.create_immutable_binding(cx, arguments_name_value, false))
        } else {
            must!(env.create_mutable_binding(cx, arguments_name_value, false))
        }

        must!(env.initialize_binding(cx, arguments_name_value, arguments_object));

        parameter_names.insert("arguments");
    }

    let binding_init_env = if func_node.has_duplicate_parameters() {
        None
    } else {
        Some(env)
    };

    // Perform inlined IteratorBindingInitialization instead of creating an actual iterator object
    let mut arg_index = 0;
    for param in &func_node.params {
        let mut value = cx.undefined();

        // For rest parameters create an array holding all the values, then evaluate pattern as
        // normal with this new array as the value.
        let pattern = match param {
            ast::FunctionParam::Pattern { pattern, .. } => {
                if arg_index < arguments.len() {
                    let argument = arguments[arg_index];
                    arg_index += 1;
                    value = argument;
                }

                pattern
            }
            ast::FunctionParam::Rest { rest, .. } => {
                let rest_array = must!(array_create(cx, 0, None));

                // Property key is shared between iterations
                let mut array_key = PropertyKey::uninit().to_handle(cx);

                for (array_index, arg_index) in (arg_index..arguments.len()).enumerate() {
                    array_key.replace(PropertyKey::array_index(cx, array_index as u32));
                    let array_property = Property::data(arguments[arg_index], true, true, true);
                    rest_array
                        .object()
                        .set_property(cx, array_key, array_property);
                }

                value = rest_array.into();

                rest.argument.as_ref()
            }
        };

        let (patt, init) = maybe_extract_initializer(pattern);

        match patt {
            ast::Pattern::Id(id) => {
                let name_value = id_string_value(cx, id);
                let name_key = PropertyKey::string(cx, name_value).to_handle(cx);
                let mut reference = maybe__!(resolve_binding(cx, name_value, binding_init_env));

                if let Some(init) = init {
                    if value.is_undefined() {
                        value = maybe__!(eval_named_anonymous_function_or_expression(
                            cx, init, name_key
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
    let mut var_env = if !func_node.has_parameter_expressions() {
        let mut instantiated_var_names = parameter_names;

        iter_var_decls(func_node, |(name, _)| {
            if instantiated_var_names.insert(name) {
                let name_value = InternedStrings::get_str(cx, name);
                must!(env.create_mutable_binding(cx, name_value, false));
                must!(env.initialize_binding(cx, name_value, cx.undefined()));
            }
        });

        env
    } else {
        // A separate Environment Record is needed to ensure that closures created by expressions in
        // the formal parameter list do not have visibility of declarations in the function body.

        // Be sure to allocate before calling setter
        let decl_env = DeclarativeEnvironment::new(cx, Some(env));
        callee_context.set_variable_env(decl_env.into_dyn_env());
        let mut var_env = callee_context.variable_env();

        let mut instantiated_var_names = HashSet::new();

        iter_var_decls(func_node, |(name, _)| {
            if instantiated_var_names.insert(name) {
                let name_value = InternedStrings::get_str(cx, name);

                must!(var_env.create_mutable_binding(cx, name_value, false));

                let initial_value =
                    if !parameter_names.contains(name.as_str()) || function_names.contains(name) {
                        cx.undefined()
                    } else {
                        must!(env.get_binding_value(cx, name_value, false))
                    };

                must!(var_env.initialize_binding(cx, name_value, initial_value));
            }
        });

        var_env
    };

    let mut lex_env = if !is_strict {
        // Non-strict functions use a separate Environment Record for top-level lexical declarations
        // so that a direct eval can determine whether any var scoped declarations introduced by the
        // eval code conflict with pre-existing top-level lexically scoped declarations.
        DeclarativeEnvironment::new(cx, Some(var_env)).into_dyn_env()
    } else {
        var_env
    };

    callee_context.set_lexical_env(lex_env);

    // Create bindings for lex decls in function body
    iter_lex_decls(func_node, |(name, binding)| {
        let name_value = InternedStrings::get_str(cx, name);
        if binding.is_const() {
            must!(lex_env.create_immutable_binding(cx, name_value, true));
        } else {
            must!(lex_env.create_mutable_binding(cx, name_value, false))
        }
    });

    // Initialize toplevel function objects
    let private_env = callee_context.private_env();
    for func in functions_to_initialize {
        let func_name = id_string_value(cx, func.id.as_deref().unwrap());
        let func_object = instantiate_function_object(cx, func, lex_env, private_env);
        must!(var_env.set_mutable_binding(cx, func_name, func_object.into(), false));
    }

    Completion::empty(cx)
}

/// Call a function on all var decls in a function, including in the function body if it has a
/// separate scope.
fn iter_var_decls<'a>(
    func_node: &'a ast::Function,
    mut f: impl FnMut((&'a String, &'a Binding)) -> (),
) {
    for item in func_node.scope.as_ref().iter_var_decls() {
        f(item);
    }

    match func_node.body.as_ref() {
        ast::FunctionBody::Block(ast::FunctionBlockBody { scope: Some(scope), .. }) => {
            for item in scope.as_ref().iter_var_decls() {
                f(item);
            }
        }
        _ => {}
    }
}

/// Call a function on all lex decls in a function, including in the function body if it has a
/// separate scope.
fn iter_lex_decls(func_node: &ast::Function, mut f: impl FnMut((&String, &Binding)) -> ()) {
    for item in func_node.scope.as_ref().iter_lex_decls() {
        f(item);
    }

    match func_node.body.as_ref() {
        ast::FunctionBody::Block(ast::FunctionBlockBody { scope: Some(scope), .. }) => {
            for item in scope.as_ref().iter_lex_decls() {
                f(item);
            }
        }
        _ => {}
    }
}

// 15.2.4 InstantiateOrdinaryFunctionObject
pub fn instantiate_ordinary_function_object(
    cx: Context,
    func_node: &ast::Function,
    env: DynEnvironment,
    private_env: Option<Handle<PrivateEnvironment>>,
) -> Handle<Function> {
    let name_key = match &func_node.id {
        None => cx.names.default(),
        Some(id) => id_property_key(cx, id),
    };

    let function_prototype = cx.get_intrinsic(Intrinsic::FunctionPrototype);
    let function_object =
        ordinary_function_create(cx, function_prototype, func_node, false, env, private_env);

    set_function_name(cx, function_object.into(), name_key, None);
    make_constructor(cx, function_object, None, None);

    function_object
}

// 15.2.5 InstantiateOrdinaryFunctionExpression
pub fn instantiate_ordinary_function_expression(
    cx: Context,
    func_node: &ast::Function,
    name: Option<Handle<PropertyKey>>,
) -> Handle<Function> {
    if func_node.is_async() || func_node.is_generator() {
        unimplemented!("async and generator functions")
    }

    let current_context = cx.current_execution_context();
    let function_prototype = cx.get_intrinsic(Intrinsic::FunctionPrototype);

    match &func_node.id {
        None => {
            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                current_context.lexical_env(),
                current_context.private_env(),
            );

            match name {
                None => set_function_name(cx, closure.into(), cx.names.empty_string(), None),
                Some(name) => set_function_name(cx, closure.into(), name, None),
            }

            make_constructor(cx, closure, None, None);

            closure
        }
        Some(id) => {
            let mut func_env = DeclarativeEnvironment::new(cx, Some(current_context.lexical_env()));

            let name_value = id_string_value(cx, id);
            let name_key = PropertyKey::string(cx, name_value).to_handle(cx);
            must!(func_env.create_immutable_binding(cx, name_value, false));

            let closure = ordinary_function_create(
                cx,
                function_prototype,
                func_node,
                false,
                func_env.into_dyn_env(),
                current_context.private_env(),
            );

            set_function_name(cx, closure.into(), name_key, None);
            make_constructor(cx, closure, None, None);

            must!(func_env.initialize_binding(cx, name_value, closure.into()));

            closure
        }
    }
}

// 15.3.4 InstantiateArrowFunctionExpression
pub fn instantiate_arrow_function_expression(
    cx: Context,
    func_node: &ast::Function,
    name: Option<Handle<PropertyKey>>,
) -> Handle<Function> {
    let current_context_ptr = cx.current_execution_context_ptr();
    let lexical_env = current_context_ptr.lexical_env();
    let private_env = current_context_ptr.private_env();

    let function_prototype = cx.get_intrinsic(Intrinsic::FunctionPrototype);

    let closure =
        ordinary_function_create(cx, function_prototype, func_node, true, lexical_env, private_env);

    match name {
        None => set_function_name(cx, closure.into(), cx.names.empty_string(), None),
        Some(name) => set_function_name(cx, closure.into(), name, None),
    }

    closure
}

// 15.4.4 DefineMethod
pub fn define_method(
    cx: Context,
    object: Handle<ObjectValue>,
    func_node: &ast::Function,
    function_prototype: Option<Handle<ObjectValue>>,
) -> Handle<Function> {
    let current_execution_context_ptr = cx.current_execution_context_ptr();
    let env = current_execution_context_ptr.lexical_env();
    let private_env = current_execution_context_ptr.private_env();

    let prototype = match function_prototype {
        Some(prototype) => prototype,
        None => current_execution_context_ptr.get_intrinsic(Intrinsic::FunctionPrototype),
    };

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    closure
}

// 15.4.5 MethodDefinitionEvaluation is split into normal and private copies
pub fn method_definition_evaluation(
    cx: Context,
    object: Handle<ObjectValue>,
    func_node: &ast::Function,
    property_key: Handle<PropertyKey>,
    property_kind: &ast::PropertyKind,
    is_enumerable: bool,
) -> EvalResult<()> {
    if func_node.is_async() || func_node.is_generator() {
        unimplemented!("async and generator functions")
    }

    // Handle regular method definitions
    if let ast::PropertyKind::Init = property_kind {
        let closure = define_method(cx, object, func_node, None);
        set_function_name(cx, closure.into(), property_key, None);
        return define_method_property(cx, object, property_key, closure, is_enumerable);
    }

    // Otherwise is a getter or setter
    let current_execution_context_ptr = cx.current_execution_context_ptr();
    let env = current_execution_context_ptr.lexical_env();
    let private_env = current_execution_context_ptr.private_env();
    let prototype = current_execution_context_ptr.get_intrinsic(Intrinsic::FunctionPrototype);

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    match property_kind {
        ast::PropertyKind::Get => {
            set_function_name(cx, closure.into(), property_key, Some("get"));

            let desc = PropertyDescriptor::get_only(Some(closure.into()), is_enumerable, true);
            define_property_or_throw(cx, object, property_key, desc)
        }
        ast::PropertyKind::Set => {
            set_function_name(cx, closure.into(), property_key, Some("set"));

            let desc = PropertyDescriptor::set_only(Some(closure.into()), is_enumerable, true);
            define_property_or_throw(cx, object, property_key, desc)
        }
        _ => unreachable!(),
    }
}

// 15.4.5 MethodDefinitionEvaluation is split into normal and private copies
pub fn private_method_definition_evaluation(
    cx: Context,
    object: Handle<ObjectValue>,
    func_node: &ast::Function,
    property_name: Handle<PropertyKey>,
    method_kind: ast::ClassMethodKind,
) -> Property {
    if func_node.is_async() || func_node.is_generator() {
        unimplemented!("async and generator functions")
    }

    // Handle regular method definitions
    if method_kind == ast::ClassMethodKind::Method {
        let closure = define_method(cx, object, func_node, None);
        set_function_name(cx, closure.into(), property_name, None);

        return Property::private_method(closure.into());
    }

    // Otherwise is a getter or setter
    let current_execution_context_ptr = cx.current_execution_context_ptr();
    let env = current_execution_context_ptr.lexical_env();
    let private_env = current_execution_context_ptr.private_env();
    let prototype = current_execution_context_ptr.get_intrinsic(Intrinsic::FunctionPrototype);

    let closure = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    make_method(closure, object);

    match method_kind {
        ast::ClassMethodKind::Get => {
            set_function_name(cx, closure.into(), property_name, Some("get"));
            Property::private_getter(cx, closure.into())
        }
        ast::ClassMethodKind::Set => {
            set_function_name(cx, closure.into(), property_name, Some("set"));
            Property::private_setter(cx, closure.into())
        }
        _ => unreachable!(),
    }
}

// 20.2.1.1.1 CreateDynamicFunction
pub fn create_dynamic_function(
    mut cx: Context,
    constructor: Handle<ObjectValue>,
    new_target: Option<Handle<ObjectValue>>,
    args: &[Handle<Value>],
) -> EvalResult<Handle<ObjectValue>> {
    let new_target = new_target.unwrap_or(constructor);

    let prefix = "function";
    let fallback_proto = Intrinsic::FunctionPrototype;
    let is_async = false;
    let is_generator = false;

    let arg_count = args.len();

    // Construct text for params, body, and entire function
    let mut params_string = Wtf8String::new();

    let body_arg = if arg_count == 0 {
        cx.names.empty_string().as_string().into()
    } else if arg_count == 1 {
        args[0]
    } else {
        params_string.push_wtf8_str(&maybe!(to_string(cx, args[0])).to_wtf8_string());

        for arg in &args[1..(args.len() - 1)] {
            params_string.push_char(',');
            params_string.push_wtf8_str(&maybe!(to_string(cx, *arg)).to_wtf8_string());
        }

        args[args.len() - 1]
    };

    // Build the body string
    let body_string = maybe!(to_string(cx, body_arg));
    let body_string = {
        let mut builder = Wtf8String::new();

        builder.push_char('\n');
        builder.push_wtf8_str(&body_string.to_wtf8_string());
        builder.push_char('\n');

        builder
    };

    // Build the full source string
    let source_string = {
        let mut builder = Wtf8String::new();

        builder.push_str(prefix);
        builder.push_str(" anonymous(");
        builder.push_wtf8_str(&params_string);
        builder.push_str("\n) {");
        builder.push_wtf8_str(&body_string);
        builder.push_str("}");

        builder
    };

    // Make sure that parameter list and body are valid by themselves. Only need to check that they
    // parse correctly, full analysis will be performed on entire function text.
    let params_source = Source::new_from_wtf8_string("", params_string);
    if let Err(err) = parse_function_params_for_function_constructor(
        &Rc::new(params_source),
        is_async,
        is_generator,
    ) {
        return syntax_error_(cx, &format!("could not parse function parameters: {}", err));
    }

    let body_source = Source::new_from_wtf8_string("", body_string);
    if let Err(err) =
        parse_function_body_for_function_constructor(&Rc::new(body_source), is_async, is_generator)
    {
        return syntax_error_(cx, &format!("could not parse function body: {}", err));
    }

    // Parse and analyze entire function
    let full_source = Rc::new(Source::new_from_wtf8_string("", source_string));
    let mut parse_result = match parse_function_for_function_constructor(&full_source) {
        Ok(parse_result) => parse_result,
        Err(err) => return syntax_error_(cx, &format!("could not parse function: {}", err)),
    };

    if let Err(errs) =
        analyze_function_for_function_constructor(&mut parse_result, full_source.clone())
    {
        return syntax_error_(cx, &format!("could not parse function: {}", errs));
    }

    // Create function object
    let proto = maybe!(get_prototype_from_constructor(cx, new_target, fallback_proto));

    if cx.options.bytecode {
        // Generate bytecode for the function
        let realm = cx.current_realm();
        let generate_result =
            BytecodeProgramGenerator::generate_from_function_constructor_parse_result(
                cx,
                &parse_result,
                realm,
            );
        let bytecode_function = match generate_result {
            Ok(func) => func,
            Err(error) => return syntax_error_(cx, &error.to_string()),
        };

        if cx.options.print_bytecode {
            dump_bytecode_function(cx, bytecode_function.get_());
        }

        // Dynamic functions are always in the global scope
        let global_scope = realm.default_global_scope();
        let closure = Closure::new(cx, bytecode_function, global_scope);

        let closure_object: Handle<ObjectValue> = closure.into();
        closure_object.into()
    } else {
        let env = cx.current_realm_ptr().global_env();

        let mut func = ordinary_function_create(
            cx,
            proto,
            &parse_result.function,
            false,
            env.into_dyn_env(),
            None,
        );
        set_function_name(cx, func.into(), cx.names.anonymous(), None);
        func.set_source(&full_source);

        if !is_async && !is_generator {
            make_constructor(cx, func, None, None);
        }

        // TODO: Need better way to save ASTs and sources, following same pattern as eval for now
        cx.function_constructor_asts
            .push((parse_result, full_source));

        let func_object: Handle<ObjectValue> = func.into();
        func_object.into()
    }
}
