use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    js::{
        parser::{
            analyze::{analyze_for_eval, PrivateNameUsage},
            ast, parse_script_for_eval,
            scope_tree::BindingKind,
            source::Source,
        },
        runtime::{
            bytecode::{
                function::{dump_bytecode_function, Closure},
                generator::BytecodeProgramGenerator,
            },
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{DynEnvironment, Environment},
                private_environment::LegacyPrivateEnvironment,
            },
            error::{syntax_error_, type_error_},
            execution_context::{get_this_environment, ExecutionContext},
            function::{instantiate_function_object, ConstructorKind},
            global_names::{
                can_declare_global_function, can_declare_global_var,
                create_global_function_binding, create_global_var_binding,
            },
            interned_strings::InternedStrings,
            property::Property,
            scope::{Scope, ScopeKind},
            string_value::FlatString,
            Completion, CompletionKind, Context, EvalResult, Handle, Value,
        },
    },
    maybe, must,
};

use super::{pattern::id_string_value, statement::eval_toplevel_list};

pub fn perform_bytecode_eval(
    mut cx: Context,
    code: Handle<Value>,
    is_strict_caller: bool,
    direct_scope: Option<Handle<Scope>>,
) -> EvalResult<Handle<Value>> {
    if !code.is_string() {
        return code.into();
    }
    let code = code.as_string();

    let is_direct = direct_scope.is_some();
    let mut in_function = false;

    // Walk scope chain, determining context that eval was called in
    if is_direct {
        let mut scope = cx.vm().scope();
        loop {
            // Check if we are inside a function, meaning `new.target` can be used in the eval
            if scope.scope_names_ptr().is_non_arrow_function_scope() {
                in_function = true;
            }

            // TODO: Check if inside a method, derived constructor, and class field initializer

            if let Some(parent_scope) = scope.parent() {
                scope = parent_scope;
            } else {
                break;
            }
        }
    }

    // TODO: Gather private names from surrounding context

    // Parse source code
    let source = Rc::new(Source::new_from_wtf8_string("<eval>", code.to_wtf8_string()));
    let parse_result = parse_script_for_eval(&source, is_direct, is_strict_caller);
    let mut parse_result = match parse_result {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_error_(cx, &error.to_string()),
    };

    // Analyze source code
    let analyze_result = analyze_for_eval(
        &mut parse_result,
        source,
        /* private_names */ None,
        in_function,
        /* in_method */ false,
        /* in_static_method */ false,
        /* in_derived_constructor */ false,
        /* in_class_field_initializer */ false,
    );
    if let Err(errors) = analyze_result {
        // TODO: Return an aggregate error with all syntax errors
        // Choose an arbitrary syntax error to return
        let error = &errors.errors[0];
        return syntax_error_(cx, &error.to_string());
    }

    // Sloppy direct evals must perform EvalDeclarationInstantiation as var scoped bindings will
    // be placed in the parent var scope. Strict direct evals do not need to call EDI as var scoped
    // bindings are handled normally when setting up a call frame in the VM.
    if !parse_result.program.is_strict_mode {
        maybe!(eval_declaration_instantiation(cx, &parse_result.program));
    }

    // Generate bytecode for the program
    let realm = cx.current_realm();
    let generate_result =
        BytecodeProgramGenerator::generate_from_eval_parse_result(cx, &parse_result, realm);
    let bytecode_function = match generate_result {
        Ok(func) => func,
        Err(error) => return syntax_error_(cx, &error.to_string()),
    };

    // Print the bytecode if necessary, optionally to the internal dump buffer
    if cx.options.print_bytecode {
        dump_bytecode_function(cx, bytecode_function.get_());
    }

    // Eval function's parent scope is the global scope in an indirect eval
    let eval_scope = direct_scope.unwrap_or_else(|| cx.current_realm().default_global_scope());
    let closure = Closure::new(cx, bytecode_function, eval_scope);

    // Determine the receiver for the eval function call
    let receiver: Handle<Value> = if is_direct {
        // Direct evals inherit their receiver from the caller
        cx.vm().receiver().to_handle(cx)
    } else {
        // For indirect evals receiver is the global object
        closure.global_object().into()
    };

    // Execute the eval function's bytecode in the VM
    cx.vm().call_from_rust(closure.into(), receiver, &[])
}

// Check if any names conflict with lexical bindings in parent scopes.
fn check_eval_var_name_conflicts(
    mut cx: Context,
    eval_var_names: &[Handle<FlatString>],
    eval_func_names: &[Handle<FlatString>],
) -> EvalResult<()> {
    let mut scope = cx.vm().scope().to_handle();

    // Special case for an eval in the function params scope
    if scope.scope_names_ptr().is_function_parameters_scope() {
        for name in eval_var_names.iter().chain(eval_func_names.iter()) {
            if let Some(index) = scope.scope_names_ptr().lookup_name(name.get_()) {
                if scope.scope_names_ptr().is_function_parameter(index) {
                    return error_name_already_declared(cx, *name);
                }
            }
        }

        return ().into();
    }

    // Otherwise walk scopes up to the parent var scope
    loop {
        // Name will conflict with a lexical binding in any parent scope
        for name in eval_var_names.iter().chain(eval_func_names.iter()) {
            if let Some(index) = scope.scope_names_ptr().lookup_name(name.get_()) {
                if scope.scope_names_ptr().is_lexical(index) {
                    return error_name_already_declared(cx, *name);
                }
            }
        }

        // If in a global scope, check for lexical bindings in other global scopes
        if scope.kind() == ScopeKind::Global {
            let realm = scope.global_scope_realm();
            for name in eval_var_names.iter().chain(eval_func_names.iter()) {
                if realm.get_lexical_name(name.get_()).is_some() {
                    return error_name_already_declared(cx, *name);
                }
            }
        }

        // Do not ascend past enclosing var scope
        if scope.scope_names_ptr().is_var_scope() {
            break;
        }

        // Move to parent scope
        if let Some(parent) = scope.parent().as_ref() {
            scope.replace(*parent);
        } else {
            break;
        }
    }

    ().into()
}

// 19.2.1.3 EvalDeclarationInstantiation
fn eval_declaration_instantiation(mut cx: Context, program: &ast::Program) -> EvalResult<()> {
    // Find the function and var names in the eval scope
    let mut eval_var_names = vec![];
    let mut eval_func_names = vec![];
    for (name, binding) in program.scope.as_ref().iter_var_decls() {
        let name = InternedStrings::get_str(cx, &name).as_flat();
        match binding.kind() {
            BindingKind::Var => eval_var_names.push(name),
            BindingKind::Function { .. } => eval_func_names.push(name),
            _ => unreachable!(),
        }
    }

    // Check if any var scoped names in eval body conflict with lexical bindings in parent scopes.
    let eval_scope = program.scope.as_ref();
    let eval_var_names = eval_scope
        .iter_var_decls()
        .map(|(name, _)| InternedStrings::get_str(cx, &name).as_flat())
        .collect::<Vec<_>>();

    maybe!(check_eval_var_name_conflicts(cx, &eval_var_names, &eval_func_names));

    // Find the enclosing var scope
    let mut var_scope = cx.vm().scope().to_handle();
    while !var_scope.scope_names_ptr().is_var_scope() {
        var_scope.replace(var_scope.parent().unwrap());
    }

    let is_global_scope = var_scope.kind() == ScopeKind::Global;
    let mut scope_object = var_scope.ensure_scope_object(cx);

    // If in global scope, check if we can declare the var or function binding
    if is_global_scope {
        for func_name in &eval_func_names {
            if !maybe!(can_declare_global_function(cx, scope_object, func_name.cast())) {
                return type_error_(
                    cx,
                    &format!("cannot declare global function {}", func_name.get_()),
                );
            }
        }

        for var_name in &eval_var_names {
            if !maybe!(can_declare_global_var(cx, scope_object, var_name.cast())) {
                return type_error_(cx, &format!("cannot declare global var {}", var_name.get_()));
            }
        }
    }

    // Create and initialize bindings for all functions
    for name in &eval_func_names {
        if is_global_scope {
            maybe!(create_global_function_binding(
                cx,
                scope_object,
                name.cast(),
                /* can_delete */ true
            ));
        } else {
            // All functions initialized to undefined, overwriting existing if already declared
            let desc = Property::data(cx.undefined(), true, true, true);
            scope_object.set_property(cx, name.cast(), desc);
        }
    }

    // Create and initialize bindings for all vars
    for name in &eval_var_names {
        if is_global_scope {
            maybe!(create_global_var_binding(
                cx,
                scope_object,
                name.cast(),
                /* can_delete */ true
            ));
        } else {
            // Vars only initialized to undefined if the do not have been declared yet
            if !maybe!(scope_object.has_property(cx, name.cast())) {
                let desc = Property::data(cx.undefined(), true, true, true);
                scope_object.set_property(cx, name.cast(), desc);
            }
        }
    }

    ().into()
}

fn error_name_already_declared(cx: Context, name: Handle<FlatString>) -> EvalResult<()> {
    syntax_error_(cx, &format!("identifier '{}' has already been declared", name.get_()))
}

pub fn perform_ast_eval(
    mut cx: Context,
    code: Handle<Value>,
    is_strict_caller: bool,
    is_direct: bool,
) -> EvalResult<Handle<Value>> {
    if !code.is_string() {
        return code.into();
    }
    let code = code.as_string();

    let running_context = cx.current_execution_context();
    let eval_realm = running_context.realm();

    let mut in_function = false;
    let mut in_method = false;
    let mut in_derived_constructor = false;
    let mut in_class_field_initializer = false;

    if is_direct {
        let mut this_env = get_this_environment(cx);
        if let Some(func_env) = this_env.as_function_environment() {
            in_function = true;
            in_method = func_env.has_super_binding();

            let func = func_env.function_object();
            in_derived_constructor = func.constructor_kind() == ConstructorKind::Derived;

            if func.is_class_property() {
                in_class_field_initializer = true;
            }
        }
    }

    // Deviation from spec: Gather private names from surrounding context right now and emit
    // missing error during analysis, instead of waiting until EvalDeclarationInstantiation.
    let private_names = if is_direct {
        let mut names = HashMap::new();

        // Walk private contexts, gathering all defined private names
        let mut current_private_env_ptr = running_context.private_env_ptr();
        while let Some(private_env_ptr) = current_private_env_ptr {
            private_env_ptr.iter_names_gc_unsafe(|name| {
                let name_string = name.as_string().to_string();
                names.insert(name_string, PrivateNameUsage::used());
            });

            current_private_env_ptr = private_env_ptr.outer_ptr();
        }

        Some(names)
    } else {
        None
    };

    // Parse source code
    let source = Rc::new(Source::new_from_wtf8_string("<eval>", code.to_wtf8_string()));
    let parse_result = parse_script_for_eval(&source, is_direct, is_strict_caller);
    let mut parse_result = match parse_result {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_error_(cx, &error.to_string()),
    };

    // Analyze source code
    let analyze_result = analyze_for_eval(
        &mut parse_result,
        source,
        private_names,
        in_function,
        in_method,
        // Always false since only used by bytecode VM
        /* in_static_method */ false,
        in_derived_constructor,
        in_class_field_initializer,
    );
    if let Err(errors) = analyze_result {
        // TODO: Return an aggregate error with all syntax errors
        // Choose an arbitrary syntax error to return
        let error = &errors.errors[0];
        return syntax_error_(cx, &error.to_string());
    }

    let is_strict_eval = is_strict_caller || parse_result.program.is_strict_mode;

    let (lex_env, var_env, private_env) = if is_direct {
        let lex_env = DeclarativeEnvironment::new(cx, Some(running_context.lexical_env()));
        (
            lex_env.into_dyn_env(),
            running_context.variable_env(),
            running_context.private_env(),
        )
    } else {
        let global_env = eval_realm.global_env().into_dyn_env();
        let lex_env = DeclarativeEnvironment::new(cx, Some(global_env));
        (lex_env.into_dyn_env(), global_env, None)
    };

    let var_env = if is_strict_eval { lex_env } else { var_env };

    let eval_context = ExecutionContext::new(
        cx,
        /* function */ None,
        eval_realm,
        running_context.script_or_module(),
        Some(lex_env),
        Some(var_env),
        private_env,
        is_strict_eval,
    );

    cx.push_execution_context(eval_context);

    let result = eval_declaration_instantiation_legacy(
        cx,
        &parse_result.program,
        var_env,
        lex_env,
        private_env,
        is_strict_eval,
    );

    let mut result = if let EvalResult::Throw(thrown_value) = result {
        Completion::throw(thrown_value)
    } else {
        eval_toplevel_list(cx, &parse_result.program.toplevels)
    };

    if result.is_normal() && result.is_empty() {
        result = Completion::normal(cx.undefined());
    }

    cx.pop_execution_context();

    // TODO: Need better way to save ASTs generated from eval instead of freeing them, as they may
    // be needed later e.g. for functions returned from eval. Currently all eval ASTs are saved in
    // the context and effectively leaked since they may not actually be needed.
    cx.eval_asts.push(parse_result);

    match result.kind() {
        CompletionKind::Normal => EvalResult::Ok(result.value()),
        CompletionKind::Throw => EvalResult::Throw(result.value()),
        CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
            panic!("unexpected abnormal completion at top level of eval")
        }
    }
}

// 19.2.1.3 EvalDeclarationInstantiation
fn eval_declaration_instantiation_legacy(
    cx: Context,
    ast: &ast::Program,
    mut var_env: DynEnvironment,
    mut lex_env: DynEnvironment,
    private_env: Option<Handle<LegacyPrivateEnvironment>>,
    is_strict_eval: bool,
) -> EvalResult<()> {
    if !is_strict_eval {
        if let Some(var_env) = var_env.as_global_environment() {
            for (name, _) in ast.scope.as_ref().iter_var_decls() {
                let name_value = InternedStrings::get_str(cx, name);
                if maybe!(var_env.has_lexical_declaration(cx, name_value)) {
                    return syntax_error_(
                        cx,
                        &format!("identifier '{}' has already been declared", name_value),
                    );
                }
            }
        }

        let mut this_env = lex_env;
        while !this_env.ptr_eq(&var_env) {
            if this_env.as_object_environment().is_none() {
                for (name, _) in ast.scope.as_ref().iter_var_decls() {
                    let name_value = InternedStrings::get_str(cx, name);
                    if must!(this_env.has_binding(cx, name_value)) {
                        return syntax_error_(
                            cx,
                            &format!("identifier '{}' has already been declared", name_value),
                        );
                    }
                }
            }

            this_env = this_env.outer().unwrap();
        }
    }

    let mut declared_function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for (name, binding) in ast.scope.as_ref().iter_var_decls() {
        if let BindingKind::Function { func_node, .. } = binding.kind() {
            if declared_function_names.insert(name) {
                if let Some(var_env) = var_env.as_global_environment() {
                    let name_value = InternedStrings::get_str(cx, name);
                    if !maybe!(var_env.can_declare_global_function(cx, name_value)) {
                        return type_error_(
                            cx,
                            &format!("cannot declare global function {}", name_value),
                        );
                    }
                }

                functions_to_initialize.push(func_node.as_ref());
            }
        }
    }

    // Order does not matter for declared var names, despite ordering in spec
    let mut declared_var_names: HashSet<Handle<FlatString>> = HashSet::new();

    for (name, binding) in ast.scope.as_ref().iter_var_decls() {
        if let BindingKind::Var = binding.kind() {
            if !declared_function_names.contains(name) {
                let name_value = InternedStrings::get_str(cx, name);

                if let Some(var_env) = var_env.as_global_environment() {
                    if !maybe!(var_env.can_declare_global_var(cx, name_value)) {
                        return type_error_(
                            cx,
                            &format!("cannot declare global var {}", name_value),
                        );
                    }
                }

                declared_var_names.insert(name_value.flatten());
            }
        }
    }

    for (name, binding) in ast.scope.as_ref().iter_lex_decls() {
        let name_value = InternedStrings::get_str(cx, name);
        if binding.is_const() {
            maybe!(lex_env.create_immutable_binding(cx, name_value, true));
        } else {
            maybe!(lex_env.create_mutable_binding(cx, name_value, false));
        }
    }

    for func in functions_to_initialize.iter().rev() {
        let name_value = id_string_value(cx, func.id.as_deref().unwrap());
        let function_object = instantiate_function_object(cx, func, lex_env, private_env);

        if let Some(mut var_env) = var_env.as_global_environment() {
            maybe!(var_env.create_global_function_binding(
                cx,
                name_value,
                function_object.into(),
                true
            ));
        } else {
            if must!(var_env.has_binding(cx, name_value)) {
                must!(var_env.set_mutable_binding(cx, name_value, function_object.into(), false));
            } else {
                must!(var_env.create_mutable_binding(cx, name_value, true));
                must!(var_env.initialize_binding(cx, name_value, function_object.into()));
            }
        }
    }

    for name in declared_var_names {
        let name = name.as_string();
        if let Some(mut var_env) = var_env.as_global_environment() {
            maybe!(var_env.create_global_var_binding(cx, name, true));
        } else {
            if !must!(var_env.has_binding(cx, name)) {
                must!(var_env.create_mutable_binding(cx, name, true));
                must!(var_env.initialize_binding(cx, name, cx.undefined()));
            }
        }
    }

    ().into()
}
