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
            bytecode::{function::Closure, generator::BytecodeProgramGenerator},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{DynEnvironment, Environment},
                private_environment::PrivateEnvironment,
            },
            error::{syntax_error_, type_error_},
            execution_context::{get_this_environment, ExecutionContext},
            function::{instantiate_function_object, ConstructorKind},
            interned_strings::InternedStrings,
            scope::Scope,
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
    if is_direct {
        // TODO: Check if inside a function, method, derived constructor, and class field initializer
    }

    // TODO: Gather private names from surrounding context

    // Parse source code
    let source = Rc::new(Source::new_from_wtf8_string("<eval>", code.to_wtf8_string()));
    let parse_result = parse_script_for_eval(&source, is_strict_caller);
    let mut parse_result = match parse_result {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_error_(cx, &error.to_string()),
    };

    // Analyze source code
    let analyze_result = analyze_for_eval(
        &mut parse_result,
        source,
        /* private_names */ None,
        /* in_function */ false,
        /* in_method */ false,
        /* in_derived_constructor */ false,
        /* in_class_field_initializer */ false,
    );
    if let Err(errors) = analyze_result {
        // TODO: Return an aggregate error with all syntax errors
        // Choose an arbitrary syntax error to return
        let error = &errors.errors[0];
        return syntax_error_(cx, &error.to_string());
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
        let bytecode_string = bytecode_function.debug_print_recursive(false);

        if let Some(mut dump_buffer) = cx.options.dump_buffer() {
            dump_buffer.push_str(&bytecode_string);
            dump_buffer.push_str("\n");
        } else {
            println!("{}", bytecode_function.debug_print_recursive(false));
        }
    }

    // Eval function's parent scope is the global scope in an indirect eval
    let eval_scope =
        direct_scope.unwrap_or_else(|| cx.current_function().cast::<Closure>().global_scope());
    let closure = Closure::new(cx, bytecode_function, eval_scope);

    // Execute in the bytecode VM
    let eval_result = cx.execute_bytecode(closure, &[]);

    match eval_result {
        Ok(value) => EvalResult::Ok(value),
        Err(error) => EvalResult::Throw(error),
    }
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
    let parse_result = parse_script_for_eval(&source, is_strict_caller);
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

    let result = eval_declaration_instantiation(
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
fn eval_declaration_instantiation(
    cx: Context,
    ast: &ast::Program,
    mut var_env: DynEnvironment,
    mut lex_env: DynEnvironment,
    private_env: Option<Handle<PrivateEnvironment>>,
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
