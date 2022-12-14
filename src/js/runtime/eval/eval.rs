use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    js::{
        parser::{
            analyze::{analyze_for_eval, PrivateNameUsage},
            ast::{self, LexDecl, VarDecl, WithDecls},
            parse_script,
            source::Source,
        },
        runtime::{
            environment::{
                declarative_environment::DeclarativeEnvironment, environment::to_trait_object,
                environment::Environment, private_environment::PrivateEnvironment,
            },
            error::{syntax_error_, type_error_},
            execution_context::{get_this_environment, ExecutionContext},
            function::{instantiate_function_object, ConstructorKind},
            Completion, CompletionKind, Context, EvalResult, Gc, Value,
        },
    },
    maybe, must,
};

use super::statement::eval_toplevel_list;

pub fn perform_eval(
    cx: &mut Context,
    code: Value,
    is_strict_caller: bool,
    is_direct: bool,
) -> EvalResult<Value> {
    if !code.is_string() {
        return code.into();
    }
    let code = code.as_string();

    let running_context = cx.current_execution_context();
    let eval_realm = running_context.realm;

    let mut in_function = false;
    let mut in_method = false;
    let mut in_derived_constructor = false;
    let mut in_class_field_initializer = false;

    if is_direct {
        let mut this_env = get_this_environment(cx);
        if let Some(func_env) = this_env.as_function_environment() {
            in_function = true;
            in_method = func_env.has_super_binding();

            let func = func_env.function_object;
            in_derived_constructor = func.constructor_kind == ConstructorKind::Derived;
            // TODO: Check class initializer field name
        }
    }

    // Deviation from spec: Gather private names from surrounding context right now and emit
    // missing error during analysis, instead of waiting until EvalDeclarationInstantiation.
    let private_names = if is_direct {
        let mut names = HashMap::new();

        // Walk private contexts, gathering all defined private names
        let mut current_private_env = running_context.private_env;
        while let Some(private_env) = current_private_env {
            for name in &private_env.names {
                names.insert(name.0.clone(), PrivateNameUsage::used());
            }

            current_private_env = private_env.outer;
        }

        Some(names)
    } else {
        None
    };

    // Parse source code
    let source = Rc::new(Source::new_from_string("<eval>", String::from(code.str())));
    let parse_result = parse_script(&source);
    let mut ast = match parse_result {
        Ok(ast) => ast,
        Err(error) => return syntax_error_(cx, &error.to_string()),
    };

    // Analyze source code
    let analyze_result = analyze_for_eval(&mut ast, source, private_names);
    if let Err(errors) = analyze_result {
        // TODO: Return an aggregate error with all syntax errors
        // Choose an arbitrary syntax error to return
        let error = &errors.errors[0];
        return syntax_error_(cx, &error.to_string());
    }

    // TODO: Check for NewTarget, SuperProperty, SuperCall, and ContainsArguments

    let is_strict_eval = is_strict_caller || ast.has_use_strict_directive;

    let (lex_env, var_env, private_env) = if is_direct {
        let lex_env = DeclarativeEnvironment::new(Some(running_context.lexical_env));
        (
            to_trait_object(cx.heap.alloc(lex_env)),
            running_context.variable_env,
            running_context.private_env,
        )
    } else {
        let global_env = to_trait_object(eval_realm.global_env);
        let lex_env = DeclarativeEnvironment::new(Some(global_env));
        (to_trait_object(cx.heap.alloc(lex_env)), global_env, None)
    };

    let var_env = if is_strict_eval { lex_env } else { var_env };

    let eval_context = cx.heap.alloc(ExecutionContext {
        function: None,
        realm: eval_realm,
        script_or_module: running_context.script_or_module,
        lexical_env: lex_env,
        variable_env: var_env,
        private_env,
        is_strict_mode: is_strict_eval,
    });

    cx.push_execution_context(eval_context);

    let result =
        eval_declaration_instantiation(cx, &ast, var_env, lex_env, private_env, is_strict_eval);

    let mut result = if let EvalResult::Throw(thrown_value) = result {
        Completion::throw(thrown_value)
    } else {
        eval_toplevel_list(cx, &ast.toplevels)
    };

    if result.is_normal() && result.is_empty() {
        result = Completion::normal(Value::undefined());
    }

    cx.pop_execution_context();

    // TODO: Need better way to save ASTs generated from eval instead of freeing them, as they may
    // be needed later e.g. for functions returned from eval. Currently all eval ASTs are saved in
    // the context and effectively leaked since they may not actually be needed.
    cx.eval_asts.push(ast);

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
    cx: &mut Context,
    ast: &ast::Program,
    mut var_env: Gc<dyn Environment>,
    mut lex_env: Gc<dyn Environment>,
    private_env: Option<Gc<PrivateEnvironment>>,
    is_strict_eval: bool,
) -> EvalResult<()> {
    if !is_strict_eval {
        if let Some(var_env) = var_env.as_global_environment() {
            for var_decl in ast.var_decls() {
                maybe!(var_decl.iter_bound_names(&mut |id| {
                    if maybe!(var_env.has_lexical_declaration(cx, &id.name)) {
                        return syntax_error_(
                            cx,
                            &format!("identifier '{}' has already been declared", &id.name),
                        );
                    }

                    ().into()
                }));
            }
        }

        let mut this_env = lex_env;
        while this_env != var_env {
            if this_env.as_object_environment().is_none() {
                for var_decl in ast.var_decls() {
                    maybe!(var_decl.iter_bound_names(&mut |id| {
                        if must!(this_env.has_binding(cx, &id.name)) {
                            return syntax_error_(
                                cx,
                                &format!("identifier '{}' has already been declared", &id.name),
                            );
                        }

                        ().into()
                    }));
                }
            }

            this_env = this_env.outer().unwrap();
        }
    }

    // TODO: Check that all private identifiers are valid

    let mut declared_function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for var_decl in ast.var_decls().iter().rev() {
        if let VarDecl::Func(func_ptr) = var_decl {
            let func = func_ptr.as_ref();
            let name = &func.id.as_deref().unwrap().name;

            if declared_function_names.insert(name) {
                if let Some(var_env) = var_env.as_global_environment() {
                    if !maybe!(var_env.can_declare_global_function(name)) {
                        return type_error_(
                            cx,
                            &format!("cannot declare global function {}", name),
                        );
                    }
                }

                functions_to_initialize.push(func);
            }
        }
    }

    // Order does not matter for declared var names, despite ordering in spec
    let mut declared_var_names: HashSet<String> = HashSet::new();

    for var_decl in ast.var_decls() {
        if let VarDecl::Var(var_decl) = var_decl {
            maybe!(var_decl.as_ref().iter_bound_names(&mut |id| {
                let name = &id.name;
                if !declared_function_names.contains(name) {
                    if let Some(var_env) = var_env.as_global_environment() {
                        if !maybe!(var_env.can_declare_global_var(name)) {
                            return type_error_(cx, &format!("cannot declare global var {}", name));
                        }
                    }

                    declared_var_names.insert(name.to_string());
                }

                ().into()
            }));
        }
    }

    for lex_decl in ast.lex_decls() {
        match lex_decl {
            LexDecl::Var(var_decl) if var_decl.as_ref().kind == ast::VarKind::Const => {
                maybe!(lex_decl.iter_bound_names(&mut |id| {
                    lex_env.create_immutable_binding(cx, id.name.to_string(), true)
                }))
            }
            _ => {
                maybe!(lex_decl.iter_bound_names(&mut |id| {
                    lex_env.create_mutable_binding(cx, id.name.to_string(), false)
                }))
            }
        }
    }

    for func in functions_to_initialize.iter().rev() {
        let name = &func.id.as_deref().unwrap().name;
        let function_object = instantiate_function_object(cx, func, lex_env, private_env);

        if let Some(var_env) = var_env.as_global_environment() {
            maybe!(var_env.create_global_function_binding(
                cx,
                name.to_string(),
                function_object.into(),
                true
            ));
        } else {
            if must!(var_env.has_binding(cx, name)) {
                must!(var_env.set_mutable_binding(cx, name, function_object.into(), false));
            } else {
                must!(var_env.create_mutable_binding(cx, name.clone(), true));
                must!(var_env.initialize_binding(cx, name, function_object.into()));
            }
        }
    }

    for name in declared_var_names {
        if let Some(var_env) = var_env.as_global_environment() {
            maybe!(var_env.create_global_var_binding(cx, &name, true));
        } else {
            if !must!(var_env.has_binding(cx, &name)) {
                must!(var_env.create_mutable_binding(cx, name.clone(), true));
                must!(var_env.initialize_binding(cx, &name, Value::undefined()));
            }
        }
    }

    ().into()
}
