use std::{collections::HashMap, rc::Rc};

use crate::js::{
    common::wtf_8::Wtf8String,
    parser::{
        analyze::{analyze_for_eval, PrivateNameUsage},
        ast, parse_script_for_eval,
        scope_tree::BindingKind,
        source::Source,
        ParseContext,
    },
    runtime::{
        abstract_operations::call_object,
        bytecode::{
            function::Closure, generator::BytecodeProgramGenerator, instruction::EvalFlags,
        },
        error::{syntax_error, syntax_parse_error, type_error},
        global_names::{
            can_declare_global_function, can_declare_global_var, create_global_function_binding,
            create_global_var_binding,
        },
        interned_strings::InternedStrings,
        property::Property,
        scope::{Scope, ScopeKind},
        string_value::FlatString,
        Context, EvalResult, Handle, HeapPtr, Value,
    },
};

pub fn perform_eval(
    mut cx: Context,
    code: Handle<Value>,
    is_strict_caller: bool,
    direct_scope: Option<Handle<Scope>>,
    flags: EvalFlags,
) -> EvalResult<Handle<Value>> {
    if !code.is_string() {
        return Ok(code);
    }
    let code = code.as_string();

    let is_direct = direct_scope.is_some();

    let private_names = get_private_names_from_scopes(direct_scope.map(|s| *s));

    // Use the file path of the active source file
    let file_path = cx.vm().current_source_file().path().to_string();

    // Parse source code
    let source = match Source::new_for_eval(file_path, code.to_wtf8_string()) {
        Ok(source) => Rc::new(source),
        Err(error) => return syntax_parse_error(cx, &error),
    };
    let pcx = ParseContext::new(source);

    let parse_result = parse_script_for_eval(&pcx, cx.options.clone(), is_direct, is_strict_caller);
    let parse_result = match parse_result {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_parse_error(cx, &error),
    };

    // Analyze source code
    let analyzed_result = analyze_for_eval(
        &pcx,
        parse_result,
        private_names,
        flags.contains(EvalFlags::IN_FUNCTION),
        flags.contains(EvalFlags::IN_METHOD),
        flags.contains(EvalFlags::IN_STATIC),
        flags.contains(EvalFlags::IN_DERIVED_CONSTRUCTOR),
        flags.contains(EvalFlags::IN_STATIC_INITIALIZER),
        flags.contains(EvalFlags::IN_CLASS_FIELD_INITIALIZER),
    );

    // Return the first syntax error
    let analyzed_result = match analyzed_result {
        Ok(analyzed_result) => analyzed_result,
        Err(errors) => return syntax_parse_error(cx, &errors.errors[0]),
    };

    // Sloppy direct evals must perform EvalDeclarationInstantiation as var scoped bindings will
    // be placed in the parent var scope. Strict direct evals do not need to call EDI as var scoped
    // bindings are handled normally when setting up a call frame in the VM.
    if !analyzed_result.program.is_strict_mode {
        eval_declaration_instantiation(cx, &analyzed_result.program)?;
    }

    // Generate bytecode for the program
    let realm = cx.current_realm();
    let generate_result =
        BytecodeProgramGenerator::generate_from_eval_parse_result(cx, &analyzed_result, realm);
    let bytecode_function = match generate_result {
        Ok(func) => func,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

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
    call_object(cx, closure.into(), receiver, &[])
}

/// Gather private names from parent class scopes.
fn get_private_names_from_scopes(
    scope: Option<HeapPtr<Scope>>,
) -> Option<HashMap<Wtf8String, PrivateNameUsage>> {
    let mut private_names = None;
    let mut scope_opt = scope;

    while let Some(scope) = scope_opt {
        let scope_names = scope.scope_names_ptr();
        if scope_names.is_class_scope() {
            if private_names.is_none() {
                private_names = Some(HashMap::new());
            }

            for (i, name) in scope_names.name_ptrs().iter().enumerate() {
                if scope_names.is_private_name(i) {
                    // Exclude the "#" prefix
                    let prefixed_private_name = name.to_string();
                    let private_name = Wtf8String::from_str(&prefixed_private_name[1..]);

                    private_names
                        .as_mut()
                        .unwrap()
                        .insert(private_name, PrivateNameUsage::used());
                }
            }
        }

        scope_opt = scope.parent();
    }

    private_names
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
            if let Some(index) = scope.scope_names_ptr().lookup_name(**name) {
                if scope.scope_names_ptr().is_function_parameter(index) {
                    return error_name_already_declared(cx, *name);
                }
            }
        }

        return Ok(());
    }

    // Otherwise walk scopes up to the parent var scope
    loop {
        // Name will conflict with a lexical binding in any parent scope
        for name in eval_var_names.iter().chain(eval_func_names.iter()) {
            if let Some(index) = scope.scope_names_ptr().lookup_name(**name) {
                if scope.scope_names_ptr().is_lexical(index) {
                    return error_name_already_declared(cx, *name);
                }
            }
        }

        // If in a global scope, check for lexical bindings in other global scopes
        if scope.kind() == ScopeKind::Global {
            let realm = scope.global_scope_realm();
            for name in eval_var_names.iter().chain(eval_func_names.iter()) {
                if realm.get_lexical_name(**name).is_some() {
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

    Ok(())
}

/// EvalDeclarationInstantiation (https://tc39.es/ecma262/#sec-evaldeclarationinstantiation)
fn eval_declaration_instantiation(mut cx: Context, program: &ast::Program) -> EvalResult<()> {
    // Find the function and var names in the eval scope
    let mut eval_var_names = vec![];
    let mut eval_func_names = vec![];
    for (name, binding) in program.scope.as_ref().iter_var_decls() {
        let name = InternedStrings::get_wtf8_str(cx, name).as_flat();
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
        .map(|(name, _)| InternedStrings::get_wtf8_str(cx, name).as_flat())
        .collect::<Vec<_>>();

    check_eval_var_name_conflicts(cx, &eval_var_names, &eval_func_names)?;

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
            if !can_declare_global_function(cx, scope_object, func_name.cast())? {
                return type_error(cx, &format!("cannot declare global function {}", *func_name));
            }
        }

        for var_name in &eval_var_names {
            if !can_declare_global_var(cx, scope_object, var_name.cast())? {
                return type_error(cx, &format!("cannot declare global var {}", *var_name));
            }
        }
    }

    // Create and initialize bindings for all functions
    for name in &eval_func_names {
        if is_global_scope {
            create_global_function_binding(
                cx,
                scope_object,
                name.cast(),
                /* can_delete */ true,
            )?;
        } else {
            // All functions initialized to undefined, overwriting existing if already declared
            let desc = Property::data(cx.undefined(), true, true, true);
            scope_object.set_property(cx, name.cast(), desc);
        }
    }

    // Create and initialize bindings for all vars
    for name in &eval_var_names {
        if is_global_scope {
            create_global_var_binding(cx, scope_object, name.cast(), /* can_delete */ true)?;
        } else {
            // Vars only initialized to undefined if the do not have been declared yet
            if !scope_object.has_property(cx, name.cast())? {
                let desc = Property::data(cx.undefined(), true, true, true);
                scope_object.set_property(cx, name.cast(), desc);
            }
        }
    }

    Ok(())
}

fn error_name_already_declared(cx: Context, name: Handle<FlatString>) -> EvalResult<()> {
    syntax_error(cx, &format!("identifier '{}' has already been declared", name))
}
