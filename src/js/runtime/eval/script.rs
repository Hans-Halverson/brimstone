use std::{collections::HashSet, rc::Rc};

use crate::{
    js::{
        parser::ast::{self, LexDecl, VarDecl, WithDecls},
        runtime::{
            completion::{Completion, EvalResult},
            environment::{environment::Environment, global_environment::GlobalEnvironment},
            error::{syntax_error_, type_error, type_error_},
            execution_context::{ExecutionContext, ScriptOrModule},
            function::instantiate_function_object,
            gc::{GcDeref, Handle},
            object_descriptor::{ObjectDescriptor, ObjectKind},
            realm::Realm,
            string_value::StringValue,
            value::Value,
            Context, HeapPtr,
        },
    },
    maybe, maybe__, must, set_uninit,
};

use super::{pattern::id_string_value, statement::eval_toplevel_list};

// 16.1.4 Script Record
#[repr(C)]
pub struct Script {
    descriptor: HeapPtr<ObjectDescriptor>,
    realm: HeapPtr<Realm>,
    script_node: Rc<ast::Program>,
}

impl GcDeref for Script {}

impl Script {
    pub fn new(
        cx: &mut Context,
        script_node: Rc<ast::Program>,
        realm: Handle<Realm>,
    ) -> Handle<Script> {
        let mut script = cx.heap.alloc_uninit::<Script>();

        set_uninit!(script.descriptor, cx.base_descriptors.get(ObjectKind::Script));
        set_uninit!(script.script_node, script_node);
        set_uninit!(script.realm, realm.get_());

        script
    }
}

/// 16.1.6 ScriptEvaluation
pub fn eval_script(
    cx: &mut Context,
    program: Rc<ast::Program>,
    realm: Handle<Realm>,
) -> Completion {
    let script = Script::new(cx, program.clone(), realm);

    let global_env = realm.global_env();
    let global_env_object = global_env.into_dyn_env();

    let script_ctx = ExecutionContext::new(
        cx,
        /* function */ None,
        realm,
        Some(ScriptOrModule::Script(script)),
        /* lexical_env */ global_env_object,
        /* variable_env */ global_env_object,
        /* private_env */ None,
        program.has_use_strict_directive,
    );

    cx.push_execution_context(script_ctx);

    let mut result = global_declaration_instantiation(cx, &program, global_env);

    if result.is_normal() {
        result = eval_toplevel_list(cx, &program.toplevels);
    }

    if result.is_empty() {
        result = Value::undefined().into();
    }

    cx.pop_execution_context();

    return result;
}

/// 16.1.7 GlobalDeclarationInstantiation
fn global_declaration_instantiation(
    cx: &mut Context,
    script: &ast::Program,
    mut env: Handle<GlobalEnvironment>,
) -> Completion {
    for lex_decl in script.lex_decls() {
        maybe__!(lex_decl.iter_bound_names(&mut |id| {
            let name_value = id_string_value(cx, id);

            if env.has_var_declaration(name_value)
                || must!(env.has_lexical_declaration(cx, name_value))
            {
                return syntax_error_(cx, &format!("redeclaration of {}", name_value));
            }

            if maybe!(env.has_restricted_global_property(cx, name_value)) {
                return syntax_error_(
                    cx,
                    &format!("cannot redeclare restricted global property {}", name_value),
                );
            }

            ().into()
        }));
    }

    for var_decl in script.var_decls() {
        maybe__!(var_decl.iter_bound_names(&mut |id| {
            let name_value = id_string_value(cx, id);
            if must!(env.has_lexical_declaration(cx, name_value)) {
                return syntax_error_(cx, &format!("redeclaration of {}", name_value));
            }

            ().into()
        }));
    }

    let mut declared_function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for var_decl in script.var_decls().iter().rev() {
        if let VarDecl::Func(func_ptr) = var_decl {
            let func = func_ptr.as_ref();
            let name = &func.id.as_deref().unwrap().name;

            if declared_function_names.insert(name) {
                let name_value = id_string_value(cx, func.id.as_deref().unwrap());
                if !maybe__!(env.can_declare_global_function(cx, name_value)) {
                    return type_error(
                        cx,
                        &format!("cannot declare global function {}", name_value),
                    );
                }

                functions_to_initialize.push(func);
            }
        }
    }

    // Order does not matter for declared var names, despite ordering in spec
    let mut declared_var_names: HashSet<Handle<StringValue>> = HashSet::new();

    for var_decl in script.var_decls() {
        match var_decl {
            VarDecl::Var(var_decl) => {
                maybe__!(var_decl.as_ref().iter_bound_names(&mut |id| {
                    let name = &id.name;
                    if !declared_function_names.contains(name) {
                        let name_value = id_string_value(cx, id);
                        if !maybe!(env.can_declare_global_var(cx, name_value)) {
                            return type_error_(
                                cx,
                                &format!("cannot declare global var {}", name_value),
                            );
                        }

                        declared_var_names.insert(name_value);
                    }

                    ().into()
                }));
            }
            VarDecl::Func(_) => {}
        }
    }

    for lex_decl in script.lex_decls() {
        match lex_decl {
            LexDecl::Var(var_decl) if var_decl.as_ref().kind == ast::VarKind::Const => {
                maybe__!(lex_decl.iter_bound_names(&mut |id| {
                    let name_value = id_string_value(cx, id);
                    env.create_immutable_binding(cx, name_value, true)
                }))
            }
            _ => {
                maybe__!(lex_decl.iter_bound_names(&mut |id| {
                    let name_value = id_string_value(cx, id);
                    env.create_mutable_binding(cx, name_value, false)
                }))
            }
        }
    }

    for func in functions_to_initialize.iter().rev() {
        let name_value = id_string_value(cx, func.id.as_deref().unwrap());
        let function_object = instantiate_function_object(cx, func, env.into_dyn_env(), None);
        maybe__!(env.create_global_function_binding(cx, name_value, function_object.into(), false));
    }

    for var_name in declared_var_names {
        maybe__!(env.create_global_var_binding(cx, var_name, false));
    }

    Completion::empty()
}
