use std::{collections::HashSet, rc::Rc};

use crate::{
    js::{
        parser::{
            analyze::Analyzer,
            ast::{self},
            facts::{LexDecl, VarDecl},
        },
        runtime::{
            completion::{AbstractResult, Completion},
            environment::{
                environment::{to_trait_object, Environment},
                global_environment::GlobalEnvironment,
            },
            error::{syntax_error_, type_error, type_error_},
            execution_context::{ExecutionContext, ScriptOrModule},
            function::instantiate_function_object,
            gc::Gc,
            realm::Realm,
            value::Value,
            Context,
        },
    },
    maybe_, maybe__, must_,
};

// 16.1.4 Script Record
pub struct Script {
    realm: Gc<Realm>,
    script_node: Rc<ast::Program>,
    analyzer: Rc<Analyzer>,
}

impl Script {
    pub fn new(script_node: Rc<ast::Program>, analyzer: Rc<Analyzer>, realm: Gc<Realm>) -> Script {
        Script {
            script_node,
            analyzer,
            realm,
        }
    }
}

/// 16.1.6 ScriptEvaluation
pub fn evaluate_script(
    cx: &mut Context,
    program: Rc<ast::Program>,
    analyzer: Rc<Analyzer>,
    realm: Gc<Realm>,
) -> Completion {
    let script = cx
        .heap
        .alloc(Script::new(program.clone(), analyzer.clone(), realm));

    let global_env = realm.global_env;
    let global_env_object = to_trait_object(global_env);

    let script_ctx = cx.heap.alloc(ExecutionContext {
        function: None,
        realm,
        script_or_module: Some(ScriptOrModule::Script(script)),
        lexical_env: global_env_object,
        variable_env: global_env_object,
        private_env: None,
    });

    cx.push_execution_context(script_ctx);

    let mut result = global_declaration_instantiation(cx, &program, &analyzer, global_env);

    if let Completion::Normal(_) = result {
        result = evaluate_program(&program);
    }

    if let Completion::Normal(None) = result {
        result = Completion::Normal(Some(Value::undefined()));
    }

    cx.pop_execution_context();

    return result;
}

/// 16.1.7 GlobalDeclarationInstantiation
fn global_declaration_instantiation(
    cx: &mut Context,
    script: &ast::Program,
    analyzer: &Analyzer,
    mut env: Gc<GlobalEnvironment>,
) -> Completion {
    let script_facts = analyzer.facts_cache().get_facts(script.ast_id).unwrap();

    for lex_decl in script_facts.lex_decls() {
        maybe__!(lex_decl.iter_bound_names(&mut |id| {
            let name = &id.name;
            if env.has_var_declaration(name) || must_!(env.has_lexical_declaration(cx, name)) {
                return syntax_error_(cx, &format!("redeclaration of {}", name));
            }

            if maybe_!(env.has_restricted_global_property(name)) {
                return syntax_error_(
                    cx,
                    &format!("cannot redeclare restricted global property {}", name),
                );
            }

            ().into()
        }));
    }

    for var_decl in script_facts.var_decls() {
        maybe__!(var_decl.iter_bound_names(&mut |id| {
            let name = &id.name;
            if must_!(env.has_lexical_declaration(cx, name)) {
                return syntax_error_(cx, &format!("redeclaration of {}", name));
            }

            ().into()
        }));
    }

    let mut declared_function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for var_decl in script_facts.var_decls().iter().rev() {
        match var_decl {
            VarDecl::Func(func_ptr) => {
                let func = func_ptr.as_ref();
                let name = &func.id.as_deref().unwrap().name;

                if !maybe__!(env.can_declare_global_function(name)) {
                    return type_error(cx, &format!("cannot declare global function {}", name));
                }

                declared_function_names.insert(name);
                functions_to_initialize.push(func);
            }
            VarDecl::Var(_) => {}
        }
    }

    // Order does not matter for declared var names, despite ordering in spec
    let mut declared_var_names: HashSet<String> = HashSet::new();

    for var_decl in script_facts.var_decls() {
        match var_decl {
            VarDecl::Var(var_decl) => {
                maybe__!(var_decl.as_ref().iter_bound_names(&mut |id| {
                    let name = &id.name;
                    if !declared_function_names.contains(name) {
                        if !maybe_!(env.can_declare_global_var(name)) {
                            return type_error_(cx, &format!("cannot declare global var {}", name));
                        }

                        declared_var_names.insert(name.to_string());
                    }

                    ().into()
                }));
            }
            VarDecl::Func(_) => {}
        }
    }

    for lex_decl in script_facts.lex_decls() {
        match lex_decl {
            LexDecl::Var(var_decl) if var_decl.as_ref().kind == ast::VarKind::Const => {
                maybe__!(lex_decl.iter_bound_names(&mut |id| {
                    env.create_immutable_binding(cx, id.name.to_string(), true)
                }))
            }
            _ => {
                maybe__!(lex_decl.iter_bound_names(&mut |id| {
                    env.create_mutable_binding(cx, id.name.to_string(), false)
                }))
            }
        }
    }

    for func in functions_to_initialize.iter().rev() {
        let name = &func.id.as_deref().unwrap().name;
        let function_object = instantiate_function_object(cx, func, to_trait_object(env), None);
        maybe__!(env.create_global_function_binding(
            cx,
            name.to_string(),
            function_object.into(),
            false
        ));
    }

    for var_name in declared_var_names {
        maybe__!(env.create_global_var_binding(cx, &var_name, false));
    }

    Completion::empty()
}

fn evaluate_program(program: &ast::Program) -> Completion {
    // TODO: Evaluate program
    Completion::empty()
}
