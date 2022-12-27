use crate::{
    js::{
        parser::{ast, facts::LexDecl},
        runtime::{
            completion::{AbstractResult, Completion},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
            },
            eval::function::{
                instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
            },
            execution_context::resolve_binding,
            gc::Gc,
            value::Value,
            Context,
        },
    },
    maybe, maybe__, must_,
};

use super::expression::eval_expression;

// 14.2.2 StatementList Evaluation
pub fn eval_statement_list(cx: &mut Context, stmts: &[ast::Statement]) -> Completion {
    // Value of the statement list is the last non-empty completion
    let mut result = Completion::empty();
    for stmt in stmts {
        let new_result = eval_statement(cx, stmt);
        maybe!(new_result);
        result = new_result.update_if_empty(result.value());
    }

    result
}

// Equivalent to evaluating StatementList, but for toplevel items
pub fn eval_toplevel_list(cx: &mut Context, toplevels: &[ast::Toplevel]) -> Completion {
    // Value of the statement list is the last non-empty completion
    let mut result = Completion::empty();
    for toplevel in toplevels {
        match toplevel {
            ast::Toplevel::Statement(stmt) => {
                let new_result = eval_statement(cx, stmt);
                maybe!(new_result);
                result = new_result.update_if_empty(result.value());
            }
        }
    }

    result
}

fn eval_statement(cx: &mut Context, stmt: &ast::Statement) -> Completion {
    match stmt {
        ast::Statement::VarDecl(var_decl) => {
            if var_decl.kind == ast::VarKind::Var {
                eval_variable_declaration(cx, var_decl)
            } else {
                eval_lexical_declaration(cx, var_decl)
            }
        }
        ast::Statement::FuncDecl(_) => eval_function_declaration(),
        ast::Statement::Expr(stmt) => eval_expression_statement(cx, stmt),
        ast::Statement::Block(block) => eval_block(cx, block),
        ast::Statement::Empty(_) => eval_empty_statement(),
        ast::Statement::Debugger(_) => eval_debugger_statement(),
        _ => unimplemented!("statement evaluation"),
    }
}

// 14.1.1 HoistableDeclaration Evaluation
// 15.2.6 FunctionDeclaration Evaluation
fn eval_function_declaration() -> Completion {
    Completion::empty()
}

// 14.2.2 Block Evaluation
fn eval_block(cx: &mut Context, block: &ast::Block) -> Completion {
    if block.body.is_empty() {
        return Completion::empty();
    }

    let mut current_context = cx.current_execution_context();
    let old_env = current_context.lexical_env;
    let block_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env)));

    block_declaration_instantiation(cx, block.ast_id, block_env);

    current_context.lexical_env = to_trait_object(block_env);
    let block_value = eval_statement_list(cx, &block.body);
    current_context.lexical_env = old_env;

    block_value
}

// 14.2.3 BlockDeclarationInstantiation
fn block_declaration_instantiation(
    cx: &mut Context,
    ast_id: ast::AstId,
    mut env: Gc<DeclarativeEnvironment>,
) {
    let script_or_module = cx.get_active_script_or_module().unwrap();
    let analyzer = script_or_module.analyzer();
    let facts = analyzer.facts_cache().get_facts(ast_id);

    let facts = match facts {
        None => return,
        Some(facts) => facts,
    };

    for lex_decl in facts.lex_decls() {
        match lex_decl {
            LexDecl::Var(var_decl) if var_decl.as_ref().kind == ast::VarKind::Const => {
                must_!(lex_decl.iter_bound_names(&mut |id| {
                    env.create_immutable_binding(cx, id.name.to_string(), true)
                }))
            }
            _ => {
                must_!(lex_decl.iter_bound_names(&mut |id| {
                    env.create_mutable_binding(cx, id.name.to_string(), false)
                }))
            }
        }
    }
}

// 14.3.1.2 Lexical Declaration Evaluation
fn eval_lexical_declaration(cx: &mut Context, var_decl: &ast::VariableDeclaration) -> Completion {
    for decl in &var_decl.declarations {
        match decl.id.as_ref() {
            ast::Pattern::Id(id) => {
                let mut id_reference = maybe__!(resolve_binding(cx, &id.name, None));

                let value = if let Some(init) = &decl.init {
                    maybe!(eval_named_anonymous_function_or_expression(
                        cx,
                        init.as_ref(),
                        &id.name
                    ))
                } else {
                    Value::undefined()
                };

                maybe__!(id_reference.initialize_referenced_binding(cx, value));
            }
            _ => unimplemented!("lexical declaration patterns"),
        }
    }

    Completion::empty()
}

// 14.3.2.1 Variable Declaration Evaluation
fn eval_variable_declaration(cx: &mut Context, var_decl: &ast::VariableDeclaration) -> Completion {
    for decl in &var_decl.declarations {
        match decl.id.as_ref() {
            ast::Pattern::Id(id) => {
                if let Some(init) = &decl.init {
                    let mut id_reference = maybe__!(resolve_binding(cx, &id.name, None));
                    let value = maybe!(eval_named_anonymous_function_or_expression(
                        cx,
                        init.as_ref(),
                        &id.name
                    ));

                    maybe__!(id_reference.put_value(cx, value));
                }
            }
            _ => unimplemented!("variable declaration patterns"),
        }
    }

    Completion::empty()
}

#[inline]
fn eval_named_anonymous_function_or_expression(
    cx: &mut Context,
    expr: &ast::Expression,
    name: &str,
) -> Completion {
    match expr {
        ast::Expression::Function(func @ ast::Function { id: None, .. }) => {
            instantiate_ordinary_function_expression(cx, &func, Some(name)).into()
        }
        ast::Expression::ArrowFunction(func) => {
            instantiate_arrow_function_expression(cx, &func, Some(name)).into()
        }
        _ => eval_expression(cx, expr),
    }
}

// 14.4.1 Empty Statement Evaluation
fn eval_empty_statement() -> Completion {
    Completion::empty()
}

// 14.5.1 Expression Statement Evaluation
fn eval_expression_statement(cx: &mut Context, stmt: &ast::ExpressionStatement) -> Completion {
    eval_expression(cx, &stmt.expr)
}

// 14.16.1 Debugger Statement Evaluation
fn eval_debugger_statement() -> Completion {
    Completion::empty()
}
