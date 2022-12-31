use crate::{
    js::{
        parser::ast::{self, LexDecl, WithDecls},
        runtime::{
            completion::{AbstractResult, Completion, CompletionKind},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
            },
            eval::{
                function::{
                    instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
                },
                pattern::binding_initialization,
            },
            execution_context::resolve_binding,
            gc::Gc,
            type_utilities::to_boolean,
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
        ast::Statement::If(stmt) => eval_if_statement(cx, stmt),
        ast::Statement::Try(stmt) => eval_try_statement(cx, stmt),
        ast::Statement::Throw(stmt) => eval_throw_statement(cx, stmt),
        ast::Statement::Return(stmt) => eval_return_statement(cx, stmt),
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

    block_declaration_instantiation(cx, block.lex_decls(), block_env);

    current_context.lexical_env = to_trait_object(block_env);
    let block_value = eval_statement_list(cx, &block.body);
    current_context.lexical_env = old_env;

    block_value
}

// 14.2.3 BlockDeclarationInstantiation
fn block_declaration_instantiation(
    cx: &mut Context,
    lex_decls: &[ast::LexDecl],
    mut env: Gc<DeclarativeEnvironment>,
) {
    for lex_decl in lex_decls {
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
            patt => {
                let value = maybe!(eval_expression(cx, decl.init.as_deref().unwrap()));
                let env = cx.current_execution_context().lexical_env;
                maybe__!(binding_initialization(cx, patt, value, Some(env))).into()
            }
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
            patt => {
                let value = maybe!(eval_expression(cx, decl.init.as_deref().unwrap()));
                maybe__!(binding_initialization(cx, patt, value, None)).into()
            }
        }
    }

    Completion::empty()
}

#[inline]
pub fn eval_named_anonymous_function_or_expression(
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

// 14.6.2 If Statement Evaluation
fn eval_if_statement(cx: &mut Context, stmt: &ast::IfStatement) -> Completion {
    let test = maybe!(eval_expression(cx, &stmt.test));

    let completion = if to_boolean(test) {
        eval_statement(cx, &stmt.conseq)
    } else {
        if let Some(ref altern) = stmt.altern {
            eval_statement(cx, altern)
        } else {
            return Value::undefined().into();
        }
    };

    completion.update_if_empty(Value::undefined())
}

// 14.10.1 Return Statement Evaluation
fn eval_return_statement(cx: &mut Context, stmt: &ast::ReturnStatement) -> Completion {
    let return_value = if let Some(ref argument) = stmt.argument {
        maybe!(eval_expression(cx, argument))
    } else {
        Value::undefined()
    };

    // TODO: Check for generator

    Completion::return_(return_value)
}

// 14.14.1 Throw Statement Evaluation
fn eval_throw_statement(cx: &mut Context, stmt: &ast::ThrowStatement) -> Completion {
    let value = maybe!(eval_expression(cx, &stmt.argument));
    Completion::throw(value)
}

// 14.15.3 Try Statement Evaluation
fn eval_try_statement(cx: &mut Context, stmt: &ast::TryStatement) -> Completion {
    let block_result = eval_block(cx, &stmt.block);

    let block_catch_result = if block_result.kind() == CompletionKind::Throw {
        if let Some(ref catch) = stmt.handler {
            eval_catch_clause(cx, catch, block_result.value())
        } else {
            block_result
        }
    } else {
        block_result
    };

    let result = if let Some(ref finally) = stmt.finalizer {
        let finally_result = eval_block(cx, finally);
        if finally_result.is_normal() {
            block_catch_result
        } else {
            finally_result
        }
    } else {
        block_catch_result
    };

    result.update_if_empty(Value::undefined())
}

// 14.15.2 CatchClauseEvaluation
fn eval_catch_clause(
    cx: &mut Context,
    catch: &ast::CatchClause,
    thrown_value: Value,
) -> Completion {
    match catch.param {
        None => eval_block(cx, &catch.body),
        Some(ref param) => {
            let mut current_context = cx.current_execution_context();
            let old_env = cx.current_execution_context().lexical_env;
            let mut catch_env =
                to_trait_object(cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env))));

            must_!(param.iter_bound_names(&mut |id| {
                catch_env.create_mutable_binding(cx, id.name.clone(), false)
            }));

            current_context.lexical_env = catch_env;

            let binding_init_result =
                binding_initialization(cx, param, thrown_value, Some(catch_env));

            // Make sure to remove new environment if binding initialization fails
            if let AbstractResult::Throw(throw_value) = binding_init_result {
                current_context.lexical_env = old_env;
                return Completion::throw(throw_value);
            }

            let result = eval_block(cx, &catch.body);

            current_context.lexical_env = old_env;

            result
        }
    }
}

// 14.16.1 Debugger Statement Evaluation
fn eval_debugger_statement() -> Completion {
    Completion::empty()
}
