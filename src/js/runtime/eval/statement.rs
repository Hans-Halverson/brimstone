use std::collections::HashSet;

use crate::{
    js::{
        parser::ast::{self, LabelId, LexDecl, WithDecls},
        runtime::{
            completion::{Completion, CompletionKind, EvalResult, EMPTY_LABEL},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
                object_environment::ObjectEnvironment,
            },
            eval::{
                function::{
                    instantiate_arrow_function_expression, instantiate_ordinary_function_expression,
                },
                pattern::binding_initialization,
            },
            execution_context::resolve_binding,
            gc::Gc,
            type_utilities::{is_strictly_equal, to_boolean, to_object},
            value::Value,
            Context,
        },
    },
    maybe, maybe_, maybe__, must,
};

use super::{
    class::{class_definition_evaluation, eval_class_declaration},
    expression::{eval_expression, eval_identifier_to_reference},
};

// 14.2.2 StatementList Evaluation
pub fn eval_statement_list(cx: &mut Context, stmts: &[ast::Statement]) -> Completion {
    // Value of the statement list is the last non-empty completion
    let mut result = Completion::empty();
    for stmt in stmts {
        let new_result = eval_statement(cx, stmt);

        result = new_result.update_if_empty(result.value());

        if !result.is_normal() {
            return result;
        }
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

                result = new_result.update_if_empty(result.value());

                if !result.is_normal() {
                    return result;
                }
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
        ast::Statement::ClassDecl(class) => eval_class_declaration(cx, class),
        ast::Statement::Expr(stmt) => eval_expression_statement(cx, stmt),
        ast::Statement::Block(block) => eval_block(cx, block),
        ast::Statement::If(stmt) => eval_if_statement(cx, stmt),
        ast::Statement::Switch(stmt) => eval_switch_statement(cx, stmt, EMPTY_LABEL),
        ast::Statement::For(stmt) => eval_for_statement(cx, stmt, EMPTY_LABEL),
        ast::Statement::ForEach(stmt) => {
            if stmt.kind == ast::ForEachKind::In {
                eval_for_in_statement(cx, stmt, EMPTY_LABEL)
            } else {
                unimplemented!("for of statement")
            }
        }
        ast::Statement::While(stmt) => eval_while_statement(cx, stmt, EMPTY_LABEL),
        ast::Statement::DoWhile(stmt) => eval_do_while_statement(cx, stmt, EMPTY_LABEL),
        ast::Statement::With(stmt) => eval_with_statement(cx, stmt),
        ast::Statement::Try(stmt) => eval_try_statement(cx, stmt),
        ast::Statement::Throw(stmt) => eval_throw_statement(cx, stmt),
        ast::Statement::Return(stmt) => eval_return_statement(cx, stmt),
        ast::Statement::Break(stmt) => eval_break_statement(stmt),
        ast::Statement::Continue(stmt) => eval_continue_statement(stmt),
        ast::Statement::Labeled(stmt) => eval_labeled_statement(cx, stmt),
        ast::Statement::Empty(_) => eval_empty_statement(),
        ast::Statement::Debugger(_) => eval_debugger_statement(),
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
                must!(lex_decl.iter_bound_names(&mut |id| {
                    env.create_immutable_binding(cx, id.name.to_string(), true)
                }))
            }
            _ => must!(lex_decl.iter_bound_names(&mut |id| {
                env.create_mutable_binding(cx, id.name.to_string(), false)
            })),
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
                    maybe__!(eval_named_anonymous_function_or_expression(
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
                let value = maybe__!(eval_expression(cx, decl.init.as_deref().unwrap()));
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
                    let value = maybe__!(eval_named_anonymous_function_or_expression(
                        cx,
                        init.as_ref(),
                        &id.name
                    ));

                    maybe__!(id_reference.put_value(cx, value));
                }
            }
            patt => {
                let value = maybe__!(eval_expression(cx, decl.init.as_deref().unwrap()));
                maybe__!(binding_initialization(cx, patt, value, None));
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
) -> EvalResult<Value> {
    match expr {
        ast::Expression::Function(func @ ast::Function { id: None, .. }) => {
            instantiate_ordinary_function_expression(cx, &func, Some(name)).into()
        }
        ast::Expression::ArrowFunction(func) => {
            instantiate_arrow_function_expression(cx, &func, Some(name)).into()
        }
        ast::Expression::Class(class @ ast::Class { id: None, .. }) => {
            let value = maybe!(class_definition_evaluation(cx, class, None, name));
            value.into()
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
    eval_expression(cx, &stmt.expr).into()
}

// 14.6.2 If Statement Evaluation
fn eval_if_statement(cx: &mut Context, stmt: &ast::IfStatement) -> Completion {
    let test = maybe__!(eval_expression(cx, &stmt.test));

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

// 14.7.1.1 LoopContinues
#[inline]
fn loop_continues(completion: &Completion, stmt_label_id: LabelId) -> bool {
    match completion.kind() {
        CompletionKind::Normal => true,
        CompletionKind::Break | CompletionKind::Return | CompletionKind::Throw => false,
        CompletionKind::Continue => {
            let label = completion.label();
            (label == EMPTY_LABEL) || (label == stmt_label_id)
        }
    }
}

// 14.7.2.2 Do While Statement Evaluation
fn eval_do_while_statement(
    cx: &mut Context,
    stmt: &ast::DoWhileStatement,
    stmt_label_id: LabelId,
) -> Completion {
    let mut last_value = Value::undefined();
    loop {
        let body_result = eval_statement(cx, &stmt.body);

        if !loop_continues(&body_result, stmt_label_id) {
            let body_result = body_result.update_if_empty(last_value);

            // Inline labeled statement evaluation break handling
            if body_result.kind() == CompletionKind::Break {
                let label = body_result.label();
                if (label == EMPTY_LABEL) || (label == stmt_label_id) {
                    return Completion::normal(body_result.value());
                } else {
                    return body_result;
                }
            }

            return body_result;
        }

        if !body_result.value().is_empty() {
            last_value = body_result.value()
        }

        let test_value = maybe__!(eval_expression(cx, &stmt.test));
        if !to_boolean(test_value) {
            return last_value.into();
        }
    }
}

// 14.7.3.2 While Statement Evaluation
fn eval_while_statement(
    cx: &mut Context,
    stmt: &ast::WhileStatement,
    stmt_label_id: LabelId,
) -> Completion {
    let mut last_value = Value::undefined();
    loop {
        let test_value = maybe__!(eval_expression(cx, &stmt.test));
        if !to_boolean(test_value) {
            return last_value.into();
        }

        let body_result = eval_statement(cx, &stmt.body);

        if !loop_continues(&body_result, stmt_label_id) {
            let body_result = body_result.update_if_empty(last_value);

            // Inline labeled statement evaluation break handling
            if body_result.kind() == CompletionKind::Break {
                let label = body_result.label();
                if (label == EMPTY_LABEL) || (label == stmt_label_id) {
                    return Completion::normal(body_result.value());
                } else {
                    return body_result;
                }
            }

            return body_result;
        }

        if !body_result.value().is_empty() {
            last_value = body_result.value()
        }
    }
}

// 14.7.4.2 For Statement Evaluation
fn eval_for_statement(
    cx: &mut Context,
    stmt: &ast::ForStatement,
    stmt_label_id: LabelId,
) -> Completion {
    match stmt.init.as_deref() {
        None => for_body_evaluation(cx, stmt, None, stmt_label_id),
        Some(ast::ForInit::Expression(expr)) => {
            maybe__!(eval_expression(cx, expr));
            for_body_evaluation(cx, stmt, None, stmt_label_id)
        }
        Some(ast::ForInit::VarDecl(
            var_decl @ ast::VariableDeclaration {
                kind: ast::VarKind::Var,
                ..
            },
        )) => {
            maybe_!(eval_variable_declaration(cx, var_decl));
            for_body_evaluation(cx, stmt, None, stmt_label_id)
        }
        Some(ast::ForInit::VarDecl(
            lex_decl @ ast::VariableDeclaration {
                kind: kind @ (ast::VarKind::Let | ast::VarKind::Const),
                ..
            },
        )) => {
            let mut current_execution_context = cx.current_execution_context();
            let old_env = current_execution_context.lexical_env;
            let mut loop_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env)));

            let is_const = *kind == ast::VarKind::Const;
            must!(lex_decl.iter_bound_names(&mut |id| {
                if is_const {
                    loop_env.create_immutable_binding(cx, id.name.clone(), true)
                } else {
                    loop_env.create_mutable_binding(cx, id.name.clone(), true)
                }
            }));

            current_execution_context.lexical_env = to_trait_object(loop_env);

            let for_decl_completion = eval_lexical_declaration(cx, lex_decl);
            if !for_decl_completion.is_normal() {
                current_execution_context.lexical_env = old_env;
                return for_decl_completion;
            }

            let per_iteration_decl = if is_const { None } else { Some(lex_decl) };
            let body_result = for_body_evaluation(cx, stmt, per_iteration_decl, stmt_label_id);

            current_execution_context.lexical_env = old_env;

            body_result
        }
    }
}

// 14.7.4.3 ForBodyEvaluation
fn for_body_evaluation(
    cx: &mut Context,
    stmt: &ast::ForStatement,
    per_iteration_decl: Option<&ast::VariableDeclaration>,
    stmt_label_id: LabelId,
) -> Completion {
    let mut last_value = Value::undefined();

    if let Some(per_iteration_decl) = per_iteration_decl {
        create_per_iteration_environment(cx, per_iteration_decl);
    }

    loop {
        if let Some(test) = stmt.test.as_deref() {
            let test_value = maybe__!(eval_expression(cx, test));
            if !to_boolean(test_value) {
                return last_value.into();
            }
        }

        let body_result = eval_statement(cx, &stmt.body);

        if !loop_continues(&body_result, stmt_label_id) {
            let body_result = body_result.update_if_empty(last_value);

            // Inline labeled statement evaluation break handling
            if body_result.kind() == CompletionKind::Break {
                let label = body_result.label();
                if (label == EMPTY_LABEL) || (label == stmt_label_id) {
                    return Completion::normal(body_result.value());
                } else {
                    return body_result;
                }
            }

            return body_result;
        }

        if !body_result.value().is_empty() {
            last_value = body_result.value()
        }

        if let Some(update) = stmt.update.as_deref() {
            maybe__!(eval_expression(cx, update));
        }
    }
}

// 14.7.4.4 CreatePerIterationEnvironment
fn create_per_iteration_environment(
    cx: &mut Context,
    per_iteration_decl: &ast::VariableDeclaration,
) -> EvalResult<()> {
    let mut current_execution_context = cx.current_execution_context();
    let last_iteration_env = current_execution_context.lexical_env;
    let mut this_iteration_env = cx
        .heap
        .alloc(DeclarativeEnvironment::new(Some(last_iteration_env)));

    maybe!(per_iteration_decl.iter_bound_names(&mut |id| {
        must!(this_iteration_env.create_mutable_binding(cx, id.name.clone(), false));
        let last_value = maybe!(last_iteration_env.get_binding_value(cx, &id.name, true));
        must!(this_iteration_env.initialize_binding(cx, &id.name, last_value));

        ().into()
    }));

    current_execution_context.lexical_env = to_trait_object(this_iteration_env);

    ().into()
}

// 14.7.5.6 ForIn/OfHeadEvaluation
// Only contains the shared part between enumerate and iterate.
fn for_each_head_evaluation_shared(
    cx: &mut Context,
    stmt: &ast::ForEachStatement,
) -> EvalResult<Value> {
    match stmt.left.as_ref() {
        ast::ForEachInit::Pattern(_)
        | ast::ForEachInit::VarDecl(ast::VariableDeclaration {
            kind: ast::VarKind::Var,
            ..
        }) => eval_expression(cx, &stmt.right),
        ast::ForEachInit::VarDecl(
            var_decl @ ast::VariableDeclaration {
                kind: ast::VarKind::Let | ast::VarKind::Const,
                ..
            },
        ) => {
            let mut current_execution_context = cx.current_execution_context();
            let old_env = current_execution_context.lexical_env;
            let mut new_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env)));

            must!(var_decl.iter_bound_names(&mut |id| {
                new_env.create_mutable_binding(cx, id.name.clone(), false)
            }));

            current_execution_context.lexical_env = to_trait_object(new_env);

            let result = eval_expression(cx, &stmt.right);

            current_execution_context.lexical_env = old_env;

            result
        }
    }
}

// 14.7.5.7 ForIn/OfBodyEvaluation
// Only contains the shared part between enumerate and iterate.
fn for_each_body_evaluation_shared(
    cx: &mut Context,
    stmt: &ast::ForEachStatement,
    right_value: Value,
) -> EvalResult<()> {
    let mut current_execution_context = cx.current_execution_context();
    let old_env = current_execution_context.lexical_env;

    match stmt.left.as_ref() {
        ast::ForEachInit::Pattern(_) => unimplemented!("for each: pattern left hand side"),
        ast::ForEachInit::VarDecl(ast::VariableDeclaration {
            kind: ast::VarKind::Var,
            declarations,
            ..
        }) => match declarations[0].id.as_ref() {
            ast::Pattern::Id(id) => {
                let reference_completion = eval_identifier_to_reference(cx, id);
                match reference_completion {
                    EvalResult::Throw(thrown_value) => EvalResult::Throw(thrown_value),
                    EvalResult::Ok(mut reference) => reference.put_value(cx, right_value),
                }
            }
            _ => {
                unimplemented!("destructuring patterns")
            }
        },
        ast::ForEachInit::VarDecl(
            var_decl @ ast::VariableDeclaration {
                kind: kind @ (ast::VarKind::Let | ast::VarKind::Const),
                declarations,
                ..
            },
        ) => {
            let mut iteration_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env)));

            // 14.7.5.4 ForDeclarationBindingInstantiation
            must!(var_decl.iter_bound_names(&mut |id| {
                if *kind == ast::VarKind::Const {
                    iteration_env.create_immutable_binding(cx, id.name.clone(), true)
                } else {
                    iteration_env.create_mutable_binding(cx, id.name.clone(), true)
                }
            }));

            current_execution_context.lexical_env = to_trait_object(iteration_env);

            match declarations[0].id.as_ref() {
                ast::Pattern::Id(id) => {
                    let reference_completion = eval_identifier_to_reference(cx, id);
                    match reference_completion {
                        EvalResult::Throw(thrown_value) => EvalResult::Throw(thrown_value),
                        EvalResult::Ok(mut reference) => {
                            reference.initialize_referenced_binding(cx, right_value)
                        }
                    }
                }
                _ => {
                    unimplemented!("destructuring patterns")
                }
            }
        }
    }
}

fn eval_for_in_statement(
    cx: &mut Context,
    stmt: &ast::ForEachStatement,
    stmt_label_id: LabelId,
) -> Completion {
    let right_value = maybe__!(for_each_head_evaluation_shared(cx, stmt));

    // Part of ForIn/OfHeadEvaluation that is specific to enumeration
    if right_value.is_nullish() {
        return Completion::break_(EMPTY_LABEL);
    }

    let object_value = maybe__!(to_object(cx, right_value));
    let mut last_value = Value::undefined();

    // 14.7.5.9 EnumerateObjectProperties inlined
    // Walk prototype chain, collecting properties that haven't already been collected
    let mut collected = HashSet::new();
    let mut current_object = object_value;

    loop {
        let own_property_keys = current_object.own_property_keys(cx);
        for key in own_property_keys {
            if !key.is_string() {
                continue;
            }

            let key_string = key.as_string();
            if !collected.insert(key_string.str()) {
                continue;
            }

            if let Some(desc) = maybe__!(current_object.get_own_property(key_string.str())) {
                if !desc.is_enumerable() {
                    continue;
                }

                maybe__!(for_each_body_evaluation_shared(cx, stmt, key));

                // Part of ForIn/OfBodyEvaluation that is specific to enumeration
                let completion = eval_statement(cx, &stmt.body);
                if !loop_continues(&completion, stmt_label_id) {
                    let completion = completion.update_if_empty(last_value);

                    // Inline labeled statement evaluation break handling
                    if completion.kind() == CompletionKind::Break {
                        let label = completion.label();
                        if (label == EMPTY_LABEL) || (label == stmt_label_id) {
                            return Completion::normal(completion.value());
                        } else {
                            return completion;
                        }
                    }

                    return completion;
                }

                if !completion.is_empty() {
                    last_value = completion.value();
                }
            }
        }

        match maybe__!(current_object.get_prototype_of()) {
            None => return last_value.into(),
            Some(proto_object) => {
                current_object = proto_object;
            }
        }
    }
}

// 14.8.2 Continue Statement Evaluation
fn eval_continue_statement(stmt: &ast::ContinueStatement) -> Completion {
    match stmt.label.as_ref() {
        None => Completion::continue_(EMPTY_LABEL),
        Some(label) => Completion::continue_(label.id),
    }
}

// 14.9.2 Break Statement Evaluation
fn eval_break_statement(stmt: &ast::BreakStatement) -> Completion {
    match stmt.label.as_ref() {
        None => Completion::break_(EMPTY_LABEL),
        Some(label) => Completion::break_(label.id),
    }
}

// 14.10.1 Return Statement Evaluation
fn eval_return_statement(cx: &mut Context, stmt: &ast::ReturnStatement) -> Completion {
    let return_value = if let Some(ref argument) = stmt.argument {
        maybe__!(eval_expression(cx, argument))
    } else {
        Value::undefined()
    };

    // TODO: Check for generator

    Completion::return_(return_value)
}

// 14.11.2 With Statement Evaluation
fn eval_with_statement(cx: &mut Context, stmt: &ast::WithStatement) -> Completion {
    let value = maybe__!(eval_expression(cx, &stmt.object));
    let object = maybe__!(to_object(cx, value));

    let mut current_execution_context = cx.current_execution_context();
    let old_env = current_execution_context.lexical_env;
    let new_env = cx
        .heap
        .alloc(ObjectEnvironment::new(object, true, Some(old_env)));
    current_execution_context.lexical_env = to_trait_object(new_env);

    let completion = eval_statement(cx, &stmt.body);

    current_execution_context.lexical_env = old_env;

    completion.update_if_empty(Value::undefined())
}

// 14.12.2 CaseBlockEvaluation
fn eval_case_block(
    cx: &mut Context,
    stmt: &ast::SwitchStatement,
    discriminant_value: Value,
) -> Completion {
    let mut v = Value::undefined();

    macro_rules! eval_case {
        ($case:expr) => {{
            let completion = eval_statement_list(cx, &$case.body);

            if !completion.is_empty() {
                v = completion.value();
            }

            if !completion.is_normal() {
                return completion.update_if_empty(v);
            }
        }};
    }

    let mut is_found = false;
    let mut default_case_index = stmt.cases.len();

    // Search all cases before the default case
    for (i, case) in stmt.cases.iter().enumerate() {
        // Check if this is the default case
        let case_selector_value = match case.test.as_ref() {
            None => {
                default_case_index = i;
                break;
            }
            Some(value) => value,
        };

        if !is_found {
            is_found = maybe__!(is_case_clause_selected(
                cx,
                case_selector_value,
                discriminant_value
            ));
        }

        if is_found {
            eval_case!(case);
        }
    }

    // Check if there is no default case
    if default_case_index >= stmt.cases.len() {
        return v.into();
    }

    let mut is_found_after_default = false;
    if !is_found {
        // Search the remaining cases after the default case
        for case in &stmt.cases[default_case_index + 1..] {
            if !is_found_after_default {
                let case_selector_value = case.test.as_deref().unwrap();
                is_found_after_default = maybe__!(is_case_clause_selected(
                    cx,
                    case_selector_value,
                    discriminant_value
                ));
            }

            if is_found_after_default {
                eval_case!(case);
            }
        }
    }

    if is_found_after_default {
        return v.into();
    }

    // Evaluate the default case
    let default_case = &stmt.cases[default_case_index];
    eval_case!(default_case);

    // Now evaluate all cases after the default case
    for case in &stmt.cases[default_case_index + 1..] {
        eval_case!(case);
    }

    v.into()
}

// 14.12.3 CaseClauseIsSelected
fn is_case_clause_selected(
    cx: &mut Context,
    case_selector_value: &ast::Expression,
    discriminant_value: Value,
) -> EvalResult<bool> {
    let case_selector_value = maybe!(eval_expression(cx, case_selector_value));
    is_strictly_equal(discriminant_value, case_selector_value).into()
}

// 14.12.4 Switch Statement Evaluation
fn eval_switch_statement(
    cx: &mut Context,
    stmt: &ast::SwitchStatement,
    stmt_label_id: LabelId,
) -> Completion {
    let discriminant_value = maybe__!(eval_expression(cx, &stmt.discriminant));

    let mut current_execution_context = cx.current_execution_context();
    let old_env = current_execution_context.lexical_env;
    let block_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(old_env)));

    block_declaration_instantiation(cx, &stmt.lex_decls, block_env);

    current_execution_context.lexical_env = to_trait_object(block_env);

    let completion = eval_case_block(cx, stmt, discriminant_value);

    current_execution_context.lexical_env = old_env;

    // Inline labeled statement evaluation break handling
    if completion.kind() == CompletionKind::Break {
        let label = completion.label();
        if (label == EMPTY_LABEL) || (label == stmt_label_id) {
            return Completion::normal(completion.value());
        } else {
            return completion;
        }
    }

    completion
}

// 14.13.4 Labeled Statement Evaluation
fn eval_labeled_statement(cx: &mut Context, stmt: &ast::LabeledStatement) -> Completion {
    let label_id = stmt.label.id;

    // Find the innermost labeled statement
    let mut inner_stmt = stmt;
    while let ast::Statement::Labeled(labeled_stmt) = inner_stmt.body.as_ref() {
        inner_stmt = labeled_stmt;
    }

    match inner_stmt.body.as_ref() {
        // Breakable statements handle break completion within statement evaluation
        ast::Statement::While(stmt) => eval_while_statement(cx, stmt, label_id),
        ast::Statement::DoWhile(stmt) => eval_do_while_statement(cx, stmt, label_id),
        ast::Statement::Switch(stmt) => eval_switch_statement(cx, stmt, label_id),
        ast::Statement::For(stmt) => eval_for_statement(cx, stmt, label_id),
        ast::Statement::ForEach(stmt) => {
            if stmt.kind == ast::ForEachKind::In {
                eval_for_in_statement(cx, stmt, label_id)
            } else {
                unimplemented!("for of statement")
            }
        }
        _ => {
            // Only labeled breaks allowed for all other statements
            let completion = eval_statement(cx, inner_stmt.body.as_ref());
            if completion.kind() == CompletionKind::Break && completion.label() == label_id {
                return Completion::normal(completion.value());
            } else {
                return completion;
            }
        }
    }
}

// 14.14.1 Throw Statement Evaluation
fn eval_throw_statement(cx: &mut Context, stmt: &ast::ThrowStatement) -> Completion {
    let value = maybe__!(eval_expression(cx, &stmt.argument));
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

            must!(param.iter_bound_names(&mut |id| {
                catch_env.create_mutable_binding(cx, id.name.clone(), false)
            }));

            current_context.lexical_env = catch_env;

            let binding_init_result =
                binding_initialization(cx, param, thrown_value, Some(catch_env));

            // Make sure to remove new environment if binding initialization fails
            if let EvalResult::Throw(throw_value) = binding_init_result {
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
