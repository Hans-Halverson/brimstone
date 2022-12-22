use super::{ast::*, loc::Loc};

pub trait AstVisitor: Sized {
    fn visit_program(&mut self, program: &Program) {
        default_visit_program(self, program)
    }

    fn visit_toplevel(&mut self, toplevel: &Toplevel) {
        default_visit_toplevel(self, toplevel)
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => self.visit_variable_declaration(var_decl),
            Statement::FuncDecl(func_decl) => self.visit_function(func_decl),
            Statement::Expr(expr) => self.visit_expression_statement(expr),
            Statement::Block(stmt) => self.visit_block(stmt),
            Statement::If(stmt) => self.visit_if_statement(stmt),
            Statement::Switch(stmt) => self.visit_switch_statement(stmt),
            Statement::For(stmt) => self.visit_for_statement(stmt),
            Statement::ForEach(stmt) => self.visit_for_each_statement(stmt),
            Statement::While(stmt) => self.visit_while_statement(stmt),
            Statement::DoWhile(stmt) => self.visit_do_while_statement(stmt),
            Statement::With(stmt) => self.visit_with_statement(stmt),
            Statement::Try(stmt) => self.visit_try_statement(stmt),
            Statement::Throw(stmt) => self.visit_throw_statement(stmt),
            Statement::Return(stmt) => self.visit_return_statement(stmt),
            Statement::Break(stmt) => self.visit_break_statement(stmt),
            Statement::Continue(stmt) => self.visit_continue_statement(stmt),
            Statement::Labeled(stmt) => self.visit_labeled_statement(stmt),
            Statement::Empty(stmt) => self.visit_empty_statement(stmt),
            Statement::Debugger(stmt) => self.visit_debugger_statement(stmt),
        }
    }

    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Id(id) => self.visit_identifier(id),
            Expression::Null(lit) => self.visit_null_literal(lit),
            Expression::Boolean(lit) => self.visit_boolean_literal(lit),
            Expression::Number(lit) => self.visit_number_literal(lit),
            Expression::String(lit) => self.visit_string_literal(lit),
            Expression::Unary(unary) => self.visit_unary_expression(unary),
            Expression::Binary(binary) => self.visit_binary_expression(binary),
            Expression::Logical(logical) => self.visit_logical_expression(logical),
            Expression::Assign(assign) => self.visit_assignment_expression(assign),
            Expression::Update(update) => self.visit_update_expression(update),
            Expression::Member(member) => self.visit_member_expression(member),
            Expression::Conditional(cond) => self.visit_conditional_expression(cond),
            Expression::Call(call) => self.visit_call_expression(call),
            Expression::New(new) => self.visit_new_expression(new),
            Expression::Sequence(seq) => self.visit_sequence_expression(seq),
            Expression::This(loc) => self.visit_this_expression(&loc),
            Expression::Array(arr) => self.visit_array_expression(arr),
            Expression::Object(arr) => self.visit_object_expression(arr),
            Expression::Function(func) => self.visit_function(func),
            Expression::ArrowFunction(func) => self.visit_function(func),
            Expression::Await(expr) => self.visit_await_expression(expr),
            Expression::Yield(expr) => self.visit_yield_expression(expr),
        }
    }

    fn visit_pattern(&mut self, patt: &Pattern) {
        match patt {
            Pattern::Id(id) => self.visit_identifier(id),
            Pattern::Array(patt) => self.visit_array_pattern(patt),
            Pattern::Object(patt) => self.visit_object_pattern(patt),
            Pattern::Assign(patt) => self.visit_assignment_pattern(patt),
        }
    }

    fn visit_identifier(&mut self, _: &Identifier) {}

    fn visit_variable_declaration(&mut self, var_decl: &VariableDeclaration) {
        default_visit_variable_declaration(self, var_decl)
    }

    fn visit_variable_declarator(&mut self, decl: &VariableDeclarator) {
        default_visit_variable_declarator(self, decl)
    }

    fn visit_function(&mut self, func: &Function) {
        default_visit_function(self, func)
    }

    fn visit_function_body(&mut self, body: &FunctionBody) {
        default_visit_function_body(self, body)
    }

    fn visit_block(&mut self, block: &Block) {
        default_visit_block(self, block)
    }

    fn visit_expression_statement(&mut self, stmt: &ExpressionStatement) {
        default_visit_expression_statement(self, stmt)
    }

    fn visit_if_statement(&mut self, stmt: &IfStatement) {
        default_visit_if_statement(self, stmt)
    }

    fn visit_switch_statement(&mut self, stmt: &SwitchStatement) {
        default_visit_switch_statement(self, stmt)
    }

    fn visit_switch_case(&mut self, case: &SwitchCase) {
        default_visit_switch_case(self, case)
    }

    fn visit_for_statement(&mut self, stmt: &ForStatement) {
        default_visit_for_statement(self, stmt)
    }

    fn visit_for_init(&mut self, init: &ForInit) {
        default_visit_for_init(self, init)
    }

    fn visit_for_each_statement(&mut self, stmt: &ForEachStatement) {
        default_visit_for_each_statement(self, stmt)
    }

    fn visit_for_each_init(&mut self, init: &ForEachInit) {
        default_visit_for_each_init(self, init)
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) {
        default_visit_while_statement(self, stmt)
    }

    fn visit_do_while_statement(&mut self, stmt: &DoWhileStatement) {
        default_visit_do_while_statement(self, stmt)
    }

    fn visit_with_statement(&mut self, stmt: &WithStatement) {
        default_visit_with_statement(self, stmt)
    }

    fn visit_try_statement(&mut self, stmt: &TryStatement) {
        default_visit_try_statement(self, stmt)
    }

    fn visit_catch_clause(&mut self, catch: &CatchClause) {
        default_visit_catch_clause(self, catch)
    }

    fn visit_throw_statement(&mut self, stmt: &ThrowStatement) {
        default_visit_throw_statement(self, stmt)
    }

    fn visit_return_statement(&mut self, stmt: &ReturnStatement) {
        default_visit_return_statement(self, stmt)
    }

    fn visit_break_statement(&mut self, _: &BreakStatement) {}

    fn visit_continue_statement(&mut self, _: &ContinueStatement) {}

    fn visit_labeled_statement(&mut self, stmt: &LabeledStatement) {
        default_visit_labeled_statement(self, stmt)
    }

    fn visit_empty_statement(&mut self, _: &Loc) {}

    fn visit_debugger_statement(&mut self, _: &Loc) {}

    fn visit_null_literal(&mut self, _: &Loc) {}

    fn visit_boolean_literal(&mut self, _: &BooleanLiteral) {}

    fn visit_number_literal(&mut self, _: &NumberLiteral) {}

    fn visit_string_literal(&mut self, _: &StringLiteral) {}

    fn visit_unary_expression(&mut self, expr: &UnaryExpression) {
        default_visit_unary_expression(self, expr)
    }

    fn visit_binary_expression(&mut self, expr: &BinaryExpression) {
        default_visit_binary_expression(self, expr)
    }

    fn visit_logical_expression(&mut self, expr: &LogicalExpression) {
        default_visit_logical_expression(self, expr)
    }

    fn visit_assignment_expression(&mut self, expr: &AssignmentExpression) {
        default_visit_assignment_expression(self, expr)
    }

    fn visit_update_expression(&mut self, expr: &UpdateExpression) {
        default_visit_update_expression(self, expr)
    }

    fn visit_member_expression(&mut self, expr: &MemberExpression) {
        default_visit_member_expression(self, expr)
    }

    fn visit_conditional_expression(&mut self, expr: &ConditionalExpression) {
        default_visit_conditional_expression(self, expr)
    }

    fn visit_call_expression(&mut self, expr: &CallExpression) {
        default_visit_call_expression(self, expr)
    }

    fn visit_new_expression(&mut self, expr: &NewExpression) {
        default_visit_new_expression(self, expr)
    }

    fn visit_sequence_expression(&mut self, expr: &SequenceExpression) {
        default_visit_sequence_expression(self, expr)
    }

    fn visit_this_expression(&mut self, _: &Loc) {}

    fn visit_array_expression(&mut self, expr: &ArrayExpression) {
        default_visit_array_expression(self, expr)
    }

    fn visit_object_expression(&mut self, expr: &ObjectExpression) {
        default_visit_object_expression(self, expr)
    }

    fn visit_property(&mut self, prop: &Property) {
        default_visit_property(self, prop)
    }

    fn visit_await_expression(&mut self, expr: &AwaitExpression) {
        default_visit_await_expression(self, expr)
    }

    fn visit_yield_expression(&mut self, expr: &YieldExpression) {
        default_visit_yield_expression(self, expr)
    }

    fn visit_array_pattern(&mut self, patt: &ArrayPattern) {
        default_visit_array_pattern(self, patt)
    }

    fn visit_object_pattern(&mut self, patt: &ObjectPattern) {
        default_visit_object_pattern(self, patt)
    }

    fn visit_object_pattern_property(&mut self, prop: &ObjectPatternProperty) {
        default_visit_object_pattern_property(self, prop)
    }

    fn visit_assignment_pattern(&mut self, patt: &AssignmentPattern) {
        default_visit_assignment_pattern(self, patt)
    }
}

macro_rules! visit_vec {
    ($visitor:expr, $field:expr, $func:ident) => {
        for node in &$field {
            $visitor.$func(node)
        }
    };
}

macro_rules! visit_opt {
    ($visitor:expr, $field:expr, $func:ident) => {
        if let Some(ref node) = $field {
            $visitor.$func(node)
        }
    };
}

pub fn default_visit_program<V: AstVisitor>(visitor: &mut V, program: &Program) {
    visit_vec!(visitor, program.toplevels, visit_toplevel)
}

pub fn default_visit_toplevel<V: AstVisitor>(visitor: &mut V, toplevel: &Toplevel) {
    match toplevel {
        Toplevel::Statement(stmt) => visitor.visit_statement(stmt),
    }
}

pub fn default_visit_variable_declaration<V: AstVisitor>(
    visitor: &mut V,
    decl: &VariableDeclaration,
) {
    visit_vec!(visitor, decl.declarations, visit_variable_declarator)
}

pub fn default_visit_variable_declarator<V: AstVisitor>(
    visitor: &mut V,
    decl: &VariableDeclarator,
) {
    visitor.visit_pattern(&decl.id);
    visit_opt!(visitor, decl.init, visit_expression);
}

pub fn default_visit_function<V: AstVisitor>(visitor: &mut V, func: &Function) {
    visit_opt!(visitor, func.id, visit_identifier);
    visit_vec!(visitor, func.params, visit_pattern);
    visitor.visit_function_body(&func.body);
}

pub fn default_visit_function_body<V: AstVisitor>(visitor: &mut V, body: &FunctionBody) {
    match body {
        FunctionBody::Block(ref block) => visitor.visit_block(block),
        FunctionBody::Expression(ref expr) => visitor.visit_expression(expr),
    }
}

pub fn default_visit_block<V: AstVisitor>(visitor: &mut V, block: &Block) {
    visit_vec!(visitor, block.body, visit_statement);
}

pub fn default_visit_expression_statement<V: AstVisitor>(
    visitor: &mut V,
    stmt: &ExpressionStatement,
) {
    visitor.visit_expression(&stmt.expr);
}

pub fn default_visit_if_statement<V: AstVisitor>(visitor: &mut V, stmt: &IfStatement) {
    visitor.visit_expression(&stmt.test);
    visitor.visit_statement(&stmt.conseq);
    visit_opt!(visitor, stmt.altern, visit_statement);
}

pub fn default_visit_switch_statement<V: AstVisitor>(visitor: &mut V, stmt: &SwitchStatement) {
    visitor.visit_expression(&stmt.discriminant);
    visit_vec!(visitor, stmt.cases, visit_switch_case);
}

pub fn default_visit_switch_case<V: AstVisitor>(visitor: &mut V, case: &SwitchCase) {
    visit_opt!(visitor, case.test, visit_expression);
    visit_vec!(visitor, case.body, visit_statement);
}

pub fn default_visit_for_statement<V: AstVisitor>(visitor: &mut V, stmt: &ForStatement) {
    visit_opt!(visitor, stmt.init, visit_for_init);
    visit_opt!(visitor, stmt.test, visit_expression);
    visit_opt!(visitor, stmt.update, visit_expression);
    visitor.visit_statement(&stmt.body);
}

pub fn default_visit_for_init<V: AstVisitor>(visitor: &mut V, init: &ForInit) {
    match init {
        ForInit::Expression(expr) => visitor.visit_expression(expr),
        ForInit::VarDecl(decl) => visitor.visit_variable_declaration(decl),
    }
}

pub fn default_visit_for_each_statement<V: AstVisitor>(visitor: &mut V, stmt: &ForEachStatement) {
    visitor.visit_for_each_init(&stmt.left);
    visitor.visit_expression(&stmt.right);
    visitor.visit_statement(&stmt.body);
}

pub fn default_visit_for_each_init<V: AstVisitor>(visitor: &mut V, init: &ForEachInit) {
    match init {
        ForEachInit::Pattern(patt) => visitor.visit_pattern(patt),
        ForEachInit::VarDecl(decl) => visitor.visit_variable_declaration(decl),
    }
}

pub fn default_visit_while_statement<V: AstVisitor>(visitor: &mut V, stmt: &WhileStatement) {
    visitor.visit_expression(&stmt.test);
    visitor.visit_statement(&stmt.body);
}

pub fn default_visit_do_while_statement<V: AstVisitor>(visitor: &mut V, stmt: &DoWhileStatement) {
    visitor.visit_expression(&stmt.test);
    visitor.visit_statement(&stmt.body);
}

pub fn default_visit_with_statement<V: AstVisitor>(visitor: &mut V, stmt: &WithStatement) {
    visitor.visit_expression(&stmt.object);
    visitor.visit_statement(&stmt.body);
}

pub fn default_visit_try_statement<V: AstVisitor>(visitor: &mut V, stmt: &TryStatement) {
    visitor.visit_block(&stmt.block);
    visit_opt!(visitor, stmt.handler, visit_catch_clause);
    visit_opt!(visitor, stmt.finalizer, visit_block);
}

pub fn default_visit_catch_clause<V: AstVisitor>(visitor: &mut V, catch: &CatchClause) {
    visit_opt!(visitor, catch.param, visit_pattern);
    visitor.visit_block(&catch.body);
}

pub fn default_visit_throw_statement<V: AstVisitor>(visitor: &mut V, stmt: &ThrowStatement) {
    visitor.visit_expression(&stmt.argument)
}

pub fn default_visit_return_statement<V: AstVisitor>(visitor: &mut V, stmt: &ReturnStatement) {
    visit_opt!(visitor, stmt.argument, visit_expression)
}

pub fn default_visit_labeled_statement<V: AstVisitor>(visitor: &mut V, stmt: &LabeledStatement) {
    visitor.visit_statement(&stmt.body)
}

pub fn default_visit_unary_expression<V: AstVisitor>(visitor: &mut V, expr: &UnaryExpression) {
    visitor.visit_expression(&expr.argument)
}

pub fn default_visit_binary_expression<V: AstVisitor>(visitor: &mut V, expr: &BinaryExpression) {
    visitor.visit_expression(&expr.left);
    visitor.visit_expression(&expr.right);
}

pub fn default_visit_logical_expression<V: AstVisitor>(visitor: &mut V, expr: &LogicalExpression) {
    visitor.visit_expression(&expr.left);
    visitor.visit_expression(&expr.right);
}

pub fn default_visit_assignment_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &AssignmentExpression,
) {
    visitor.visit_expression(&expr.left);
    visitor.visit_expression(&expr.right);
}

pub fn default_visit_update_expression<V: AstVisitor>(visitor: &mut V, expr: &UpdateExpression) {
    visitor.visit_expression(&expr.argument);
}

pub fn default_visit_member_expression<V: AstVisitor>(visitor: &mut V, expr: &MemberExpression) {
    visitor.visit_expression(&expr.object);
    visitor.visit_expression(&expr.property);
}

pub fn default_visit_conditional_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &ConditionalExpression,
) {
    visitor.visit_expression(&expr.test);
    visitor.visit_expression(&expr.conseq);
    visitor.visit_expression(&expr.altern);
}

pub fn default_visit_call_expression<V: AstVisitor>(visitor: &mut V, expr: &CallExpression) {
    visitor.visit_expression(&expr.callee);
    visit_vec!(visitor, expr.arguments, visit_expression);
}

pub fn default_visit_new_expression<V: AstVisitor>(visitor: &mut V, expr: &NewExpression) {
    visitor.visit_expression(&expr.callee);
    visit_vec!(visitor, expr.arguments, visit_expression);
}

pub fn default_visit_sequence_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &SequenceExpression,
) {
    visit_vec!(visitor, expr.expressions, visit_expression)
}

pub fn default_visit_array_expression<V: AstVisitor>(visitor: &mut V, expr: &ArrayExpression) {
    for element in &expr.elements {
        match element {
            None => {}
            Some(element) => visitor.visit_expression(element),
        }
    }
}

pub fn default_visit_object_expression<V: AstVisitor>(visitor: &mut V, expr: &ObjectExpression) {
    visit_vec!(visitor, expr.properties, visit_property)
}

pub fn default_visit_property<V: AstVisitor>(visitor: &mut V, prop: &Property) {
    visitor.visit_expression(&prop.key);
    visit_opt!(visitor, prop.value, visit_expression);
}

pub fn default_visit_await_expression<V: AstVisitor>(visitor: &mut V, expr: &AwaitExpression) {
    visitor.visit_expression(&expr.argument)
}

pub fn default_visit_yield_expression<V: AstVisitor>(visitor: &mut V, expr: &YieldExpression) {
    visit_opt!(visitor, expr.argument, visit_expression)
}

pub fn default_visit_array_pattern<V: AstVisitor>(visitor: &mut V, patt: &ArrayPattern) {
    for element in &patt.elements {
        match element {
            None => {}
            Some(element) => visitor.visit_pattern(element),
        }
    }
}

pub fn default_visit_object_pattern<V: AstVisitor>(visitor: &mut V, patt: &ObjectPattern) {
    visit_vec!(visitor, patt.properties, visit_object_pattern_property)
}

pub fn default_visit_object_pattern_property<V: AstVisitor>(
    visitor: &mut V,
    prop: &ObjectPatternProperty,
) {
    visit_opt!(visitor, prop.key, visit_expression);
    visitor.visit_pattern(&prop.value);
}

pub fn default_visit_assignment_pattern<V: AstVisitor>(visitor: &mut V, patt: &AssignmentPattern) {
    visitor.visit_pattern(&patt.left);
    visitor.visit_expression(&patt.right);
}
