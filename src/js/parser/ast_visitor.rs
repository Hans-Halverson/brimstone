use super::{ast::*, loc::Loc};

pub trait AstVisitor: Sized {
    fn visit_program(&mut self, program: &mut Program) {
        default_visit_program(self, program)
    }

    fn visit_toplevel(&mut self, toplevel: &mut Toplevel) {
        default_visit_toplevel(self, toplevel)
    }

    fn visit_statement(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => self.visit_variable_declaration(var_decl),
            Statement::FuncDecl(func_decl) => self.visit_function_declaration(func_decl),
            Statement::ClassDecl(class_decl) => self.visit_class_declaration(class_decl),
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

    fn visit_expression(&mut self, expr: &mut Expression) {
        match expr {
            Expression::Id(id) => self.visit_identifier_expression(id),
            Expression::Null(lit) => self.visit_null_literal(lit),
            Expression::Boolean(lit) => self.visit_boolean_literal(lit),
            Expression::Number(lit) => self.visit_number_literal(lit),
            Expression::String(lit) => self.visit_string_literal(lit),
            Expression::BigInt(lit) => self.visit_bigint_literal(lit),
            Expression::RegExp(lit) => self.visit_regexp_literal(lit),
            Expression::Unary(unary) => self.visit_unary_expression(unary),
            Expression::Binary(binary) => self.visit_binary_expression(binary),
            Expression::Logical(logical) => self.visit_logical_expression(logical),
            Expression::Assign(assign) => self.visit_assignment_expression(assign),
            Expression::Update(update) => self.visit_update_expression(update),
            Expression::Member(member) => self.visit_member_expression(member),
            Expression::Chain(chain) => self.visit_chain_expression(chain),
            Expression::Conditional(cond) => self.visit_conditional_expression(cond),
            Expression::Call(call) => self.visit_call_expression(call),
            Expression::New(new) => self.visit_new_expression(new),
            Expression::Sequence(seq) => self.visit_sequence_expression(seq),
            Expression::This(loc) => self.visit_this_expression(loc),
            Expression::Array(arr) => self.visit_array_expression(arr),
            Expression::Object(arr) => self.visit_object_expression(arr),
            Expression::Function(func) => self.visit_function_expression(func),
            Expression::ArrowFunction(func) => self.visit_arrow_function(func),
            Expression::Class(class) => self.visit_class_expression(class),
            Expression::Await(expr) => self.visit_await_expression(expr),
            Expression::Yield(expr) => self.visit_yield_expression(expr),
            Expression::SuperMember(expr) => self.visit_super_member_expression(expr),
            Expression::SuperCall(expr) => self.visit_super_call_expression(expr),
            Expression::Template(lit) => self.visit_template_literal(lit),
            Expression::TaggedTemplate(expr) => self.visit_tagged_template_expression(expr),
            Expression::MetaProperty(expr) => self.visit_meta_property(expr),
            Expression::Import(expr) => self.visit_import_expression(expr),
        }
    }

    fn visit_pattern(&mut self, patt: &mut Pattern) {
        match patt {
            Pattern::Id(id) => self.visit_identifier_pattern(id),
            Pattern::Array(patt) => self.visit_array_pattern(patt),
            Pattern::Object(patt) => self.visit_object_pattern(patt),
            Pattern::Assign(patt) => self.visit_assignment_pattern(patt),
            Pattern::Member(expr) => self.visit_member_expression(expr),
            Pattern::SuperMember(expr) => self.visit_super_member_expression(expr),
        }
    }

    fn visit_identifier(&mut self, _: &mut Identifier) {}

    fn visit_outer_expression(&mut self, expr: &mut OuterExpression) {
        default_visit_outer_expression(self, expr)
    }

    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration) {
        default_visit_variable_declaration(self, var_decl)
    }

    fn visit_variable_declarator(&mut self, decl: &mut VariableDeclarator) {
        default_visit_variable_declarator(self, decl)
    }

    fn visit_function_declaration(&mut self, func_decl: &mut Function) {
        default_visit_function_declaration(self, func_decl)
    }

    fn visit_function(&mut self, func: &mut Function) {
        default_visit_function(self, func)
    }

    fn visit_function_param(&mut self, param: &mut FunctionParam) {
        default_visit_function_param(self, param)
    }

    fn visit_function_body(&mut self, body: &mut FunctionBody) {
        default_visit_function_body(self, body)
    }

    fn visit_function_block_body(&mut self, block_body: &mut FunctionBlockBody) {
        default_visit_function_block_body(self, block_body)
    }

    fn visit_class_declaration(&mut self, class_decl: &mut Class) {
        default_visit_class_declaration(self, class_decl)
    }

    fn visit_class(&mut self, class: &mut Class) {
        default_visit_class(self, class)
    }

    fn visit_class_element(&mut self, element: &mut ClassElement) {
        default_visit_class_element(self, element)
    }

    fn visit_class_method(&mut self, method: &mut ClassMethod) {
        default_visit_class_method(self, method)
    }

    fn visit_class_property(&mut self, prop: &mut ClassProperty) {
        default_visit_class_property(self, prop)
    }

    fn visit_block(&mut self, block: &mut Block) {
        default_visit_block(self, block)
    }

    fn visit_expression_statement(&mut self, stmt: &mut ExpressionStatement) {
        default_visit_expression_statement(self, stmt)
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStatement) {
        default_visit_if_statement(self, stmt)
    }

    fn visit_switch_statement(&mut self, stmt: &mut SwitchStatement) {
        default_visit_switch_statement(self, stmt)
    }

    fn visit_switch_case(&mut self, case: &mut SwitchCase) {
        default_visit_switch_case(self, case)
    }

    fn visit_for_statement(&mut self, stmt: &mut ForStatement) {
        default_visit_for_statement(self, stmt)
    }

    fn visit_for_init(&mut self, init: &mut ForInit) {
        default_visit_for_init(self, init)
    }

    fn visit_for_each_statement(&mut self, stmt: &mut ForEachStatement) {
        default_visit_for_each_statement(self, stmt)
    }

    fn visit_for_each_init(&mut self, init: &mut ForEachInit) {
        default_visit_for_each_init(self, init)
    }

    fn visit_while_statement(&mut self, stmt: &mut WhileStatement) {
        default_visit_while_statement(self, stmt)
    }

    fn visit_do_while_statement(&mut self, stmt: &mut DoWhileStatement) {
        default_visit_do_while_statement(self, stmt)
    }

    fn visit_with_statement(&mut self, stmt: &mut WithStatement) {
        default_visit_with_statement(self, stmt)
    }

    fn visit_try_statement(&mut self, stmt: &mut TryStatement) {
        default_visit_try_statement(self, stmt)
    }

    fn visit_catch_clause(&mut self, catch: &mut CatchClause) {
        default_visit_catch_clause(self, catch)
    }

    fn visit_throw_statement(&mut self, stmt: &mut ThrowStatement) {
        default_visit_throw_statement(self, stmt)
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) {
        default_visit_return_statement(self, stmt)
    }

    fn visit_break_statement(&mut self, _: &mut BreakStatement) {}

    fn visit_continue_statement(&mut self, _: &mut ContinueStatement) {}

    fn visit_labeled_statement(&mut self, stmt: &mut LabeledStatement) {
        default_visit_labeled_statement(self, stmt)
    }

    fn visit_empty_statement(&mut self, _: &mut Loc) {}

    fn visit_debugger_statement(&mut self, _: &mut Loc) {}

    fn visit_identifier_expression(&mut self, id: &mut Identifier) {
        default_visit_identifier_expression(self, id)
    }

    fn visit_null_literal(&mut self, _: &mut Loc) {}

    fn visit_boolean_literal(&mut self, _: &mut BooleanLiteral) {}

    fn visit_number_literal(&mut self, _: &mut NumberLiteral) {}

    fn visit_string_literal(&mut self, _: &mut StringLiteral) {}

    fn visit_bigint_literal(&mut self, _: &mut BigIntLiteral) {}

    fn visit_regexp_literal(&mut self, _: &mut RegExpLiteral) {}

    fn visit_unary_expression(&mut self, expr: &mut UnaryExpression) {
        default_visit_unary_expression(self, expr)
    }

    fn visit_binary_expression(&mut self, expr: &mut BinaryExpression) {
        default_visit_binary_expression(self, expr)
    }

    fn visit_logical_expression(&mut self, expr: &mut LogicalExpression) {
        default_visit_logical_expression(self, expr)
    }

    fn visit_assignment_expression(&mut self, expr: &mut AssignmentExpression) {
        default_visit_assignment_expression(self, expr)
    }

    fn visit_update_expression(&mut self, expr: &mut UpdateExpression) {
        default_visit_update_expression(self, expr)
    }

    fn visit_member_expression(&mut self, expr: &mut MemberExpression) {
        default_visit_member_expression(self, expr)
    }

    fn visit_chain_expression(&mut self, expr: &mut ChainExpression) {
        default_visit_chain_expression(self, expr)
    }

    fn visit_conditional_expression(&mut self, expr: &mut ConditionalExpression) {
        default_visit_conditional_expression(self, expr)
    }

    fn visit_call_expression(&mut self, expr: &mut CallExpression) {
        default_visit_call_expression(self, expr)
    }

    fn visit_call_argument(&mut self, argument: &mut CallArgument) {
        default_visit_call_argument(self, argument)
    }

    fn visit_new_expression(&mut self, expr: &mut NewExpression) {
        default_visit_new_expression(self, expr)
    }

    fn visit_sequence_expression(&mut self, expr: &mut SequenceExpression) {
        default_visit_sequence_expression(self, expr)
    }

    fn visit_this_expression(&mut self, _: &mut ThisExpression) {}

    fn visit_array_expression(&mut self, expr: &mut ArrayExpression) {
        default_visit_array_expression(self, expr)
    }

    fn visit_spread_element(&mut self, spread: &mut SpreadElement) {
        default_visit_spread_element(self, spread)
    }

    fn visit_object_expression(&mut self, expr: &mut ObjectExpression) {
        default_visit_object_expression(self, expr)
    }

    fn visit_property(&mut self, prop: &mut Property) {
        default_visit_property(self, prop)
    }

    fn visit_function_expression(&mut self, func: &mut Function) {
        default_visit_function_expression(self, func)
    }

    fn visit_arrow_function(&mut self, func: &mut Function) {
        default_visit_arrow_function(self, func)
    }

    fn visit_class_expression(&mut self, class: &mut Class) {
        default_visit_class_expression(self, class)
    }

    fn visit_await_expression(&mut self, expr: &mut AwaitExpression) {
        default_visit_await_expression(self, expr)
    }

    fn visit_yield_expression(&mut self, expr: &mut YieldExpression) {
        default_visit_yield_expression(self, expr)
    }

    fn visit_super_member_expression(&mut self, expr: &mut SuperMemberExpression) {
        default_visit_super_member_expression(self, expr)
    }

    fn visit_super_call_expression(&mut self, expr: &mut SuperCallExpression) {
        default_visit_super_call_expression(self, expr)
    }

    fn visit_template_literal(&mut self, expr: &mut TemplateLiteral) {
        default_visit_template_literal(self, expr)
    }

    fn visit_template_element(&mut self, _: &mut TemplateElement) {}

    fn visit_tagged_template_expression(&mut self, expr: &mut TaggedTemplateExpression) {
        default_visit_tagged_template_expression(self, expr)
    }

    fn visit_meta_property(&mut self, _: &mut MetaProperty) {}

    fn visit_import_expression(&mut self, expr: &mut ImportExpression) {
        default_visit_import_expression(self, expr)
    }

    fn visit_identifier_pattern(&mut self, id: &mut Identifier) {
        default_visit_identifier_pattern(self, id)
    }

    fn visit_array_pattern(&mut self, patt: &mut ArrayPattern) {
        default_visit_array_pattern(self, patt)
    }

    fn visit_rest_element(&mut self, rest: &mut RestElement) {
        default_visit_rest_element(self, rest)
    }

    fn visit_object_pattern(&mut self, patt: &mut ObjectPattern) {
        default_visit_object_pattern(self, patt)
    }

    fn visit_object_pattern_property(&mut self, prop: &mut ObjectPatternProperty) {
        default_visit_object_pattern_property(self, prop)
    }

    fn visit_assignment_pattern(&mut self, patt: &mut AssignmentPattern) {
        default_visit_assignment_pattern(self, patt)
    }

    fn visit_import_declaration(&mut self, import: &mut ImportDeclaration) {
        default_visit_import_declaration(self, import)
    }

    fn visit_import_specifier(&mut self, spec: &mut ImportSpecifier) {
        default_visit_import_specifier(self, spec)
    }

    fn visit_import_default_specifier(&mut self, spec: &mut ImportDefaultSpecifier) {
        default_visit_import_default_specifier(self, spec)
    }

    fn visit_import_named_specifier(&mut self, spec: &mut ImportNamedSpecifier) {
        default_visit_import_named_specifier(self, spec)
    }

    fn visit_import_namespace_specifier(&mut self, spec: &mut ImportNamespaceSpecifier) {
        default_visit_import_namespace_specifier(self, spec)
    }

    fn visit_export_default_declaration(&mut self, export: &mut ExportDefaultDeclaration) {
        default_visit_export_default_declaration(self, export)
    }

    fn visit_export_default_kind(&mut self, kind: &mut ExportDefaultKind) {
        default_visit_export_default_kind(self, kind)
    }

    fn visit_export_named_declaration(&mut self, export: &mut ExportNamedDeclaration) {
        default_visit_export_named_declaration(self, export)
    }

    fn visit_export_specifier(&mut self, spec: &mut ExportSpecifier) {
        default_visit_export_specifier(self, spec)
    }

    fn visit_export_all_declaration(&mut self, export: &mut ExportAllDeclaration) {
        default_visit_export_all_declaration(self, export)
    }

    fn visit_export_name(&mut self, _: &mut ExportName) {}
}

#[macro_export]
macro_rules! visit_vec {
    ($visitor:expr, $field:expr, $func:ident) => {
        for node in &mut $field {
            $visitor.$func(node)
        }
    };
}

#[macro_export]
macro_rules! visit_opt {
    ($visitor:expr, $field:expr, $func:ident) => {
        if let Some(ref mut node) = $field {
            $visitor.$func(node)
        }
    };
}

pub fn default_visit_program<V: AstVisitor>(visitor: &mut V, program: &mut Program) {
    visit_vec!(visitor, program.toplevels, visit_toplevel)
}

pub fn default_visit_toplevel<V: AstVisitor>(visitor: &mut V, toplevel: &mut Toplevel) {
    match toplevel {
        Toplevel::Statement(stmt) => visitor.visit_statement(stmt),
        Toplevel::Import(import) => visitor.visit_import_declaration(import),
        Toplevel::ExportDefault(import) => visitor.visit_export_default_declaration(import),
        Toplevel::ExportNamed(import) => visitor.visit_export_named_declaration(import),
        Toplevel::ExportAll(import) => visitor.visit_export_all_declaration(import),
    }
}

pub fn default_visit_outer_expression<V: AstVisitor>(visitor: &mut V, expr: &mut OuterExpression) {
    visitor.visit_expression(&mut expr.expr)
}

pub fn default_visit_variable_declaration<V: AstVisitor>(
    visitor: &mut V,
    decl: &mut VariableDeclaration,
) {
    visit_vec!(visitor, decl.declarations, visit_variable_declarator)
}

pub fn default_visit_variable_declarator<V: AstVisitor>(
    visitor: &mut V,
    decl: &mut VariableDeclarator,
) {
    visitor.visit_pattern(&mut decl.id);
    visit_opt!(visitor, decl.init, visit_outer_expression);
}

pub fn default_visit_function_declaration<V: AstVisitor>(visitor: &mut V, func: &mut Function) {
    visitor.visit_function(func);
}

pub fn default_visit_function<V: AstVisitor>(visitor: &mut V, func: &mut Function) {
    visit_opt!(visitor, func.id, visit_identifier);
    visit_vec!(visitor, func.params, visit_function_param);
    visitor.visit_function_body(&mut func.body);
}

pub fn default_visit_function_param<V: AstVisitor>(visitor: &mut V, param: &mut FunctionParam) {
    match param {
        FunctionParam::Pattern { ref mut pattern, .. } => visitor.visit_pattern(pattern),
        FunctionParam::Rest { ref mut rest, .. } => visitor.visit_rest_element(rest),
    }
}

pub fn default_visit_function_body<V: AstVisitor>(visitor: &mut V, body: &mut FunctionBody) {
    match body {
        FunctionBody::Block(ref mut block_body) => visitor.visit_function_block_body(block_body),
        FunctionBody::Expression(ref mut expr) => visitor.visit_outer_expression(expr),
    }
}

pub fn default_visit_function_block_body<V: AstVisitor>(
    visitor: &mut V,
    body: &mut FunctionBlockBody,
) {
    visit_vec!(visitor, body.body, visit_statement);
}

pub fn default_visit_class_declaration<V: AstVisitor>(visitor: &mut V, class: &mut Class) {
    visitor.visit_class(class);
}

pub fn default_visit_class<V: AstVisitor>(visitor: &mut V, class: &mut Class) {
    visit_opt!(visitor, class.id, visit_identifier);
    visit_opt!(visitor, class.super_class, visit_outer_expression);
    visit_vec!(visitor, class.body, visit_class_element);
}

pub fn default_visit_class_element<V: AstVisitor>(visitor: &mut V, element: &mut ClassElement) {
    match element {
        ClassElement::Method(method) => visitor.visit_class_method(method),
        ClassElement::Property(prop) => visitor.visit_class_property(prop),
    }
}

pub fn default_visit_class_method<V: AstVisitor>(visitor: &mut V, method: &mut ClassMethod) {
    visitor.visit_outer_expression(&mut method.key);
    visitor.visit_function_expression(&mut method.value);
}

pub fn default_visit_class_property<V: AstVisitor>(visitor: &mut V, prop: &mut ClassProperty) {
    visitor.visit_outer_expression(&mut prop.key);
    visit_opt!(visitor, prop.value, visit_outer_expression);
}

pub fn default_visit_block<V: AstVisitor>(visitor: &mut V, block: &mut Block) {
    visit_vec!(visitor, block.body, visit_statement);
}

pub fn default_visit_expression_statement<V: AstVisitor>(
    visitor: &mut V,
    stmt: &mut ExpressionStatement,
) {
    visitor.visit_outer_expression(&mut stmt.expr);
}

pub fn default_visit_if_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut IfStatement) {
    visitor.visit_outer_expression(&mut stmt.test);
    visitor.visit_statement(&mut stmt.conseq);
    visit_opt!(visitor, stmt.altern, visit_statement);
}

pub fn default_visit_switch_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut SwitchStatement) {
    visitor.visit_outer_expression(&mut stmt.discriminant);
    visit_vec!(visitor, stmt.cases, visit_switch_case);
}

pub fn default_visit_switch_case<V: AstVisitor>(visitor: &mut V, case: &mut SwitchCase) {
    visit_opt!(visitor, case.test, visit_outer_expression);
    visit_vec!(visitor, case.body, visit_statement);
}

pub fn default_visit_for_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut ForStatement) {
    visit_opt!(visitor, stmt.init, visit_for_init);
    visit_opt!(visitor, stmt.test, visit_outer_expression);
    visit_opt!(visitor, stmt.update, visit_outer_expression);
    visitor.visit_statement(&mut stmt.body);
}

pub fn default_visit_for_init<V: AstVisitor>(visitor: &mut V, init: &mut ForInit) {
    match init {
        ForInit::Expression(expr) => visitor.visit_outer_expression(expr),
        ForInit::VarDecl(decl) => visitor.visit_variable_declaration(decl),
    }
}

pub fn default_visit_for_each_statement<V: AstVisitor>(
    visitor: &mut V,
    stmt: &mut ForEachStatement,
) {
    visitor.visit_for_each_init(&mut stmt.left);
    visitor.visit_outer_expression(&mut stmt.right);
    visitor.visit_statement(&mut stmt.body);
}

pub fn default_visit_for_each_init<V: AstVisitor>(visitor: &mut V, init: &mut ForEachInit) {
    match init {
        ForEachInit::Pattern { pattern, .. } => visitor.visit_pattern(pattern),
        ForEachInit::VarDecl(decl) => visitor.visit_variable_declaration(decl),
    }
}

pub fn default_visit_while_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut WhileStatement) {
    visitor.visit_outer_expression(&mut stmt.test);
    visitor.visit_statement(&mut stmt.body);
}

pub fn default_visit_do_while_statement<V: AstVisitor>(
    visitor: &mut V,
    stmt: &mut DoWhileStatement,
) {
    visitor.visit_outer_expression(&mut stmt.test);
    visitor.visit_statement(&mut stmt.body);
}

pub fn default_visit_with_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut WithStatement) {
    visitor.visit_outer_expression(&mut stmt.object);
    visitor.visit_statement(&mut stmt.body);
}

pub fn default_visit_try_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut TryStatement) {
    visitor.visit_block(&mut stmt.block);
    visit_opt!(visitor, stmt.handler, visit_catch_clause);
    visit_opt!(visitor, stmt.finalizer, visit_block);
}

pub fn default_visit_catch_clause<V: AstVisitor>(visitor: &mut V, catch: &mut CatchClause) {
    visit_opt!(visitor, catch.param, visit_pattern);
    visitor.visit_block(&mut catch.body);
}

pub fn default_visit_throw_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut ThrowStatement) {
    visitor.visit_outer_expression(&mut stmt.argument)
}

pub fn default_visit_return_statement<V: AstVisitor>(visitor: &mut V, stmt: &mut ReturnStatement) {
    visit_opt!(visitor, stmt.argument, visit_outer_expression)
}

pub fn default_visit_labeled_statement<V: AstVisitor>(
    visitor: &mut V,
    stmt: &mut LabeledStatement,
) {
    visitor.visit_statement(&mut stmt.body)
}

pub fn default_visit_identifier_expression<V: AstVisitor>(visitor: &mut V, id: &mut Identifier) {
    visitor.visit_identifier(id)
}

pub fn default_visit_unary_expression<V: AstVisitor>(visitor: &mut V, expr: &mut UnaryExpression) {
    visitor.visit_expression(&mut expr.argument)
}

pub fn default_visit_binary_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut BinaryExpression,
) {
    visitor.visit_expression(&mut expr.left);
    visitor.visit_expression(&mut expr.right);
}

pub fn default_visit_logical_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut LogicalExpression,
) {
    visitor.visit_expression(&mut expr.left);
    visitor.visit_expression(&mut expr.right);
}

pub fn default_visit_assignment_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut AssignmentExpression,
) {
    visitor.visit_pattern(&mut expr.left);
    visitor.visit_expression(&mut expr.right);
}

pub fn default_visit_update_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut UpdateExpression,
) {
    visitor.visit_expression(&mut expr.argument);
}

pub fn default_visit_member_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut MemberExpression,
) {
    visitor.visit_expression(&mut expr.object);
    visitor.visit_expression(&mut expr.property);
}

pub fn default_visit_chain_expression<V: AstVisitor>(visitor: &mut V, expr: &mut ChainExpression) {
    visitor.visit_expression(&mut expr.expression);
}

pub fn default_visit_conditional_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut ConditionalExpression,
) {
    visitor.visit_expression(&mut expr.test);
    visitor.visit_expression(&mut expr.conseq);
    visitor.visit_expression(&mut expr.altern);
}

pub fn default_visit_call_expression<V: AstVisitor>(visitor: &mut V, expr: &mut CallExpression) {
    visitor.visit_expression(&mut expr.callee);
    visit_vec!(visitor, expr.arguments, visit_call_argument);
}

pub fn default_visit_call_argument<V: AstVisitor>(visitor: &mut V, argument: &mut CallArgument) {
    match argument {
        CallArgument::Expression(expr) => visitor.visit_expression(expr),
        CallArgument::Spread(spread) => visitor.visit_spread_element(spread),
    }
}

pub fn default_visit_new_expression<V: AstVisitor>(visitor: &mut V, expr: &mut NewExpression) {
    visitor.visit_expression(&mut expr.callee);
    visit_vec!(visitor, expr.arguments, visit_call_argument);
}

pub fn default_visit_sequence_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut SequenceExpression,
) {
    visit_vec!(visitor, expr.expressions, visit_expression)
}

pub fn default_visit_array_expression<V: AstVisitor>(visitor: &mut V, expr: &mut ArrayExpression) {
    for element in &mut expr.elements {
        match element {
            ArrayElement::Expression(expr) => visitor.visit_expression(expr),
            ArrayElement::Spread(spread) => visitor.visit_spread_element(spread),
            ArrayElement::Hole => {}
        }
    }
}

pub fn default_visit_spread_element<V: AstVisitor>(visitor: &mut V, spread: &mut SpreadElement) {
    visitor.visit_expression(&mut spread.argument);
}

pub fn default_visit_object_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut ObjectExpression,
) {
    visit_vec!(visitor, expr.properties, visit_property)
}

pub fn default_visit_property<V: AstVisitor>(visitor: &mut V, prop: &mut Property) {
    visitor.visit_expression(&mut prop.key);
    visit_opt!(visitor, prop.value, visit_expression);
}

pub fn default_visit_function_expression<V: AstVisitor>(visitor: &mut V, func: &mut Function) {
    visitor.visit_function(func);
}

pub fn default_visit_arrow_function<V: AstVisitor>(visitor: &mut V, func: &mut Function) {
    visitor.visit_function(func);
}

pub fn default_visit_class_expression<V: AstVisitor>(visitor: &mut V, class: &mut Class) {
    visitor.visit_class(class);
}

pub fn default_visit_await_expression<V: AstVisitor>(visitor: &mut V, expr: &mut AwaitExpression) {
    visitor.visit_expression(&mut expr.argument)
}

pub fn default_visit_yield_expression<V: AstVisitor>(visitor: &mut V, expr: &mut YieldExpression) {
    visit_opt!(visitor, expr.argument, visit_expression)
}

pub fn default_visit_super_member_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut SuperMemberExpression,
) {
    visitor.visit_expression(&mut expr.property);
}

pub fn default_visit_super_call_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut SuperCallExpression,
) {
    visit_vec!(visitor, expr.arguments, visit_call_argument);
}

pub fn default_visit_template_literal<V: AstVisitor>(visitor: &mut V, lit: &mut TemplateLiteral) {
    visitor.visit_template_element(&mut lit.quasis[0]);

    for i in 1..lit.quasis.len() {
        visitor.visit_expression(&mut lit.expressions[i - 1]);
        visitor.visit_template_element(&mut lit.quasis[i]);
    }
}

pub fn default_visit_tagged_template_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut TaggedTemplateExpression,
) {
    visitor.visit_expression(&mut expr.tag);
    visitor.visit_template_literal(&mut expr.quasi);
}

pub fn default_visit_import_expression<V: AstVisitor>(
    visitor: &mut V,
    expr: &mut ImportExpression,
) {
    visitor.visit_expression(&mut expr.source);
}

pub fn default_visit_identifier_pattern<V: AstVisitor>(visitor: &mut V, id: &mut Identifier) {
    visitor.visit_identifier(id);
}

pub fn default_visit_array_pattern<V: AstVisitor>(visitor: &mut V, patt: &mut ArrayPattern) {
    for element in &mut patt.elements {
        match element {
            ArrayPatternElement::Pattern(pattern) => visitor.visit_pattern(pattern),
            ArrayPatternElement::Rest(rest) => visitor.visit_rest_element(rest),
            ArrayPatternElement::Hole => {}
        }
    }
}

pub fn default_visit_rest_element<V: AstVisitor>(visitor: &mut V, patt: &mut RestElement) {
    visitor.visit_pattern(&mut patt.argument);
}

pub fn default_visit_object_pattern<V: AstVisitor>(visitor: &mut V, patt: &mut ObjectPattern) {
    visit_vec!(visitor, patt.properties, visit_object_pattern_property)
}

pub fn default_visit_object_pattern_property<V: AstVisitor>(
    visitor: &mut V,
    prop: &mut ObjectPatternProperty,
) {
    visit_opt!(visitor, prop.key, visit_expression);
    visitor.visit_pattern(&mut prop.value);
}

pub fn default_visit_assignment_pattern<V: AstVisitor>(
    visitor: &mut V,
    patt: &mut AssignmentPattern,
) {
    visitor.visit_pattern(&mut patt.left);
    visitor.visit_expression(&mut patt.right);
}

pub fn default_visit_import_declaration<V: AstVisitor>(
    visitor: &mut V,
    import: &mut ImportDeclaration,
) {
    visit_vec!(visitor, import.specifiers, visit_import_specifier);
    visitor.visit_string_literal(&mut import.source);
}

pub fn default_visit_import_specifier<V: AstVisitor>(visitor: &mut V, spec: &mut ImportSpecifier) {
    match spec {
        ImportSpecifier::Default(spec) => visitor.visit_import_default_specifier(spec),
        ImportSpecifier::Named(spec) => visitor.visit_import_named_specifier(spec),
        ImportSpecifier::Namespace(spec) => visitor.visit_import_namespace_specifier(spec),
    }
}

pub fn default_visit_import_default_specifier<V: AstVisitor>(
    visitor: &mut V,
    spec: &mut ImportDefaultSpecifier,
) {
    visitor.visit_identifier(&mut spec.local);
}

pub fn default_visit_import_named_specifier<V: AstVisitor>(
    visitor: &mut V,
    spec: &mut ImportNamedSpecifier,
) {
    visit_opt!(visitor, spec.imported, visit_export_name);
    visitor.visit_identifier(&mut spec.local);
}

pub fn default_visit_export_default_declaration<V: AstVisitor>(
    visitor: &mut V,
    export: &mut ExportDefaultDeclaration,
) {
    visitor.visit_export_default_kind(&mut export.declaration);
}

pub fn default_visit_export_default_kind<V: AstVisitor>(
    visitor: &mut V,
    kind: &mut ExportDefaultKind,
) {
    match kind {
        ExportDefaultKind::Function(func) => visitor.visit_function_declaration(func),
        ExportDefaultKind::Class(class) => visitor.visit_class_declaration(class),
        ExportDefaultKind::Expression(expr) => visitor.visit_outer_expression(expr),
    }
}

pub fn default_visit_export_named_declaration<V: AstVisitor>(
    visitor: &mut V,
    export: &mut ExportNamedDeclaration,
) {
    visit_opt!(visitor, export.declaration, visit_statement);
    visit_vec!(visitor, export.specifiers, visit_export_specifier);
    visit_opt!(visitor, export.source, visit_string_literal);
}

pub fn default_visit_export_specifier<V: AstVisitor>(visitor: &mut V, spec: &mut ExportSpecifier) {
    visitor.visit_export_name(&mut spec.local);
    visit_opt!(visitor, spec.exported, visit_export_name);
}

pub fn default_visit_export_all_declaration<V: AstVisitor>(
    visitor: &mut V,
    export: &mut ExportAllDeclaration,
) {
    visit_opt!(visitor, export.exported, visit_export_name);
    visitor.visit_string_literal(&mut export.source);
}

pub fn default_visit_import_namespace_specifier<V: AstVisitor>(
    visitor: &mut V,
    spec: &mut ImportNamespaceSpecifier,
) {
    visitor.visit_identifier(&mut spec.local);
}
