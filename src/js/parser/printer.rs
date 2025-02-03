use crate::js::common::unicode::to_string_or_unicode_escape_sequence;
use crate::js::common::wtf_8::Wtf8String;

use super::ast::*;
use super::loc::{find_line_col_for_pos, Loc};
use super::regexp::{
    Alternative, AnonymousGroup, Assertion, Backreference, CaptureGroup, CharacterClass,
    ClassExpressionType, ClassRange, Disjunction, Lookaround, Quantifier, RegExp, RegExpFlags,
    Term,
};
use super::source::Source;

struct Printer<'a> {
    buf: String,
    indent: usize,
    source: &'a Source,
}

impl<'a> Printer<'a> {
    fn new(source: &'a Source) -> Printer<'a> {
        Printer { buf: String::new(), indent: 0, source }
    }

    fn finish(self) -> String {
        self.buf
    }

    fn inc_indent(&mut self) {
        self.indent += 1;
    }

    fn dec_indent(&mut self) {
        self.indent -= 1;
    }

    fn indent(&mut self) {
        self.string(&"  ".repeat(self.indent))
    }

    fn string(&mut self, str: &str) {
        self.buf.push_str(str);
    }

    fn print_wtf8_string(&mut self, string: &Wtf8String) {
        self.buf.push('\"');
        self.buf.push_str(&string.to_string());
        self.buf.push('\"');
    }

    fn print_str(&mut self, string: &str) {
        self.buf.push('\"');
        self.buf.push_str(string);
        self.buf.push('\"');
    }

    fn print_bool(&mut self, value: bool) {
        if value {
            self.string("true")
        } else {
            self.string("false")
        }
    }

    fn print_number(&mut self, value: impl ToString) {
        self.string(&value.to_string())
    }

    fn print_null(&mut self) {
        self.string("null")
    }

    fn print_null_in_property(&mut self, _: ()) {
        self.print_null()
    }

    fn start_node(&mut self, name: &str, loc: &Loc) {
        self.string("{\n");
        self.inc_indent();

        self.property("type", name, Printer::print_str);

        // Calculate line/column offsets for loc
        let line_offsets = self.source.line_offsets();
        let (start_line, start_col) = find_line_col_for_pos(loc.start, line_offsets);
        let (end_line, end_col) = find_line_col_for_pos(loc.end, line_offsets);

        // Write loc as string in concise format
        self.indent();
        self.string(&format!("loc: \"{}:{}-{}:{}\",\n", start_line, start_col, end_line, end_col));
    }

    fn start_regexp_node(&mut self, name: &str) {
        self.string("{\n");
        self.inc_indent();

        self.property("type", name, Printer::print_str);
    }

    fn end_node(&mut self) {
        self.dec_indent();
        self.indent();
        self.string("}");
    }

    fn property<T>(&mut self, name: &str, value: T, print_value_fn: fn(&mut Self, T)) {
        self.indent();

        self.string(name);
        self.string(": ");
        print_value_fn(self, value);

        self.string(",\n");
    }

    fn array_property<T>(
        &mut self,
        name: &str,
        values: &Vec<T>,
        print_value_fn: fn(&mut Self, &T),
    ) {
        self.indent();
        self.string(name);
        self.string(": ");
        if values.is_empty() {
            self.string("[],\n")
        } else {
            self.string("[\n");
            self.inc_indent();

            for value in values {
                self.indent();
                print_value_fn(self, value);
                self.string(",\n");
            }

            self.dec_indent();
            self.indent();
            self.string("],\n");
        }
    }

    fn print_program(&mut self, program: &Program) {
        self.start_node("Program", &program.loc);
        self.array_property("body", &program.toplevels, Printer::print_toplevel);
        self.property("sourceType", &program.kind, Printer::print_program_kind);
        self.property(
            "has_use_strict_directive",
            program.has_use_strict_directive,
            Printer::print_bool,
        );
        self.end_node();
    }

    fn print_program_kind(&mut self, program_kind: &ProgramKind) {
        let str = match program_kind {
            ProgramKind::Script => "script",
            ProgramKind::Module => "module",
        };
        self.print_str(str)
    }

    fn print_toplevel(&mut self, toplevel: &Toplevel) {
        match toplevel {
            Toplevel::Statement(stmt) => self.print_statement(stmt),
            Toplevel::Import(import) => self.print_import_declaration(import),
            Toplevel::ExportDefault(export) => self.print_export_default_declaration(export),
            Toplevel::ExportNamed(export) => self.print_export_named_declaration(export),
            Toplevel::ExportAll(export) => self.print_export_all_declaration(export),
        }
    }

    fn print_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => self.print_variable_declaration(var_decl),
            Statement::FuncDecl(func_decl) => self.print_function(func_decl, "FunctionDeclaration"),
            Statement::ClassDecl(class_decl) => self.print_class(class_decl, "ClassDeclaration"),
            Statement::Expr(expr) => self.print_expression_statement(expr),
            Statement::Block(stmt) => self.print_block(stmt),
            Statement::If(stmt) => self.print_if_statement(stmt),
            Statement::Switch(stmt) => self.print_switch_statement(stmt),
            Statement::For(stmt) => self.print_for_statement(stmt),
            Statement::ForEach(stmt) => self.print_for_each_statement(stmt),
            Statement::While(stmt) => self.print_while_statement(stmt),
            Statement::DoWhile(stmt) => self.print_do_while_statement(stmt),
            Statement::With(stmt) => self.print_with_statement(stmt),
            Statement::Try(stmt) => self.print_try_statement(stmt),
            Statement::Throw(stmt) => self.print_throw_statement(stmt),
            Statement::Return(stmt) => self.print_return_statement(stmt),
            Statement::Break(stmt) => self.print_break_statement(stmt),
            Statement::Continue(stmt) => self.print_continue_statement(stmt),
            Statement::Labeled(stmt) => self.print_labeled_statement(stmt),
            Statement::Empty(stmt) => self.print_empty_statement(stmt),
            Statement::Debugger(stmt) => self.print_debugger_statement(stmt),
        }
    }

    fn print_variable_declaration(&mut self, var_decl: &VariableDeclaration) {
        self.start_node("VariableDeclaration", &var_decl.loc);
        self.property("kind", &var_decl.kind, Printer::print_var_kind);
        self.array_property(
            "declarations",
            var_decl.declarations.as_ref(),
            Printer::print_variable_declarator,
        );
        self.end_node();
    }

    fn print_function_expression(&mut self, func: &Function) {
        self.print_function(func, "FunctionExpression");
    }

    fn print_function(&mut self, func: &Function, name: &str) {
        self.start_node(name, &func.loc);
        self.property("id", func.id.as_ref(), Printer::print_optional_identifier);
        self.array_property("params", func.params.as_ref(), Printer::print_function_param);
        self.property("body", func.body.as_ref(), Printer::print_function_body);
        self.property("async", func.is_async(), Printer::print_bool);
        self.property("generator", func.is_generator(), Printer::print_bool);
        self.property(
            "has_use_strict_directive",
            func.has_use_strict_directive(),
            Printer::print_bool,
        );
        self.end_node();
    }

    fn print_function_param(&mut self, param: &FunctionParam) {
        match param {
            FunctionParam::Pattern { pattern, .. } => self.print_pattern(pattern),
            FunctionParam::Rest { rest, .. } => self.print_rest_element(rest),
        }
    }

    fn print_function_body(&mut self, body: &FunctionBody) {
        match body {
            FunctionBody::Block(block) => self.print_function_block_body(block),
            FunctionBody::Expression(expr) => self.print_outer_expression(expr),
        }
    }

    fn print_function_block_body(&mut self, block: &FunctionBlockBody) {
        self.start_node("Block", &block.loc);
        self.array_property("body", &block.body, Printer::print_statement);
        self.end_node();
    }

    fn print_class(&mut self, class: &Class, name: &str) {
        self.start_node(name, &class.loc);
        self.property("id", class.id.as_ref(), Printer::print_optional_identifier);
        self.property(
            "superClass",
            class.super_class.as_ref(),
            Printer::print_optional_outer_expression,
        );
        self.property("body", class, Printer::print_class_body);
        self.end_node();
    }

    fn print_class_body(&mut self, class: &Class) {
        self.start_node("ClassBody", &class.loc);
        self.array_property("body", &class.body, Printer::print_class_element);
        self.end_node();
    }

    fn print_class_element(&mut self, element: &ClassElement) {
        match element {
            ClassElement::Method(method) => self.print_class_method(method),
            ClassElement::Property(prop) => self.print_class_property(prop),
        }
    }

    fn print_class_method(&mut self, method: &ClassMethod) {
        if method.kind == ClassMethodKind::StaticInitializer {
            self.print_class_static_block(method);
            return;
        }

        self.start_node("MethodDefinition", &method.loc);

        if method.is_private {
            self.property("key", &method.key.expr, Printer::print_private_identifier);
        } else {
            self.property("key", method.key.as_ref(), Printer::print_outer_expression);
        }

        self.property("value", method.value.as_ref(), Printer::print_function_expression);
        self.property("kind", method.kind, Printer::print_class_method_kind);
        self.property("computed", method.is_computed, Printer::print_bool);
        self.property("static", method.is_static, Printer::print_bool);
        self.end_node();
    }

    fn print_class_method_kind(&mut self, kind: ClassMethodKind) {
        let str = match kind {
            ClassMethodKind::Method => "method",
            ClassMethodKind::Constructor => "constructor",
            ClassMethodKind::Get => "get",
            ClassMethodKind::Set => "set",
            ClassMethodKind::StaticInitializer => "static initializer",
        };
        self.print_str(str)
    }

    fn print_class_property(&mut self, prop: &ClassProperty) {
        self.start_node("PropertyDefinition", &prop.loc);

        if prop.is_private {
            self.property("key", &prop.key.expr, Printer::print_private_identifier);
        } else {
            self.property("key", prop.key.as_ref(), Printer::print_outer_expression);
        }

        self.property("value", prop.value.as_ref(), Printer::print_optional_outer_expression);
        self.property("computed", prop.is_computed, Printer::print_bool);
        self.property("static", prop.is_static, Printer::print_bool);
        self.end_node();
    }

    fn print_class_static_block(&mut self, static_block: &ClassMethod) {
        let block = match static_block.value.body.as_ref() {
            FunctionBody::Block(block) => block,
            FunctionBody::Expression(_) => unreachable!("static block requires block body"),
        };

        self.start_node("StaticBlock", &static_block.loc);
        self.array_property("body", &block.body, Printer::print_statement);
        self.end_node();
    }

    fn print_expression_statement(&mut self, expr: &ExpressionStatement) {
        self.start_node("ExpressionStatement", &expr.loc);
        self.property("kind", expr.expr.as_ref(), Printer::print_outer_expression);
        self.end_node();
    }

    fn print_block(&mut self, block: &Block) {
        self.start_node("Block", &block.loc);
        self.array_property("body", &block.body, Printer::print_statement);
        self.end_node();
    }

    fn print_if_statement(&mut self, stmt: &IfStatement) {
        self.start_node("IfStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_outer_expression);
        self.property("consequent", stmt.conseq.as_ref(), Printer::print_statement);
        self.property("alternate", stmt.altern.as_ref(), Printer::print_optional_statement);
        self.end_node();
    }

    fn print_switch_statement(&mut self, stmt: &SwitchStatement) {
        self.start_node("SwitchStatement", &stmt.loc);
        self.property("discriminant", stmt.discriminant.as_ref(), Printer::print_outer_expression);
        self.array_property("cases", stmt.cases.as_ref(), Printer::print_switch_case);
        self.end_node();
    }

    fn print_switch_case(&mut self, case: &SwitchCase) {
        self.start_node("SwitchCase", &case.loc);
        self.property("test", case.test.as_ref(), Printer::print_optional_outer_expression);
        self.array_property("body", case.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_for_statement(&mut self, stmt: &ForStatement) {
        self.start_node("ForStatement", &stmt.loc);
        self.property("init", stmt.init.as_ref(), Printer::print_for_init);
        self.property("test", stmt.test.as_ref(), Printer::print_optional_outer_expression);
        self.property("update", stmt.update.as_ref(), Printer::print_optional_outer_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_for_init(&mut self, init: Option<&P<ForInit>>) {
        match init {
            None => self.print_null(),
            Some(init) => match init.as_ref() {
                ForInit::Expression(expr) => self.print_outer_expression(expr),
                ForInit::VarDecl(decl) => self.print_variable_declaration(decl),
            },
        }
    }

    fn print_for_each_statement(&mut self, stmt: &ForEachStatement) {
        let name = match stmt.kind {
            ForEachKind::In => "ForInStatement",
            ForEachKind::Of => "ForOfStatement",
        };
        self.start_node(name, &stmt.loc);
        self.property("left", stmt.left.as_ref(), Printer::print_for_each_init);
        self.property("right", stmt.right.as_ref(), Printer::print_outer_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);

        if stmt.kind == ForEachKind::Of {
            self.property("await", stmt.is_await, Printer::print_bool);
        }

        self.end_node();
    }

    fn print_for_each_init(&mut self, init: &ForEachInit) {
        match init {
            ForEachInit::Pattern { pattern, .. } => self.print_pattern(pattern),
            ForEachInit::VarDecl(decl) => self.print_variable_declaration(decl),
        }
    }

    fn print_while_statement(&mut self, stmt: &WhileStatement) {
        self.start_node("WhileStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_outer_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_do_while_statement(&mut self, stmt: &DoWhileStatement) {
        self.start_node("DoWhileStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_outer_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_with_statement(&mut self, stmt: &WithStatement) {
        self.start_node("WithStatement", &stmt.loc);
        self.property("object", stmt.object.as_ref(), Printer::print_outer_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_try_statement(&mut self, stmt: &TryStatement) {
        self.start_node("TryStatement", &stmt.loc);
        self.property("block", stmt.block.as_ref(), Printer::print_block);
        self.property("handler", stmt.handler.as_deref(), Printer::print_try_handler);
        self.property("finalizer", stmt.finalizer.as_ref(), Printer::print_optional_block);
        self.end_node();
    }

    fn print_try_handler(&mut self, handler: Option<&CatchClause>) {
        if let Some(handler) = handler {
            self.start_node("CatchClause", &handler.loc);
            self.property("param", handler.param.as_ref(), Printer::print_optional_pattern);
            self.property("body", handler.body.as_ref(), Printer::print_block);
            self.end_node();
        } else {
            self.print_null();
        }
    }

    fn print_throw_statement(&mut self, stmt: &ThrowStatement) {
        self.start_node("ThrowStatement", &stmt.loc);
        self.property("argument", stmt.argument.as_ref(), Printer::print_outer_expression);
        self.end_node();
    }

    fn print_return_statement(&mut self, stmt: &ReturnStatement) {
        self.start_node("ReturnStatement", &stmt.loc);
        self.property("argument", stmt.argument.as_ref(), Printer::print_optional_outer_expression);
        self.end_node();
    }

    fn print_break_statement(&mut self, stmt: &BreakStatement) {
        self.start_node("BreakStatement", &stmt.loc);
        self.property("label", stmt.label.as_ref(), Printer::print_optional_label);
        self.end_node();
    }

    fn print_continue_statement(&mut self, stmt: &ContinueStatement) {
        self.start_node("ContinueStatement", &stmt.loc);
        self.property("label", stmt.label.as_ref(), Printer::print_optional_label);
        self.end_node();
    }

    fn print_labeled_statement(&mut self, stmt: &LabeledStatement) {
        self.start_node("LabeledStatement", &stmt.loc);
        self.property("label", stmt.label.as_ref(), Printer::print_label);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_label(&mut self, label: &Label) {
        self.print_identifier_parts(&label.loc, &label.name);
    }

    fn print_empty_statement(&mut self, loc: &Loc) {
        self.start_node("EmptyStatement", loc);
        self.end_node();
    }

    fn print_debugger_statement(&mut self, loc: &Loc) {
        self.start_node("DebuggerStatement", loc);
        self.end_node();
    }

    fn print_var_kind(&mut self, kind: &VarKind) {
        let str = match kind {
            VarKind::Var => "var",
            VarKind::Let => "let",
            VarKind::Const => "const",
        };
        self.print_str(str);
    }

    fn print_variable_declarator(&mut self, var_decl: &VariableDeclarator) {
        self.start_node("VariableDeclarator", &var_decl.loc);
        self.property("id", var_decl.id.as_ref(), Printer::print_pattern);
        self.property("init", var_decl.init.as_ref(), Printer::print_optional_outer_expression);
        self.end_node();
    }

    fn print_outer_expression(&mut self, expr: &OuterExpression) {
        self.print_expression(&expr.expr);
    }

    fn print_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Id(id) => self.print_identifier(id),
            Expression::Null(lit) => self.print_null_literal(lit),
            Expression::Boolean(lit) => self.print_boolean_literal(lit),
            Expression::Number(lit) => self.print_number_literal(lit),
            Expression::String(lit) => self.print_string_literal(lit),
            Expression::BigInt(lit) => self.print_bigint_literal(lit),
            Expression::RegExp(lit) => self.print_regexp_literal(lit),
            Expression::Unary(unary) => self.print_unary_expression(unary),
            Expression::Binary(binary) => self.print_binary_expression(binary),
            Expression::Logical(logical) => self.print_logical_expression(logical),
            Expression::Assign(assign) => self.print_assignment_expression(assign),
            Expression::Update(update) => self.print_update_expression(update),
            Expression::Member(member) => self.print_member_expression(member),
            Expression::Chain(chain) => self.print_chain_expression(chain),
            Expression::Conditional(cond) => self.print_conditional_expression(cond),
            Expression::Call(call) => self.print_call_expression(call),
            Expression::New(new) => self.print_new_expression(new),
            Expression::Sequence(seq) => self.print_sequence_expression(seq),
            Expression::This(this) => self.print_this_expression(this),
            Expression::Array(arr) => self.print_array_expression(arr),
            Expression::Object(arr) => self.print_object_expression(arr),
            Expression::Function(func) => self.print_function_expression(func),
            Expression::ArrowFunction(func) => self.print_function(func, "ArrowFunctionExpression"),
            Expression::Class(class) => self.print_class(class, "ClassExpression"),
            Expression::Await(expr) => self.print_await_expression(expr),
            Expression::Yield(expr) => self.print_yield_expression(expr),
            Expression::SuperCall(expr) => self.print_super_call_expression(expr),
            Expression::SuperMember(expr) => self.print_super_member_expression(expr),
            Expression::Template(lit) => self.print_template_literal(lit),
            Expression::TaggedTemplate(expr) => self.print_tagged_template_expression(expr),
            Expression::MetaProperty(expr) => self.print_meta_property(expr),
            Expression::Import(expr) => self.print_import_expression(expr),
        }
    }

    fn print_null_literal(&mut self, loc: &Loc) {
        self.start_node("Literal", loc);
        self.property("value", (), Printer::print_null_in_property);
        self.end_node();
    }

    fn print_boolean_literal(&mut self, lit: &BooleanLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("value", lit.value, Printer::print_bool);
        self.end_node();
    }

    fn print_number_literal(&mut self, lit: &NumberLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("value", lit.value, Printer::print_number);
        self.end_node();
    }

    fn print_string_literal(&mut self, lit: &StringLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("value", &lit.value, Printer::print_wtf8_string);
        self.end_node();
    }

    fn print_bigint_literal(&mut self, lit: &BigIntLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("value", (), Printer::print_null_in_property);
        self.property("bigint", lit.value.to_string().as_str(), Printer::print_str);
        self.end_node();
    }

    fn print_regexp_literal(&mut self, lit: &RegExpLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("raw", lit.raw.as_ref(), Printer::print_wtf8_string);
        self.property("value", lit, Printer::print_regex_value);
        self.property("regexp", lit.regexp.as_ref(), Printer::print_regexp);
        self.end_node();
    }

    fn print_regex_value(&mut self, lit: &RegExpLiteral) {
        self.string("{\n");
        self.inc_indent();

        self.property("pattern", lit.pattern.as_ref(), Printer::print_wtf8_string);
        self.property("flags", lit.flags.as_ref(), Printer::print_wtf8_string);

        self.dec_indent();
        self.indent();
        self.string("}");
    }

    fn print_unary_operator(&mut self, op: &UnaryOperator) {
        let str = match op {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitwiseNot => "~",
            UnaryOperator::TypeOf => "typeof",
            UnaryOperator::Void => "void",
            UnaryOperator::Delete => "delete",
        };
        self.print_str(str);
    }

    fn print_unary_expression(&mut self, unary: &UnaryExpression) {
        self.start_node("UnaryExpression", &unary.loc);
        self.property("operator", &unary.operator, Printer::print_unary_operator);
        self.property("argument", unary.argument.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_binary_operator(&mut self, op: &BinaryOperator) {
        let str = match op {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Remainder => "%",
            BinaryOperator::Exponent => "**",
            BinaryOperator::EqEq => "==",
            BinaryOperator::EqEqEq => "===",
            BinaryOperator::NotEq => "!=",
            BinaryOperator::NotEqEq => "!==",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanOrEqual => ">=",
            BinaryOperator::And => "&",
            BinaryOperator::Or => "|",
            BinaryOperator::Xor => "^",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRightArithmetic => ">>",
            BinaryOperator::ShiftRightLogical => ">>>",
            BinaryOperator::In | BinaryOperator::InPrivate => "in",
            BinaryOperator::InstanceOf => "instanceof",
        };
        self.print_str(str);
    }

    fn print_binary_expression(&mut self, binary: &BinaryExpression) {
        self.start_node("BinaryExpression", &binary.loc);
        self.property("operator", &binary.operator, Printer::print_binary_operator);

        if binary.operator == BinaryOperator::InPrivate {
            self.property("left", binary.left.as_ref(), Printer::print_private_identifier);
        } else {
            self.property("left", binary.left.as_ref(), Printer::print_expression);
        }

        self.property("right", binary.right.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_logical_operator(&mut self, op: &LogicalOperator) {
        let str = match op {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
            LogicalOperator::NullishCoalesce => "??",
        };
        self.print_str(str);
    }

    fn print_logical_expression(&mut self, logical: &LogicalExpression) {
        self.start_node("LogicalExpression", &logical.loc);
        self.property("operator", &logical.operator, Printer::print_logical_operator);
        self.property("left", logical.left.as_ref(), Printer::print_expression);
        self.property("right", logical.right.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_assignment_operator(&mut self, op: &AssignmentOperator) {
        let str = match op {
            AssignmentOperator::Equals => "=",
            AssignmentOperator::Add => "+=",
            AssignmentOperator::Subtract => "-=",
            AssignmentOperator::Multiply => "*=",
            AssignmentOperator::Divide => "/=",
            AssignmentOperator::Remainder => "%=",
            AssignmentOperator::Exponent => "**=",
            AssignmentOperator::And => "&=",
            AssignmentOperator::Or => "|=",
            AssignmentOperator::Xor => "^=",
            AssignmentOperator::ShiftLeft => "<<=",
            AssignmentOperator::ShiftRightArithmetic => ">>=",
            AssignmentOperator::ShiftRightLogical => ">>>=",
            AssignmentOperator::LogicalAnd => "&&=",
            AssignmentOperator::LogicalOr => "||=",
            AssignmentOperator::NullishCoalesce => "??=",
        };
        self.print_str(str);
    }

    fn print_assignment_expression(&mut self, assign: &AssignmentExpression) {
        self.start_node("AssignmentExpression", &assign.loc);
        self.property("operator", &assign.operator, Printer::print_assignment_operator);
        self.property("left", assign.left.as_ref(), Printer::print_pattern);
        self.property("right", assign.right.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_update_operator(&mut self, op: &UpdateOperator) {
        let str = match op {
            UpdateOperator::Increment => "++",
            UpdateOperator::Decrement => "--",
        };
        self.print_str(str);
    }

    fn print_update_expression(&mut self, update: &UpdateExpression) {
        self.start_node("UpdateExpression", &update.loc);
        self.property("operator", &update.operator, Printer::print_update_operator);
        self.property("argument", update.argument.as_ref(), Printer::print_expression);
        self.property("prefix", update.is_prefix, Printer::print_bool);
        self.end_node();
    }

    fn print_member_expression(&mut self, member: &MemberExpression) {
        self.start_node("MemberExpression", &member.loc);
        self.property("object", member.object.as_ref(), Printer::print_expression);

        if member.is_private {
            self.property("property", member.property.as_ref(), Printer::print_private_identifier);
        } else {
            self.property("property", member.property.as_ref(), Printer::print_expression);
        }

        self.property("computed", member.is_computed, Printer::print_bool);
        self.property("optional", member.is_optional, Printer::print_bool);
        self.end_node();
    }

    fn print_chain_expression(&mut self, chain: &ChainExpression) {
        self.start_node("ChainExpression", &chain.loc);
        self.property("expression", chain.expression.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_private_identifier(&mut self, expr: &Expression) {
        let id = expr.to_id();

        self.start_node("PrivateIdentifier", &id.loc);
        self.property("name", id.name.as_str(), Printer::print_str);
        self.end_node();
    }

    fn print_conditional_expression(&mut self, cond: &ConditionalExpression) {
        self.start_node("ConditionalExpression", &cond.loc);
        self.property("test", cond.test.as_ref(), Printer::print_expression);
        self.property("consequent", cond.conseq.as_ref(), Printer::print_expression);
        self.property("alternate", cond.altern.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_call_expression(&mut self, call: &CallExpression) {
        self.start_node("CallExpression", &call.loc);
        self.property("callee", call.callee.as_ref(), Printer::print_expression);
        self.array_property("arguments", call.arguments.as_ref(), Printer::print_call_argument);
        self.property("optional", call.is_optional, Printer::print_bool);
        self.end_node();
    }

    fn print_call_argument(&mut self, argument: &CallArgument) {
        match argument {
            CallArgument::Expression(expr) => self.print_expression(expr),
            CallArgument::Spread(spread) => self.print_spread_element(spread),
        }
    }

    fn print_new_expression(&mut self, new: &NewExpression) {
        self.start_node("NewExpression", &new.loc);
        self.property("callee", new.callee.as_ref(), Printer::print_expression);
        self.array_property("arguments", new.arguments.as_ref(), Printer::print_call_argument);
        self.end_node();
    }

    fn print_sequence_expression(&mut self, seq: &SequenceExpression) {
        self.start_node("SequenceExpression", &seq.loc);
        self.array_property("expressions", seq.expressions.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_this_expression(&mut self, this: &ThisExpression) {
        self.start_node("ThisExpression", &this.loc);
        self.end_node();
    }

    fn print_array_expression(&mut self, arr: &ArrayExpression) {
        self.start_node("ArrayExpression", &arr.loc);
        self.array_property("elements", arr.elements.as_ref(), Printer::print_array_element);
        self.end_node();
    }

    fn print_array_element(&mut self, element: &ArrayElement) {
        match element {
            ArrayElement::Expression(expr) => self.print_expression(expr),
            ArrayElement::Spread(spread) => self.print_spread_element(spread),
            ArrayElement::Hole(_) => self.print_null(),
        }
    }

    fn print_spread_element(&mut self, spread: &SpreadElement) {
        self.start_node("SpreadElement", &spread.loc);
        self.property("argument", spread.argument.as_ref(), Printer::print_expression);
        self.end_node()
    }

    fn print_object_expression(&mut self, obj: &ObjectExpression) {
        self.start_node("ObjectExpression", &obj.loc);
        self.array_property("properties", obj.properties.as_ref(), Printer::print_property);
        self.end_node();
    }

    fn print_property(&mut self, prop: &Property) {
        if let PropertyKind::Spread(_) = prop.kind {
            self.start_node("SpreadElement", &prop.loc);
            self.property("argument", prop.key.as_ref(), Printer::print_expression);
            self.end_node();
            return;
        }

        self.start_node("Property", &prop.loc);
        self.property("key", prop.key.as_ref(), Printer::print_expression);
        self.property("value", prop.value.as_ref(), Printer::print_optional_expression);
        self.property("computed", prop.is_computed, Printer::print_bool);
        self.property("shorthand", prop.value.is_none(), Printer::print_bool);
        self.property("method", prop.is_method, Printer::print_bool);
        self.property("kind", &prop.kind, Printer::print_property_kind);
        self.end_node();
    }

    fn print_property_kind(&mut self, kind: &PropertyKind) {
        let str = match kind {
            PropertyKind::Init => "init",
            PropertyKind::Get => "get",
            PropertyKind::Set => "set",
            PropertyKind::Spread(_) => "spread",
            PropertyKind::PatternInitializer(_) => "<pattern initializer>",
        };
        self.print_str(str)
    }

    fn print_await_expression(&mut self, expr: &AwaitExpression) {
        self.start_node("AwaitExpression", &expr.loc);
        self.property("argument", expr.argument.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_yield_expression(&mut self, expr: &YieldExpression) {
        self.start_node("YieldExpression", &expr.loc);
        self.property("argument", expr.argument.as_ref(), Printer::print_optional_expression);
        self.property("delegate", expr.is_delegate, Printer::print_bool);
        self.end_node();
    }

    fn print_super(&mut self, loc: &Loc) {
        self.start_node("Super", loc);
        self.end_node();
    }

    fn print_super_member_expression(&mut self, member: &SuperMemberExpression) {
        self.start_node("MemberExpression", &member.loc);
        self.property("object", &member.super_, Printer::print_super);
        self.property("property", member.property.as_ref(), Printer::print_expression);
        self.property("computed", member.is_computed, Printer::print_bool);
        self.end_node();
    }

    fn print_super_call_expression(&mut self, call: &SuperCallExpression) {
        self.start_node("CallExpression", &call.loc);
        self.property("callee", &call.super_, Printer::print_super);
        self.array_property("arguments", call.arguments.as_ref(), Printer::print_call_argument);
        self.end_node();
    }

    fn print_template_literal(&mut self, lit: &TemplateLiteral) {
        self.start_node("TemplateLiteral", &lit.loc);

        let num_quasis = lit.quasis.len();
        let quasis_with_is_tail = lit
            .quasis
            .iter()
            .enumerate()
            .map(|(i, quasi)| (quasi, i == num_quasis - 1))
            .collect::<Vec<_>>();

        self.array_property("quasis", &quasis_with_is_tail, Printer::print_template_element);
        self.array_property("expressions", lit.expressions.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_template_element(&mut self, (element, is_tail): &(&TemplateElement, bool)) {
        self.start_node("TemplateElement", &element.loc);
        self.property("value", *element, Printer::print_template_element_value);
        self.property("tail", *is_tail, Printer::print_bool);
        self.end_node();
    }

    fn print_template_element_value(&mut self, element: &TemplateElement) {
        self.string("{\n");
        self.inc_indent();

        self.property("cooked", element.cooked.as_ref(), Printer::print_optional_wtf8_string);
        self.property("raw", &element.raw, Printer::print_wtf8_string);

        self.dec_indent();
        self.indent();
        self.string("}");
    }

    fn print_tagged_template_expression(&mut self, expr: &TaggedTemplateExpression) {
        self.start_node("TaggedTemplateExpression", &expr.loc);
        self.property("tag", expr.tag.as_ref(), Printer::print_expression);
        self.property("quasi", expr.quasi.as_ref(), Printer::print_template_literal);
        self.end_node();
    }

    fn print_meta_property(&mut self, expr: &MetaProperty) {
        self.start_node("MetaProperty", &expr.loc);

        match expr.kind {
            MetaPropertyKind::NewTarget { .. } => {
                self.property("meta", (&expr.loc, "new"), Printer::print_str_as_identifier);
                self.property("property", (&expr.loc, "target"), Printer::print_str_as_identifier);
            }
            MetaPropertyKind::ImportMeta => {
                self.property("meta", (&expr.loc, "import"), Printer::print_str_as_identifier);
                self.property("property", (&expr.loc, "meta"), Printer::print_str_as_identifier);
            }
        }

        self.end_node();
    }

    fn print_import_expression(&mut self, expr: &ImportExpression) {
        self.start_node("ImportExpression", &expr.loc);
        self.property("source", expr.source.as_ref(), Printer::print_expression);
        self.property("options", expr.options.as_ref(), Printer::print_optional_expression);
        self.end_node();
    }

    fn print_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Id(id) => self.print_identifier(id),
            Pattern::Array(patt) => self.print_array_pattern(patt),
            Pattern::Object(patt) => self.print_object_pattern(patt),
            Pattern::Assign(patt) => self.print_assign_pattern(patt),
            Pattern::Member(expr) => self.print_member_expression(expr),
            Pattern::SuperMember(expr) => self.print_super_member_expression(expr),
        }
    }

    fn print_identifier(&mut self, id: &Identifier) {
        self.print_identifier_parts(&id.loc, &id.name);
    }

    fn print_identifier_parts(&mut self, loc: &Loc, name: &str) {
        self.start_node("Identifier", loc);
        self.property("name", name, Printer::print_str);
        self.end_node();
    }

    fn print_str_as_identifier(&mut self, (loc, string): (&Loc, &str)) {
        self.start_node("Identifier", loc);
        self.property("name", string, Printer::print_str);
        self.end_node();
    }

    fn print_array_pattern(&mut self, patt: &ArrayPattern) {
        self.start_node("ArrayPattern", &patt.loc);
        self.array_property(
            "elements",
            patt.elements.as_ref(),
            Printer::print_array_pattern_element,
        );
        self.end_node();
    }

    fn print_array_pattern_element(&mut self, element: &ArrayPatternElement) {
        match element {
            ArrayPatternElement::Pattern(pattern) => self.print_pattern(pattern),
            ArrayPatternElement::Rest(rest) => self.print_rest_element(rest),
            ArrayPatternElement::Hole(_) => self.print_null(),
        }
    }

    fn print_rest_element(&mut self, rest: &RestElement) {
        self.start_node("RestElement", &rest.loc);
        self.property("argument", rest.argument.as_ref(), Printer::print_pattern);
        self.end_node();
    }

    fn print_object_pattern(&mut self, patt: &ObjectPattern) {
        self.start_node("ObjectPattern", &patt.loc);
        self.array_property(
            "properties",
            patt.properties.as_ref(),
            Printer::print_object_pattern_property,
        );
        self.end_node();
    }

    fn print_object_pattern_property(&mut self, prop: &ObjectPatternProperty) {
        if prop.is_rest {
            self.start_node("RestElement", &prop.loc);
            self.property("property", prop.value.as_ref(), Printer::print_pattern);
            self.end_node();
            return;
        }

        self.start_node("Property", &prop.loc);
        self.property("key", prop.key.as_ref(), Printer::print_optional_expression);
        self.property("value", prop.value.as_ref(), Printer::print_pattern);
        self.property("computed", prop.is_computed, Printer::print_bool);
        self.end_node();
    }

    fn print_assign_pattern(&mut self, patt: &AssignmentPattern) {
        self.start_node("AssignmentPattern", &patt.loc);
        self.property("left", patt.left.as_ref(), Printer::print_pattern);
        self.property("right", patt.right.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_import_declaration(&mut self, import: &ImportDeclaration) {
        self.start_node("ImportDeclaration", &import.loc);
        self.array_property(
            "specifiers",
            import.specifiers.as_ref(),
            Printer::print_import_specifier,
        );
        self.property("source", import.source.as_ref(), Printer::print_string_literal);
        self.print_import_attributes_property(import.attributes.as_ref());

        self.end_node();
    }

    fn print_import_attributes_property(&mut self, attributes: Option<&P<ImportAttributes>>) {
        if let Some(attributes) = attributes {
            self.array_property(
                "attributes",
                &attributes.attributes,
                Printer::print_import_attribute,
            );
        }
    }

    fn print_import_attribute(&mut self, attribute: &ImportAttribute) {
        self.start_node("ImportAttribute", &attribute.loc);
        self.property("key", attribute.key.as_ref(), Printer::print_expression);
        self.property("value", attribute.value.as_ref(), Printer::print_string_literal);
        self.end_node();
    }

    fn print_import_specifier(&mut self, specifier: &ImportSpecifier) {
        match specifier {
            ImportSpecifier::Default(spec) => self.print_import_default_specifier(spec),
            ImportSpecifier::Named(spec) => self.print_import_named_specifier(spec),
            ImportSpecifier::Namespace(spec) => self.print_import_namespace_specifier(spec),
        }
    }

    fn print_import_default_specifier(&mut self, spec: &ImportDefaultSpecifier) {
        self.start_node("ImportDefaultSpecifier", &spec.loc);
        self.property("local", spec.local.as_ref(), Printer::print_identifier);
        self.end_node();
    }

    fn print_import_named_specifier(&mut self, spec: &ImportNamedSpecifier) {
        self.start_node("ImportSpecifier", &spec.loc);
        self.property("imported", spec.imported.as_ref(), Printer::print_optional_export_name);
        self.property("local", spec.local.as_ref(), Printer::print_identifier);
        self.end_node();
    }

    fn print_import_namespace_specifier(&mut self, spec: &ImportNamespaceSpecifier) {
        self.start_node("ImportNamespaceSpecifier", &spec.loc);
        self.property("local", spec.local.as_ref(), Printer::print_identifier);
        self.end_node();
    }

    fn print_export_default_declaration(&mut self, export: &ExportDefaultDeclaration) {
        self.start_node("ExportDefaultDeclaration", &export.loc);
        self.property("declaration", &export.declaration, Printer::print_export_default_kind);
        self.end_node();
    }

    fn print_export_default_kind(&mut self, kind: &ExportDefaultKind) {
        match kind {
            ExportDefaultKind::Function(func) => {
                self.print_function(func.as_ref(), "FunctionDeclaration")
            }
            ExportDefaultKind::Class(class) => self.print_class(class.as_ref(), "ClassDeclaration"),
            ExportDefaultKind::Expression(expr) => self.print_outer_expression(expr),
        }
    }

    fn print_export_named_declaration(&mut self, export: &ExportNamedDeclaration) {
        self.start_node("ExportNamedDeclaration", &export.loc);
        self.property(
            "declaration",
            export.declaration.as_ref(),
            Printer::print_optional_statement,
        );
        self.array_property(
            "specifiers",
            export.specifiers.as_ref(),
            Printer::print_export_specifier,
        );
        self.property("source", export.source.as_ref(), Printer::print_optional_string_literal);
        self.print_import_attributes_property(export.source_attributes.as_ref());
        self.end_node();
    }

    fn print_export_all_declaration(&mut self, export: &ExportAllDeclaration) {
        self.start_node("ExportAllDeclaration", &export.loc);
        self.property("exported", export.exported.as_ref(), Printer::print_optional_export_name);
        self.property("source", export.source.as_ref(), Printer::print_string_literal);
        self.print_import_attributes_property(export.source_attributes.as_ref());
        self.end_node();
    }

    fn print_export_specifier(&mut self, spec: &ExportSpecifier) {
        self.start_node("ExportSpecifier", &spec.loc);
        self.property("exported", spec.exported.as_ref(), Printer::print_optional_export_name);
        self.property("local", spec.local.as_ref(), Printer::print_export_name);
        self.end_node();
    }

    fn print_export_name(&mut self, export_name: &ExportName) {
        match export_name {
            ExportName::Id(id) => self.print_identifier_parts(&id.loc, &id.name),
            ExportName::String(lit) => self.print_string_literal(lit),
        }
    }

    fn print_optional_expression(&mut self, expr: Option<&P<Expression>>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_expression(expr),
        }
    }

    fn print_optional_outer_expression(&mut self, expr: Option<&P<OuterExpression>>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_outer_expression(expr),
        }
    }

    fn print_optional_statement(&mut self, stmt: Option<&P<Statement>>) {
        match stmt {
            None => self.print_null(),
            Some(stmt) => self.print_statement(stmt),
        }
    }

    fn print_optional_identifier(&mut self, id: Option<&P<Identifier>>) {
        match id {
            None => self.print_null(),
            Some(id) => self.print_identifier(id),
        }
    }

    fn print_optional_label(&mut self, id: Option<&Label>) {
        match id {
            None => self.print_null(),
            Some(label) => self.print_label(label),
        }
    }

    fn print_optional_block(&mut self, block: Option<&P<Block>>) {
        match block {
            None => self.print_null(),
            Some(block) => self.print_block(block),
        }
    }

    fn print_optional_pattern(&mut self, pattern: Option<&P<Pattern>>) {
        match pattern {
            None => self.print_null(),
            Some(pattern) => self.print_pattern(pattern),
        }
    }

    fn print_optional_string_literal(&mut self, lit: Option<&P<StringLiteral>>) {
        match lit {
            None => self.print_null(),
            Some(lit) => self.print_string_literal(lit),
        }
    }

    fn print_optional_string(&mut self, string: Option<&String>) {
        match string {
            None => self.print_null(),
            Some(string) => self.print_str(string),
        }
    }

    fn print_optional_wtf8_string(&mut self, string: Option<&Wtf8String>) {
        match string {
            None => self.print_null(),
            Some(string) => self.print_wtf8_string(string),
        }
    }

    fn print_optional_number(&mut self, number: Option<impl ToString>) {
        match number {
            None => self.print_null(),
            Some(number) => self.print_number(number),
        }
    }

    fn print_optional_export_name(&mut self, export_name: Option<&P<ExportName>>) {
        match export_name {
            None => self.print_null(),
            Some(export_name) => self.print_export_name(export_name),
        }
    }

    fn print_regexp(&mut self, regexp: &RegExp) {
        self.start_regexp_node("RegExp");
        self.print_disjunction(&regexp.disjunction);
        self.end_node();
    }

    fn print_disjunction(&mut self, disjunction: &Disjunction) {
        self.array_property("alternatives", &disjunction.alternatives, Printer::print_alternative);
    }

    fn print_alternative(&mut self, alternative: &Alternative) {
        self.start_regexp_node("Alternative");
        self.array_property("terms", &alternative.terms, Printer::print_term);
        self.end_node();
    }

    fn print_term(&mut self, term: &Term) {
        match term {
            Term::Literal(literal) => self.print_regexp_literal_pattern(literal),
            Term::Wildcard => self.print_regexp_wildcard(),
            Term::Quantifier(quantifier) => self.print_regexp_quantifier(quantifier),
            Term::Assertion(assertion) => self.print_regexp_assertion(assertion),
            Term::Lookaround(lookaround) => self.print_regexp_lookaround(lookaround),
            Term::CaptureGroup(group) => self.print_regexp_capture_group(group),
            Term::AnonymousGroup(group) => self.print_regexp_anonymous_group(group),
            Term::CharacterClass(class) => self.print_regexp_character_class(class),
            Term::Backreference(backreference) => {
                self.print_regexp_backreference(backreference.as_ref())
            }
        }
    }

    fn print_regexp_literal_pattern(&mut self, literal: &Wtf8String) {
        self.start_regexp_node("Literal");
        self.property("value", literal, Printer::print_wtf8_string);
        self.end_node();
    }

    fn print_regexp_wildcard(&mut self) {
        self.start_regexp_node("Wildcard");
        self.end_node();
    }

    fn print_regexp_quantifier(&mut self, quantifier: &Quantifier) {
        self.start_regexp_node("Quantifier");
        self.property("term", quantifier.term.as_ref(), Printer::print_term);
        self.property("min", quantifier.min, Printer::print_number);
        self.property("max", quantifier.max, Printer::print_optional_number);
        self.property("is_greedy", quantifier.is_greedy, Printer::print_bool);
        self.end_node();
    }

    fn print_regexp_assertion_kind(&mut self, kind: &Assertion) {
        let str = match kind {
            Assertion::Start => "^",
            Assertion::End => "$",
            Assertion::WordBoundary => "\\b",
            Assertion::NotWordBoundary => "\\B",
        };
        self.print_str(str);
    }

    fn print_regexp_assertion(&mut self, assertion: &Assertion) {
        self.start_regexp_node("Assertion");
        self.property("kind", assertion, Printer::print_regexp_assertion_kind);
        self.end_node();
    }

    fn print_regexp_lookaround(&mut self, lookaround: &Lookaround) {
        self.start_regexp_node("Lookaround");
        self.property("is_ahead", lookaround.is_ahead, Printer::print_bool);
        self.property("is_positive", lookaround.is_positive, Printer::print_bool);
        self.print_disjunction(&lookaround.disjunction);
        self.end_node();
    }

    fn print_regexp_capture_group(&mut self, group: &CaptureGroup) {
        self.start_regexp_node("CaptureGroup");
        self.property("index", group.index, Printer::print_number);
        self.property("name", group.name.as_ref(), Printer::print_optional_string);
        self.print_disjunction(&group.disjunction);
        self.end_node();
    }

    fn print_regexp_anonymous_group(&mut self, group: &AnonymousGroup) {
        self.start_regexp_node("AnonymousGroup");

        if !group.positive_modifiers.is_empty() || !group.negative_modifiers.is_empty() {
            self.property(
                "modifiers",
                (group.positive_modifiers, group.negative_modifiers),
                Printer::print_regexp_modifiers,
            );
        }

        self.print_disjunction(&group.disjunction);
        self.end_node();
    }

    fn print_regexp_modifiers(&mut self, (positive, negative): (RegExpFlags, RegExpFlags)) {
        self.buf.push_str("\"(+");
        self.print_regexp_modifier(positive);
        self.buf.push_str(", ");
        self.buf.push('-');
        self.print_regexp_modifier(negative);
        self.buf.push_str(")\"");
    }

    fn print_regexp_modifier(&mut self, modifier: RegExpFlags) {
        if modifier.is_case_insensitive() {
            self.buf.push('i');
        }

        if modifier.is_multiline() {
            self.buf.push('m');
        }

        if modifier.is_dot_all() {
            self.buf.push('s');
        }
    }

    fn print_regexp_character_class_range(&mut self, range: &ClassRange) {
        match range {
            ClassRange::Single(single) => self
                .print_str(&format!("Single({})", to_string_or_unicode_escape_sequence(*single))),
            ClassRange::Range(start, end) => {
                let start_string = to_string_or_unicode_escape_sequence(*start);
                let end_string = to_string_or_unicode_escape_sequence(*end);
                self.print_str(&format!("Range({}, {})", start_string, end_string))
            }
            ClassRange::Digit => self.print_str("\\d"),
            ClassRange::NotDigit => self.print_str("\\D"),
            ClassRange::Word => self.print_str("\\w"),
            ClassRange::NotWord => self.print_str("\\W"),
            ClassRange::Whitespace => self.print_str("\\s"),
            ClassRange::NotWhitespace => self.print_str("\\S"),
            ClassRange::UnicodeProperty(property) => {
                self.print_str(&format!("UnicodeProperty({:?})", property))
            }
            ClassRange::NotUnicodeProperty(property) => {
                self.print_str(&format!("NotUnicodeProperty({:?})", property))
            }
            ClassRange::NestedClass(class) => self.print_regexp_character_class(class),
        }
    }

    fn print_regexp_character_class(&mut self, class: &CharacterClass) {
        self.start_regexp_node("CharacterClass");

        if class.expression_type != ClassExpressionType::Union {
            self.property(
                "expression_type",
                &class.expression_type,
                Printer::print_class_expression_type,
            );
        }

        self.property("is_inverted", class.is_inverted, Printer::print_bool);
        self.array_property("ranges", &class.operands, Printer::print_regexp_character_class_range);
        self.end_node();
    }

    fn print_class_expression_type(&mut self, class_expression_type: &ClassExpressionType) {
        let str = match class_expression_type {
            ClassExpressionType::Union => "Union",
            ClassExpressionType::Intersection => "Intersection",
            ClassExpressionType::Difference => "Difference",
        };
        self.print_str(str);
    }

    fn print_regexp_backreference(&mut self, backreference: &Backreference) {
        self.start_regexp_node("Backreference");
        self.property("index", backreference.index, Printer::print_number);
        self.end_node();
    }
}

// Prints JSON in ESTree format
pub fn print_program(program: &Program) -> String {
    let mut printer = Printer::new(&program.source);
    printer.print_program(program);
    printer.finish()
}
