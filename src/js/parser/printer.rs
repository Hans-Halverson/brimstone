use super::ast::*;
use super::loc::{find_line_col_for_pos, Loc};
use super::source::Source;

struct Printer<'a> {
    buf: String,
    indent: usize,
    source: &'a Source,
}

impl<'a> Printer<'a> {
    fn new(source: &'a Source) -> Printer<'a> {
        Printer {
            buf: String::new(),
            indent: 0,
            source,
        }
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

    fn print_string(&mut self, string: &String) {
        self.buf.push('\"');
        self.buf.push_str(string);
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

    fn print_f64(&mut self, value: f64) {
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
        self.string(&format!(
            "loc: \"{}:{}-{}:{}\",\n",
            start_line, start_col, end_line, end_col
        ));
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
        self.property(
            "has_use_strict_directive",
            program.has_use_strict_directive,
            Printer::print_bool,
        );
        self.end_node();
    }

    fn print_toplevel(&mut self, toplevel: &Toplevel) {
        match toplevel {
            Toplevel::Statement(stmt) => self.print_statement(stmt),
        }
    }

    fn print_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => self.print_variable_declaration(var_decl),
            Statement::FuncDecl(func_decl) => self.print_function(func_decl, "FunctionDeclaration"),
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

    fn print_function(&mut self, func: &Function, name: &str) {
        self.start_node(name, &func.loc);
        self.property("id", func.id.as_ref(), Printer::print_optional_identifier);
        self.array_property("params", func.params.as_ref(), Printer::print_pattern);
        self.property("body", func.body.as_ref(), Printer::print_function_body);
        self.property("async", func.is_async, Printer::print_bool);
        self.property("generator", func.is_generator, Printer::print_bool);
        self.property(
            "has_use_strict_directive",
            func.has_use_strict_directive,
            Printer::print_bool,
        );
        self.end_node();
    }

    fn print_function_body(&mut self, body: &FunctionBody) {
        match body {
            FunctionBody::Block(block) => self.print_block(block),
            FunctionBody::Expression(expr) => self.print_expression(&expr),
        }
    }

    fn print_expression_statement(&mut self, expr: &ExpressionStatement) {
        self.start_node("ExpressionStatement", &expr.loc);
        self.property("kind", expr.expr.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_block(&mut self, block: &Block) {
        self.start_node("Block", &block.loc);
        self.array_property("body", &block.body, Printer::print_statement);
        self.end_node();
    }

    fn print_if_statement(&mut self, stmt: &IfStatement) {
        self.start_node("IfStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_expression);
        self.property("consequent", stmt.conseq.as_ref(), Printer::print_statement);
        self.property(
            "alternate",
            stmt.altern.as_ref(),
            Printer::print_optional_statement,
        );
        self.end_node();
    }

    fn print_switch_statement(&mut self, stmt: &SwitchStatement) {
        self.start_node("SwitchStatement", &stmt.loc);
        self.property(
            "discriminant",
            stmt.discriminant.as_ref(),
            Printer::print_expression,
        );
        self.array_property("cases", stmt.cases.as_ref(), Printer::print_switch_case);
        self.end_node();
    }

    fn print_switch_case(&mut self, case: &SwitchCase) {
        self.start_node("SwitchCase", &case.loc);
        self.property(
            "test",
            case.test.as_ref(),
            Printer::print_optional_expression,
        );
        self.array_property("body", case.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_for_statement(&mut self, stmt: &ForStatement) {
        self.start_node("ForStatement", &stmt.loc);
        self.property("init", stmt.init.as_ref(), Printer::print_for_init);
        self.property(
            "test",
            stmt.test.as_ref(),
            Printer::print_optional_expression,
        );
        self.property(
            "update",
            stmt.update.as_ref(),
            Printer::print_optional_expression,
        );
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_for_init(&mut self, init: Option<&P<ForInit>>) {
        match init {
            None => self.print_null(),
            Some(init) => match init.as_ref() {
                ForInit::Expression(expr) => self.print_expression(&expr),
                ForInit::VarDecl(decl) => self.print_variable_declaration(&decl),
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
        self.property("right", stmt.right.as_ref(), Printer::print_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_for_each_init(&mut self, init: &ForEachInit) {
        match init {
            ForEachInit::Pattern(expr) => self.print_pattern(&expr),
            ForEachInit::VarDecl(decl) => self.print_variable_declaration(&decl),
        }
    }

    fn print_while_statement(&mut self, stmt: &WhileStatement) {
        self.start_node("WhileStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_do_while_statement(&mut self, stmt: &DoWhileStatement) {
        self.start_node("DoWhileStatement", &stmt.loc);
        self.property("test", stmt.test.as_ref(), Printer::print_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_with_statement(&mut self, stmt: &WithStatement) {
        self.start_node("WithStatement", &stmt.loc);
        self.property("object", stmt.object.as_ref(), Printer::print_expression);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_try_statement(&mut self, stmt: &TryStatement) {
        self.start_node("TryStatement", &stmt.loc);
        self.property("block", stmt.block.as_ref(), Printer::print_block);
        self.property("handler", stmt.handler.as_ref(), Printer::print_try_handler);
        self.property(
            "finalizer",
            stmt.finalizer.as_ref(),
            Printer::print_optional_block,
        );
        self.end_node();
    }

    fn print_try_handler(&mut self, handler: Option<&Box<CatchClause>>) {
        if let Some(handler) = handler {
            self.start_node("CatchClause", &handler.loc);
            self.property(
                "param",
                handler.param.as_ref(),
                Printer::print_optional_pattern,
            );
            self.property("body", handler.body.as_ref(), Printer::print_block);
            self.end_node();
        } else {
            self.print_null();
        }
    }

    fn print_throw_statement(&mut self, stmt: &ThrowStatement) {
        self.start_node("ThrowStatement", &stmt.loc);
        self.property(
            "argument",
            stmt.argument.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_return_statement(&mut self, stmt: &ReturnStatement) {
        self.start_node("ReturnStatement", &stmt.loc);
        self.property(
            "argument",
            stmt.argument.as_ref(),
            Printer::print_optional_expression,
        );
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
        self.property("label", &stmt.label, Printer::print_label);
        self.property("body", stmt.body.as_ref(), Printer::print_statement);
        self.end_node();
    }

    fn print_label(&mut self, label: &Label) {
        self.print_identifier(&label.label)
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
        self.property(
            "init",
            var_decl.init.as_ref(),
            Printer::print_optional_expression,
        );
        self.end_node();
    }

    fn print_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Id(id) => self.print_identifier(id),
            Expression::Null(lit) => self.print_null_literal(lit),
            Expression::Boolean(lit) => self.print_boolean_literal(lit),
            Expression::Number(lit) => self.print_number_literal(lit),
            Expression::String(lit) => self.print_string_literal(lit),
            Expression::Unary(unary) => self.print_unary_expression(unary),
            Expression::Binary(binary) => self.print_binary_expression(binary),
            Expression::Logical(logical) => self.print_logical_expression(logical),
            Expression::Assign(assign) => self.print_assignment_expression(assign),
            Expression::Update(update) => self.print_update_expression(update),
            Expression::Member(member) => self.print_member_expression(member),
            Expression::Conditional(cond) => self.print_conditional_expression(cond),
            Expression::Call(call) => self.print_call_expression(call),
            Expression::New(new) => self.print_new_expression(new),
            Expression::Sequence(seq) => self.print_sequence_expression(seq),
            Expression::This(loc) => self.print_this_expression(&loc),
            Expression::Array(arr) => self.print_array_expression(arr),
            Expression::Object(arr) => self.print_object_expression(arr),
            Expression::Function(func) => self.print_function(func, "FunctionExpression"),
            Expression::ArrowFunction(func) => self.print_function(func, "ArrowFunctionExpression"),
            Expression::Await(expr) => self.print_await_expression(expr),
            Expression::Yield(expr) => self.print_yield_expression(expr),
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
        self.property("value", lit.value, Printer::print_f64);
        self.end_node();
    }

    fn print_string_literal(&mut self, lit: &StringLiteral) {
        self.start_node("Literal", &lit.loc);
        self.property("value", &lit.value, Printer::print_string);
        self.end_node();
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
        self.property(
            "argument",
            unary.argument.as_ref(),
            Printer::print_expression,
        );
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
            BinaryOperator::In => "in",
            BinaryOperator::InstanceOf => "instanceof",
        };
        self.print_str(str);
    }

    fn print_binary_expression(&mut self, binary: &BinaryExpression) {
        self.start_node("BinaryExpression", &binary.loc);
        self.property("operator", &binary.operator, Printer::print_binary_operator);
        self.property("left", binary.left.as_ref(), Printer::print_expression);
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
        self.property(
            "operator",
            &logical.operator,
            Printer::print_logical_operator,
        );
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
        };
        self.print_str(str);
    }

    fn print_assignment_expression(&mut self, assign: &AssignmentExpression) {
        self.start_node("AssignmentExpression", &assign.loc);
        self.property(
            "operator",
            &assign.operator,
            Printer::print_assignment_operator,
        );
        self.property("left", assign.left.as_ref(), Printer::print_expression);
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
        self.property(
            "argument",
            update.argument.as_ref(),
            Printer::print_expression,
        );
        self.property("prefix", update.is_prefix, Printer::print_bool);
        self.end_node();
    }

    fn print_member_expression(&mut self, member: &MemberExpression) {
        self.start_node("MemberExpression", &member.loc);
        self.property("object", member.object.as_ref(), Printer::print_expression);
        self.property(
            "property",
            member.property.as_ref(),
            Printer::print_expression,
        );
        self.property("computed", member.is_computed, Printer::print_bool);
        self.property("optional", member.is_optional, Printer::print_bool);
        self.end_node();
    }

    fn print_conditional_expression(&mut self, cond: &ConditionalExpression) {
        self.start_node("ConditionalExpression", &cond.loc);
        self.property("test", cond.test.as_ref(), Printer::print_expression);
        self.property(
            "consequent",
            cond.conseq.as_ref(),
            Printer::print_expression,
        );
        self.property("alternate", cond.altern.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_call_expression(&mut self, call: &CallExpression) {
        self.start_node("CallExpression", &call.loc);
        self.property("callee", call.callee.as_ref(), Printer::print_expression);
        self.array_property(
            "arguments",
            call.arguments.as_ref(),
            Printer::print_expression,
        );
        self.property("optional", call.is_optional, Printer::print_bool);
        self.end_node();
    }

    fn print_new_expression(&mut self, new: &NewExpression) {
        self.start_node("NewExpression", &new.loc);
        self.property("callee", new.callee.as_ref(), Printer::print_expression);
        self.array_property(
            "arguments",
            new.arguments.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_sequence_expression(&mut self, seq: &SequenceExpression) {
        self.start_node("SequenceExpression", &seq.loc);
        self.array_property(
            "expressions",
            seq.expressions.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_this_expression(&mut self, loc: &Loc) {
        self.start_node("ThisExpression", loc);
        self.end_node();
    }

    fn print_array_expression(&mut self, arr: &ArrayExpression) {
        self.start_node("ArrayExpression", &arr.loc);
        self.array_property(
            "elements",
            arr.elements.as_ref(),
            Printer::print_optional_expression_in_array,
        );
        self.end_node();
    }

    fn print_object_expression(&mut self, obj: &ObjectExpression) {
        self.start_node("ObjectExpression", &obj.loc);
        self.array_property(
            "properties",
            obj.properties.as_ref(),
            Printer::print_property,
        );
        self.end_node();
    }

    fn print_property(&mut self, prop: &Property) {
        self.start_node("Property", &prop.loc);
        self.property("key", prop.key.as_ref(), Printer::print_expression);
        self.property(
            "value",
            prop.value.as_ref(),
            Printer::print_optional_expression,
        );
        self.property("computed", prop.is_computed, Printer::print_bool);
        self.property("shorthand", prop.value.is_none(), Printer::print_bool);
        self.property("method", prop.is_method, Printer::print_bool);
        self.property("kind", prop.kind, Printer::print_property_kind);
        self.end_node();
    }

    fn print_property_kind(&mut self, kind: PropertyKind) {
        let str = match kind {
            PropertyKind::Init => "init",
            PropertyKind::Get => "get",
            PropertyKind::Set => "set",
        };
        self.print_str(str)
    }

    fn print_await_expression(&mut self, expr: &AwaitExpression) {
        self.start_node("AwaitExpression", &expr.loc);
        self.property(
            "argument",
            expr.argument.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_yield_expression(&mut self, expr: &YieldExpression) {
        self.start_node("YieldExpression", &expr.loc);
        self.property(
            "argument",
            expr.argument.as_ref(),
            Printer::print_optional_expression,
        );
        self.property("delegate", expr.delegate, Printer::print_bool);
        self.end_node();
    }

    fn print_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Id(id) => self.print_identifier(id),
            Pattern::Array(patt) => self.print_array_pattern(patt),
            Pattern::Object(patt) => self.print_object_pattern(patt),
            Pattern::Assign(patt) => self.print_assign_pattern(patt),
        }
    }

    fn print_identifier(&mut self, id: &Identifier) {
        self.start_node("Identifier", &id.loc);
        self.property("name", &id.name, Printer::print_string);
        self.end_node();
    }

    fn print_array_pattern(&mut self, patt: &ArrayPattern) {
        self.start_node("ArrayPattern", &patt.loc);
        self.array_property(
            "elements",
            patt.elements.as_ref(),
            Printer::print_optional_pattern_in_array,
        );
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

    fn print_optional_expression(&mut self, expr: Option<&P<Expression>>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_expression(expr),
        }
    }

    fn print_optional_expression_in_array(&mut self, expr: &Option<Expression>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_expression(expr),
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

    fn print_optional_pattern_in_array(&mut self, pattern: &Option<Pattern>) {
        match pattern {
            None => self.print_null(),
            Some(pattern) => self.print_pattern(pattern),
        }
    }
}

// Prints JSON in ESTree format
pub fn print_program(program: &Program, source: &Source) -> String {
    let mut printer = Printer::new(source);
    printer.print_program(program);
    printer.finish()
}
