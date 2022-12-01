use super::loc::{find_line_col_for_pos, Loc};
use super::{ast, source::Source};

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

    fn print_null(&mut self) {
        self.string("null")
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

    fn print_program(&mut self, program: &ast::Program) {
        self.start_node("Program", &program.loc);
        self.array_property("body", &program.toplevels, Printer::print_toplevel);
        self.end_node();
    }

    fn print_toplevel(&mut self, toplevel: &ast::Toplevel) {
        match toplevel {
            ast::Toplevel::Statement(stmt) => self.print_statement(stmt),
        }
    }

    fn print_statement(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::VarDecl(var_decl) => self.print_variable_declaration(var_decl),
            ast::Statement::Expr(expr) => self.print_expression_statement(expr),
        }
    }

    fn print_variable_declaration(&mut self, var_decl: &ast::VariableDeclaration) {
        self.start_node("VariableDeclaration", &var_decl.loc);
        self.property("kind", &var_decl.kind, Printer::print_var_kind);
        self.array_property(
            "declarations",
            var_decl.declarations.as_ref(),
            Printer::print_variable_declarator,
        );
        self.end_node();
    }

    fn print_expression_statement(&mut self, expr: &ast::ExpressionStatement) {
        self.start_node("ExpressionStatement", &expr.loc);
        self.property("kind", expr.expr.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_var_kind(&mut self, kind: &ast::VarKind) {
        let str = match kind {
            ast::VarKind::Var => "var",
            ast::VarKind::Let => "let",
            ast::VarKind::Const => "const",
        };
        self.print_str(str);
    }

    fn print_variable_declarator(&mut self, var_decl: &ast::VariableDeclarator) {
        self.start_node("VariableDeclarator", &var_decl.loc);
        self.property("id", &(*var_decl.id), Printer::print_pattern);
        if let Some(ref init) = var_decl.init {
            self.property("id", (*init).as_ref(), Printer::print_expression);
        }
        self.end_node();
    }

    fn print_expression(&mut self, expr: &ast::Expression) {
        match expr {
            ast::Expression::Id(id) => self.print_identifier(id),
            ast::Expression::Unary(unary) => self.print_unary_expression(unary),
            ast::Expression::Binary(binary) => self.print_binary_expression(binary),
            ast::Expression::Logical(logical) => self.print_logical_expression(logical),
            ast::Expression::Assign(assign) => self.print_assignment_expression(assign),
            ast::Expression::Update(update) => self.print_update_expression(update),
            ast::Expression::Member(member) => self.print_member_expression(member),
            ast::Expression::Conditional(cond) => self.print_conditional_expression(cond),
            ast::Expression::Call(call) => self.print_call_expression(call),
            ast::Expression::New(new) => self.print_new_expression(new),
            ast::Expression::Sequence(seq) => self.print_sequence_expression(seq),
            ast::Expression::This(loc) => self.print_this_expression(&loc),
            ast::Expression::Array(arr) => self.print_array_expression(arr),
            ast::Expression::Await(expr) => self.print_await_expression(expr),
            ast::Expression::Yield(expr) => self.print_yield_expression(expr),
        }
    }

    fn print_optional_expression(&mut self, expr: Option<&ast::P<ast::Expression>>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_expression(expr),
        }
    }

    fn print_optional_expression_in_array(&mut self, expr: &Option<ast::Expression>) {
        match expr {
            None => self.print_null(),
            Some(expr) => self.print_expression(expr),
        }
    }

    fn print_unary_operator(&mut self, op: &ast::UnaryOperator) {
        let str = match op {
            ast::UnaryOperator::Plus => "+",
            ast::UnaryOperator::Minus => "-",
            ast::UnaryOperator::LogicalNot => "!",
            ast::UnaryOperator::BitwiseNot => "~",
            ast::UnaryOperator::TypeOf => "typeof",
            ast::UnaryOperator::Void => "void",
            ast::UnaryOperator::Delete => "delete",
        };
        self.print_str(str);
    }

    fn print_unary_expression(&mut self, unary: &ast::UnaryExpression) {
        self.start_node("UnaryExpression", &unary.loc);
        self.property("operator", &unary.operator, Printer::print_unary_operator);
        self.property(
            "argument",
            unary.argument.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_binary_operator(&mut self, op: &ast::BinaryOperator) {
        let str = match op {
            ast::BinaryOperator::Add => "+",
            ast::BinaryOperator::Subtract => "-",
            ast::BinaryOperator::Multiply => "*",
            ast::BinaryOperator::Divide => "/",
            ast::BinaryOperator::Remainder => "%",
            ast::BinaryOperator::Exponent => "**",
            ast::BinaryOperator::EqEq => "==",
            ast::BinaryOperator::EqEqEq => "===",
            ast::BinaryOperator::NotEq => "!=",
            ast::BinaryOperator::NotEqEq => "!==",
            ast::BinaryOperator::LessThan => "<",
            ast::BinaryOperator::LessThanOrEqual => "<=",
            ast::BinaryOperator::GreaterThan => ">",
            ast::BinaryOperator::GreaterThanOrEqual => ">=",
            ast::BinaryOperator::And => "&",
            ast::BinaryOperator::Or => "|",
            ast::BinaryOperator::Xor => "^",
            ast::BinaryOperator::ShiftLeft => "<<",
            ast::BinaryOperator::ShiftRightArithmetic => ">>",
            ast::BinaryOperator::ShiftRightLogical => ">>>",
            ast::BinaryOperator::In => "in",
            ast::BinaryOperator::InstanceOf => "instanceof",
        };
        self.print_str(str);
    }

    fn print_binary_expression(&mut self, binary: &ast::BinaryExpression) {
        self.start_node("BinaryExpression", &binary.loc);
        self.property("operator", &binary.operator, Printer::print_binary_operator);
        self.property("left", binary.left.as_ref(), Printer::print_expression);
        self.property("right", binary.right.as_ref(), Printer::print_expression);
        self.end_node();
    }

    fn print_logical_operator(&mut self, op: &ast::LogicalOperator) {
        let str = match op {
            ast::LogicalOperator::And => "&",
            ast::LogicalOperator::Or => "|",
            ast::LogicalOperator::NullishCoalesce => "??",
        };
        self.print_str(str);
    }

    fn print_logical_expression(&mut self, logical: &ast::LogicalExpression) {
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

    fn print_assignment_operator(&mut self, op: &ast::AssignmentOperator) {
        let str = match op {
            ast::AssignmentOperator::Equals => "=",
            ast::AssignmentOperator::Add => "+=",
            ast::AssignmentOperator::Subtract => "-=",
            ast::AssignmentOperator::Multiply => "*=",
            ast::AssignmentOperator::Divide => "/=",
            ast::AssignmentOperator::Remainder => "%=",
            ast::AssignmentOperator::Exponent => "**=",
            ast::AssignmentOperator::And => "&=",
            ast::AssignmentOperator::Or => "|=",
            ast::AssignmentOperator::Xor => "^=",
            ast::AssignmentOperator::ShiftLeft => "<<=",
            ast::AssignmentOperator::ShiftRightArithmetic => ">>=",
            ast::AssignmentOperator::ShiftRightLogical => ">>>=",
        };
        self.print_str(str);
    }

    fn print_assignment_expression(&mut self, assign: &ast::AssignmentExpression) {
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

    fn print_update_operator(&mut self, op: &ast::UpdateOperator) {
        let str = match op {
            ast::UpdateOperator::Increment => "++",
            ast::UpdateOperator::Decrement => "--",
        };
        self.print_str(str);
    }

    fn print_update_expression(&mut self, update: &ast::UpdateExpression) {
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

    fn print_member_expression(&mut self, member: &ast::MemberExpression) {
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

    fn print_conditional_expression(&mut self, cond: &ast::ConditionalExpression) {
        self.start_node("ConditionalExpression", &cond.loc);
        self.property("test", cond.test.as_ref(), Printer::print_expression);
        self.property("alternate", cond.altern.as_ref(), Printer::print_expression);
        self.property(
            "consequent",
            cond.conseq.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_call_expression(&mut self, call: &ast::CallExpression) {
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

    fn print_new_expression(&mut self, new: &ast::NewExpression) {
        self.start_node("NewExpression", &new.loc);
        self.property("callee", new.callee.as_ref(), Printer::print_expression);
        self.array_property(
            "arguments",
            new.arguments.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_sequence_expression(&mut self, seq: &ast::SequenceExpression) {
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

    fn print_array_expression(&mut self, arr: &ast::ArrayExpression) {
        self.start_node("ArrayExpression", &arr.loc);
        self.array_property(
            "elements",
            arr.elements.as_ref(),
            Printer::print_optional_expression_in_array,
        );
        self.end_node();
    }

    fn print_await_expression(&mut self, expr: &ast::AwaitExpression) {
        self.start_node("AwaitExpression", &expr.loc);
        self.property(
            "argument",
            expr.argument.as_ref(),
            Printer::print_expression,
        );
        self.end_node();
    }

    fn print_yield_expression(&mut self, expr: &ast::YieldExpression) {
        self.start_node("YieldExpression", &expr.loc);
        self.property(
            "argument",
            expr.argument.as_ref(),
            Printer::print_optional_expression,
        );
        self.property("delegate", expr.delegate, Printer::print_bool);
        self.end_node();
    }

    fn print_pattern(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Id(id) => self.print_identifier(id),
        }
    }

    fn print_identifier(&mut self, id: &ast::Identifier) {
        self.start_node("Identifier", &id.loc);
        self.property("name", &id.name, Printer::print_string);
        self.end_node();
    }
}

// Prints JSON in ESTree format
pub fn print_program(program: &ast::Program, source: &Source) -> String {
    let mut printer = Printer::new(source);
    printer.print_program(program);
    printer.finish()
}
