use super::{ast, ast_visitor::AstVisitor};

pub struct Analyzer {}

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {}
    }
}

impl AstVisitor for Analyzer {}

pub fn analyze(program: &ast::Program) {
    let mut analyzer = Analyzer::new();
    analyzer.visit_program(program);
}
