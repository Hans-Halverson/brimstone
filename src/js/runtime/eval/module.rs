use std::rc::Rc;

use crate::js::{
    parser::ast,
    runtime::{Completion, Context, Gc, Realm},
};

#[allow(dead_code)]
pub fn eval_module(_cx: &mut Context, _program: Rc<ast::Program>, _realm: Gc<Realm>) -> Completion {
    unimplemented!("module evaluation")
}
