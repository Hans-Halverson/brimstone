use std::rc::Rc;

use crate::js::{
    parser::ast,
    runtime::{Completion, Context, Gc, Realm},
};

pub fn eval_module(cx: &mut Context, program: Rc<ast::Program>, realm: Gc<Realm>) -> Completion {
    unimplemented!("module evaluation")
}
