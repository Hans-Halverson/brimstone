use std::rc::Rc;

use crate::js::{
    parser::parser::ParseProgramResult,
    runtime::{Completion, Context, Handle, Realm},
};

#[allow(dead_code)]
pub fn eval_module(
    _cx: Context,
    _program: Rc<ParseProgramResult>,
    _realm: Handle<Realm>,
) -> Completion {
    unimplemented!("module evaluation")
}
