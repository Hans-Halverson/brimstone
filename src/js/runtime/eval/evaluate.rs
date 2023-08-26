use core::panic;
use std::{error::Error, fmt, rc::Rc};

use crate::js::{
    parser::ast,
    runtime::{
        completion::CompletionKind, console::to_console_string, realm::Realm, Context, Handle,
    },
};

use super::script::eval_script;

#[derive(Debug)]
pub struct EvalError {
    message: String,
}

impl Error for EvalError {}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn evaluate(
    cx: Context,
    program: Rc<ast::Program>,
    realm: Handle<Realm>,
) -> Result<(), EvalError> {
    let completion = eval_script(cx, program, realm);

    match completion.kind() {
        CompletionKind::Normal => Ok(()),
        CompletionKind::Throw => {
            let value = completion.value();
            let string_value = to_console_string(cx, value);

            return Err(EvalError { message: string_value });
        }
        CompletionKind::Return => panic!("Cannot return at top level"),
        CompletionKind::Break => panic!("Cannot break at top level"),
        CompletionKind::Continue => panic!("Cannot continue at top level"),
    }
}
