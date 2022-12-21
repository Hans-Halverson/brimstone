use std::{error::Error, fmt, rc::Rc};

use crate::js::{
    parser::ast,
    runtime::{completion::Completion, gc::Gc, realm::Realm, Context},
};

use super::script::evaluate_script;

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
    cx: &mut Context,
    program: Rc<ast::Program>,
    realm: Gc<Realm>,
) -> Result<(), EvalError> {
    let completion = evaluate_script(cx, program, realm);
    if let Completion::Throw(value) = completion {
        if value.is_string() {
            return Err(EvalError {
                message: value.as_string().to_string(),
            });
        } else {
            return Err(EvalError {
                message: "Evaluation threw value with non-string type".to_string(),
            });
        }
    }

    return Ok(());
}
