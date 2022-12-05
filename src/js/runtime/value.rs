use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Number(f64),
    Object(Rc<ObjectValue>),
    String(Box<String>),
}

pub struct ObjectValue {}

impl ObjectValue {
    pub const EMPTY: ObjectValue = ObjectValue {};
}
