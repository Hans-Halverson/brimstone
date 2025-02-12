use crate::js::runtime::{
    builtin_function::BuiltinFunction, error::type_error, eval_result::EvalResult, gc::Handle,
    object_descriptor::ObjectKind, object_value::ObjectValue,
    ordinary_object::object_create_from_constructor, realm::Realm, Context, Value,
};

use super::intrinsics::Intrinsic;

pub struct IteratorConstructor;

impl IteratorConstructor {
    /// Properties of the Iterator Constructor (https://tc39.es/ecma262/#sec-properties-of-the-iterator-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.iterator(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::IteratorPrototype).into(),
        );

        func
    }

    /// Iterator (https://tc39.es/ecma262/#sec-iterator-constructor)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let throw_type_error = match new_target {
            None => true,
            Some(new_target) => new_target.ptr_eq(&cx.current_function()),
        };

        if throw_type_error {
            return type_error(cx, "Iterator is not a constructor");
        }

        let object = object_create_from_constructor::<ObjectValue>(
            cx,
            new_target.unwrap(),
            ObjectKind::OrdinaryObject,
            Intrinsic::IteratorPrototype,
        )?;

        Ok(object.to_handle().as_value())
    }
}
