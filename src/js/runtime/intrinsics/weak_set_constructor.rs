use crate::runtime::{
    abstract_operations::{call_object, get},
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    iterator::{get_iterator, iterator_close, iterator_step, iterator_value, IteratorHint},
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::is_callable,
    Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_set_object::WeakSetObject};

pub struct WeakSetConstructor;

impl WeakSetConstructor {
    /// Properties of the WeakSet Constructor (https://tc39.es/ecma262/#sec-properties-of-the-weakset-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.weak_set(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::WeakSetPrototype).into(),
        )?;

        Ok(func)
    }

    /// WeakSet (https://tc39.es/ecma262/#sec-weakset-iterable)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "WeakSet constructor must be called with new");
        };

        let weak_set = WeakSetObject::new_from_constructor(cx, new_target)?;

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return Ok(weak_set.as_value());
        }

        let adder = get(cx, weak_set.into(), cx.names.add())?;
        if !is_callable(adder) {
            return type_error(cx, "WeakSet adder is not callable");
        }

        let iterator = get_iterator(cx, iterable, IteratorHint::Sync, None)?;

        loop {
            let next = iterator_step(cx, &iterator)?;
            match next {
                None => return Ok(weak_set.as_value()),
                Some(next) => {
                    let next_value = iterator_value(cx, next)?;

                    let add_result =
                        call_object(cx, adder.as_object(), weak_set.into(), &[next_value]);

                    if add_result.is_err() {
                        return iterator_close(cx, iterator.iterator, add_result);
                    }
                }
            }
        }
    }
}
