use crate::{
    js::runtime::{
        abstract_operations::{call_object, get},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        iterator::{get_iterator, iterator_close, iterator_step, iterator_value, IteratorHint},
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
        Context, Handle, Value,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, weak_set_object::WeakSetObject};

pub struct WeakSetConstructor;

impl WeakSetConstructor {
    /// Properties of the WeakSet Constructor (https://tc39.es/ecma262/#sec-properties-of-the-weakset-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.weak_set(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::WeakSetPrototype).into(),
        );

        func
    }

    /// WeakSet (https://tc39.es/ecma262/#sec-weakset-iterable)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "WeakSet constructor must be called with new");
        };

        let weak_set = maybe!(WeakSetObject::new_from_constructor(cx, new_target));

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return weak_set.into();
        }

        let adder = maybe!(get(cx, weak_set.into(), cx.names.add()));
        if !is_callable(adder) {
            return type_error(cx, "WeakSet adder is not callable");
        }

        let iterator = maybe!(get_iterator(cx, iterable, IteratorHint::Sync, None));

        loop {
            let next = maybe!(iterator_step(cx, &iterator));
            match next {
                None => return weak_set.into(),
                Some(next) => {
                    let next_value = maybe!(iterator_value(cx, next));

                    let add_result =
                        call_object(cx, adder.as_object(), weak_set.into(), &[next_value]);

                    if let EvalResult::Throw(_) = add_result {
                        return iterator_close(cx, &iterator, add_result);
                    }
                }
            }
        }
    }
}
