use crate::runtime::{
    abstract_operations::{call_object, get},
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::is_callable,
    Context, Handle, Value,
};

use super::{
    intrinsics::Intrinsic, map_constructor::add_entries_from_iterable,
    weak_map_object::WeakMapObject,
};

pub struct WeakMapConstructor;

impl WeakMapConstructor {
    /// Properties of the WeakMap Constructor (https://tc39.es/ecma262/#sec-properties-of-the-weakmap-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.weak_map(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::WeakMapPrototype).into(),
        )?;

        Ok(func)
    }

    /// WeakMap (https://tc39.es/ecma262/#sec-weakmap-iterable)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "WeakMap constructor must be called with new");
        };

        let weak_map = WeakMapObject::new_from_constructor(cx, new_target)?;

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return Ok(weak_map.as_value());
        }

        let adder = get(cx, weak_map.into(), cx.names.set_())?;
        if !is_callable(adder) {
            return type_error(cx, "WeakMap adder is not callable");
        }

        add_entries_from_iterable(cx, weak_map.into(), iterable, |cx, key, value| {
            call_object(cx, adder.as_object(), weak_map.into(), &[key, value])?;
            Ok(())
        })
    }
}
