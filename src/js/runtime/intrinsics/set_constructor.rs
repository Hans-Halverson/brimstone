use crate::runtime::{
    abstract_operations::call_object, alloc_error::AllocResult, builtin_function::BuiltinFunction,
    error::type_error, eval_result::EvalResult, function::get_argument, get,
    intrinsics::set_object::SetObject, iterator::iter_iterator_values, object_value::ObjectValue,
    realm::Realm, type_utilities::is_callable, Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

pub struct SetConstructor;

impl SetConstructor {
    /// The Set Constructor (https://tc39.es/ecma262/#sec-set-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.set(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::SetPrototype).into(),
        )?;

        // get Set [ @@species ] (https://tc39.es/ecma262/#sec-get-set-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm)?;

        Ok(func)
    }

    /// Set (https://tc39.es/ecma262/#sec-set-iterable)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "Set constructor must be called with new");
        };

        let set_object = SetObject::new_from_constructor(cx, new_target)?.as_object();

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return Ok(set_object.as_value());
        }

        let adder = get(cx, set_object, cx.names.add())?;
        if !is_callable(adder) {
            return type_error(cx, "set must contain an add method");
        }

        let adder = adder.as_object();
        let set_value = set_object.as_value();

        iter_iterator_values(cx, iterable, &mut |cx, value| {
            let result = call_object(cx, adder, set_value, &[value]);
            match result {
                Ok(_) => None,
                Err(_) => Some(result),
            }
        })?;

        Ok(set_value)
    }
}
