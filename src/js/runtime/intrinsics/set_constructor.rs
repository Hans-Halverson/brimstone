use crate::{
    js::runtime::{
        abstract_operations::call_object, builtin_function::BuiltinFunction,
        completion::EvalResult, error::type_error, function::get_argument, get,
        intrinsics::set_object::SetObject, iterator::iter_iterator_values,
        object_value::ObjectValue, realm::Realm, type_utilities::is_callable, Context, Handle,
        Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct SetConstructor;

impl SetConstructor {
    // 24.2.1 The Set Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.set(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::SetPrototype).into(),
        );

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 24.2.1.1 Set
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "Set constructor must be called with new");
        };

        let set_object: Handle<ObjectValue> =
            maybe!(SetObject::new_from_constructor(cx, new_target)).into();

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return set_object.into();
        }

        let adder = maybe!(get(cx, set_object, cx.names.add()));
        if !is_callable(adder) {
            return type_error(cx, "set must contain an add method");
        }

        let adder = adder.as_object();
        let set_value = set_object.into();

        maybe!(iter_iterator_values(cx, iterable, &mut |cx, value| {
            let result = call_object(cx, adder, set_value, &[value]);
            match result {
                EvalResult::Ok(_) => None,
                EvalResult::Throw(_) => Some(result),
            }
        }));

        set_value.into()
    }

    // 24.2.2.2 get Set [ @@species ]
    pub fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
