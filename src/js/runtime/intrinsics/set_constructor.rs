use crate::{
    js::runtime::{
        abstract_operations::call_object, builtin_function::BuiltinFunction,
        completion::EvalResult, error::type_error_, function::get_argument, get,
        intrinsics::set_object::SetObject, iterator::iter_iterator_values,
        object_value::ObjectValue, property::Property, realm::Realm, type_utilities::is_callable,
        Completion, Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct SetConstructor;

impl SetConstructor {
    // 24.2.1 The Set Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.set(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::SetPrototype).into(),
                false,
                false,
                false,
            ),
        );

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 24.2.1.1 Set
    fn construct(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "Set constructor must be called with new");
        };

        let set_object: Handle<ObjectValue> =
            maybe!(SetObject::new_from_constructor(cx, new_target)).into();

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return set_object.into();
        }

        let adder = maybe!(get(cx, set_object, cx.names.add()));
        if !is_callable(adder) {
            return type_error_(cx, "set must contain an add method");
        }

        let adder = adder.as_object();
        let set_value = set_object.into();

        let completion = iter_iterator_values(cx, iterable, &mut |cx, value| {
            let result = call_object(cx, adder, set_value, &[value]);
            match result {
                EvalResult::Ok(_) => None,
                EvalResult::Throw(thrown_value) => Some(Completion::throw(thrown_value)),
            }
        });

        maybe!(completion.into_eval_result());

        set_value.into()
    }

    // 24.2.2.2 get Set [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
