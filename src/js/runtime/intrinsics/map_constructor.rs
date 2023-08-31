use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{type_error, type_error_},
        function::get_argument,
        get,
        iterator::iter_iterator_values,
        object_value::ObjectValue,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::Value,
        Completion, Context, Handle,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, map_object::MapObject};

pub struct MapConstructor;

impl MapConstructor {
    // 24.1.1 The Map Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.map(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::MapPrototype).into(),
        );

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 24.1.1.1 Map
    fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "Map constructor must be called with new");
        };

        let map_object: Handle<ObjectValue> =
            maybe!(MapObject::new_from_constructor(cx, new_target)).into();

        let iterable = get_argument(cx, arguments, 0);
        if iterable.is_nullish() {
            return map_object.into();
        }

        let adder = maybe!(get(cx, map_object, cx.names.set_()));
        if !is_callable(adder) {
            return type_error_(cx, "map must contain a set method");
        }

        add_entries_from_iterable(cx, map_object.into(), iterable, |cx, key, value| {
            maybe!(call_object(cx, adder.as_object(), map_object.into(), &[key, value]));
            ().into()
        })
    }

    // 24.1.2.2 get Map [ @@species ]
    fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}

// 24.1.1.2 AddEntriesFromIterable
pub fn add_entries_from_iterable(
    cx: Context,
    target: Handle<Value>,
    iterable: Handle<Value>,
    mut adder: impl FnMut(Context, Handle<Value>, Handle<Value>) -> EvalResult<()>,
) -> EvalResult<Handle<Value>> {
    let key_index = PropertyKey::array_index(cx, 0).to_handle(cx);
    let value_index = PropertyKey::array_index(cx, 1).to_handle(cx);

    let completion = iter_iterator_values(cx, iterable, &mut |cx, entry| {
        if !entry.is_object() {
            return Some(type_error(cx, "entry must be an object"));
        }

        let entry = entry.as_object();

        // Extract key from entry, returning throw completion on error
        let key_result = get(cx, entry, key_index);
        let key = match key_result {
            EvalResult::Ok(key) => key,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        };

        // Extract value from entry, returning throw completion on error
        let value_result = get(cx, entry, value_index);
        let value = match value_result {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        };

        // Add key and value to target
        let result = adder(cx, key, value);
        match result {
            EvalResult::Ok(_) => None,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        }
    });

    maybe!(completion.into_eval_result());

    target.into()
}
