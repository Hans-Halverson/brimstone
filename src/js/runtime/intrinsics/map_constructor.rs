use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{type_error, type_error_},
        function::get_argument,
        gc::{Gc, HandleValue},
        get,
        iterator::iter_iterator_values,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::{Value, ValueMap},
        Completion, Context, Handle,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 24.1 Map Objects
extend_object! {
    pub struct MapObject {
        map_data: ValueMap<Value>,
    }
}

impl MapObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<MapObject>> {
        let mut object = maybe!(object_create_from_constructor::<MapObject>(
            cx,
            constructor,
            ObjectKind::MapObject,
            Intrinsic::MapPrototype
        ));

        object.map_data = ValueMap::new();

        Handle::from_heap(object).into()
    }

    pub fn map_data(&mut self) -> &mut ValueMap<Value> {
        &mut self.map_data
    }
}

pub struct MapConstructor;

impl MapConstructor {
    // 24.1.1 The Map Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
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
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::MapPrototype).into(),
                false,
                false,
                false,
            ),
        );

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 24.1.1.1 Map
    fn construct(
        cx: &mut Context,
        _: HandleValue,
        arguments: &[HandleValue],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "Map constructor must be called with new");
        };

        let map_object: Handle<ObjectValue> =
            maybe!(MapObject::new_from_constructor(cx, new_target)).into();

        let iterable = get_argument(arguments, 0);
        if iterable.is_nullish() {
            return map_object.into();
        }

        let adder = maybe!(get(cx, map_object, cx.names.set_()));
        if !is_callable(adder) {
            return type_error_(cx, "map must contain a set method");
        }

        add_entries_from_iterable(cx, map_object.into(), iterable, adder.as_object())
    }

    // 24.1.2.2 get Map [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        this_value.into()
    }
}

// 24.1.1.2 AddEntriesFromIterable
fn add_entries_from_iterable(
    cx: &mut Context,
    target: HandleValue,
    iterable: HandleValue,
    adder: Handle<ObjectValue>,
) -> EvalResult<HandleValue> {
    let completion = iter_iterator_values(cx, iterable, &mut |cx, entry| {
        if !entry.is_object() {
            return Some(type_error(cx, "entry must be an object"));
        }

        let entry = entry.as_object();

        // Extract key from entry, returning throw completion on error
        let key_index = PropertyKey::array_index(cx, 0);
        let key_result = get(cx, entry, key_index);
        let key = match key_result {
            EvalResult::Ok(key) => key,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        };

        // Extract value from entry, returning throw completion on error
        let value_index = PropertyKey::array_index(cx, 1);
        let value_result = get(cx, entry, value_index);
        let value = match value_result {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        };

        // Add key and value to target
        let result = call_object(cx, adder, target, &[key, value]);
        match result {
            EvalResult::Ok(_) => None,
            EvalResult::Throw(thrown_value) => return Some(Completion::throw(thrown_value)),
        }
    });

    maybe!(completion.into_eval_result());

    target.into()
}
