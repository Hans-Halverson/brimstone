use crate::{
    intrinsic_methods, must,
    runtime::{
        Context, Handle,
        abstract_operations::{GroupByKeyCoercion, call_object, construct, group_by},
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, map_object::MapObject, rust_runtime::RuntimeFunction},
        iterator::iter_iterator_values,
        object_value::ObjectValue,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::Value,
    },
    runtime_fn,
};

pub struct MapConstructor;

impl MapConstructor {
    /// The Map Constructor (https://tc39.es/ecma262/#sec-map-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::MapConstructor_construct,
            0,
            cx.names.map(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::MapPrototype)?;

        intrinsic_methods!(cx, builder, {
            group_by MapConstructor_group_by (2),
        });

        // get Map [ %Symbol.species% ] (https://tc39.es/ecma262/#sec-get-map-%symbol.species%)
        builder.getter(cx.symbols.species(), RuntimeFunction::ReturnThis)?;

        builder.build()
    }

    runtime_fn! {
    /// Map (https://tc39.es/ecma262/#sec-map-iterable)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "Map constructor must be called with new");
        };

        let map_object = MapObject::new_from_constructor(cx, new_target)?.as_object();

        let iterable = arguments.get(cx, 0);
        if iterable.is_nullish() {
            return Ok(map_object.as_value());
        }

        let adder = get(cx, map_object, cx.names.set_())?;
        if !is_callable(adder) {
            return type_error(cx, "Map constructor result must have a `set` method");
        }

        add_entries_from_iterable(cx, map_object.into(), iterable, |cx, key, value| {
            call_object(cx, adder.as_object(), map_object.into(), &[key, value])?;
            Ok(())
        })
    }}

    runtime_fn! {
    /// Map.groupBy (https://tc39.es/ecma262/#sec-map.groupby)
    fn group_by(cx, _, arguments) {
        let items = arguments.get(cx, 0);
        let callback = arguments.get(cx, 1);

        let groups = group_by(cx, items, callback, GroupByKeyCoercion::Collection)?;

        let map_constructor = cx.get_intrinsic(Intrinsic::MapConstructor);
        let map = must!(construct(cx, map_constructor, &[], None));

        for group in groups {
            let items: Handle<Value> = create_array_from_list(cx, &group.items)?.into();

            map.cast::<MapObject>().insert(cx, group.key, items)?;
        }

        Ok(map.as_value())
    }}
}

/// AddEntriesFromIterable (https://tc39.es/ecma262/#sec-add-entries-from-iterable)
pub fn add_entries_from_iterable(
    cx: Context,
    target: Handle<Value>,
    iterable: Handle<Value>,
    mut adder: impl FnMut(Context, Handle<Value>, Handle<Value>) -> EvalResult<()>,
) -> EvalResult<Handle<Value>> {
    let key_index = PropertyKey::array_index_handle(cx, 0)?;
    let value_index = PropertyKey::array_index_handle(cx, 1)?;

    iter_iterator_values(cx, iterable, &mut |cx, entry| {
        if !entry.is_object() {
            return Some(type_error(cx, "entry must be an object"));
        }

        let entry = entry.as_object();

        // Extract key from entry, returning throw completion on error
        let key_result = get(cx, entry, key_index);
        let key = match key_result {
            Ok(key) => key,
            Err(_) => return Some(key_result),
        };

        // Extract value from entry, returning throw completion on error
        let value_result = get(cx, entry, value_index);
        let value = match value_result {
            Ok(value) => value,
            Err(_) => return Some(value_result),
        };

        // Add key and value to target
        let result = adder(cx, key, value);
        match result {
            Ok(_) => None,
            Err(thrown_value) => Some(Err(thrown_value)),
        }
    })?;

    Ok(target)
}
