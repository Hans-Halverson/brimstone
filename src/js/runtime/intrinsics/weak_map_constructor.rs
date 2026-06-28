use crate::{
    runtime::{
        Context, Handle,
        abstract_operations::{call_object, get},
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, map_constructor::add_entries_from_iterable,
            rust_runtime::RuntimeFunction, weak_map_object::WeakMapObject,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
    },
    runtime_fn,
};

pub struct WeakMapConstructor;

impl WeakMapConstructor {
    /// Properties of the WeakMap Constructor (https://tc39.es/ecma262/#sec-properties-of-the-weakmap-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::WeakMapConstructor_construct,
            0,
            cx.names.weak_map(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::WeakMapPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// WeakMap (https://tc39.es/ecma262/#sec-weakmap-iterable)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "WeakMap constructor must be called with new");
        };

        let weak_map = WeakMapObject::new_from_constructor(cx, new_target)?;

        let iterable = arguments.get(cx, 0);
        if iterable.is_nullish() {
            return Ok(weak_map.as_value());
        }

        let adder = get(cx, weak_map.into(), cx.names.set_())?;
        if !is_callable(adder) {
            return type_error(cx, "WeakMap constructor result must have a `set` method");
        }

        add_entries_from_iterable(cx, weak_map.into(), iterable, |cx, key, value| {
            call_object(cx, adder.as_object(), weak_map.into(), &[key, value])?;
            Ok(())
        })
    }}
}
