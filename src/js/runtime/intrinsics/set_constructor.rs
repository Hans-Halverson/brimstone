use crate::{
    runtime::{
        Context, Handle,
        abstract_operations::call_object,
        alloc_error::AllocResult,
        error::type_error,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction, set_object::SetObject},
        iterator::iter_iterator_values,
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
    },
    runtime_fn,
};

pub struct SetConstructor;

impl SetConstructor {
    /// The Set Constructor (https://tc39.es/ecma262/#sec-set-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::SetConstructor_construct,
            0,
            cx.names.set(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::SetPrototype)?;

        // get Set [ @@species ] (https://tc39.es/ecma262/#sec-get-set-%symbol.species%)
        builder.getter(cx.symbols.species(), RuntimeFunction::ReturnThis)?;

        builder.build()
    }

    runtime_fn! {
    /// Set (https://tc39.es/ecma262/#sec-set-iterable)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "Set constructor must be called with new");
        };

        let set_object = SetObject::new_from_constructor(cx, new_target)?.as_object();

        let iterable = arguments.get(cx, 0);
        if iterable.is_nullish() {
            return Ok(set_object.as_value());
        }

        let adder = get(cx, set_object, cx.names.add())?;
        if !is_callable(adder) {
            return type_error(cx, "Set constructor result must have an `add` method");
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
    }}
}
