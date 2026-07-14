use crate::{
    intrinsic_methods,
    runtime::{
        Context, Value,
        abstract_operations::{call, call_object, get_method, ordinary_has_instance},
        alloc_error::AllocResult,
        collections::array::ValueArray,
        error::type_error,
        eval_result::EvalResult,
        gc::Handle,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, iterator_helper_object::IteratorHelperObject,
            rust_runtime::RuntimeFunction,
            wrapped_valid_iterator_object::WrappedValidIteratorObject,
        },
        iterator::{create_iter_result_object, get_iterator_flattenable},
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
        realm::Realm,
    },
    runtime_fn,
};

pub struct IteratorConstructor;

impl IteratorConstructor {
    /// Properties of the Iterator Constructor (https://tc39.es/ecma262/#sec-properties-of-the-iterator-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::IteratorConstructor_construct,
            0,
            cx.names.iterator(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            concat IteratorConstructor_concat (0),
            from   IteratorConstructor_from   (1),
        });

        builder.build()
    }

    runtime_fn! {
    /// Iterator (https://tc39.es/ecma262/#sec-iterator-constructor)
    fn construct(cx, _, _) {
        let new_target = cx.current_new_target();
        let throw_type_error = match new_target {
            None => true,
            Some(new_target) => new_target.ptr_eq(&cx.current_function()),
        };

        if throw_type_error {
            return type_error(cx, "Iterator is not a constructor");
        }

        let object = ObjectBuilder::<ObjectValue>::new(cx)
            .constructor_proto(new_target.unwrap(), Intrinsic::IteratorPrototype)?
            .build()?;

        Ok(object.to_handle().as_value())
    }}

    runtime_fn! {
    /// Iterator.concat (https://tc39.es/ecma262/#sec-iterator.concat)
    fn concat(cx, _, arguments) {
        let mut iterator_methods = vec![];

        for argument in arguments.iter() {
            if !argument.is_object() {
                return type_error(cx, "Iterator.concat argument is not an object");
            }

            if let Some(method) = get_method(cx, *argument, cx.symbols.iterator())? {
                iterator_methods.push(method.as_value());
            } else {
                return type_error(cx, "Iterator.concat argument is not iterable");
            }
        }

        let iterables_array =
            ValueArray::new_from_handle_slice(cx, arguments.as_slice())?.to_handle();
        let iterator_methods_array =
            ValueArray::new_from_handle_slice(cx, &iterator_methods)?.to_handle();

        Ok(IteratorHelperObject::new_concat(cx, iterables_array, iterator_methods_array)?
            .as_value())
    }}

    runtime_fn! {
    /// Iterator.from (https://tc39.es/ecma262/#sec-iterator.from)
    fn from(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        let iterator = get_iterator_flattenable(cx, value, /* reject_primitives */ false)?;

        let iterator_constructor = cx.get_intrinsic(Intrinsic::IteratorConstructor);
        if ordinary_has_instance(cx, iterator_constructor.as_value(), iterator.iterator.as_value())?
        {
            return Ok(iterator.iterator.as_value());
        }

        Ok(
            WrappedValidIteratorObject::new(cx, iterator.iterator, iterator.next_method)?
                .as_value(),
        )
    }}
}

/// The WrapForValidIteratorPrototype Object (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%-object)
pub struct WrapForValidIteratorPrototype;

impl WrapForValidIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            next    WrapForValidIteratorPrototype_next    (0),
            return_ WrapForValidIteratorPrototype_return_ (0),
        });

        builder.build()
    }

    runtime_fn! {
    /// %WrapForValidIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%.next)
    fn next(cx, this_value, _) {
        let wrapper = this_wrapped_valid_iterator_object(cx, this_value, "next")?;

        // Call the wrapped iterator's next method
        let iterator = wrapper.iterator().as_value();
        call(cx, wrapper.next_method(cx), iterator, &[])
    }}

    runtime_fn! {
    /// %WrapForValidIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%.next)
    fn return_(cx, this_value, _) {
        let wrapper = this_wrapped_valid_iterator_object(cx, this_value, "return")?;

        // Call the wrapped iterator's return method if one exists
        let iterator = wrapper.iterator().as_value();

        match get_method(cx, iterator, cx.names.return_())? {
            None => Ok(create_iter_result_object(cx, cx.undefined(), true)?),
            Some(return_) => call_object(cx, return_, iterator, &[]),
        }
    }}
}

fn this_wrapped_valid_iterator_object(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<WrappedValidIteratorObject>> {
    if value.is_object() {
        if let Some(wrapper) = value.as_opt::<WrappedValidIteratorObject>() {
            return Ok(wrapper);
        }
    }

    type_error(
        cx,
        &format!(
            "%WrappedValidIterator%.prototype.{method_name} must be called on a WrappedValidIterator"
        ),
    )
}
