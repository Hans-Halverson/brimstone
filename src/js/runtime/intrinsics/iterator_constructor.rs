use crate::{
    extend_object,
    runtime::{
        abstract_operations::{call, call_object, get_method, ordinary_has_instance},
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        gc::{Handle, HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        iterator::{create_iter_result_object, get_iterator_flattenable},
        object_value::ObjectValue,
        ordinary_object::{object_create_from_constructor, object_create_with_proto},
        realm::Realm,
        Context, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

pub struct IteratorConstructor;

impl IteratorConstructor {
    /// Properties of the Iterator Constructor (https://tc39.es/ecma262/#sec-properties-of-the-iterator-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.iterator(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::IteratorPrototype).into(),
        )?;

        func.intrinsic_func(cx, cx.names.from(), Self::from, 1, realm)?;

        Ok(func)
    }

    /// Iterator (https://tc39.es/ecma262/#sec-iterator-constructor)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = cx.current_new_target();
        let throw_type_error = match new_target {
            None => true,
            Some(new_target) => new_target.ptr_eq(&cx.current_function()),
        };

        if throw_type_error {
            return type_error(cx, "Iterator is not a constructor");
        }

        let object = object_create_from_constructor::<ObjectValue>(
            cx,
            new_target.unwrap(),
            HeapItemKind::OrdinaryObject,
            Intrinsic::IteratorPrototype,
        )?;

        Ok(object.to_handle().as_value())
    }

    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        let iterator = get_iterator_flattenable(cx, value, /* reject_primitives */ false)?;

        let iterator_constructor = cx.get_intrinsic(Intrinsic::IteratorConstructor);
        if ordinary_has_instance(cx, iterator_constructor.as_value(), iterator.iterator.as_value())?
        {
            return Ok(iterator.iterator.as_value());
        }

        Ok(WrappedValidIterator::new(cx, iterator.iterator, iterator.next_method)?.as_value())
    }
}

// A WrappedValidIterator wraps an iterator object and its next method.
extend_object! {
    pub struct WrappedValidIterator {
        iterator: HeapPtr<ObjectValue>,
        next_method: Value,
    }
}

impl WrappedValidIterator {
    fn new(
        cx: Context,
        iterator: Handle<ObjectValue>,
        next_method: Handle<Value>,
    ) -> AllocResult<Handle<ObjectValue>> {
        let mut object = object_create_with_proto::<WrappedValidIterator>(
            cx,
            HeapItemKind::WrappedValidIterator,
            cx.get_intrinsic(Intrinsic::WrapForValidIteratorPrototype),
        )?;

        set_uninit!(object.iterator, *iterator);
        set_uninit!(object.next_method, *next_method);

        Ok(object.as_object().to_handle())
    }

    fn iterator(&self) -> Handle<ObjectValue> {
        self.iterator.to_handle()
    }

    fn next_method(&self, cx: Context) -> Handle<Value> {
        self.next_method.to_handle(cx)
    }
}

impl HeapItem for HeapPtr<WrappedValidIterator> {
    fn byte_size(&self) -> usize {
        size_of::<WrappedValidIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.iterator);
        visitor.visit_value(&mut self.next_method);
    }
}

/// The WrapForValidIteratorPrototype Object (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%-object)
pub struct WrapForValidIteratorPrototype;

impl WrapForValidIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let iterator_prototype = realm.get_intrinsic(Intrinsic::IteratorPrototype);
        let mut object = object_create_with_proto::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            iterator_prototype,
        )?
        .to_handle();

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm)?;
        object.intrinsic_func(cx, cx.names.return_(), Self::return_, 0, realm)?;

        Ok(object)
    }

    /// %WrapForValidIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Check if called on a WrappedIteratorObject
        if this_value.is_object() {
            if let Some(wrapper) = this_value.as_object().as_wrapped_valid_iterator_object() {
                // Call the wrapped iterator's next method
                let iterator = wrapper.iterator().as_value();
                return call(cx, wrapper.next_method(cx), iterator, &[]);
            }
        }

        type_error(cx, "%WrappedValidIterator%.prototype.next called on incompatible object")
    }

    /// %WrapForValidIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%.next)
    pub fn return_(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Check if called on a WrappedIteratorObject
        if this_value.is_object() {
            if let Some(wrapper) = this_value.as_object().as_wrapped_valid_iterator_object() {
                // Call the wrapped iterator's return method if one exists
                let iterator = wrapper.iterator().as_value();
                return match get_method(cx, iterator, cx.names.return_())? {
                    None => Ok(create_iter_result_object(cx, cx.undefined(), true)?),
                    Some(return_) => call_object(cx, return_, iterator, &[]),
                };
            }
        }

        type_error(cx, "%WrappedValidIterator%.prototype.return called on incompatible object")
    }
}
