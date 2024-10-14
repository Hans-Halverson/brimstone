use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        abstract_operations::set,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapObject, HeapVisitor},
        get,
        intrinsics::regexp_prototype::{advance_u64_string_index, regexp_exec},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        string_value::StringValue,
        to_string,
        type_utilities::to_length,
        Context, Handle, HeapPtr, PropertyKey, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// RegExp String Iterator Objects (https://tc39.es/ecma262/#sec-regexp-string-iterator-objects)
extend_object! {
    pub struct RegExpStringIterator {
        regexp_object: HeapPtr<ObjectValue>,
        target_string: HeapPtr<StringValue>,
        is_global: bool,
        is_unicode: bool,
        is_done: bool,
    }
}

impl RegExpStringIterator {
    pub fn new(
        cx: Context,
        regexp_object: Handle<ObjectValue>,
        target_string: Handle<StringValue>,
        is_global: bool,
        is_unicode: bool,
    ) -> Handle<RegExpStringIterator> {
        let mut object = object_create::<RegExpStringIterator>(
            cx,
            ObjectKind::RegExpStringIterator,
            Intrinsic::RegExpStringIteratorPrototype,
        );

        set_uninit!(object.regexp_object, regexp_object.get_());
        set_uninit!(object.target_string, target_string.get_());
        set_uninit!(object.is_global, is_global);
        set_uninit!(object.is_unicode, is_unicode);
        set_uninit!(object.is_done, false);

        object.to_handle()
    }

    #[inline]
    fn regexp_object(&self) -> Handle<ObjectValue> {
        self.regexp_object.to_handle()
    }

    #[inline]
    fn target_string(&self) -> Handle<StringValue> {
        self.target_string.to_handle()
    }

    cast_from_value_fn!(RegExpStringIterator, "RegExp String Iterator");
}

/// The %RegExpStringIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-object)
pub struct RegExpStringIteratorPrototype;

impl RegExpStringIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::IteratorPrototype);
        let mut object = ObjectValue::new(cx, Some(proto), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // %RegExpStringIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("RegExp String Iterator").into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    /// %RegExpStringIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut regexp_iterator = RegExpStringIterator::cast_from_value(cx, this_value)?;

        let regexp_object = regexp_iterator.regexp_object();
        let target_string = regexp_iterator.target_string();

        // Check if we have already marked the iterator as done
        if regexp_iterator.is_done {
            return Ok(create_iter_result_object(cx, cx.undefined(), true));
        }

        // Run the regular expression
        let match_result = regexp_exec(cx, regexp_object, target_string)?;

        // No match so return a completed iterator
        if match_result.is_null() {
            regexp_iterator.is_done = true;
            return Ok(create_iter_result_object(cx, cx.undefined(), true));
        }

        if !regexp_iterator.is_global {
            regexp_iterator.is_done = true;
            return Ok(create_iter_result_object(cx, match_result, false));
        }

        debug_assert!(match_result.is_object());

        // Find matched string length
        let zero_key = PropertyKey::array_index(cx, 0).to_handle(cx);
        let match_string = get(cx, match_result.as_object(), zero_key)?;
        let match_string = to_string(cx, match_string)?;

        // Increment the lastIndex if the empty string was matched to avoid infinite loops
        if match_string.len() == 0 {
            let last_index = get(cx, regexp_object, cx.names.last_index())?;
            let last_index = to_length(cx, last_index)?;

            let next_index =
                advance_u64_string_index(target_string, last_index, regexp_iterator.is_unicode);
            let next_index_value = Value::from(next_index).to_handle(cx);
            set(cx, regexp_object, cx.names.last_index(), next_index_value, true)?;
        }

        Ok(create_iter_result_object(cx, match_result, false))
    }
}

impl HeapObject for HeapPtr<RegExpStringIterator> {
    fn byte_size(&self) -> usize {
        size_of::<RegExpStringIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.regexp_object);
        visitor.visit_pointer(&mut self.target_string);
    }
}
