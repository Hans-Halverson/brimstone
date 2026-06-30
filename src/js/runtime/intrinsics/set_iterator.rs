use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapPtr, Value,
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        collections::{BsIndexMap, IndexSetInstance, index_map::GcSafeEntriesIter},
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            set_object::{SetObject, ValueIndexSet},
        },
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        value::ValueCollectionKey,
    },
    runtime_fn, set_uninit,
};

// Set Iterator Objects (https://tc39.es/ecma262/#sec-set-iterator-objects)
extend_object! {
    pub struct SetIterator {
        // Component parts of an index_map::GcSafeEntriesIter
        set: HeapPtr<ValueIndexSet>,
        next_entry_index: usize,
        kind: SetIteratorKind,
        is_done: bool,
    }
}

pub enum SetIteratorKind {
    Value,
    KeyAndValue,
}

impl SetIterator {
    pub fn new(
        cx: Context,
        set: Handle<SetObject>,
        kind: SetIteratorKind,
    ) -> AllocResult<Handle<SetIterator>> {
        let mut object = object_create::<SetIterator>(
            cx,
            HeapItemKind::SetIterator,
            Intrinsic::SetIteratorPrototype,
        )?;

        set_uninit!(object.set, set.set_data_ptr().cast());
        set_uninit!(object.next_entry_index, 0);
        set_uninit!(object.kind, kind);
        set_uninit!(object.is_done, false);

        Ok(object.to_handle())
    }

    cast_from_value_fn!(SetIterator, "Set Iterator");

    fn get_iter(&self) -> GcSafeEntriesIter<ValueCollectionKey, ()> {
        GcSafeEntriesIter::<ValueCollectionKey, ()>::from_parts(
            self.set.to_handle().cast(),
            self.next_entry_index,
        )
    }

    fn store_iter(&mut self, iter: GcSafeEntriesIter<ValueCollectionKey, ()>) {
        let (set, next_entry_index) = iter.to_parts();
        self.set = (*set).cast();
        self.next_entry_index = next_entry_index;
    }

    fn set_inner_map(&self) -> HeapPtr<BsIndexMap<ValueCollectionKey, ()>> {
        self.set.cast()
    }
}

/// The %SetIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%setiteratorprototype%-object)
pub struct SetIteratorPrototype;

impl SetIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            next SetIteratorPrototype_next (0),
        });

        // %SetIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%setiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_value = cx.alloc_static_string("Set Iterator")?.into();
        builder.property(
            cx.symbols.to_string_tag(),
            Property::data(to_string_tag_value, false, false, true),
        )?;

        builder.build()
    }

    runtime_fn! {
    /// %SetIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%setiteratorprototype%.next)
    ///
    /// Adapted from the abstract closure in CreateSetIterator (https://tc39.es/ecma262/#sec-createsetiterator)
    fn next(cx, this_value, _) {
        let mut set_iterator = SetIterator::cast_from_value(cx, this_value)?;

        // Check if iterator is already done
        if set_iterator.is_done {
            return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
        }

        // Follow tombstone objects, fixing up iterator as needed. This may be a chain of tombstone
        // objects and we need to fix up the iterator at each step.
        while set_iterator.set_inner_map().is_tombstone() {
            set_iterator.set = ValueIndexSet::fix_iterator_for_resized_map(
                set_iterator.set,
                &mut set_iterator.next_entry_index,
            );
        }

        // Perform a single iteration, mutating iterator object
        let mut iter = set_iterator.get_iter();
        let iter_result = iter.next();
        set_iterator.store_iter(iter);

        match iter_result {
            None => {
                set_iterator.is_done = true;
                Ok(create_iter_result_object(cx, cx.undefined(), true)?)
            }
            Some((value, _)) => {
                let value_value: Value = value.into();
                let value_handle = value_value.to_handle(cx);

                match set_iterator.kind {
                    SetIteratorKind::Value => {
                        Ok(create_iter_result_object(cx, value_handle, false)?)
                    }
                    SetIteratorKind::KeyAndValue => {
                        let result_pair =
                            create_array_from_list(cx, &[value_handle, value_handle])?;
                        Ok(create_iter_result_object(cx, result_pair.into(), false)?)
                    }
                }
            }
        }
    }}
}

impl HeapItem for HeapPtr<SetIterator> {
    fn byte_size(&self) -> usize {
        size_of::<SetIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.set);
    }
}
