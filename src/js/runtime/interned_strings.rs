use allocator_api2::alloc::Global;
use hashbrown::HashMap;

use crate::{
    common::wtf_8::{Wtf8Str, Wtf8String},
    runtime::alloc_error::AllocResult,
    set_uninit,
};

use super::{
    collections::{BsHashSet, BsHashSetField},
    gc::HeapVisitor,
    heap_item_descriptor::HeapItemKind,
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr,
};

pub struct InternedStrings {
    // Set of canonical interned strings for each code unit sequence. Maintain invariant that only
    // flat one byte and two byte strings with their is_interned flag set are present in this set,
    // and is_interned is not set on any other string values.
    strings: HeapPtr<InternedStringsSet>,
    // Weak map from wtf8 strs to their canonical interned strings, used for mapping strings from
    // AST to string values on the heap during bytecode generation.
    generator_cache: HashMap<Wtf8String, HeapPtr<FlatString>>,
}

type InternedStringsSet = BsHashSet<HeapPtr<FlatString>>;

impl InternedStrings {
    pub fn init(mut cx: Context) -> AllocResult<()> {
        let interned_strings =
            InternedStringsSet::new_initial(cx, HeapItemKind::InternedStringsSet)?;

        set_uninit!(cx.interned_strings.strings, interned_strings);
        set_uninit!(cx.interned_strings.generator_cache, HashMap::new());

        Ok(())
    }

    pub fn uninit() -> InternedStrings {
        InternedStrings { strings: HeapPtr::uninit(), generator_cache: HashMap::new() }
    }

    pub fn strings(cx: Context) -> HeapPtr<InternedStringsSet> {
        cx.interned_strings.strings
    }

    pub fn strings_field(&mut self) -> InternedStringsSetField {
        InternedStringsSetField
    }

    pub fn generator_cache_mut(&mut self) -> &mut HashMap<Wtf8String, HeapPtr<FlatString>> {
        &mut self.generator_cache
    }

    pub fn get(
        mut cx: Context,
        mut string: HeapPtr<FlatString>,
    ) -> AllocResult<HeapPtr<FlatString>> {
        // Fast path if string is already interned
        if string.is_interned() {
            return Ok(string);
        }

        match cx.interned_strings.strings.get(&string) {
            Some(interned_string) => Ok(*interned_string),
            None => {
                string.intern();

                // Preserve string before potentially growing set
                let string = string.to_handle();

                cx.interned_strings
                    .strings_field()
                    .maybe_grow_for_insertion(cx)?
                    .insert_without_growing(*string);

                Ok(*string)
            }
        }
    }

    pub fn alloc_wtf8_str(mut cx: Context, str: &Wtf8Str) -> AllocResult<Handle<FlatString>> {
        let string_value = cx.alloc_wtf8_str_ptr(str)?;
        Ok(InternedStrings::get(cx, string_value)?.to_handle())
    }

    pub fn get_generator_cache_str(mut cx: Context, str: &str) -> AllocResult<Handle<StringValue>> {
        match cx.interned_strings.generator_cache.get(str.as_bytes()) {
            Some(interned_string) => Ok(interned_string.as_string().to_handle()),
            None => {
                let string_value = cx.alloc_string_ptr(str)?;
                let interned_string = InternedStrings::get(cx, string_value)?.to_handle();

                cx.interned_strings
                    .generator_cache
                    .insert(Wtf8String::from_str(str), *interned_string);

                Ok(interned_string.as_string())
            }
        }
    }

    pub fn get_generator_cache_wtf8_str(
        mut cx: Context,
        str: &Wtf8Str,
    ) -> AllocResult<Handle<StringValue>> {
        match cx.interned_strings.generator_cache.get(str) {
            Some(interned_string) => Ok(interned_string.as_string().to_handle()),
            None => {
                let string_value = cx.alloc_wtf8_str_ptr(str)?;
                let interned_string = InternedStrings::get(cx, string_value)?.to_handle();

                cx.interned_strings
                    .generator_cache
                    .insert(str.to_owned_in(Global), *interned_string);

                Ok(interned_string.as_string())
            }
        }
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.strings);

        // Interned strings are weak references
        for (_, string) in self.generator_cache.iter_mut() {
            visitor.visit_weak_pointer(string);
        }
    }
}

#[derive(Clone)]
pub struct InternedStringsSetField;

impl BsHashSetField<HeapPtr<FlatString>> for InternedStringsSetField {
    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<InternedStringsSet>> {
        InternedStringsSet::new(cx, HeapItemKind::InternedStringsSet, capacity)
    }

    fn get(&self, cx: Context) -> HeapPtr<InternedStringsSet> {
        cx.interned_strings.strings
    }

    fn set(&mut self, mut cx: Context, set: HeapPtr<InternedStringsSet>) {
        cx.interned_strings.strings = set;
    }
}

impl InternedStringsSetField {
    pub fn byte_size(set: &HeapPtr<InternedStringsSet>) -> usize {
        InternedStringsSet::calculate_size_in_bytes(set.capacity())
    }

    pub fn visit_pointers(set: &mut HeapPtr<InternedStringsSet>, visitor: &mut impl HeapVisitor) {
        set.visit_pointers(visitor);

        // Interned strings are weak references
        for string in set.iter_mut_gc_unsafe() {
            visitor.visit_weak_pointer(string);
        }
    }
}
