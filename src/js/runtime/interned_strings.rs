use allocator_api2::alloc::Global;
use hashbrown::HashMap;

use crate::{
    common::wtf_8::{Wtf8Str, Wtf8String},
    impl_hash_set_instance, must_a,
    runtime::{
        Context, EvalResult, Handle, HeapPtr,
        alloc_error::AllocResult,
        collections::{BsHashSetField, FastHasher, HashSetInstance},
        gc::{HeapItem, HeapVisitor},
        string_value::{FlatString, StringValue},
    },
    set_uninit,
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

impl InternedStrings {
    pub fn init(mut cx: Context) -> AllocResult<()> {
        let interned_strings = InternedStringsSet::new_initial(cx)?;

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

    pub fn alloc_wtf8_str(mut cx: Context, str: &Wtf8Str) -> EvalResult<Handle<FlatString>> {
        let string_value = cx.alloc_wtf8_str_ptr(str)?;
        Ok(InternedStrings::get(cx, string_value)?.to_handle())
    }

    pub fn alloc_static_wtf8_str(
        mut cx: Context,
        str: &'static Wtf8Str,
    ) -> AllocResult<Handle<FlatString>> {
        let string_value = cx.alloc_static_wtf8_str_ptr(str)?;
        Ok(InternedStrings::get(cx, string_value)?.to_handle())
    }

    pub fn get_generator_cache_static_str(
        mut cx: Context,
        str: &'static str,
    ) -> AllocResult<Handle<StringValue>> {
        match cx.interned_strings.generator_cache.get(str.as_bytes()) {
            Some(interned_string) => Ok(interned_string.as_string().to_handle()),
            None => {
                let string_value = cx.alloc_static_string_ptr(str)?;
                let interned_string = InternedStrings::get(cx, string_value)?.to_handle();

                cx.interned_strings
                    .generator_cache
                    .insert(Wtf8String::from_str(str), *interned_string);

                Ok(interned_string.as_string())
            }
        }
    }

    /// Get the interned heap string for the given Wtf8Str during bytecode generation.
    ///
    /// Wtf8Str is assumed to have come from the source, meaning it must be smaller than the max
    /// string length.
    pub fn get_generator_cache_wtf8_str(
        mut cx: Context,
        str: &Wtf8Str,
    ) -> AllocResult<Handle<StringValue>> {
        match cx.interned_strings.generator_cache.get(str) {
            Some(interned_string) => Ok(interned_string.as_string().to_handle()),
            None => {
                // Safe to unwrap since string came from source text and must be smaller than max
                // string length.
                let string_value = must_a!(cx.alloc_wtf8_str_ptr(str));
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

impl_hash_set_instance!(InternedStringsSet, HeapPtr<FlatString>, FastHasher);

#[derive(Clone)]
pub struct InternedStringsSetField;

impl BsHashSetField<InternedStringsSet> for InternedStringsSetField {
    fn get(&self, cx: Context) -> HeapPtr<InternedStringsSet> {
        cx.interned_strings.strings
    }

    fn set_new(
        &mut self,
        mut cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<InternedStringsSet>> {
        let set = InternedStringsSet::new(cx, capacity)?;
        cx.interned_strings.strings = set;
        Ok(set)
    }
}

impl HeapItem for InternedStringsSet {
    fn byte_size(set: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(set.capacity())
    }

    fn visit_pointers(mut set: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        set.visit_set_pointers(visitor);

        // Interned strings are weak references
        for string in set.iter_mut_gc_unsafe() {
            visitor.visit_weak_pointer(string);
        }
    }
}
