use crate::{js::common::wtf_8::Wtf8String, set_uninit};

use super::{
    collections::{BsHashMap, BsHashMapField, BsHashSet, BsHashSetField},
    gc::HeapVisitor,
    object_descriptor::ObjectKind,
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr,
};

pub struct InternedStrings {
    // Set of canonical interned strings for each code unit sequence. Maintain invariant that only
    // flat one byte and two byte strings with their is_interned flag set are present in this set,
    // and is_interned is not set on any other string values.
    strings: HeapPtr<InternedStringsSet>,
    // Map from wtf8 strs to their canonical interned strings, used for mapping strings from AST
    // to string values on the heap.
    // TODO: Drop Strings whose values are garbage collected so we don't leak memory
    str_cache: HeapPtr<InternedStringsMap>,
}

type InternedStringsSet = BsHashSet<HeapPtr<FlatString>>;

type InternedStringsMap = BsHashMap<Wtf8String, HeapPtr<FlatString>>;

impl InternedStrings {
    pub fn init(mut cx: Context) {
        set_uninit!(
            cx.interned_strings.strings,
            InternedStringsSet::new_initial(cx, ObjectKind::InternedStringsSet)
        );
        set_uninit!(
            cx.interned_strings.str_cache,
            InternedStringsMap::new_initial(cx, ObjectKind::InternedStringsMap)
        );
    }

    pub fn uninit() -> InternedStrings {
        InternedStrings { strings: HeapPtr::uninit(), str_cache: HeapPtr::uninit() }
    }

    pub fn strings_field(&mut self) -> InternedStringsSetField {
        InternedStringsSetField
    }

    pub fn str_cache_field(&mut self) -> InternedStringsMapField {
        InternedStringsMapField
    }

    pub fn get(mut cx: Context, mut string: HeapPtr<FlatString>) -> HeapPtr<FlatString> {
        // Fast path if string is already interned
        if string.is_interned() {
            return string;
        }

        match cx.interned_strings.strings.get(&string) {
            Some(interned_string) => *interned_string,
            None => {
                string.intern();

                // Preserve string before potentially growing set
                let string = string.to_handle();

                cx.interned_strings
                    .strings_field()
                    .maybe_grow_for_insertion(cx)
                    .insert_without_growing(string.get_());

                string.get_()
            }
        }
    }

    pub fn get_str(mut cx: Context, str: &str) -> Handle<StringValue> {
        match cx.interned_strings.str_cache.get(str.as_bytes()) {
            Some(interned_string) => interned_string.as_string().to_handle(),
            None => {
                let string_value = cx.alloc_string_ptr(str);
                let interned_string = InternedStrings::get(cx, string_value).to_handle();

                cx.interned_strings
                    .str_cache_field()
                    .maybe_grow_for_insertion(cx)
                    .insert_without_growing(Wtf8String::from_str(str), interned_string.get_());

                interned_string.as_string()
            }
        }
    }

    pub fn get_wtf8_str(mut cx: Context, str: &Wtf8String) -> Handle<StringValue> {
        match cx.interned_strings.str_cache.get(str) {
            Some(interned_string) => interned_string.as_string().to_handle(),
            None => {
                let string_value = cx.alloc_wtf8_string_ptr(str);
                let interned_string = InternedStrings::get(cx, string_value).to_handle();

                cx.interned_strings
                    .str_cache_field()
                    .maybe_grow_for_insertion(cx)
                    .insert_without_growing(str.clone(), interned_string.get_());

                interned_string.as_string()
            }
        }
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        // TODO: Do not mark interned strings, treat them as weak references that will be GC'd if
        // nothing else references them.
        visitor.visit_pointer(&mut self.strings);
        visitor.visit_pointer(&mut self.str_cache);
    }
}

#[derive(Clone)]
pub struct InternedStringsSetField;

impl BsHashSetField<HeapPtr<FlatString>> for InternedStringsSetField {
    fn new(cx: Context, capacity: usize) -> HeapPtr<InternedStringsSet> {
        InternedStringsSet::new(cx, ObjectKind::InternedStringsSet, capacity)
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

        for element in set.iter_mut_gc_unsafe() {
            visitor.visit_pointer(element);
        }
    }
}

pub struct InternedStringsMapField;

impl BsHashMapField<Wtf8String, HeapPtr<FlatString>> for InternedStringsMapField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<InternedStringsMap> {
        InternedStringsMap::new(cx, ObjectKind::InternedStringsMap, capacity)
    }

    fn get(&self, cx: Context) -> HeapPtr<InternedStringsMap> {
        cx.interned_strings.str_cache
    }

    fn set(&mut self, mut cx: Context, map: HeapPtr<InternedStringsMap>) {
        cx.interned_strings.str_cache = map;
    }
}

impl InternedStringsMapField {
    pub fn byte_size(map: &HeapPtr<InternedStringsMap>) -> usize {
        InternedStringsMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<InternedStringsMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (_, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(value);
        }
    }
}
