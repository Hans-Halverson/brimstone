use crate::set_uninit;

use super::{
    collections::{BsHashMap, BsHashMapField, BsHashSet, BsHashSetField},
    object_descriptor::ObjectKind,
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr,
};

pub struct InternedStrings {
    // Set of canonical interned strings for each code unit sequence. Maintain invariant that only
    // flat one byte and two byte strings with their is_interned flag set are present in this set,
    // and is_interned is not set on any other string values.
    strings: HeapPtr<BsHashSet<HeapPtr<FlatString>>>,
    // Map from utf8 strs to their canonical interned strings, used for mapping strings from AST
    // to string values on the heap.
    // TODO: Drop Strings whose values are garbage collected so we don't leak memory
    str_cache: HeapPtr<BsHashMap<String, HeapPtr<FlatString>>>,
}

type InternedStringsSet = BsHashSet<HeapPtr<FlatString>>;

type InternedStringsMap = BsHashMap<String, HeapPtr<FlatString>>;

impl InternedStrings {
    pub fn init(cx: &mut Context) {
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

    pub fn get(cx: &mut Context, mut string: HeapPtr<FlatString>) -> HeapPtr<FlatString> {
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

    pub fn get_str(cx: &mut Context, str: &str) -> Handle<StringValue> {
        match cx.interned_strings.str_cache.get(str) {
            Some(interned_string) => interned_string.as_string().to_handle(),
            None => {
                let string_value = cx.alloc_string_ptr(String::from(str));
                let interned_string = InternedStrings::get(cx, string_value).to_handle();

                cx.interned_strings
                    .str_cache_field()
                    .maybe_grow_for_insertion(cx)
                    .insert_without_growing(String::from(str), interned_string.get_());

                interned_string.as_string()
            }
        }
    }
}

#[derive(Clone)]
pub struct InternedStringsSetField;

impl BsHashSetField<HeapPtr<FlatString>> for InternedStringsSetField {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<InternedStringsSet> {
        InternedStringsSet::new(cx, ObjectKind::InternedStringsSet, capacity)
    }

    fn get(&self, cx: &mut Context) -> HeapPtr<InternedStringsSet> {
        cx.interned_strings.strings
    }

    fn set(&mut self, cx: &mut Context, set: HeapPtr<InternedStringsSet>) {
        cx.interned_strings.strings = set;
    }
}

pub struct InternedStringsMapField;

impl BsHashMapField<String, HeapPtr<FlatString>> for InternedStringsMapField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<InternedStringsMap> {
        InternedStringsMap::new(cx, ObjectKind::InternedStringsMap, capacity)
    }

    fn get(&self, cx: &mut Context) -> HeapPtr<InternedStringsMap> {
        cx.interned_strings.str_cache
    }

    fn set(&mut self, cx: &mut Context, map: HeapPtr<InternedStringsMap>) {
        cx.interned_strings.str_cache = map;
    }
}
