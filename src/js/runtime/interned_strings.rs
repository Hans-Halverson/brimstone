use std::collections::{HashMap, HashSet};

use super::{string_value::StringValue, Context, Gc};

pub struct InternedStrings {
    // Set of canonical interned strings for each code unit sequence. Maintain invariant that only
    // flat one byte and two byte strings with their is_interned flag set are present in this set,
    // and is_interned is not set on any other string values.
    strings: HashSet<Gc<StringValue>>,
    // Map from utf8 strs to their canonical interned strings, used for mapping strings from AST
    // to string values on the heap.
    str_cache: HashMap<String, Gc<StringValue>>,
}

impl InternedStrings {
    pub fn new() -> InternedStrings {
        InternedStrings { strings: HashSet::new(), str_cache: HashMap::new() }
    }

    pub fn get(cx: &mut Context, mut string: Gc<StringValue>) -> Gc<StringValue> {
        // Fast path if string is already interned
        if string.is_interned() {
            return string;
        }

        match cx.interned_strings.strings.get(&string) {
            Some(interned_string) => *interned_string,
            None => {
                string.intern();
                cx.interned_strings.strings.insert(string);
                string
            }
        }
    }

    pub fn get_str(cx: &mut Context, str: &str) -> Gc<StringValue> {
        match cx.interned_strings.str_cache.get(str) {
            Some(interned_string) => *interned_string,
            None => {
                let string_value = cx.alloc_string(String::from(str));
                let interned_string = InternedStrings::get(cx, string_value);

                cx.interned_strings
                    .str_cache
                    .insert(String::from(str), interned_string);

                interned_string
            }
        }
    }
}
