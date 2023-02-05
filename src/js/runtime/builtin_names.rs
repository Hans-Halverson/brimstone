use super::{gc::Heap, property_key::PropertyKey, value::SymbolValue, Gc};

// All built-in string property keys referenced in the spec
macro_rules! builtin_names {
    ( $( ($rust_name:ident, $js_name:expr) ),* ) => {
        pub struct BuiltinNames {
            $(
                pub $rust_name: PropertyKey,
            )*
        }

        impl BuiltinNames {
            pub fn new(heap: &mut Heap) -> BuiltinNames {
                BuiltinNames {
                    $(
                        $rust_name: {
                            let string_value = heap.alloc_string(String::from($js_name));
                            PropertyKey::string_not_number(string_value)
                        },
                    )*
                }
            }

            $(
                #[inline]
                pub fn $rust_name(&self) -> PropertyKey {
                    self.$rust_name.clone()
                }
            )*
        }
    };
}

builtin_names!(
    (empty_string, ""),
    (__proto__, "__proto__"),
    (e, "E"),
    (epsilon, "EPSILON"),
    (ln10, "LN10"),
    (ln2, "LN2"),
    (log10e, "LOG10E"),
    (log2e, "LOG2E"),
    (max_safe_integer, "MAX_SAFE_INTEGER"),
    (max_value, "MAX_VALUE"),
    (min_safe_integer, "MIN_SAFE_INTEGER"),
    (min_value, "MIN_VALUE"),
    (pi, "PI"),
    (positive_infinity, "POSITIVE_INFINITY"),
    (negative_infinity, "NEGATIVE_INFINITY"),
    (sqrt1_2, "SQRT1_2"),
    (sqrt2, "SQRT2"),
    (array, "Array"),
    (bigint, "BigInt"),
    (boolean, "Boolean"),
    (error, "Error"),
    (eval_error, "EvalError"),
    (function, "Function"),
    (infinity, "Infinity"),
    (math, "Math"),
    (number, "Number"),
    (object, "Object"),
    (proxy, "Proxy"),
    (range_error, "RangeError"),
    (reference_error, "ReferenceError"),
    (reflect, "Reflect"),
    (string, "String"),
    (symbol, "Symbol"),
    (syntax_error, "SyntaxError"),
    (type_error, "TypeError"),
    (uri_error, "URIError"),
    (apply, "apply"),
    (arguments, "arguments"),
    (assign, "assign"),
    (async_iterator, "asyncIterator"),
    (at, "at"),
    (bind, "bind"),
    (call, "call"),
    (callee, "callee"),
    (caller, "caller"),
    (cause, "cause"),
    (char_at, "charAt"),
    (concat, "concat"),
    (configurable, "configurable"),
    (console, "console"),
    (construct, "construct"),
    (constructor, "constructor"),
    (copy_within, "copyWithin"),
    (create, "create"),
    (default, "default"),
    (define_properties, "defineProperties"),
    (define_property, "defineProperty"),
    (delete_property, "deleteProperty"),
    (description, "description"),
    (done, "done"),
    (entries, "entries"),
    (enumerable, "enumerable"),
    (eval, "eval"),
    (every, "every"),
    (fill, "fill"),
    (filter, "filter"),
    (find, "find"),
    (find_index, "findIndex"),
    (for_, "for"),
    (for_each, "forEach"),
    (freeze, "freeze"),
    (get, "get"),
    (get_own_property_descriptor, "getOwnPropertyDescriptor"),
    (get_own_property_descriptors, "getOwnPropertyDescriptors"),
    (get_own_property_names, "getOwnPropertyNames"),
    (get_own_property_symbols, "getOwnPropertySymbols"),
    (get_prototype_of, "getPrototypeOf"),
    (global_this, "globalThis"),
    (has, "has"),
    (has_instance, "hasInstance"),
    (has_own, "hasOwn"),
    (has_own_property, "hasOwnProperty"),
    (includes, "includes"),
    (index_of, "indexOf"),
    (is, "is"),
    (is_array, "isArray"),
    (is_concat_spreadable, "isConcatSpreadable"),
    (is_extensible, "isExtensible"),
    (is_finite, "isFinite"),
    (is_frozen, "isFrozen"),
    (is_integer, "isInteger"),
    (is_nan, "isNaN"),
    (is_prototype_of, "isPrototypeOf"),
    (is_safe_integer, "isSafeInteger"),
    (is_sealed, "isSealed"),
    (iterator, "iterator"),
    (join, "join"),
    (keys, "keys"),
    (key_for, "keyFor"),
    (last_index_of, "lastIndexOf"),
    (length, "length"),
    (log, "log"),
    (map, "map"),
    (match_, "match"),
    (match_all, "matchAll"),
    (message, "message"),
    (name, "name"),
    (nan, "NaN"),
    (next, "next"),
    (of, "of"),
    (own_keys, "ownKeys"),
    (parse_float, "parseFloat"),
    (parse_int, "parseInt"),
    (pop, "pop"),
    (pow, "pow"),
    (prevent_extensions, "preventExtensions"),
    (property_is_enumerable, "propertyIsEnumerable"),
    (prototype, "prototype"),
    (proxy_, "proxy"),
    (push, "push"),
    (raw, "raw"),
    (reduce, "reduce"),
    (reduce_right, "reduceRight"),
    (replace, "replace"),
    (return_, "return"),
    (reverse, "reverse"),
    (revocable, "revocable"),
    (revoke, "revoke"),
    (seal, "seal"),
    (search, "search"),
    (set, "set"),
    (set_prototype_of, "setPrototypeOf"),
    (shift, "shift"),
    (species, "species"),
    (split, "split"),
    (some, "some"),
    (to_primitive, "toPrimitive"),
    (to_string, "toString"),
    (to_string_tag, "toStringTag"),
    (undefined, "undefined"),
    (unscopables, "unscopables"),
    (unshift, "unshift"),
    (value, "value"),
    (values, "values"),
    (value_of, "valueOf"),
    (writable, "writable")
);

// 6.1.5.1 Well-Known Symbols
macro_rules! builtin_symbols {
    ( $( ($rust_name:ident, $description:expr) ),* ) => {
        pub struct BuiltinSymbols {
            $(
                pub $rust_name: Gc<SymbolValue>,
            )*
        }

        impl BuiltinSymbols {
            pub fn new(heap: &mut Heap) -> BuiltinSymbols {
                BuiltinSymbols {
                    $(
                        $rust_name: heap.alloc(SymbolValue::new(Some(String::from($description)))),
                    )*
                }
            }
        }
    };
}

builtin_symbols!(
    (async_iterator, "Symbol.asyncIterator"),
    (has_instance, "Symbol.hasInstance"),
    (is_concat_spreadable, "Symbol.isConcatSpreadable"),
    (iterator, "Symbol.iterator"),
    (match_, "Symbol.match"),
    (match_all, "Symbol.matchAll"),
    (replace, "Symbol.replace"),
    (search, "Symbol.search"),
    (species, "Symbol.species"),
    (split, "Symbol.split"),
    (to_primitive, "Symbol.toPrimitive"),
    (to_string_tag, "Symbol.toStringTag"),
    (unscopables, "Symbol.unscopables")
);
