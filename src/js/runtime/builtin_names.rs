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
                            PropertyKey::String(string_value)
                        },
                    )*
                }
            }
        }
    };
}

builtin_names!(
    (empty_string, ""),
    (__proto__, "__proto__"),
    (epsilon, "EPSILON"),
    (max_safe_integer, "MAX_SAFE_INTEGER"),
    (max_value, "MAX_VALUE"),
    (min_safe_integer, "MIN_SAFE_INTEGER"),
    (min_value, "MIN_VALUE"),
    (positive_infinity, "POSITIVE_INFINITY"),
    (negative_infinity, "NEGATIVE_INFINITY"),
    (array, "Array"),
    (boolean, "Boolean"),
    (error, "Error"),
    (eval_error, "EvalError"),
    (default, "default"),
    (function, "Function"),
    (infinity, "Infinity"),
    (number, "Number"),
    (object, "Object"),
    (range_error, "RangeError"),
    (reference_error, "ReferenceError"),
    (string, "String"),
    (symbol, "Symbol"),
    (syntax_error, "SyntaxError"),
    (type_error, "TypeError"),
    (uri_error, "URIError"),
    (arguments, "arguments"),
    (call, "call"),
    (caller, "caller"),
    (cause, "cause"),
    (configurable, "configurable"),
    (console, "console"),
    (constructor, "constructor"),
    (define_property, "defineProperty"),
    (description, "description"),
    (enumerable, "enumerable"),
    (eval, "eval"),
    (for_, "for"),
    (get, "get"),
    (get_own_property_descriptor, "getOwnPropertyDescriptor"),
    (global_this, "globalThis"),
    (has_own_property, "hasOwnProperty"),
    (is_array, "isArray"),
    (is_finite, "isFinite"),
    (is_nan, "isNaN"),
    (is_prototype_of, "isPrototypeOf"),
    (key_for, "keyFor"),
    (length, "length"),
    (log, "log"),
    (name, "name"),
    (nan, "NaN"),
    (message, "message"),
    (property_is_enumerable, "propertyIsEnumerable"),
    (prototype, "prototype"),
    (set, "set"),
    (to_string, "toString"),
    (undefined, "undefined"),
    (value, "value"),
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
