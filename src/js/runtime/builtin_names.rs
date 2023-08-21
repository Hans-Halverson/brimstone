use super::{
    context::Context, gc::HeapVisitor, property_key::PropertyKey, value::SymbolValue, Handle,
};

// All built-in string property keys referenced in the spec
macro_rules! builtin_names {
    ( $( ($rust_name:ident, $js_name:expr) ),* ) => {
        pub struct BuiltinNames {
            $(
                pub $rust_name: PropertyKey,
            )*
        }

        impl BuiltinNames {
            pub fn uninit() -> BuiltinNames {
                BuiltinNames {
                    $(
                        $rust_name: PropertyKey::uninit(),
                    )*
                }
            }

            $(
                #[inline]
                pub fn $rust_name(&self) -> Handle<PropertyKey> {
                    Handle::<PropertyKey>::from_fixed_non_heap_ptr(&self.$rust_name)
                }
            )*

            pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
                $(
                    visitor.visit_property_key(&mut self.$rust_name);
                )*
            }
        }

        impl Context {
            pub fn init_builtin_names(&mut self) {
                $(
                    self.names.$rust_name = {
                        let string_value = self.alloc_string($js_name);
                        PropertyKey::string_not_array_index(self, string_value)
                    };
                )*
            }
        }
    };
}

builtin_names!(
    (empty_string, ""),
    (__define_getter__, "__defineGetter__"),
    (__define_setter__, "__defineSetter__"),
    (__lookup_getter__, "__lookupGetter__"),
    (__lookup_setter__, "__lookupSetter__"),
    (__proto__, "__proto__"),
    (symbol_to_primitive, "[Symbol.toPrimitive]"),
    (bytes_per_element, "BYTES_PER_ELEMENT"),
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
    (aggregate_error, "AggregateError"),
    (array, "Array"),
    (array_buffer, "ArrayBuffer"),
    (bigint, "BigInt"),
    (big_int64_array, "BigInt64Array"),
    (big_uint64_array, "BigUint64Array"),
    (boolean, "Boolean"),
    (data_view, "DataView"),
    (date, "Date"),
    (error, "Error"),
    (eval_error, "EvalError"),
    (finalization_registry, "FinalizationRegistry"),
    (float32_array, "Float32Array"),
    (float64_array, "Float64Array"),
    (function, "Function"),
    (infinity, "Infinity"),
    (int8_array, "Int8Array"),
    (int16_array, "Int16Array"),
    (int32_array, "Int32Array"),
    (map, "Map"),
    (math, "Math"),
    (number, "Number"),
    (object, "Object"),
    (proxy, "Proxy"),
    (range_error, "RangeError"),
    (reference_error, "ReferenceError"),
    (reflect, "Reflect"),
    (regexp, "RegExp"),
    (set, "Set"),
    (string, "String"),
    (symbol, "Symbol"),
    (syntax_error, "SyntaxError"),
    (typed_array, "TypedArray"),
    (type_error, "TypeError"),
    (uint8_array, "Uint8Array"),
    (uint8_clamped_array, "Uint8ClampedArray"),
    (uint16_array, "Uint16Array"),
    (uint32_array, "Uint32Array"),
    (uri_error, "URIError"),
    (utc, "UTC"),
    (weak_map, "WeakMap"),
    (weak_ref, "WeakRef"),
    (weak_set, "WeakSet"),
    (abs, "abs"),
    (acos, "acos"),
    (acosh, "acosh"),
    (add, "add"),
    (anonymous, "anonymous"),
    (apply, "apply"),
    (arguments, "arguments"),
    (asin, "asin"),
    (asinh, "asinh"),
    (assign, "assign"),
    (async_iterator, "asyncIterator"),
    (at, "at"),
    (atan, "atan"),
    (atanh, "atanh"),
    (atan2, "atan2"),
    (bind, "bind"),
    (buffer, "buffer"),
    (byte_length, "byteLength"),
    (byte_offset, "byteOffset"),
    (call, "call"),
    (callee, "callee"),
    (caller, "caller"),
    (cause, "cause"),
    (cbrt, "cbrt"),
    (ceil, "ceil"),
    (char_at, "charAt"),
    (char_code_at, "charCodeAt"),
    (clear, "clear"),
    (clz32, "clz32"),
    (code_point_at, "codePointAt"),
    (concat, "concat"),
    (configurable, "configurable"),
    (console, "console"),
    (construct, "construct"),
    (constructor, "constructor"),
    (copy_within, "copyWithin"),
    (cos, "cos"),
    (cosh, "cosh"),
    (create, "create"),
    (default, "default"),
    (define_properties, "defineProperties"),
    (define_property, "defineProperty"),
    (delete, "delete"),
    (delete_property, "deleteProperty"),
    (deref, "deref"),
    (description, "description"),
    (done, "done"),
    (dot_all, "dotAll"),
    (ends_with, "endsWith"),
    (entries, "entries"),
    (enumerable, "enumerable"),
    (errors, "errors"),
    (eval, "eval"),
    (every, "every"),
    (exec, "exec"),
    (exp, "exp"),
    (expm1, "expm1"),
    (fill, "fill"),
    (filter, "filter"),
    (find, "find"),
    (find_index, "findIndex"),
    (flags, "flags"),
    (flat, "flat"),
    (flat_map, "flatMap"),
    (floor, "floor"),
    (for_, "for"),
    (for_each, "forEach"),
    (freeze, "freeze"),
    (from, "from"),
    (from_char_code, "fromCharCode"),
    (from_code_point, "fromCodePoint"),
    (from_entries, "fromEntries"),
    (fround, "fround"),
    (gc, "gc"),
    (get, "get"),
    (get_big_int64, "getBigInt64"),
    (get_big_uint64, "getBigUint64"),
    (get_date, "getDate"),
    (get_day, "getDay"),
    (get_float32, "getFloat32"),
    (get_float64, "getFloat64"),
    (get_full_year, "getFullYear"),
    (get_hours, "getHours"),
    (get_int8, "getInt8"),
    (get_int16, "getInt16"),
    (get_int32, "getInt32"),
    (get_milliseconds, "getMilliseconds"),
    (get_minutes, "getMinutes"),
    (get_month, "getMonth"),
    (get_own_property_descriptor, "getOwnPropertyDescriptor"),
    (get_own_property_descriptors, "getOwnPropertyDescriptors"),
    (get_own_property_names, "getOwnPropertyNames"),
    (get_own_property_symbols, "getOwnPropertySymbols"),
    (get_prototype_of, "getPrototypeOf"),
    (get_seconds, "getSeconds"),
    (get_time, "getTime"),
    (get_timezone_offset, "getTimezoneOffset"),
    (get_uint8, "getUint8"),
    (get_uint16, "getUint16"),
    (get_uint32, "getUint32"),
    (get_utc_date, "getUTCDate"),
    (get_utc_day, "getUTCDay"),
    (get_utc_full_year, "getUTCFullYear"),
    (get_utc_hours, "getUTCHours"),
    (get_utc_milliseconds, "getUTCMilliseconds"),
    (get_utc_minutes, "getUTCMinutes"),
    (get_utc_month, "getUTCMonth"),
    (get_utc_seconds, "getUTCSeconds"),
    (global, "global"),
    (global_this, "globalThis"),
    (groups, "groups"),
    (has, "has"),
    (has_indices, "hasIndices"),
    (has_instance, "hasInstance"),
    (has_own, "hasOwn"),
    (has_own_property, "hasOwnProperty"),
    (hypot, "hypot"),
    (ignore_case, "ignoreCase"),
    (imul, "imul"),
    (includes, "includes"),
    (index, "index"),
    (index_of, "indexOf"),
    (indices, "indices"),
    (input, "input"),
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
    (last_index, "lastIndex"),
    (last_index_of, "lastIndexOf"),
    (length, "length"),
    (log, "log"),
    (log1p, "log1p"),
    (log10, "log10"),
    (log2, "log2"),
    (map_, "map"),
    (match_, "match"),
    (match_all, "matchAll"),
    (max, "max"),
    (message, "message"),
    (min, "min"),
    (multiline, "multiline"),
    (name, "name"),
    (nan, "NaN"),
    (next, "next"),
    (nfc, "NFC"),
    (nfd, "NFD"),
    (nfkc, "NFKC"),
    (nfkd, "NFKD"),
    (normalize, "normalize"),
    (now, "now"),
    (number_, "number"),
    (of, "of"),
    (own_keys, "ownKeys"),
    (pad_end, "padEnd"),
    (pad_start, "padStart"),
    (parse, "parse"),
    (parse_float, "parseFloat"),
    (parse_int, "parseInt"),
    (pop, "pop"),
    (pow, "pow"),
    (prevent_extensions, "preventExtensions"),
    (property_is_enumerable, "propertyIsEnumerable"),
    (prototype, "prototype"),
    (proxy_, "proxy"),
    (push, "push"),
    (random, "random"),
    (raw, "raw"),
    (reduce, "reduce"),
    (reduce_right, "reduceRight"),
    (register, "register"),
    (repeat, "repeat"),
    (replace, "replace"),
    (return_, "return"),
    (reverse, "reverse"),
    (revocable, "revocable"),
    (revoke, "revoke"),
    (round, "round"),
    (run, "run"),
    (seal, "seal"),
    (search, "search"),
    (set_, "set"),
    (set_big_int64, "setBigInt64"),
    (set_big_uint64, "setBigUint64"),
    (set_date, "setDate"),
    (set_float32, "setFloat32"),
    (set_float64, "setFloat64"),
    (set_full_year, "setFullYear"),
    (set_hours, "setHours"),
    (set_int8, "setInt8"),
    (set_int16, "setInt16"),
    (set_int32, "setInt32"),
    (set_milliseconds, "setMilliseconds"),
    (set_minutes, "setMinutes"),
    (set_month, "setMonth"),
    (set_prototype_of, "setPrototypeOf"),
    (set_seconds, "setSeconds"),
    (set_time, "setTime"),
    (set_uint8, "setUint8"),
    (set_uint16, "setUint16"),
    (set_uint32, "setUint32"),
    (set_utc_date, "setUTCDate"),
    (set_utc_full_year, "setUTCFullYear"),
    (set_utc_hours, "setUTCHours"),
    (set_utc_milliseconds, "setUTCMilliseconds"),
    (set_utc_minutes, "setUTCMinutes"),
    (set_utc_month, "setUTCMonth"),
    (set_utc_seconds, "setUTCSeconds"),
    (shift, "shift"),
    (sign, "sign"),
    (sin, "sin"),
    (sinh, "sinh"),
    (size, "size"),
    (slice, "slice"),
    (some, "some"),
    (sort, "sort"),
    (source, "source"),
    (species, "species"),
    (split, "split"),
    (splice, "splice"),
    (sqrt, "sqrt"),
    (starts_with, "startsWith"),
    (sticky, "sticky"),
    (string_, "string"),
    (subarray, "subarray"),
    (substring, "substring"),
    (tan, "tan"),
    (tanh, "tanh"),
    (test, "test"),
    (to_date_string, "toDateString"),
    (to_fixed, "toFixed"),
    (to_iso_string, "toISOString"),
    (to_json, "toJSON"),
    (to_locale_date_string, "toLocaleDateString"),
    (to_locale_lower_case, "toLocaleLowerCase"),
    (to_locale_string, "toLocaleString"),
    (to_locale_time_string, "toLocaleTimeString"),
    (to_locale_upper_case, "toLocaleUpperCase"),
    (to_lower_case, "toLowerCase"),
    (to_primitive, "toPrimitive"),
    (to_sorted, "toSorted"),
    (to_string, "toString"),
    (to_string_tag, "toStringTag"),
    (to_time_string, "toTimeString"),
    (to_upper_case, "toUpperCase"),
    (to_utc_string, "toUTCString"),
    (trim, "trim"),
    (trim_end, "trimEnd"),
    (trim_start, "trimStart"),
    (trunc, "trunc"),
    (undefined, "undefined"),
    (unicode, "unicode"),
    (unregister, "unregister"),
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
                pub $rust_name: PropertyKey,
            )*
        }

        impl BuiltinSymbols {
            pub fn uninit() -> BuiltinSymbols {
                BuiltinSymbols {
                    $(
                        $rust_name: PropertyKey::uninit(),
                    )*
                }
            }

            $(
                #[inline]
                pub fn $rust_name(&self) -> Handle<PropertyKey> {
                    Handle::<PropertyKey>::from_fixed_non_heap_ptr(&self.$rust_name)
                }
            )*

            pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
                $(
                    visitor.visit_property_key(&mut self.$rust_name);
                )*
            }
        }

        impl Context {
            pub fn init_builtin_symbols(&mut self) {
                $(
                    self.well_known_symbols.$rust_name = {
                        let description = self.alloc_string($description);
                        PropertyKey::symbol(SymbolValue::new(self, Some(description))).get()
                    };
                )*
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
