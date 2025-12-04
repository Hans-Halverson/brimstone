use crate::handle_scope_guard;

use super::{
    alloc_error::AllocResult, context::Context, gc::HeapVisitor, property_key::PropertyKey,
    value::SymbolValue, Handle,
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
                #[allow(dead_code, clippy::wrong_self_convention)]
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
            pub fn init_builtin_names(&mut self) -> AllocResult<()> {
                $({
                    handle_scope_guard!(*self);
                    self.names.$rust_name = {
                        let string_value = self.alloc_string($js_name)?.as_string();
                        PropertyKey::string_not_array_index(*self, string_value)?
                    };
                })*

                Ok(())
            }
        }
    };
}

builtin_names!(
    (empty_string, ""),
    (space, " "),
    (comma, ","),
    (slash, "/"),
    (zero, "0"),
    (negative_zero, "-0"),
    (negative_infinity_literal, "-Infinity"),
    (default_name, "*default*"),
    (__define_getter__, "__defineGetter__"),
    (__define_setter__, "__defineSetter__"),
    (__lookup_getter__, "__lookupGetter__"),
    (__lookup_setter__, "__lookupSetter__"),
    (__proto__, "__proto__"),
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
    (async_function, "AsyncFunction"),
    (async_generator, "AsyncGenerator"),
    (async_generator_function, "AsyncGeneratorFunction"),
    (bigint, "BigInt"),
    (big_int64_array, "BigInt64Array"),
    (big_uint64_array, "BigUint64Array"),
    (boolean, "Boolean"),
    (data_view, "DataView"),
    (date, "Date"),
    (error, "Error"),
    (eval_error, "EvalError"),
    (finalization_registry, "FinalizationRegistry"),
    (float16_array, "Float16Array"),
    (float32_array, "Float32Array"),
    (float64_array, "Float64Array"),
    (function, "Function"),
    (generator, "Generator"),
    (generator_function, "GeneratorFunction"),
    (infinity, "Infinity"),
    (int8_array, "Int8Array"),
    (int16_array, "Int16Array"),
    (int32_array, "Int32Array"),
    (iterator, "Iterator"),
    (json, "JSON"),
    (map, "Map"),
    (math, "Math"),
    (module, "Module"),
    (number, "Number"),
    (object, "Object"),
    (promise, "Promise"),
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
    (all, "all"),
    (all_settled, "allSettled"),
    (anonymous, "anonymous"),
    (any, "any"),
    (apply, "apply"),
    (arguments, "arguments"),
    (as_int_n, "asIntN"),
    (as_uint_n, "asUintN"),
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
    (catch, "catch"),
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
    (decode_uri, "decodeURI"),
    (decode_uri_component, "decodeURIComponent"),
    (default, "default"),
    (define_properties, "defineProperties"),
    (define_property, "defineProperty"),
    (delete, "delete"),
    (delete_property, "deleteProperty"),
    (deref, "deref"),
    (description, "description"),
    (detached, "detached"),
    (difference, "difference"),
    (done, "done"),
    (dot_all, "dotAll"),
    (drop, "drop"),
    (encode_uri, "encodeURI"),
    (encode_uri_component, "encodeURIComponent"),
    (ends_with, "endsWith"),
    (entries, "entries"),
    (enumerable, "enumerable"),
    (errors, "errors"),
    (escape, "escape"),
    (eval, "eval"),
    (every, "every"),
    (exec, "exec"),
    (exp, "exp"),
    (expm1, "expm1"),
    (f16_round, "f16round"),
    (fill, "fill"),
    (filter, "filter"),
    (finally, "finally"),
    (find, "find"),
    (find_index, "findIndex"),
    (find_last, "findLast"),
    (find_last_index, "findLastIndex"),
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
    (fulfilled, "fulfilled"),
    (gc, "gc"),
    (get, "get"),
    (get_big_int64, "getBigInt64"),
    (get_big_uint64, "getBigUint64"),
    (get_date, "getDate"),
    (get_day, "getDay"),
    (get_float16, "getFloat16"),
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
    (group_by, "groupBy"),
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
    (intersection, "intersection"),
    (is, "is"),
    (is_array, "isArray"),
    (is_concat_spreadable, "isConcatSpreadable"),
    (is_error, "isError"),
    (is_extensible, "isExtensible"),
    (is_finite, "isFinite"),
    (is_frozen, "isFrozen"),
    (is_integer, "isInteger"),
    (is_nan, "isNaN"),
    (is_prototype_of, "isPrototypeOf"),
    (is_safe_integer, "isSafeInteger"),
    (is_sealed, "isSealed"),
    (is_disjoint_from, "isDisjointFrom"),
    (is_subset_of, "isSubsetOf"),
    (is_superset_of, "isSupersetOf"),
    (is_view, "isView"),
    (is_well_formed, "isWellFormed"),
    (iterator_, "iterator"),
    (join, "join"),
    (keys, "keys"),
    (key_for, "keyFor"),
    (last_index, "lastIndex"),
    (last_index_of, "lastIndexOf"),
    (length, "length"),
    (locale_compare, "localeCompare"),
    (log, "log"),
    (log1p, "log1p"),
    (log10, "log10"),
    (log2, "log2"),
    (map_, "map"),
    (match_, "match"),
    (match_all, "matchAll"),
    (max, "max"),
    (max_byte_length, "maxByteLength"),
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
    (promise_, "promise"),
    (property_is_enumerable, "propertyIsEnumerable"),
    (prototype, "prototype"),
    (proxy_, "proxy"),
    (push, "push"),
    (race, "race"),
    (random, "random"),
    (raw, "raw"),
    (reason, "reason"),
    (reduce, "reduce"),
    (reduce_right, "reduceRight"),
    (register, "register"),
    (reject, "reject"),
    (rejected, "rejected"),
    (resize, "resize"),
    (resizable, "resizable"),
    (resolve, "resolve"),
    (repeat, "repeat"),
    (replace, "replace"),
    (replace_all, "replaceAll"),
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
    (set_float16, "setFloat16"),
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
    (stack, "stack"),
    (starts_with, "startsWith"),
    (status, "status"),
    (sticky, "sticky"),
    (string_, "string"),
    (stringify, "stringify"),
    (subarray, "subarray"),
    (substring, "substring"),
    (symmetric_difference, "symmetricDifference"),
    (take, "take"),
    (tan, "tan"),
    (tanh, "tanh"),
    (test, "test"),
    (then, "then"),
    (this, "this"),
    (throw, "throw"),
    (to_array, "toArray"),
    (to_date_string, "toDateString"),
    (to_exponential, "toExponential"),
    (to_fixed, "toFixed"),
    (to_iso_string, "toISOString"),
    (to_json, "toJSON"),
    (to_locale_date_string, "toLocaleDateString"),
    (to_locale_lower_case, "toLocaleLowerCase"),
    (to_locale_string, "toLocaleString"),
    (to_locale_time_string, "toLocaleTimeString"),
    (to_locale_upper_case, "toLocaleUpperCase"),
    (to_lower_case, "toLowerCase"),
    (to_precision, "toPrecision"),
    (to_primitive, "toPrimitive"),
    (to_reversed, "toReversed"),
    (to_sorted, "toSorted"),
    (to_spliced, "toSpliced"),
    (to_string, "toString"),
    (to_string_tag, "toStringTag"),
    (to_time_string, "toTimeString"),
    (to_upper_case, "toUpperCase"),
    (to_utc_string, "toUTCString"),
    (to_well_formed, "toWellFormed"),
    (transfer, "transfer"),
    (transfer_to_fixed_length, "transferToFixedLength"),
    (trim, "trim"),
    (trim_end, "trimEnd"),
    (trim_start, "trimStart"),
    (trunc, "trunc"),
    (try_, "try"),
    (undefined, "undefined"),
    (unicode, "unicode"),
    (unicode_sets, "unicodeSets"),
    (union, "union"),
    (unregister, "unregister"),
    (unscopables, "unscopables"),
    (unshift, "unshift"),
    (value, "value"),
    (values, "values"),
    (value_of, "valueOf"),
    (with, "with"),
    (with_resolvers, "withResolvers"),
    (writable, "writable")
);

/// Well-Known Symbols (https://tc39.es/ecma262/#sec-well-known-symbols)
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
            pub fn init_builtin_symbols(&mut self) -> AllocResult<()> {
                $({
                    handle_scope_guard!(*self);
                    self.well_known_symbols.$rust_name = {
                        let description = self.alloc_string($description)?.as_string();
                        *PropertyKey::symbol(SymbolValue::new(*self, Some(description), /* is_private */ false)?)
                    };
                })*

                Ok(())
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
    (unscopables, "Symbol.unscopables"),
    // Symbols used for private properties of bound functions
    (bound_target, ""),
    (bound_this, ""),
    (bound_arguments, ""),
    // Symbols used for private properties of revocable proxy closures
    (revocable_proxy, ""),
    // Symbols used for private proprties of module functions
    (module, ""),
    // Symbols used for private properties of resolve/reject functions
    (promise, ""),
    // Symbols used for private properties of capability executor function
    (capability, ""),
    // Symbols used for private properties of Promise.{all, allSettled, any} resolve/reject functions
    (already_called, ""),
    (index, ""),
    (values, ""),
    (remaining_elements, ""),
    // Symbols used for private properties of Promise.prototype.finally resolve/reject functions
    (constructor, ""),
    (on_finally, ""),
    // Symbols used for private properties of async generator resolve/reject functions
    (async_generator, "")
);
