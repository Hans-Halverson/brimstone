use std::{collections::HashMap, mem::size_of};

use crate::{
    js::runtime::{
        arguments_object, console::ConsoleObject, gc_object::GcObject, object_value::ObjectValue,
        Context, EvalResult, Handle, Value,
    },
    static_assert,
};

use super::{
    aggregate_error_constructor::AggregateErrorConstructor,
    array_buffer_constructor::ArrayBufferConstructor,
    array_buffer_prototype::ArrayBufferPrototype,
    array_constructor::ArrayConstructor,
    array_iterator::ArrayIteratorPrototype,
    array_prototype::ArrayPrototype,
    bigint_constructor::BigIntConstructor,
    bigint_prototype::BigIntPrototype,
    boolean_constructor::BooleanConstructor,
    boolean_prototype::BooleanPrototype,
    data_view_constructor::DataViewConstructor,
    data_view_prototype::DataViewPrototype,
    date_constructor::DateConstructor,
    date_prototype::DatePrototype,
    error_constructor::ErrorConstructor,
    error_prototype::ErrorPrototype,
    finalization_registry_constructor::FinalizationRegistryConstructor,
    finalization_registry_prototype::FinalizationRegistryPrototype,
    function_constructor::FunctionConstructor,
    function_prototype::FunctionPrototype,
    global_object, intrinsics,
    iterator_prototype::IteratorPrototype,
    json_object::JSONObject,
    map_constructor::MapConstructor,
    map_iterator::MapIteratorPrototype,
    map_prototype::MapPrototype,
    math_object::MathObject,
    native_error::{
        EvalErrorConstructor, RangeErrorConstructor, ReferenceErrorConstructor,
        SyntaxErrorConstructor, TypeErrorConstructor, URIErrorConstructor,
    },
    number_constructor::NumberConstructor,
    number_prototype::NumberPrototype,
    object_constructor::ObjectConstructor,
    object_prototype::ObjectPrototype,
    proxy_constructor::{self, ProxyConstructor},
    reflect_object::ReflectObject,
    regexp_constructor::RegExpConstructor,
    regexp_prototype::RegExpPrototype,
    regexp_string_iterator::RegExpStringIteratorPrototype,
    set_constructor::SetConstructor,
    set_iterator::SetIteratorPrototype,
    set_prototype::SetPrototype,
    string_constructor::StringConstructor,
    string_iterator::StringIteratorPrototype,
    string_prototype::StringPrototype,
    symbol_constructor::SymbolConstructor,
    symbol_prototype::SymbolPrototype,
    typed_array::{
        BigInt64ArrayConstructor, BigUInt64ArrayConstructor, Float32ArrayConstructor,
        Float64ArrayConstructor, Int16ArrayConstructor, Int32ArrayConstructor,
        Int8ArrayConstructor, UInt16ArrayConstructor, UInt32ArrayConstructor,
        UInt8ArrayConstructor, UInt8ClampedArrayConstructor,
    },
    typed_array_constructor::TypedArrayConstructor,
    typed_array_prototype::TypedArrayPrototype,
    weak_map_constructor::WeakMapConstructor,
    weak_map_prototype::WeakMapPrototype,
    weak_ref_constructor::WeakRefConstructor,
    weak_ref_prototype::WeakRefPrototype,
    weak_set_constructor::WeakSetConstructor,
    weak_set_prototype::WeakSetPrototype,
};

/// Collection of all functions in the Rust runtime that can be called with a CallRustRuntime
/// instruction.
pub struct RustRuntimeFunctionRegistry {
    /// Lookup table from runtime function id to the function pointer itself
    id_to_function: [RustRuntimeFunction; NUM_RUST_RUNTIME_FUNCTIONS],
    function_to_id: HashMap<RustRuntimeFunction, RustRuntimeFunctionId>,
}

/// Each Rust runtime function is assigned a unique id.
pub type RustRuntimeFunctionId = u16;

// Check that the number of runtime functions fits in the RustRuntimeFunctionId type.
static_assert!(NUM_RUST_RUNTIME_FUNCTIONS <= (1 << (8 * size_of::<RustRuntimeFunctionId>())));

/// Encode a Rust runtime function id into a byte pair (high byte, low byte).
#[inline]
pub fn encode_rust_runtime_id(id: RustRuntimeFunctionId) -> (u8, u8) {
    let bytes = u16::to_ne_bytes(id);
    (bytes[0], bytes[1])
}

/// Encode a Rust runtime function id into a byte pair (high byte, low byte).
#[inline]
pub fn decode_rust_runtime_id(high_byte: u8, low_byte: u8) -> u16 {
    u16::from_ne_bytes([high_byte, low_byte])
}

type RustRuntimeFunction = fn(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>>;

impl RustRuntimeFunctionRegistry {
    #[inline]
    pub fn get_function(&self, id: RustRuntimeFunctionId) -> RustRuntimeFunction {
        self.id_to_function[id as usize]
    }

    pub fn get_id(&self, function: RustRuntimeFunction) -> Option<&RustRuntimeFunctionId> {
        self.function_to_id.get(&function)
    }
}

macro_rules! rust_runtime_functions {
    ($($rust_function:expr,)*) => {
        const NUM_RUST_RUNTIME_FUNCTIONS: usize = [
            $($rust_function,)*
        ].len();

        impl RustRuntimeFunctionRegistry {
            pub fn new() -> Self {
                let mut function_to_id: HashMap<RustRuntimeFunction, RustRuntimeFunctionId> =
                    HashMap::with_capacity(NUM_RUST_RUNTIME_FUNCTIONS);

                $(
                    let id = function_to_id.len() as RustRuntimeFunctionId;
                    function_to_id.insert($rust_function, id);
                )*

                Self {
                    id_to_function: [
                        $($rust_function,)*
                    ],
                    function_to_id,
                }
            }
        }
    };
}

// Every builtin function must be registered in this list so that an id can be assigned.
rust_runtime_functions!(
    AggregateErrorConstructor::construct,
    arguments_object::arg_getter,
    arguments_object::arg_setter,
    ArrayBufferConstructor::construct,
    ArrayBufferConstructor::is_view,
    ArrayBufferConstructor::get_species,
    ArrayBufferPrototype::get_byte_length,
    ArrayBufferPrototype::slice,
    ArrayConstructor::construct,
    ArrayConstructor::from,
    ArrayConstructor::is_array,
    ArrayConstructor::of,
    ArrayConstructor::get_species,
    ArrayIteratorPrototype::next,
    ArrayPrototype::at,
    ArrayPrototype::concat,
    ArrayPrototype::copy_within,
    ArrayPrototype::entries,
    ArrayPrototype::every,
    ArrayPrototype::fill,
    ArrayPrototype::filter,
    ArrayPrototype::find,
    ArrayPrototype::find_index,
    ArrayPrototype::find_last,
    ArrayPrototype::find_last_index,
    ArrayPrototype::flat,
    ArrayPrototype::flat_map,
    ArrayPrototype::for_each,
    ArrayPrototype::includes,
    ArrayPrototype::index_of,
    ArrayPrototype::join,
    ArrayPrototype::keys,
    ArrayPrototype::last_index_of,
    ArrayPrototype::map,
    ArrayPrototype::pop,
    ArrayPrototype::push,
    ArrayPrototype::reduce,
    ArrayPrototype::reduce_right,
    ArrayPrototype::reverse,
    ArrayPrototype::shift,
    ArrayPrototype::slice,
    ArrayPrototype::some,
    ArrayPrototype::sort,
    ArrayPrototype::splice,
    ArrayPrototype::to_locale_string,
    ArrayPrototype::to_reversed,
    ArrayPrototype::to_sorted,
    ArrayPrototype::to_spliced,
    ArrayPrototype::to_string,
    ArrayPrototype::unshift,
    ArrayPrototype::values,
    ArrayPrototype::with,
    BigInt64ArrayConstructor::construct,
    BigIntConstructor::construct,
    BigIntConstructor::as_int_n,
    BigIntConstructor::as_uint_n,
    BigIntPrototype::to_string,
    BigIntPrototype::value_of,
    BigUInt64ArrayConstructor::construct,
    BooleanConstructor::construct,
    BooleanPrototype::to_string,
    BooleanPrototype::value_of,
    DataViewConstructor::construct,
    DataViewPrototype::get_big_int64,
    DataViewPrototype::get_big_uint64,
    DataViewPrototype::get_buffer,
    DataViewPrototype::get_byte_length,
    DataViewPrototype::get_byte_offset,
    DataViewPrototype::get_float32,
    DataViewPrototype::get_float64,
    DataViewPrototype::get_int8,
    DataViewPrototype::get_int16,
    DataViewPrototype::get_int32,
    DataViewPrototype::get_uint8,
    DataViewPrototype::get_uint16,
    DataViewPrototype::get_uint32,
    DataViewPrototype::set_big_int64,
    DataViewPrototype::set_big_uint64,
    DataViewPrototype::set_float32,
    DataViewPrototype::set_float64,
    DataViewPrototype::set_int8,
    DataViewPrototype::set_int16,
    DataViewPrototype::set_int32,
    DataViewPrototype::set_uint8,
    DataViewPrototype::set_uint16,
    DataViewPrototype::set_uint32,
    DateConstructor::construct,
    DateConstructor::now,
    DateConstructor::parse,
    DateConstructor::utc,
    DatePrototype::get_date,
    DatePrototype::get_day,
    DatePrototype::get_full_year,
    DatePrototype::get_hours,
    DatePrototype::get_milliseconds,
    DatePrototype::get_minutes,
    DatePrototype::get_month,
    DatePrototype::get_seconds,
    DatePrototype::get_time,
    DatePrototype::get_timezone_offset,
    DatePrototype::get_utc_date,
    DatePrototype::get_utc_day,
    DatePrototype::get_utc_full_year,
    DatePrototype::get_utc_hours,
    DatePrototype::get_utc_milliseconds,
    DatePrototype::get_utc_minutes,
    DatePrototype::get_utc_month,
    DatePrototype::get_utc_seconds,
    DatePrototype::set_date,
    DatePrototype::set_full_year,
    DatePrototype::set_hours,
    DatePrototype::set_milliseconds,
    DatePrototype::set_minutes,
    DatePrototype::set_month,
    DatePrototype::set_seconds,
    DatePrototype::set_time,
    DatePrototype::set_utc_date,
    DatePrototype::set_utc_full_year,
    DatePrototype::set_utc_hours,
    DatePrototype::set_utc_milliseconds,
    DatePrototype::set_utc_minutes,
    DatePrototype::set_utc_month,
    DatePrototype::set_utc_seconds,
    DatePrototype::to_date_string,
    DatePrototype::to_iso_string,
    DatePrototype::to_json,
    DatePrototype::to_locale_date_string,
    DatePrototype::to_locale_string,
    DatePrototype::to_locale_time_string,
    DatePrototype::to_primitive,
    DatePrototype::to_string,
    DatePrototype::to_time_string,
    DatePrototype::to_utc_string,
    DatePrototype::value_of,
    ErrorConstructor::construct,
    ErrorPrototype::to_string,
    EvalErrorConstructor::construct,
    FinalizationRegistryConstructor::construct,
    FinalizationRegistryPrototype::register,
    FinalizationRegistryPrototype::unregister,
    Float32ArrayConstructor::construct,
    Float64ArrayConstructor::construct,
    FunctionConstructor::construct,
    FunctionPrototype::apply,
    FunctionPrototype::bind,
    FunctionPrototype::call_intrinsic,
    FunctionPrototype::has_instance,
    FunctionPrototype::to_string,
    global_object::decode_uri,
    global_object::decode_uri_component,
    global_object::encode_uri,
    global_object::encode_uri_component,
    global_object::eval,
    global_object::is_finite,
    global_object::is_nan,
    global_object::parse_float,
    global_object::parse_int,
    Int8ArrayConstructor::construct,
    Int16ArrayConstructor::construct,
    Int32ArrayConstructor::construct,
    IteratorPrototype::iterator,
    intrinsics::throw_type_error,
    JSONObject::parse,
    JSONObject::stringify,
    MapConstructor::construct,
    MapConstructor::get_species,
    MapIteratorPrototype::next,
    MapPrototype::clear,
    MapPrototype::delete,
    MapPrototype::entries,
    MapPrototype::for_each,
    MapPrototype::get,
    MapPrototype::has,
    MapPrototype::keys,
    MapPrototype::set,
    MapPrototype::size,
    MapPrototype::values,
    MathObject::abs,
    MathObject::acos,
    MathObject::acosh,
    MathObject::asin,
    MathObject::asinh,
    MathObject::atan,
    MathObject::atan2,
    MathObject::atanh,
    MathObject::cbrt,
    MathObject::ceil,
    MathObject::clz32,
    MathObject::cos,
    MathObject::cosh,
    MathObject::exp,
    MathObject::expm1,
    MathObject::floor,
    MathObject::fround,
    MathObject::hypot,
    MathObject::imul,
    MathObject::log,
    MathObject::log1p,
    MathObject::log10,
    MathObject::log2,
    MathObject::max,
    MathObject::min,
    MathObject::pow,
    MathObject::random,
    MathObject::round,
    MathObject::sign,
    MathObject::sin,
    MathObject::sinh,
    MathObject::sqrt,
    MathObject::tan,
    MathObject::tanh,
    MathObject::trunc,
    ObjectConstructor::construct,
    ObjectConstructor::assign,
    ObjectConstructor::create,
    ObjectConstructor::define_properties,
    ObjectConstructor::define_property,
    ObjectConstructor::entries,
    ObjectConstructor::freeze,
    ObjectConstructor::from_entries,
    ObjectConstructor::get_own_property_descriptor,
    ObjectConstructor::get_own_property_descriptors,
    ObjectConstructor::get_own_property_names,
    ObjectConstructor::get_own_property_symbols,
    ObjectConstructor::get_prototype_of,
    ObjectConstructor::has_own,
    ObjectConstructor::is,
    ObjectConstructor::is_extensible,
    ObjectConstructor::is_frozen,
    ObjectConstructor::is_sealed,
    ObjectConstructor::keys,
    ObjectConstructor::prevent_extensions,
    ObjectConstructor::seal,
    ObjectConstructor::set_prototype_of,
    ObjectConstructor::values,
    ObjectPrototype::has_own_property,
    ObjectPrototype::is_prototype_of,
    ObjectPrototype::property_is_enumerable,
    ObjectPrototype::to_locale_string,
    ObjectPrototype::to_string,
    ObjectPrototype::value_of,
    ObjectPrototype::get_proto,
    ObjectPrototype::set_proto,
    ObjectPrototype::define_getter,
    ObjectPrototype::define_setter,
    ObjectPrototype::lookup_getter,
    ObjectPrototype::lookup_setter,
    ProxyConstructor::construct,
    ProxyConstructor::revocable,
    proxy_constructor::revoke,
    NumberConstructor::construct,
    NumberConstructor::is_finite,
    NumberConstructor::is_integer,
    NumberConstructor::is_nan,
    NumberConstructor::is_safe_integer,
    NumberPrototype::to_exponential,
    NumberPrototype::to_fixed,
    NumberPrototype::to_locale_string,
    NumberPrototype::to_precision,
    NumberPrototype::to_string,
    NumberPrototype::value_of,
    RangeErrorConstructor::construct,
    ReferenceErrorConstructor::construct,
    ReflectObject::apply,
    ReflectObject::construct,
    ReflectObject::define_property,
    ReflectObject::delete_property,
    ReflectObject::get,
    ReflectObject::get_own_property_descriptor,
    ReflectObject::get_prototype_of,
    ReflectObject::has,
    ReflectObject::is_extensible,
    ReflectObject::own_keys,
    ReflectObject::prevent_extensions,
    ReflectObject::set,
    ReflectObject::set_prototype_of,
    RegExpConstructor::construct,
    RegExpConstructor::get_species,
    RegExpPrototype::dot_all,
    RegExpPrototype::exec,
    RegExpPrototype::flags,
    RegExpPrototype::global,
    RegExpPrototype::has_indices,
    RegExpPrototype::ignore_case,
    RegExpPrototype::match_,
    RegExpPrototype::match_all,
    RegExpPrototype::multiline,
    RegExpPrototype::replace,
    RegExpPrototype::search,
    RegExpPrototype::source,
    RegExpPrototype::split,
    RegExpPrototype::sticky,
    RegExpPrototype::test,
    RegExpPrototype::to_string,
    RegExpPrototype::unicode,
    RegExpPrototype::unicode_sets,
    RegExpStringIteratorPrototype::next,
    SetConstructor::construct,
    SetConstructor::get_species,
    SetIteratorPrototype::next,
    SetPrototype::add,
    SetPrototype::clear,
    SetPrototype::delete,
    SetPrototype::entries,
    SetPrototype::for_each,
    SetPrototype::has,
    SetPrototype::size,
    SetPrototype::values,
    StringConstructor::construct,
    StringConstructor::from_char_code,
    StringConstructor::from_code_point,
    StringConstructor::raw,
    StringIteratorPrototype::next,
    StringPrototype::at,
    StringPrototype::char_at,
    StringPrototype::char_code_at,
    StringPrototype::code_point_at,
    StringPrototype::concat,
    StringPrototype::ends_with,
    StringPrototype::includes,
    StringPrototype::index_of,
    StringPrototype::is_well_formed,
    StringPrototype::last_index_of,
    StringPrototype::locale_compare,
    StringPrototype::match_,
    StringPrototype::match_all,
    StringPrototype::normalize,
    StringPrototype::pad_end,
    StringPrototype::pad_start,
    StringPrototype::repeat,
    StringPrototype::replace,
    StringPrototype::replace_all,
    StringPrototype::search,
    StringPrototype::slice,
    StringPrototype::split,
    StringPrototype::starts_with,
    StringPrototype::substring,
    StringPrototype::to_locale_lower_case,
    StringPrototype::to_locale_upper_case,
    StringPrototype::to_lower_case,
    StringPrototype::to_string,
    StringPrototype::to_upper_case,
    StringPrototype::to_well_formed,
    StringPrototype::trim,
    StringPrototype::trim_end,
    StringPrototype::trim_start,
    StringPrototype::value_of,
    StringPrototype::iterator,
    SymbolConstructor::construct,
    SymbolConstructor::for_,
    SymbolConstructor::key_for,
    SymbolPrototype::get_description,
    SymbolPrototype::to_primitive,
    SymbolPrototype::to_string,
    SymbolPrototype::value_of,
    SyntaxErrorConstructor::construct,
    TypeErrorConstructor::construct,
    TypedArrayConstructor::construct,
    TypedArrayConstructor::from,
    TypedArrayConstructor::of,
    TypedArrayConstructor::get_species,
    TypedArrayPrototype::at,
    TypedArrayPrototype::buffer,
    TypedArrayPrototype::byte_length,
    TypedArrayPrototype::byte_offset,
    TypedArrayPrototype::copy_within,
    TypedArrayPrototype::entries,
    TypedArrayPrototype::every,
    TypedArrayPrototype::fill,
    TypedArrayPrototype::filter,
    TypedArrayPrototype::find,
    TypedArrayPrototype::find_index,
    TypedArrayPrototype::find_last,
    TypedArrayPrototype::find_last_index,
    TypedArrayPrototype::for_each,
    TypedArrayPrototype::get_to_string_tag,
    TypedArrayPrototype::includes,
    TypedArrayPrototype::index_of,
    TypedArrayPrototype::join,
    TypedArrayPrototype::keys,
    TypedArrayPrototype::last_index_of,
    TypedArrayPrototype::length,
    TypedArrayPrototype::map,
    TypedArrayPrototype::reduce,
    TypedArrayPrototype::reduce_right,
    TypedArrayPrototype::reverse,
    TypedArrayPrototype::set,
    TypedArrayPrototype::slice,
    TypedArrayPrototype::some,
    TypedArrayPrototype::sort,
    TypedArrayPrototype::subarray,
    TypedArrayPrototype::to_locale_string,
    TypedArrayPrototype::to_reversed,
    TypedArrayPrototype::to_sorted,
    TypedArrayPrototype::values,
    TypedArrayPrototype::with,
    UInt8ArrayConstructor::construct,
    UInt8ClampedArrayConstructor::construct,
    UInt16ArrayConstructor::construct,
    UInt32ArrayConstructor::construct,
    URIErrorConstructor::construct,
    WeakMapConstructor::construct,
    WeakMapPrototype::delete,
    WeakMapPrototype::get,
    WeakMapPrototype::has,
    WeakMapPrototype::set,
    WeakRefConstructor::construct,
    WeakRefPrototype::deref,
    WeakSetConstructor::construct,
    WeakSetPrototype::add,
    WeakSetPrototype::delete,
    WeakSetPrototype::has,
    // Non-standard functions
    ConsoleObject::log,
    GcObject::run,
);
