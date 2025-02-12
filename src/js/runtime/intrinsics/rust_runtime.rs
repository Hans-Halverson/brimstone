use std::collections::HashMap;

use crate::{
    js::runtime::{
        async_generator_object, bound_function_object::BoundFunctionObject, console::ConsoleObject,
        gc_object::GcObject, global_names, module, object_value::ObjectValue,
        promise_object::PromiseCapability, test_262_object::Test262Object, Context, EvalResult,
        Handle, Value,
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
    async_from_sync_iterator_prototype::{self, AsyncFromSyncIteratorPrototype},
    async_function_constructor::AsyncFunctionConstructor,
    async_generator_function_constructor::AsyncGeneratorFunctionConstructor,
    async_generator_prototype::AsyncGeneratorPrototype,
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
    generator_function_constructor::GeneratorFunctionConstructor,
    generator_prototype::GeneratorPrototype,
    global_object, intrinsics,
    iterator_constructor::{IteratorConstructor, WrapForValidIteratorPrototype},
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
    promise_constructor::{self, PromiseConstructor},
    promise_prototype::PromisePrototype,
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
static_assert!(NUM_RUST_RUNTIME_FUNCTIONS <= (1 << (RustRuntimeFunctionId::BITS as usize)));

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

                let mut id = 0;

                $(
                    function_to_id.insert($rust_function, id);

                    #[allow(unused_assignments)]
                    {
                        id += 1;
                    }
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
    ArrayBufferConstructor::construct,
    ArrayBufferConstructor::is_view,
    ArrayBufferPrototype::get_byte_length,
    ArrayBufferPrototype::get_detached,
    ArrayBufferPrototype::get_max_byte_length,
    ArrayBufferPrototype::get_resizable,
    ArrayBufferPrototype::resize,
    ArrayBufferPrototype::slice,
    ArrayBufferPrototype::transfer,
    ArrayBufferPrototype::transfer_to_fixed_length,
    ArrayConstructor::construct,
    ArrayConstructor::from,
    ArrayConstructor::is_array,
    ArrayConstructor::of,
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
    async_from_sync_iterator_prototype::async_from_sync_iterator_continuation_on_reject,
    async_from_sync_iterator_prototype::create_continuing_iter_result_object,
    async_from_sync_iterator_prototype::create_done_iter_result_object,
    AsyncFromSyncIteratorPrototype::next,
    AsyncFromSyncIteratorPrototype::return_,
    AsyncFromSyncIteratorPrototype::throw,
    AsyncFunctionConstructor::construct,
    AsyncGeneratorFunctionConstructor::construct,
    async_generator_object::await_return_reject,
    async_generator_object::await_return_resolve,
    AsyncGeneratorPrototype::next,
    AsyncGeneratorPrototype::return_,
    AsyncGeneratorPrototype::throw,
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
    BoundFunctionObject::call,
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
    ErrorPrototype::get_stack,
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
    GeneratorFunctionConstructor::construct,
    GeneratorPrototype::next,
    GeneratorPrototype::return_,
    GeneratorPrototype::throw,
    global_names::global_declaration_instantiation_runtime,
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
    intrinsics::throw_type_error,
    IteratorConstructor::construct,
    IteratorConstructor::from,
    IteratorPrototype::every,
    IteratorPrototype::find,
    IteratorPrototype::for_each,
    IteratorPrototype::get_constructor,
    IteratorPrototype::iterator_prototype_get_to_string_tag,
    IteratorPrototype::reduce,
    IteratorPrototype::set_constructor,
    IteratorPrototype::set_to_string_tag,
    IteratorPrototype::some,
    IteratorPrototype::to_array,
    JSONObject::parse,
    JSONObject::stringify,
    MapConstructor::construct,
    MapConstructor::group_by,
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
    module::execute::async_module_execution_fulfilled,
    module::execute::async_module_execution_rejected_runtime,
    module::execute::load_requested_modules_dynamic_resolve,
    module::execute::load_requested_modules_reject,
    module::execute::load_requested_modules_static_resolve,
    module::execute::module_evaluate_dynamic_resolve,
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
    ObjectConstructor::group_by,
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
    PromiseCapability::executor,
    PromiseConstructor::all,
    PromiseConstructor::all_settled,
    PromiseConstructor::any,
    PromiseConstructor::construct,
    PromiseConstructor::promise_all_resolve,
    PromiseConstructor::promise_all_settled_reject,
    PromiseConstructor::promise_all_settled_resolve,
    PromiseConstructor::promise_any_reject,
    PromiseConstructor::race,
    PromiseConstructor::reject,
    PromiseConstructor::resolve,
    PromiseConstructor::try_,
    PromiseConstructor::with_resolvers,
    promise_constructor::reject_builtin_function,
    promise_constructor::resolve_builtin_function,
    PromisePrototype::catch,
    PromisePrototype::finally,
    PromisePrototype::finally_catch,
    PromisePrototype::finally_catch_continue,
    PromisePrototype::finally_then,
    PromisePrototype::finally_then_continue,
    PromisePrototype::then,
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
    SetIteratorPrototype::next,
    SetPrototype::add,
    SetPrototype::clear,
    SetPrototype::delete,
    SetPrototype::difference,
    SetPrototype::entries,
    SetPrototype::for_each,
    SetPrototype::has,
    SetPrototype::intersection,
    SetPrototype::is_disjoint_from,
    SetPrototype::is_subset_of,
    SetPrototype::is_superset_of,
    SetPrototype::size,
    SetPrototype::symmetric_difference,
    SetPrototype::union,
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
    StringPrototype::to_lower_case,
    StringPrototype::to_string,
    StringPrototype::to_upper_case,
    StringPrototype::to_well_formed,
    StringPrototype::trim,
    StringPrototype::trim_end,
    StringPrototype::trim_start,
    StringPrototype::iterator,
    SymbolConstructor::construct,
    SymbolConstructor::for_,
    SymbolConstructor::key_for,
    SymbolPrototype::get_description,
    SymbolPrototype::to_string,
    SymbolPrototype::value_of,
    SyntaxErrorConstructor::construct,
    TypeErrorConstructor::construct,
    TypedArrayConstructor::construct,
    TypedArrayConstructor::from,
    TypedArrayConstructor::of,
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
    WrapForValidIteratorPrototype::next,
    WrapForValidIteratorPrototype::return_,
    // Non-standard functions
    ConsoleObject::log,
    GcObject::run,
    Test262Object::create_realm,
    Test262Object::detach_array_buffer,
    Test262Object::eval_script,
    Test262Object::print,
    // Shared runtime functions
    return_this,
    return_undefined,
);

/// Rust runtime function that simply returns the `this` argument.
#[no_mangle]
pub fn return_this(
    _: Context,
    this_value: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    Ok(this_value)
}

/// Rust runtime function that simply returns `undefined`.
#[no_mangle]
pub fn return_undefined(
    cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    Ok(cx.undefined())
}
