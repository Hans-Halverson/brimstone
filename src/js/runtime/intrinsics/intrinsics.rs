use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        collections::InlineArray,
        error::type_error,
        eval_result::EvalResult,
        gc::{HandleScope, HeapVisitor},
        get,
        global_names::create_global_declaration_instantiation_intrinsic,
        intrinsics::{
            aggregate_error_constructor::AggregateErrorConstructor,
            aggregate_error_prototype::AggregateErrorPrototype,
            array_buffer_constructor::ArrayBufferConstructor,
            array_buffer_prototype::ArrayBufferPrototype,
            array_constructor::ArrayConstructor,
            array_iterator::ArrayIteratorPrototype,
            array_prototype::ArrayPrototype,
            async_from_sync_iterator_prototype::AsyncFromSyncIteratorPrototype,
            async_function_constructor::AsyncFunctionConstructor,
            async_function_prototype::AsyncFunctionPrototype,
            async_generator_function_constructor::AsyncGeneratorFunctionConstructor,
            async_generator_function_prototype::AsyncGeneratorFunctionPrototype,
            async_generator_prototype::AsyncGeneratorPrototype,
            async_iterator_prototype::AsyncIteratorPrototype,
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
            generator_function_prototype::GeneratorFunctionPrototype,
            generator_prototype::GeneratorPrototype,
            global_object::{create_eval, create_parse_float, create_parse_int},
            iterator_prototype::IteratorPrototype,
            json_object::JSONObject,
            map_constructor::MapConstructor,
            map_iterator::MapIteratorPrototype,
            map_prototype::MapPrototype,
            math_object::MathObject,
            native_error::*,
            number_constructor::NumberConstructor,
            number_prototype::NumberPrototype,
            object_constructor::ObjectConstructor,
            object_prototype::ObjectPrototype,
            promise_constructor::PromiseConstructor,
            promise_prototype::PromisePrototype,
            proxy_constructor::ProxyConstructor,
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
                BigInt64ArrayConstructor, BigInt64ArrayPrototype, BigUInt64ArrayConstructor,
                BigUInt64ArrayPrototype, Float32ArrayConstructor, Float32ArrayPrototype,
                Float64ArrayConstructor, Float64ArrayPrototype, Int16ArrayConstructor,
                Int16ArrayPrototype, Int32ArrayConstructor, Int32ArrayPrototype,
                Int8ArrayConstructor, Int8ArrayPrototype, UInt16ArrayConstructor,
                UInt16ArrayPrototype, UInt32ArrayConstructor, UInt32ArrayPrototype,
                UInt8ArrayConstructor, UInt8ArrayPrototype, UInt8ClampedArrayConstructor,
                UInt8ClampedArrayPrototype,
            },
            typed_array_constructor::TypedArrayConstructor,
            typed_array_prototype::TypedArrayPrototype,
            weak_map_constructor::WeakMapConstructor,
            weak_map_prototype::WeakMapPrototype,
            weak_ref_constructor::WeakRefConstructor,
            weak_ref_prototype::WeakRefPrototype,
            weak_set_constructor::WeakSetConstructor,
            weak_set_prototype::WeakSetPrototype,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_with_proto,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        Context, Handle, HeapPtr, Value,
    },
    must,
};

#[repr(u8)]
pub enum Intrinsic {
    ArrayBufferConstructor = 0,
    ArrayBufferPrototype,
    ArrayConstructor,
    AggregateErrorConstructor,
    AggregateErrorPrototype,
    ArrayIteratorPrototype,
    ArrayPrototype,
    ArrayPrototypeToString,
    ArrayPrototypeValues,
    AsyncFromSyncIteratorPrototype,
    AsyncFunctionConstructor,
    AsyncFunctionPrototype,
    AsyncGeneratorFunctionConstructor,
    AsyncGeneratorFunctionPrototype,
    AsyncGeneratorPrototype,
    AsyncIteratorPrototype,
    BigInt64ArrayConstructor,
    BigInt64ArrayPrototype,
    BigUInt64ArrayConstructor,
    BigUInt64ArrayPrototype,
    BigIntConstructor,
    BigIntPrototype,
    BooleanConstructor,
    BooleanPrototype,
    DateConstructor,
    DatePrototype,
    DataViewConstructor,
    DataViewPrototype,
    ErrorConstructor,
    ErrorPrototype,
    Eval,
    EvalErrorConstructor,
    EvalErrorPrototype,
    FinalizationRegistryConstructor,
    FinalizationRegistryPrototype,
    Float32ArrayConstructor,
    Float32ArrayPrototype,
    Float64ArrayConstructor,
    Float64ArrayPrototype,
    FunctionConstructor,
    FunctionPrototype,
    GeneratorFunctionConstructor,
    GeneratorFunctionPrototype,
    GeneratorPrototype,
    GlobalDeclarationInstantiation,
    Int8ArrayConstructor,
    Int8ArrayPrototype,
    Int16ArrayConstructor,
    Int16ArrayPrototype,
    Int32ArrayConstructor,
    Int32ArrayPrototype,
    IteratorPrototype,
    #[allow(clippy::upper_case_acronyms)]
    JSON,
    MapConstructor,
    MapIteratorPrototype,
    MapPrototype,
    Math,
    NumberConstructor,
    NumberPrototype,
    ObjectConstructor,
    ObjectPrototype,
    ObjectPrototypeToString,
    ParseFloat,
    ParseInt,
    PromiseConstructor,
    PromisePrototype,
    ProxyConstructor,
    RangeErrorConstructor,
    RangeErrorPrototype,
    ReferenceErrorConstructor,
    ReferenceErrorPrototype,
    Reflect,
    RegExpConstructor,
    RegExpPrototype,
    RegExpStringIteratorPrototype,
    SetConstructor,
    SetIteratorPrototype,
    SetPrototype,
    StringConstructor,
    StringIteratorPrototype,
    StringPrototype,
    SymbolConstructor,
    SymbolPrototype,
    SyntaxErrorConstructor,
    SyntaxErrorPrototype,
    ThrowTypeError,
    TypedArrayConstructor,
    TypedArrayPrototype,
    TypeErrorConstructor,
    TypeErrorPrototype,
    UInt8ArrayConstructor,
    UInt8ArrayPrototype,
    UInt8ClampedArrayConstructor,
    UInt8ClampedArrayPrototype,
    UInt16ArrayConstructor,
    UInt16ArrayPrototype,
    UInt32ArrayConstructor,
    UInt32ArrayPrototype,
    URIErrorConstructor,
    URIErrorPrototype,
    WeakMapConstructor,
    WeakMapPrototype,
    WeakRefConstructor,
    WeakRefPrototype,
    WeakSetConstructor,
    WeakSetPrototype,
    Last,
}

impl Intrinsic {
    const fn num_intrinsics() -> usize {
        Intrinsic::Last as usize
    }
}

#[repr(C)]
pub struct Intrinsics {
    intrinsics: InlineArray<HeapPtr<ObjectValue>>,
}

impl Intrinsics {
    /// CreateIntrinsics (https://tc39.es/ecma262/#sec-createintrinsics)
    pub fn initialize(cx: Context, mut realm: Handle<Realm>) {
        // Initialize all pointers to valid pointer outside heap in case a GC is triggered before
        // they are set to real intrinsic object.
        realm
            .intrinsics
            .intrinsics
            .init_with(Intrinsic::num_intrinsics(), HeapPtr::uninit());

        macro_rules! register_existing_intrinsic {
            ($intrinsic_name:ident, $expr:expr) => {
                realm.intrinsics.intrinsics.as_mut_slice()[Intrinsic::$intrinsic_name as usize] =
                    ($expr).cast::<ObjectValue>().get_();
            };
        }

        macro_rules! register_intrinsic {
            ($intrinsic_name:ident, $struct_name:ident) => {
                // Each intrinsic may use many handles during initialization
                let intrinsic_object = HandleScope::new(cx, |cx| $struct_name::new(cx, realm));

                register_existing_intrinsic!($intrinsic_name, intrinsic_object)
            };
        }

        macro_rules! register_intrinsic_pair {
            ($prototype:ident, $constructor:ident) => {
                register_intrinsic!($prototype, $prototype);
                register_intrinsic!($constructor, $constructor);
                Self::add_constructor_to_prototype(
                    cx,
                    realm,
                    Intrinsic::$prototype,
                    Intrinsic::$constructor,
                );
            };
        }

        // Intrinsics which are used by many other intrinsics during creation. These intrinsics
        // form dependency cycles, so first create uninitialized and then initialize later.
        let object_prototype = ObjectPrototype::new_uninit(cx);
        let function_prototype = FunctionPrototype::new_uninit(cx);

        register_existing_intrinsic!(ObjectPrototype, object_prototype);
        register_existing_intrinsic!(FunctionPrototype, function_prototype);

        // Initialize the global object and scope. Global object will be referenced when settings
        // up intrinsics, but needs object prototype set up first.
        let global_object = object_create_with_proto::<ObjectValue>(
            cx,
            ObjectKind::OrdinaryObject,
            object_prototype.into(),
        );
        realm.set_global_object(global_object);

        // Now that global object has been created, create default global scope which will be used
        // by all intrinsic closures.
        realm.init_global_scope(cx);

        ObjectPrototype::initialize(cx, object_prototype, realm);
        FunctionPrototype::initialize(cx, function_prototype, realm);

        // Normal intrinsic creation
        register_intrinsic!(ObjectConstructor, ObjectConstructor);
        Self::add_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::ObjectPrototype,
            Intrinsic::ObjectConstructor,
        );

        register_intrinsic!(FunctionConstructor, FunctionConstructor);
        Self::add_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::FunctionPrototype,
            Intrinsic::FunctionConstructor,
        );

        // Builtin functions needed for Number constructor
        register_existing_intrinsic!(ParseFloat, create_parse_float(cx, realm));
        register_existing_intrinsic!(ParseInt, create_parse_int(cx, realm));

        register_intrinsic_pair!(ErrorPrototype, ErrorConstructor);
        register_intrinsic_pair!(BooleanPrototype, BooleanConstructor);
        register_intrinsic_pair!(NumberPrototype, NumberConstructor);
        register_intrinsic_pair!(StringPrototype, StringConstructor);
        register_intrinsic_pair!(SymbolPrototype, SymbolConstructor);
        register_intrinsic_pair!(BigIntPrototype, BigIntConstructor);
        register_intrinsic_pair!(DatePrototype, DateConstructor);
        register_intrinsic_pair!(ArrayPrototype, ArrayConstructor);
        register_intrinsic_pair!(RegExpPrototype, RegExpConstructor);
        register_intrinsic_pair!(PromisePrototype, PromiseConstructor);
        register_intrinsic_pair!(ArrayBufferPrototype, ArrayBufferConstructor);
        register_intrinsic_pair!(DataViewPrototype, DataViewConstructor);
        register_intrinsic_pair!(MapPrototype, MapConstructor);
        register_intrinsic_pair!(SetPrototype, SetConstructor);
        register_intrinsic_pair!(WeakRefPrototype, WeakRefConstructor);
        register_intrinsic_pair!(WeakSetPrototype, WeakSetConstructor);
        register_intrinsic_pair!(WeakMapPrototype, WeakMapConstructor);
        register_intrinsic_pair!(FinalizationRegistryPrototype, FinalizationRegistryConstructor);

        // Properties of basic intrinsics
        let object_prototype = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let object_prototype_to_string = must!(get(cx, object_prototype, cx.names.to_string()));
        register_existing_intrinsic!(
            ObjectPrototypeToString,
            object_prototype_to_string.as_object()
        );

        let array_prototype = realm.get_intrinsic(Intrinsic::ArrayPrototype);
        let array_prototype_values = must!(get(cx, array_prototype, cx.names.values()));
        register_existing_intrinsic!(ArrayPrototypeValues, array_prototype_values.as_object());

        let array_prototype_to_string = must!(get(cx, array_prototype, cx.names.to_string()));
        register_existing_intrinsic!(ArrayPrototypeToString, array_prototype_to_string.as_object());

        // Native errors
        register_intrinsic_pair!(AggregateErrorPrototype, AggregateErrorConstructor);
        register_intrinsic_pair!(EvalErrorPrototype, EvalErrorConstructor);
        register_intrinsic_pair!(RangeErrorPrototype, RangeErrorConstructor);
        register_intrinsic_pair!(ReferenceErrorPrototype, ReferenceErrorConstructor);
        register_intrinsic_pair!(SyntaxErrorPrototype, SyntaxErrorConstructor);
        register_intrinsic_pair!(TypeErrorPrototype, TypeErrorConstructor);
        register_intrinsic_pair!(URIErrorPrototype, URIErrorConstructor);

        // Typed arrays
        register_intrinsic_pair!(TypedArrayPrototype, TypedArrayConstructor);
        register_intrinsic_pair!(Int8ArrayPrototype, Int8ArrayConstructor);
        register_intrinsic_pair!(UInt8ArrayPrototype, UInt8ArrayConstructor);
        register_intrinsic_pair!(UInt8ClampedArrayPrototype, UInt8ClampedArrayConstructor);
        register_intrinsic_pair!(Int16ArrayPrototype, Int16ArrayConstructor);
        register_intrinsic_pair!(UInt16ArrayPrototype, UInt16ArrayConstructor);
        register_intrinsic_pair!(Int32ArrayPrototype, Int32ArrayConstructor);
        register_intrinsic_pair!(UInt32ArrayPrototype, UInt32ArrayConstructor);
        register_intrinsic_pair!(BigInt64ArrayPrototype, BigInt64ArrayConstructor);
        register_intrinsic_pair!(BigUInt64ArrayPrototype, BigUInt64ArrayConstructor);
        register_intrinsic_pair!(Float32ArrayPrototype, Float32ArrayConstructor);
        register_intrinsic_pair!(Float64ArrayPrototype, Float64ArrayConstructor);

        // Iterators
        register_intrinsic!(IteratorPrototype, IteratorPrototype);
        register_intrinsic!(AsyncIteratorPrototype, AsyncIteratorPrototype);
        register_intrinsic!(AsyncFromSyncIteratorPrototype, AsyncFromSyncIteratorPrototype);
        register_intrinsic!(ArrayIteratorPrototype, ArrayIteratorPrototype);
        register_intrinsic!(StringIteratorPrototype, StringIteratorPrototype);
        register_intrinsic!(MapIteratorPrototype, MapIteratorPrototype);
        register_intrinsic!(SetIteratorPrototype, SetIteratorPrototype);
        register_intrinsic!(RegExpStringIteratorPrototype, RegExpStringIteratorPrototype);

        // Async functions
        register_intrinsic_pair!(AsyncFunctionPrototype, AsyncFunctionConstructor);

        // Generators
        register_intrinsic!(GeneratorPrototype, GeneratorPrototype);
        register_intrinsic!(GeneratorFunctionPrototype, GeneratorFunctionPrototype);
        register_intrinsic!(GeneratorFunctionConstructor, GeneratorFunctionConstructor);
        Self::add_non_writable_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::GeneratorFunctionPrototype,
            Intrinsic::GeneratorFunctionConstructor,
        );
        Self::add_non_writable_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::GeneratorPrototype,
            Intrinsic::GeneratorFunctionPrototype,
        );

        // Async generators
        register_intrinsic!(AsyncGeneratorPrototype, AsyncGeneratorPrototype);
        register_intrinsic!(AsyncGeneratorFunctionPrototype, AsyncGeneratorFunctionPrototype);
        register_intrinsic!(AsyncGeneratorFunctionConstructor, AsyncGeneratorFunctionConstructor);
        Self::add_non_writable_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::AsyncGeneratorFunctionPrototype,
            Intrinsic::AsyncGeneratorFunctionConstructor,
        );
        Self::add_non_writable_constructor_to_prototype(
            cx,
            realm,
            Intrinsic::AsyncGeneratorPrototype,
            Intrinsic::AsyncGeneratorFunctionPrototype,
        );

        // Builtin objects
        register_intrinsic!(JSON, JSONObject);
        register_intrinsic!(Math, MathObject);
        register_intrinsic!(ProxyConstructor, ProxyConstructor);
        register_intrinsic!(Reflect, ReflectObject);

        // Builtin functions
        register_existing_intrinsic!(Eval, create_eval(cx, realm));
        register_existing_intrinsic!(
            GlobalDeclarationInstantiation,
            create_global_declaration_instantiation_intrinsic(cx, realm)
        );

        let throw_type_error_intrinsic = create_throw_type_error_intrinsic(cx, realm);
        register_existing_intrinsic!(ThrowTypeError, throw_type_error_intrinsic);

        add_restricted_function_properties(
            cx,
            realm.get_intrinsic(Intrinsic::FunctionPrototype),
            realm,
        );
    }

    #[inline]
    pub fn calculate_size_in_bytes() -> usize {
        InlineArray::<HeapPtr<ObjectValue>>::calculate_size_in_bytes(Intrinsic::num_intrinsics())
    }

    pub fn get_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.intrinsics.as_slice()[intrinsic as usize]
    }

    pub fn get(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.get_ptr(intrinsic).to_handle()
    }

    // Intrinsic prototypes are created before their corresponding constructors, so we must add a
    // constructor property after creation.
    fn add_constructor_to_prototype(
        cx: Context,
        realm: Handle<Realm>,
        prototype: Intrinsic,
        constructor: Intrinsic,
    ) {
        let mut prototype_object = realm.get_intrinsic(prototype);
        let constructor_object = realm.get_intrinsic(constructor);

        let constructor_desc =
            PropertyDescriptor::data(constructor_object.into(), true, false, true);
        must!(prototype_object.define_own_property(cx, cx.names.constructor(), constructor_desc));
    }

    // Same as `add_constructor_to_prototype` but sets the constructor property to be non-writable.
    fn add_non_writable_constructor_to_prototype(
        cx: Context,
        realm: Handle<Realm>,
        prototype: Intrinsic,
        constructor: Intrinsic,
    ) {
        let mut prototype_object = realm.get_intrinsic(prototype);
        let constructor_object = realm.get_intrinsic(constructor);

        let constructor_desc =
            PropertyDescriptor::data(constructor_object.into(), false, false, true);
        must!(prototype_object.define_own_property(cx, cx.names.constructor(), constructor_desc));
    }
}

pub fn throw_type_error(
    cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    type_error(cx, "'caller', 'callee', and 'arguments' properties may not be accessed on strict mode functions or the arguments objects for calls to them")
}

/// %ThrowTypeError% (https://tc39.es/ecma262/#sec-%throwtypeerror%)
fn create_throw_type_error_intrinsic(cx: Context, realm: Handle<Realm>) -> Handle<Value> {
    let throw_type_error_func = BuiltinFunction::create_builtin_function_without_properties(
        cx,
        throw_type_error,
        /* name */ None,
        realm,
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into();

    let zero_value = cx.zero();
    let length_desc = PropertyDescriptor::data(zero_value, false, false, false);
    must!(define_property_or_throw(
        cx,
        throw_type_error_func,
        cx.names.length(),
        length_desc,
    ));

    // Is anonymous function so name is empty
    let name = cx.names.empty_string().as_string().into();
    let name_desc = PropertyDescriptor::data(name, false, false, false);
    must!(define_property_or_throw(cx, throw_type_error_func, cx.names.name(), name_desc,));

    must!(throw_type_error_func
        .cast::<ObjectValue>()
        .prevent_extensions(cx));

    throw_type_error_func.into()
}

/// AddRestrictedFunctionProperties (https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties)
fn add_restricted_function_properties(
    cx: Context,
    func: Handle<ObjectValue>,
    realm: Handle<Realm>,
) {
    let thrower_func = realm.get_intrinsic(Intrinsic::ThrowTypeError);

    let caller_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must!(define_property_or_throw(cx, func, cx.names.caller(), caller_desc));

    let arguments_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must!(define_property_or_throw(cx, func, cx.names.arguments(), arguments_desc,));
}

impl Intrinsics {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        for intrinsic in self.intrinsics.as_mut_slice() {
            visitor.visit_pointer(intrinsic);
        }
    }
}
