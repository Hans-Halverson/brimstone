use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        gc::Gc,
        get,
        intrinsics::{
            array_constructor::ArrayConstructor, array_iterator::ArrayIteratorPrototype,
            array_prototype::ArrayPrototype, bigint_constructor::BigIntConstructor,
            bigint_prototype::BigIntPrototype, boolean_constructor::BooleanConstructor,
            boolean_prototype::BooleanPrototype, error_constructor::ErrorConstructor,
            error_prototype::ErrorPrototype, function_constructor::FunctionConstructor,
            function_prototype::FunctionPrototype, global_object::create_eval,
            iterator_prototype::IteratorPrototype, math_object::MathObject, native_error::*,
            number_constructor::NumberConstructor, number_prototype::NumberPrototype,
            object_constructor::ObjectConstructor, object_prototype::ObjectPrototype,
            reflect_object::ReflectObject, string_constructor::StringConstructor,
            string_prototype::StringPrototype, symbol_constructor::SymbolConstructor,
            symbol_prototype::SymbolPrototype,
        },
        object_value::{Object, ObjectValue},
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        value::Value,
        Context,
    },
    must,
};

#[repr(u8)]
pub enum Intrinsic {
    ArrayConstructor = 0,
    ArrayIteratorPrototype,
    ArrayPrototype,
    ArrayPrototypeValues,
    BigIntConstructor,
    BigIntPrototype,
    BooleanConstructor,
    BooleanPrototype,
    ErrorConstructor,
    ErrorPrototype,
    Eval,
    EvalErrorConstructor,
    EvalErrorPrototype,
    FunctionConstructor,
    FunctionPrototype,
    IteratorPrototype,
    Math,
    NumberConstructor,
    NumberPrototype,
    ObjectConstructor,
    ObjectPrototype,
    ObjectPrototypeToString,
    RangeErrorConstructor,
    RangeErrorPrototype,
    ReferenceErrorConstructor,
    ReferenceErrorPrototype,
    Reflect,
    StringConstructor,
    StringPrototype,
    SymbolConstructor,
    SymbolPrototype,
    SyntaxErrorConstructor,
    SyntaxErrorPrototype,
    ThrowTypeError,
    TypeErrorConstructor,
    TypeErrorPrototype,
    URIErrorConstructor,
    URIErrorPrototype,
    Last,
}

impl Intrinsic {
    const fn num_intrinsics() -> usize {
        Intrinsic::Last as usize
    }
}

pub struct Intrinsics {
    intrinsics: Vec<Gc<ObjectValue>>,
}

impl Intrinsics {
    pub fn new_uninit() -> Intrinsics {
        Intrinsics { intrinsics: Vec::new() }
    }

    // 9.3.2 CreateIntrinsics
    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        self.intrinsics.reserve_exact(Intrinsic::num_intrinsics());
        unsafe { self.intrinsics.set_len(Intrinsic::num_intrinsics()) };

        macro_rules! register_existing_intrinsic {
            ($intrinsic_name:ident, $expr:expr) => {
                self.intrinsics[Intrinsic::$intrinsic_name as usize] = ($expr);
            };
        }

        macro_rules! register_intrinsic {
            ($intrinsic_name:ident, $struct_name:ident) => {
                register_existing_intrinsic!($intrinsic_name, $struct_name::new(cx, realm).into())
            };
        }

        macro_rules! register_intrinsic_pair {
            ($prototype:ident, $constructor:ident) => {
                register_intrinsic!($prototype, $prototype);
                register_intrinsic!($constructor, $constructor);
                self.add_constructor_to_prototype(
                    cx,
                    Intrinsic::$prototype,
                    Intrinsic::$constructor,
                );
            };
        }

        // Intrinsics which are used by many other intrinsics during creation. These intrinsics
        // form depenency cycles, so first create uninitialized and then initialize later.
        let mut object_prototype = ObjectPrototype::new_uninit(cx);
        let mut function_prototype = FunctionPrototype::new_uninit(cx);

        register_existing_intrinsic!(ObjectPrototype, object_prototype.into());
        register_existing_intrinsic!(FunctionPrototype, function_prototype.into());

        object_prototype.initialize(cx, realm);
        function_prototype.initialize(cx, realm);

        // Normal intrinsic creation
        register_intrinsic!(ObjectConstructor, ObjectConstructor);
        self.add_constructor_to_prototype(
            cx,
            Intrinsic::ObjectPrototype,
            Intrinsic::ObjectConstructor,
        );

        register_intrinsic!(FunctionConstructor, FunctionConstructor);
        self.add_constructor_to_prototype(
            cx,
            Intrinsic::FunctionPrototype,
            Intrinsic::FunctionConstructor,
        );

        register_intrinsic_pair!(ErrorPrototype, ErrorConstructor);
        register_intrinsic_pair!(BooleanPrototype, BooleanConstructor);
        register_intrinsic_pair!(NumberPrototype, NumberConstructor);
        register_intrinsic_pair!(StringPrototype, StringConstructor);
        register_intrinsic_pair!(SymbolPrototype, SymbolConstructor);
        register_intrinsic_pair!(BigIntPrototype, BigIntConstructor);
        register_intrinsic_pair!(ArrayPrototype, ArrayConstructor);

        // Native errors
        register_intrinsic_pair!(EvalErrorPrototype, EvalErrorConstructor);
        register_intrinsic_pair!(RangeErrorPrototype, RangeErrorConstructor);
        register_intrinsic_pair!(ReferenceErrorPrototype, ReferenceErrorConstructor);
        register_intrinsic_pair!(SyntaxErrorPrototype, SyntaxErrorConstructor);
        register_intrinsic_pair!(TypeErrorPrototype, TypeErrorConstructor);
        register_intrinsic_pair!(URIErrorPrototype, URIErrorConstructor);

        // Iterators
        register_intrinsic!(IteratorPrototype, IteratorPrototype);
        register_intrinsic!(ArrayIteratorPrototype, ArrayIteratorPrototype);

        // Builtin objects
        register_intrinsic!(Math, MathObject);
        register_intrinsic!(Reflect, ReflectObject);

        // Builtin functions
        register_existing_intrinsic!(Eval, create_eval(cx, realm).into());

        let throw_type_error_intrinsic = create_throw_type_error_intrinsic(cx, realm);
        register_existing_intrinsic!(ThrowTypeError, throw_type_error_intrinsic.into());

        // Properties of other intrinsics
        let object_prototype = self.get(Intrinsic::ObjectPrototype);
        let object_prototype_to_string = must!(get(cx, object_prototype, &cx.names.to_string()));
        register_existing_intrinsic!(
            ObjectPrototypeToString,
            object_prototype_to_string.as_object()
        );

        let array_prototype = self.get(Intrinsic::ArrayPrototype);
        let array_prototype_values = must!(get(cx, array_prototype, &cx.names.values()));
        register_existing_intrinsic!(ArrayPrototypeValues, array_prototype_values.as_object());

        add_restricted_function_properties(cx, self.get(Intrinsic::FunctionPrototype), realm);
    }

    pub fn get(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics[intrinsic as usize]
    }

    // Intrinsic prototypes are created before their corresponding constructors, so we must add a
    // constructor property after creation.
    fn add_constructor_to_prototype(
        &self,
        cx: &mut Context,
        prototype: Intrinsic,
        constructor: Intrinsic,
    ) {
        let mut prototype_object = self.get(prototype);
        let constructor_object = self.get(constructor);

        let constructor_desc =
            PropertyDescriptor::data(constructor_object.into(), true, false, true);
        must!(prototype_object.define_own_property(cx, &cx.names.constructor(), constructor_desc));
    }
}

fn throw_type_error(
    cx: &mut Context,
    _: Value,
    _: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    type_error_(cx, "'caller', 'callee', and 'arguments' properties may not be accessed on strict mode functions or the arguments objects for calls to them")
}

// 10.2.4.1 %ThrowTypeError%
fn create_throw_type_error_intrinsic(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
    let mut throw_type_error_func =
        BuiltinFunction::create_without_properties(cx, throw_type_error, Some(realm), None);

    let length_desc = PropertyDescriptor::data(0.into(), false, false, false);
    must!(define_property_or_throw(
        cx,
        throw_type_error_func.into(),
        &cx.names.length(),
        length_desc,
    ));

    // Is anonymous function so name is empty
    let name = cx.heap.alloc_string(String::new());
    let name_desc = PropertyDescriptor::data(name.into(), false, false, false);
    must!(define_property_or_throw(
        cx,
        throw_type_error_func.into(),
        &cx.names.name(),
        name_desc,
    ));

    throw_type_error_func.prevent_extensions();

    throw_type_error_func
}

// 10.2.4 AddRestrictedFunctionProperties
fn add_restricted_function_properties(cx: &mut Context, func: Gc<ObjectValue>, realm: Gc<Realm>) {
    let thrower_func = realm.get_intrinsic(Intrinsic::ThrowTypeError);

    let caller_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must!(define_property_or_throw(cx, func, &cx.names.caller(), caller_desc));

    let arguments_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must!(define_property_or_throw(cx, func, &cx.names.arguments(), arguments_desc,));
}
