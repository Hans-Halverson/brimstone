use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction,
        error::type_error_,
        intrinsics::array_iterator::{ArrayIterator, ArrayIteratorKind},
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        Context, EvalResult, Gc, PropertyKey, Realm, Value,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, typed_array::TypedArray};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    // 23.2.3 Properties of the %TypedArray% Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once TypedArrayConstructor has been created

        // Create values function as it is referenced by multiple properties
        let values_function = BuiltinFunction::create(
            cx,
            Self::values,
            0,
            &cx.names.values(),
            Some(realm),
            None,
            None,
        )
        .into();

        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_data_prop(&cx.names.values(), values_function);

        // 23.2.3.32 %TypedArray%.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(values_function, true, false, true));

        // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.intrinsic_getter(cx, &to_string_tag_key, Self::get_to_string_tag, realm);

        cx.heap.alloc(object).into()
    }

    // 23.2.3.7 %TypedArray%.prototype.entries
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.2.3.17 %TypedArray%.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::Key).into()
    }

    // 23.2.3.31 %TypedArray%.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::Value).into()
    }

    // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
    pub fn get_to_string_tag(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !this_value.is_object() {
            return Value::undefined().into();
        }

        let this_object = this_value.as_object();
        if !this_object.is_typed_array() {
            return Value::undefined().into();
        }

        this_object.as_typed_array().name(cx).into()
    }
}

#[macro_export]
macro_rules! create_typed_array_prototype {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
        pub struct $prototype;

        impl $prototype {
            // 23.2.7 Properties of the TypedArray Prototype Objects
            pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
                let mut object = OrdinaryObject::new(
                    Some(realm.get_intrinsic(Intrinsic::TypedArrayPrototype)),
                    true,
                );

                // Constructor property is added once TypedArrayConstructor has been created
                object.set_property(
                    &cx.names.bytes_per_element(),
                    Property::data(
                        Value::smi(std::mem::size_of::<$element_type>() as i32),
                        false,
                        false,
                        false,
                    ),
                );

                cx.heap.alloc(object).into()
            }
        }
    };
}

// 23.2.4.3 ValidateTypedArray
#[inline]
fn validate_typed_array(cx: &mut Context, value: Value) -> EvalResult<Gc<dyn TypedArray>> {
    if !value.is_object() {
        return type_error_(cx, "expected typed array");
    }

    let object = value.as_object();
    if !object.is_typed_array() {
        return type_error_(cx, "expected typed array");
    }

    let typed_array = object.as_typed_array();
    if typed_array.viewed_array_buffer().is_detached() {
        return type_error_(cx, "array buffer is detached");
    }

    typed_array.into()
}
