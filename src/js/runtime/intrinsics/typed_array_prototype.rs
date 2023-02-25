use crate::js::runtime::{
    object_value::ObjectValue, ordinary_object::OrdinaryObject, Context, EvalResult, Gc,
    PropertyKey, Realm, Value,
};

use super::intrinsics::Intrinsic;

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    // 23.2.3 Properties of the %TypedArray% Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once TypedArrayConstructor has been created

        // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.intrinsic_getter(cx, &to_string_tag_key, Self::get_to_string_tag, realm);

        cx.heap.alloc(object).into()
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
