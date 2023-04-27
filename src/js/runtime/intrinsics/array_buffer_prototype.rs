use crate::js::runtime::{
    error::type_error_, gc::Gc, object_value::ObjectValue, property::Property, realm::Realm,
    Context, EvalResult, PropertyKey, Value,
};

use super::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic};

pub struct ArrayBufferPrototype;

impl ArrayBufferPrototype {
    // 25.1.5 Properties of the ArrayBuffer Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ArrayBufferConstructor has been created
        object.intrinsic_getter(cx, cx.names.byte_length(), Self::get_byte_length, realm);

        // 25.1.5.4 ArrayBuffer.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.array_buffer().as_string().into(), false, false, true),
        );

        object
    }

    // 25.1.5.1 get ArrayBuffer.prototype.byteLength
    fn get_byte_length(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if this_value.is_object() {
            let this_object = this_value.as_object();
            if this_object.is_array_buffer() {
                let mut array_buffer = this_object.cast::<ArrayBufferObject>();

                return Value::from(array_buffer.data().len()).into();
            } else if this_object.is_shared_array_buffer() {
                return type_error_(cx, "cannot access byteLength of shared array buffer");
            }
        }

        type_error_(cx, "expected array buffer")
    }
}
