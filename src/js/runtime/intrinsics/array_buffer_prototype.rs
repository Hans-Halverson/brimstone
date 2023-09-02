use crate::{
    js::runtime::{
        abstract_operations::{construct, species_constructor},
        error::type_error_,
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::to_integer_or_infinity,
        Context, EvalResult, Handle, Value,
    },
    maybe,
};

use super::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic};

pub struct ArrayBufferPrototype;

impl ArrayBufferPrototype {
    // 25.1.5 Properties of the ArrayBuffer Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ArrayBufferConstructor has been created
        object.intrinsic_getter(cx, cx.names.byte_length(), Self::get_byte_length, realm);
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);

        // 25.1.5.4 ArrayBuffer.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.array_buffer().as_string().into(), false, false, true),
        );

        object
    }

    // 25.1.5.3 ArrayBuffer.prototype.slice
    fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Check type of array buffer argument
        let mut array_buffer = if this_value.is_object() && this_value.as_object().is_array_buffer()
        {
            this_value.as_object().cast::<ArrayBufferObject>()
        } else if this_value.is_object() && this_value.as_object().is_shared_array_buffer() {
            return type_error_(cx, "cannot slice SharedArrayBuffer");
        } else {
            return type_error_(cx, "expected array buffer");
        };

        if array_buffer.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        let length = array_buffer.byte_length() as u64;

        // Calculate the start index of the slice
        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = maybe!(to_integer_or_infinity(cx, start_arg));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        // Calculate the end index of the slice
        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = maybe!(to_integer_or_infinity(cx, end_argument));

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    i64::max(length as i64 + relative_end as i64, 0) as u64
                }
            } else {
                u64::min(relative_end as u64, length)
            }
        } else {
            length
        };

        let new_length = end_index.saturating_sub(start_index);
        let new_length_value = Value::from(new_length).to_handle(cx);

        // Call species constructor to create new array buffer with the given length
        let constructor =
            maybe!(species_constructor(cx, array_buffer.into(), Intrinsic::ArrayBufferConstructor));
        let new_object = maybe!(construct(cx, constructor, &[new_length_value], None));

        // Check type of object returned from constructor
        let mut new_array_buffer = if new_object.is_array_buffer() {
            new_object.cast::<ArrayBufferObject>()
        } else if new_object.is_shared_array_buffer() {
            return type_error_(cx, "constructor cannot return SharedArrayBuffer");
        } else {
            return type_error_(cx, "expected array buffer");
        };

        if new_array_buffer.is_detached() {
            return type_error_(cx, "array buffer is detached");
        } else if new_array_buffer.ptr_eq(&array_buffer) {
            return type_error_(cx, "constructor cannot return same array buffer");
        } else if (new_array_buffer.byte_length() as u64) < new_length {
            return type_error_(cx, "new array buffer is too small");
        }

        // Original array buffer may have become detached during previous calls
        if array_buffer.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        // Copy data from original array buffer to new array buffer
        unsafe {
            let source = array_buffer.data().as_ptr().add(start_index as usize);
            let target = new_array_buffer.data().as_mut_ptr();

            std::ptr::copy_nonoverlapping(source, target, new_length as usize)
        }

        new_array_buffer.into()
    }

    // 25.1.5.1 get ArrayBuffer.prototype.byteLength
    fn get_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if this_value.is_object() {
            let this_object = this_value.as_object();
            if this_object.is_array_buffer() {
                let array_buffer = this_object.cast::<ArrayBufferObject>();

                return Value::from(array_buffer.byte_length()).to_handle(cx).into();
            } else if this_object.is_shared_array_buffer() {
                return type_error_(cx, "cannot access byteLength of SharedArrayBuffer");
            }
        }

        type_error_(cx, "expected array buffer")
    }
}
