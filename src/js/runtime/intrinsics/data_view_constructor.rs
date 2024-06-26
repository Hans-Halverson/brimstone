use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{range_error, type_error},
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
        type_utilities::to_index,
        Context, Handle, HeapPtr, Value,
    },
    maybe,
};

use super::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic};

// 25.3 DataView Objects
extend_object! {
    pub struct DataViewObject {
        viewed_array_buffer: HeapPtr<ArrayBufferObject>,
        byte_length: usize,
        byte_offset: usize,
    }
}

impl DataViewObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        viewed_array_buffer: Handle<ArrayBufferObject>,
        byte_length: usize,
        byte_offset: usize,
    ) -> EvalResult<Handle<DataViewObject>> {
        let mut object = maybe!(object_create_from_constructor::<DataViewObject>(
            cx,
            constructor,
            ObjectKind::DataViewObject,
            Intrinsic::DataViewPrototype
        ));

        let viewed_array_buffer = viewed_array_buffer.get_();

        object.viewed_array_buffer = viewed_array_buffer;
        object.byte_length = byte_length;
        object.byte_offset = byte_offset;

        // Be sure to check for array buffer detachment since constructor may have run user code
        if viewed_array_buffer.is_detached() {
            return type_error(cx, "array buffer is detached");
        }

        object.to_handle().into()
    }

    pub fn viewed_array_buffer_ptr(&self) -> HeapPtr<ArrayBufferObject> {
        self.viewed_array_buffer
    }

    pub fn viewed_array_buffer(&self) -> Handle<ArrayBufferObject> {
        self.viewed_array_buffer.to_handle()
    }

    pub fn byte_length(&self) -> usize {
        self.byte_length
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }
}

pub struct DataViewConstructor;

impl DataViewConstructor {
    // 25.3.3 Properties of the DataView Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.data_view(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::DataViewPrototype).into(),
        );

        func
    }

    // 25.3.2.1 DataView
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "DataView constructor must be called with new");
        };

        let buffer_argument = get_argument(cx, arguments, 0);
        if !buffer_argument.is_object() {
            return type_error(cx, "first argument must be an array buffer");
        }

        let buffer_object = buffer_argument.as_object();
        if !buffer_object.is_array_buffer() {
            return type_error(cx, "first argument must be an array buffer");
        }

        let buffer_object = buffer_object.cast::<ArrayBufferObject>();

        let offset_arg = get_argument(cx, arguments, 1);
        let offset = maybe!(to_index(cx, offset_arg));

        if buffer_object.is_detached() {
            return type_error(cx, "array buffer is detached");
        }

        let buffer_byte_length = buffer_object.byte_length();
        if offset > buffer_byte_length {
            return range_error(
                cx,
                &format!(
                    "offset {} must is out of bounds for buffer with byte length {}",
                    offset, buffer_byte_length
                ),
            );
        }

        let byte_length_argument = get_argument(cx, arguments, 2);
        let view_byte_length = if byte_length_argument.is_undefined() {
            buffer_byte_length - offset
        } else {
            let view_byte_length = maybe!(to_index(cx, byte_length_argument));

            if offset + view_byte_length > buffer_byte_length {
                return range_error(cx, "data view byte length is too large for this buffer");
            }

            view_byte_length
        };

        let data_view = maybe!(DataViewObject::new_from_constructor(
            cx,
            new_target,
            buffer_object,
            view_byte_length,
            offset,
        ));

        data_view.into()
    }
}

impl HeapObject for HeapPtr<DataViewObject> {
    fn byte_size(&self) -> usize {
        size_of::<DataViewObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.viewed_array_buffer);
    }
}
