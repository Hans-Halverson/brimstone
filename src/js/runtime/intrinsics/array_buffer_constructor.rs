use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        collections::BsArray,
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
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 4GB max array buffer size
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 32;

// 25.1 ArrayBuffer Objects
extend_object! {
    pub struct ArrayBufferObject {
        // Byte length of the array buffer. Stored separately from data as data might be detached
        byte_length: usize,
        // Data block containing array buffer's binary data. Detached array buffers represented as
        // a null data pointer.
        data: Option<HeapPtr<DataArray>>,
    }
}

type DataArray = BsArray<u8>;

impl ArrayBufferObject {
    // 25.1.2.1 AllocateArrayBuffer
    pub fn new(
        cx: Context,
        constructor: Handle<ObjectValue>,
        byte_length: usize,
    ) -> EvalResult<Handle<ArrayBufferObject>> {
        let mut object = maybe!(object_create_from_constructor::<ArrayBufferObject>(
            cx,
            constructor,
            ObjectKind::ArrayBufferObject,
            Intrinsic::ArrayBufferPrototype
        ));

        // Temporarily fill default values so object is fully initialized before GC may be triggered
        set_uninit!(object.byte_length, byte_length);
        set_uninit!(object.data, None);

        if byte_length > MAX_ARRAY_BUFFER_SIZE {
            return range_error(
                cx,
                &format!("cannot allocate array buffer of size {}", byte_length),
            );
        }

        // Save object pointer behind handle as we are about to allocate
        let mut object = object.to_handle();

        // Initialize data block to all zeros
        object.data =
            Some(BsArray::<u8>::new(cx, ObjectKind::ArrayBufferDataArray, byte_length, 0));

        object.into()
    }

    pub fn byte_length(&self) -> usize {
        self.byte_length
    }

    pub fn data(&mut self) -> &mut [u8] {
        self.data.as_mut().unwrap().as_mut_slice()
    }

    pub fn is_detached(&self) -> bool {
        self.data.is_none()
    }

    // 25.1.2.3 DetachArrayBuffer
    #[allow(dead_code)]
    pub fn detach(&mut self) {
        self.data = None;
        self.byte_length = 0;
    }
}

pub struct ArrayBufferConstructor;

impl ArrayBufferConstructor {
    // 25.1.4 Properties of the ArrayBuffer Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.array_buffer(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ArrayBufferPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.is_view(), Self::is_view, 1, realm);

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 25.1.3.1 ArrayBuffer
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "ArrayBuffer constructor must be called with new");
        };

        let byte_length_arg = get_argument(cx, arguments, 0);
        let byte_length = maybe!(to_index(cx, byte_length_arg));

        maybe!(ArrayBufferObject::new(cx, new_target, byte_length)).into()
    }

    // 25.1.4.1 ArrayBuffer.isView
    pub fn is_view(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.bool(false).into();
        }

        let object = value.as_object();
        let is_view = object.is_data_view() || object.is_typed_array();

        cx.bool(is_view).into()
    }

    // 25.1.4.3 get ArrayBuffer [ @@species ]
    pub fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}

// 25.1.2.4 CloneArrayBuffer
pub fn clone_array_buffer(
    cx: Context,
    mut source_buffer: Handle<ArrayBufferObject>,
    source_byte_offset: usize,
    source_length: usize,
) -> EvalResult<Handle<ArrayBufferObject>> {
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let mut target_buffer =
        maybe!(ArrayBufferObject::new(cx, array_buffer_constructor, source_length));

    if source_buffer.is_detached() {
        return type_error(cx, "detached array buffer cannot be cloned");
    }

    // Copy a portion of the source buffer after the given offset to the target buffer
    let source_buffer_view =
        &source_buffer.data()[source_byte_offset..(source_byte_offset + source_length)];

    target_buffer.data().copy_from_slice(source_buffer_view);

    target_buffer.into()
}

impl HeapObject for HeapPtr<ArrayBufferObject> {
    fn byte_size(&self) -> usize {
        size_of::<ArrayBufferObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer_opt(&mut self.data);
    }
}

pub struct ArrayBufferDataField;

impl ArrayBufferDataField {
    pub fn byte_size(array: &HeapPtr<DataArray>) -> usize {
        DataArray::calculate_size_in_bytes(array.len())
    }

    pub fn visit_pointers(array: &mut HeapPtr<DataArray>, visitor: &mut impl HeapVisitor) {
        array.visit_pointers(visitor);
    }
}
