use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        collections::{array::ByteArray, BsArray},
        completion::EvalResult,
        error::{range_error, type_error},
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        get,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
        type_utilities::to_index,
        Context, Handle, HeapPtr, Value,
    },
    maybe, set_uninit,
};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

// 4GB max array buffer size
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 32;

// ArrayBuffer Objects (https://tc39.es/ecma262/#sec-arraybuffer-objects)
extend_object! {
    pub struct ArrayBufferObject {
        // Byte length of the array buffer. Stored separately from data as data might be detached
        byte_length: usize,
        // Maximum byte length that the array buffer can grow to. Fixed length if no max is set.
        max_byte_length: Option<usize>,
        // Data block containing array buffer's binary data. Detached array buffers represented as
        // a null data pointer.
        data: Option<HeapPtr<ByteArray>>,
    }
}

impl ArrayBufferObject {
    /// AllocateArrayBuffer (https://tc39.es/ecma262/#sec-allocatearraybuffer)
    pub fn new(
        cx: Context,
        constructor: Handle<ObjectValue>,
        byte_length: usize,
        max_byte_length: Option<usize>,
        data: Option<Handle<ByteArray>>,
    ) -> EvalResult<Handle<ArrayBufferObject>> {
        if let Some(max_byte_length) = max_byte_length {
            if byte_length > max_byte_length {
                return range_error(cx, "byte length exceeds max byte length");
            }
        }

        let mut object = maybe!(object_create_from_constructor::<ArrayBufferObject>(
            cx,
            constructor,
            ObjectKind::ArrayBufferObject,
            Intrinsic::ArrayBufferPrototype
        ));

        // Temporarily fill default values so object is fully initialized before GC may be triggered
        set_uninit!(object.byte_length, byte_length);
        set_uninit!(object.max_byte_length, max_byte_length);
        set_uninit!(object.data, None);

        if byte_length > MAX_ARRAY_BUFFER_SIZE {
            return range_error(
                cx,
                &format!("cannot allocate array buffer of size {}", byte_length),
            );
        }

        if let Some(max_byte_length) = max_byte_length {
            if max_byte_length > MAX_ARRAY_BUFFER_SIZE {
                return range_error(cx, "max byte length exceeds maximum array buffer size");
            }
        }

        // Save object pointer behind handle as we are about to allocate
        let mut object = object.to_handle();

        // Initialize data block to all zeros
        object.data = if let Some(data) = data {
            Some(data.get_())
        } else {
            Some(BsArray::<u8>::new(cx, ObjectKind::ByteArray, byte_length, 0))
        };

        Ok(object)
    }

    pub fn byte_length(&self) -> usize {
        self.byte_length
    }

    pub fn set_byte_length(&mut self, byte_length: usize) {
        self.byte_length = byte_length;
    }

    pub fn max_byte_length(&self) -> Option<usize> {
        self.max_byte_length
    }

    pub fn is_fixed_length(&self) -> bool {
        self.max_byte_length.is_none()
    }

    pub fn data(&mut self) -> &mut [u8] {
        self.data.as_mut().unwrap().as_mut_slice()
    }

    pub fn data_opt(&self) -> Option<Handle<ByteArray>> {
        self.data.map(|data| data.to_handle())
    }

    pub fn set_data(&mut self, data: HeapPtr<ByteArray>) {
        self.data = Some(data);
    }

    pub fn is_detached(&self) -> bool {
        self.data.is_none()
    }

    /// DetachArrayBuffer (https://tc39.es/ecma262/#sec-detacharraybuffer)
    #[allow(dead_code)]
    pub fn detach(&mut self) {
        self.data = None;
        self.byte_length = 0;

        if self.max_byte_length.is_some() {
            self.max_byte_length = Some(0);
        }
    }
}

pub struct ArrayBufferConstructor;

impl ArrayBufferConstructor {
    /// Properties of the ArrayBuffer Constructor (https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-constructor)
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

        // get ArrayBuffer [ @@species ] (https://tc39.es/ecma262/#sec-get-arraybuffer-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm);

        func
    }

    /// ArrayBuffer (https://tc39.es/ecma262/#sec-arraybuffer-length)
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

        let options_arg = get_argument(cx, arguments, 1);
        let max_byte_length = maybe!(get_array_buffer_max_byte_length_option(cx, options_arg));

        Ok(maybe!(ArrayBufferObject::new(
            cx,
            new_target,
            byte_length,
            max_byte_length,
            /* data */ None
        ))
        .as_value())
    }

    /// ArrayBuffer.isView (https://tc39.es/ecma262/#sec-arraybuffer.isview)
    pub fn is_view(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return Ok(cx.bool(false));
        }

        let object = value.as_object();
        let is_view = object.is_data_view() || object.is_typed_array();

        Ok(cx.bool(is_view))
    }
}

/// ArrayBufferCopyAndDetach (https://tc39.es/ecma262/#sec-arraybuffercopyanddetach)
pub fn array_buffer_copy_and_detach(
    cx: Context,
    mut array_buffer: Handle<ArrayBufferObject>,
    new_length: Handle<Value>,
    to_fixed: bool,
) -> EvalResult<Handle<ArrayBufferObject>> {
    let new_byte_length = if new_length.is_undefined() {
        array_buffer.byte_length()
    } else {
        maybe!(to_index(cx, new_length))
    };

    maybe!(throw_if_detached(cx, array_buffer.get_()));

    // Can only remain auto-resizable if ArrayBuffer was already resizable and we are not forced
    // to fix the length.
    let new_max_byte_length = if !to_fixed && !array_buffer.is_fixed_length() {
        array_buffer.max_byte_length()
    } else {
        None
    };

    // Create a new ArrayBuffer with the new length and a direct reference to the old buffer's data
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let new_buffer = maybe!(ArrayBufferObject::new(
        cx,
        array_buffer_constructor,
        new_byte_length,
        new_max_byte_length,
        array_buffer.data_opt(),
    ));

    // Finally detach the original buffer
    array_buffer.detach();

    Ok(new_buffer)
}

/// CloneArrayBuffer (https://tc39.es/ecma262/#sec-clonearraybuffer)
pub fn clone_array_buffer(
    cx: Context,
    mut source_buffer: Handle<ArrayBufferObject>,
    source_byte_offset: usize,
    source_length: usize,
) -> EvalResult<Handle<ArrayBufferObject>> {
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let mut target_buffer = maybe!(ArrayBufferObject::new(
        cx,
        array_buffer_constructor,
        source_length,
        /* max_byte_length */ None,
        /* data */ None,
    ));

    // Copy a portion of the source buffer after the given offset to the target buffer
    let source_buffer_view =
        &source_buffer.data()[source_byte_offset..(source_byte_offset + source_length)];

    target_buffer.data().copy_from_slice(source_buffer_view);

    Ok(target_buffer)
}

/// GetArrayBufferMaxByteLengthOption (https://tc39.es/ecma262/#sec-getarraybuffermaxbytelengthoption)
fn get_array_buffer_max_byte_length_option(
    cx: Context,
    options: Handle<Value>,
) -> EvalResult<Option<usize>> {
    if !options.is_object() {
        return Ok(None);
    }

    let options = options.as_object();
    let max_byte_length = maybe!(get(cx, options, cx.names.max_byte_length()));

    if max_byte_length.is_undefined() {
        return Ok(None);
    }

    Ok(Some(maybe!(to_index(cx, max_byte_length))))
}

#[inline]
pub fn throw_if_detached(cx: Context, array_buffer: HeapPtr<ArrayBufferObject>) -> EvalResult<()> {
    if array_buffer.is_detached() {
        type_error(cx, "array buffer is detached")
    } else {
        Ok(())
    }
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
