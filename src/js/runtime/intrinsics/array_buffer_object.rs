use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        collections::{ArrayInstance, array::ByteArray},
        error::range_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
    },
    set_uninit,
};

#[cfg(target_pointer_width = "64")]
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 32;
#[cfg(target_pointer_width = "32")]
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 31;

extend_object! {
    /// ArrayBuffer Objects (https://tc39.es/ecma262/#sec-arraybuffer-objects)
    pub struct ArrayBufferObject {
        /// Byte length of the array buffer. Stored separately from data as data might be detached
        byte_length: usize,
        /// Maximum byte length that the array buffer can grow to. Fixed length if no max is set.
        max_byte_length: Option<usize>,
        /// Data block containing array buffer's binary data. Detached array buffers represented as
        /// a null data pointer.
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

        let mut object = object_create_from_constructor::<ArrayBufferObject>(
            cx,
            constructor,
            HeapItemKind::ArrayBufferObject,
            Intrinsic::ArrayBufferPrototype,
        )?;

        // Temporarily fill default values so object is fully initialized before GC may be triggered
        set_uninit!(object.byte_length, byte_length);
        set_uninit!(object.max_byte_length, max_byte_length);
        set_uninit!(object.data, None);

        if byte_length > MAX_ARRAY_BUFFER_SIZE {
            return range_error(cx, &format!("cannot allocate array buffer of size {byte_length}"));
        }

        if let Some(max_byte_length) = max_byte_length {
            if max_byte_length > MAX_ARRAY_BUFFER_SIZE {
                return range_error(cx, "max byte length exceeds maximum array buffer size");
            }
        }

        // Save object pointer behind handle as we are about to allocate
        let mut object = object.to_handle();

        object.data = if let Some(data) = data {
            // If requested size matches the underlying data block then simply reuse it
            if byte_length == data.len() {
                Some(*data)
            } else {
                // Otherwise allocate new zeroed data block and copy the data into it
                let mut new_uninit = ByteArray::new_uninit(cx, byte_length)?;

                // Copy data from the old to new data block
                let copied_size = byte_length.min(data.len());
                new_uninit.as_mut_slice()[..copied_size]
                    .copy_from_slice(&data.as_slice()[..copied_size]);

                // Zero out the rest of the new data block
                new_uninit.as_mut_slice()[copied_size..].fill(0);

                Some(new_uninit)
            }
        } else {
            // Initialize data block to all zeros
            Some(ByteArray::new(cx, byte_length, 0)?)
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

    pub fn data(&self) -> &[u8] {
        self.data.as_ref().unwrap().as_slice()
    }

    pub fn data_mut(&mut self) -> &mut [u8] {
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

impl HeapItem for ArrayBufferObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ArrayBufferObject>()
    }

    fn visit_pointers(mut array_buffer_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        array_buffer_object.visit_object_pointers(visitor);
        visitor.visit_pointer_opt(&mut array_buffer_object.data);
    }
}
