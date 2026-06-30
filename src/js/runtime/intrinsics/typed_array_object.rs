use crate::{
    must_a,
    runtime::{
        Context, Handle, PropertyKey,
        alloc_error::AllocResult,
        intrinsics::{
            typed_array::DynTypedArray,
            typed_array_prototype::{
                is_typed_array_out_of_bounds, make_typed_array_with_buffer_witness_record,
                typed_array_length,
            },
        },
        to_string,
        type_utilities::{is_integral_number, to_number},
    },
};

#[macro_export]
macro_rules! create_typed_array_object {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $content_type:expr, $to_element:ident, $from_element:ident) => {
        macro_rules! element_size {
            () => {
                std::mem::size_of::<$element_type>()
            };
        }

        extend_object! {
            pub struct $typed_array {
                viewed_array_buffer: HeapPtr<ArrayBufferObject>,
                byte_length: Option<usize>,
                array_length: Option<usize>,
                byte_offset: usize,
            }
        }

        impl $typed_array {
            pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();
            pub const TYPED_ARRAY_VTABLE: *const () = extract_typed_array_vtable::<Self>();

            fn new_with_proto(
                cx: Context,
                proto: Handle<ObjectValue>,
                viewed_array_buffer: Handle<ArrayBufferObject>,
                byte_length: Option<usize>,
                byte_offset: usize,
                array_length: Option<usize>,
            ) -> AllocResult<Handle<ObjectValue>> {
                let mut object = object_create_with_proto::<$typed_array>(
                    cx,
                    HeapItemKind::$typed_array,
                    proto,
                )?;

                set_uninit!(object.viewed_array_buffer, *viewed_array_buffer);
                set_uninit!(object.byte_length, byte_length);
                set_uninit!(object.array_length, array_length);
                set_uninit!(object.byte_offset, byte_offset);

                Ok(object.to_handle().into())
            }

            #[inline]
            fn write_element(
                mut array_buffer: HeapPtr<ArrayBufferObject>,
                byte_index: usize,
                value: $element_type,
            ) {
                unsafe {
                    let byte_ptr = array_buffer.data_mut().as_mut_ptr().add(byte_index);
                    let element_ptr = byte_ptr.cast::<$element_type>();

                    element_ptr.write(value)
                }
            }
        }

        #[wrap_ordinary_object]
        impl VirtualObject for Handle<$typed_array> {
            /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-typedarray-getownproperty)
            fn get_own_property(
                &self,
                cx: Context,
                key: Handle<PropertyKey>,
            ) -> EvalResult<Option<PropertyDescriptor>> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => {
                        Ok(ordinary_get_own_property(cx, self.as_object(), key))
                    }
                    CanonicalIndexType::Invalid => Ok(None),
                    CanonicalIndexType::Valid(index) => {
                        let array_buffer_ptr = self.viewed_array_buffer_ptr();
                        let byte_index = index * element_size!() + self.byte_offset;

                        let value = self.read_element_value(cx, array_buffer_ptr, byte_index)?;

                        let desc = PropertyDescriptor::data(value, true, true, true);

                        Ok(Some(desc))
                    }
                }
            }

            /// [[HasProperty]] (https://tc39.es/ecma262/#sec-typedarray-hasproperty)
            fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_has_property(cx, self.as_object(), key)
                    }
                    CanonicalIndexType::Invalid => Ok(false),
                    CanonicalIndexType::Valid(_) => Ok(true),
                }
            }

            /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-typedarray-defineownproperty)
            fn define_own_property(
                &mut self,
                cx: Context,
                key: Handle<PropertyKey>,
                desc: PropertyDescriptor,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_define_own_property(cx, self.as_object(), key, desc)
                    }
                    CanonicalIndexType::Invalid => Ok(false),
                    CanonicalIndexType::Valid(index) => {
                        if let Some(false) = desc.is_configurable {
                            return Ok(false);
                        } else if let Some(false) = desc.is_enumerable {
                            return Ok(false);
                        } else if desc.is_accessor_descriptor() {
                            return Ok(false);
                        } else if let Some(false) = desc.is_writable {
                            return Ok(false);
                        }

                        let value = if let Some(value) = desc.value {
                            value
                        } else {
                            return Ok(true);
                        };

                        // May allocate and invoke arbitrary user code
                        let element_value = $to_element(cx, value)?;

                        let mut index = index;
                        let mut array_buffer_ptr = self.viewed_array_buffer_ptr();

                        // Fast path - if typed array has known size and underlying ArrayBuffer is
                        // fixed then the bounds checks in `canonical_numeric_index_string` could
                        // not have been invalidated by the call to `$to_element`.
                        //
                        // The only additional case we need to check is if the ArrayBuffer was
                        // detached by the call to `$to_element`.
                        if array_buffer_ptr.is_fixed_length() && self.array_length().is_some() {
                            if array_buffer_ptr.is_detached() {
                                return Ok(true);
                            }
                        } else {
                            // Slow path - all bets are off and we must redo all bounds checks.
                            // This can allocate so be sure to refetch array buffer in case it has
                            // moved.
                            if let CanonicalIndexType::Valid(new_index) =
                                canonical_numeric_index_string(cx, key, self.as_typed_array())?
                            {
                                index = new_index;
                                array_buffer_ptr = self.viewed_array_buffer_ptr();
                            } else {
                                return Ok(true);
                            }
                        }

                        let byte_index = index * element_size!() + self.byte_offset;

                        $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                        Ok(true)
                    }
                }
            }

            /// [[Get]] (https://tc39.es/ecma262/#sec-typedarray-get)
            fn get(
                &self,
                cx: Context,
                key: Handle<PropertyKey>,
                receiver: Handle<Value>,
            ) -> EvalResult<Handle<Value>> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_get(cx, self.as_object(), key, receiver)
                    }
                    CanonicalIndexType::Invalid => Ok(cx.undefined()),
                    CanonicalIndexType::Valid(index) => {
                        let array_buffer_ptr = self.viewed_array_buffer_ptr();
                        let byte_index = index * element_size!() + self.byte_offset;

                        Ok(self.read_element_value(cx, array_buffer_ptr, byte_index)?)
                    }
                }
            }

            /// [[Set]] (https://tc39.es/ecma262/#sec-typedarray-set)
            fn set(
                &mut self,
                cx: Context,
                key: Handle<PropertyKey>,
                value: Handle<Value>,
                receiver: Handle<Value>,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_set(cx, self.as_object(), key, value, receiver)
                    }
                    result @ (CanonicalIndexType::Invalid | CanonicalIndexType::Valid(_)) => {
                        // Check if this is not the same object as the specified receiver
                        if !receiver.is_object()
                            || !(*receiver.as_object()).ptr_eq(&self.as_object())
                        {
                            if matches!(result, CanonicalIndexType::Valid(_)) {
                                return ordinary_set(cx, self.as_object(), key, value, receiver);
                            } else {
                                return Ok(true);
                            }
                        }

                        // May allocate and invoke arbitrary user code
                        let element_value = $to_element(cx, value)?;

                        let index;
                        let mut array_buffer_ptr = self.viewed_array_buffer_ptr();

                        // Fast path - if typed array has known size and underlying ArrayBuffer is
                        // fixed then the bounds checks in `canonical_numeric_index_string` could
                        // not have been invalidated by the call to `$to_element`.
                        //
                        // The only additional case we need to check is if the ArrayBuffer was
                        // detached by the call to `$to_element`.
                        if array_buffer_ptr.is_fixed_length() && self.array_length().is_some() {
                            if array_buffer_ptr.is_detached() {
                                return Ok(true);
                            }

                            if let CanonicalIndexType::Valid(new_index) = result {
                                index = new_index;
                            } else {
                                return Ok(true);
                            }
                        } else {
                            // Slow path - all bets are off and we must redo all bounds checks.
                            // This can allocate so be sure to refetch array buffer in case it has
                            // moved.
                            if let CanonicalIndexType::Valid(new_index) =
                                canonical_numeric_index_string(cx, key, self.as_typed_array())?
                            {
                                index = new_index;
                                array_buffer_ptr = self.viewed_array_buffer_ptr();
                            } else {
                                return Ok(true);
                            }
                        }

                        let byte_index = index * element_size!() + self.byte_offset;

                        $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                        Ok(true)
                    }
                }
            }

            /// [[Delete]] (https://tc39.es/ecma262/#sec-typedarray-delete)
            fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array())? {
                    CanonicalIndexType::NotAnIndex => ordinary_delete(cx, self.as_object(), key),
                    CanonicalIndexType::Invalid => Ok(true),
                    CanonicalIndexType::Valid(_) => Ok(false),
                }
            }

            /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-typedarray-ownpropertykeys)
            fn own_property_keys(&self, mut cx: Context) -> EvalResult<Vec<Handle<Value>>> {
                let typed_array_record =
                    make_typed_array_with_buffer_witness_record(self.as_typed_array());

                let mut keys = vec![];

                if !is_typed_array_out_of_bounds(&typed_array_record) {
                    let length = typed_array_length(&typed_array_record);
                    for i in 0..length {
                        let index_string = cx.alloc_string(&i.to_string())?;
                        keys.push(index_string.into());
                    }
                }

                ordinary_own_string_symbol_property_keys(self.as_object(), &mut keys);

                Ok(keys)
            }

            fn as_typed_array(&self) -> DynTypedArray {
                self.into_dyn_typed_array()
            }
        }

        impl TypedArray for Handle<$typed_array> {
            fn array_length(&self) -> Option<usize> {
                self.array_length
            }

            fn byte_length(&self) -> Option<usize> {
                self.byte_length
            }

            fn byte_offset(&self) -> usize {
                self.byte_offset
            }

            fn viewed_array_buffer_ptr(&self) -> HeapPtr<ArrayBufferObject> {
                self.viewed_array_buffer
            }

            fn viewed_array_buffer(&self) -> Handle<ArrayBufferObject> {
                self.viewed_array_buffer.to_handle()
            }

            fn data(&self) -> &[u8] {
                let byte_offset = self.byte_offset();
                self.viewed_array_buffer.data()[byte_offset..].as_ref()
            }

            fn data_mut(&mut self) -> &mut [u8] {
                let byte_offset = self.byte_offset();
                self.viewed_array_buffer.data_mut()[byte_offset..].as_mut()
            }

            fn name(&self, cx: Context) -> Handle<StringValue> {
                cx.names.$rust_name().as_string()
            }

            fn content_type(&self) -> ContentType {
                $content_type
            }

            fn kind(&self) -> TypedArrayKind {
                TypedArrayKind::$typed_array
            }

            fn element_size(&self) -> usize {
                element_size!()
            }

            #[inline]
            fn read_element_value(
                &self,
                cx: Context,
                array_buffer: HeapPtr<ArrayBufferObject>,
                byte_index: usize,
            ) -> AllocResult<Handle<Value>> {
                let element = unsafe {
                    let byte_ptr = array_buffer.data().as_ptr().add(byte_index);
                    byte_ptr.cast::<$element_type>().read()
                };

                // May allocate
                $from_element(cx, element)
            }

            #[inline]
            fn write_element_value(
                &mut self,
                cx: Context,
                byte_index: usize,
                value: Handle<Value>,
            ) -> EvalResult<()> {
                // May allocate, so call before accessing array buffer
                let element_value = $to_element(cx, value)?;

                unsafe {
                    let byte_ptr = self
                        .viewed_array_buffer_ptr()
                        .data_mut()
                        .as_mut_ptr()
                        .add(byte_index);
                    let element_ptr = byte_ptr.cast::<$element_type>();

                    element_ptr.write(element_value)
                }

                Ok(())
            }

            #[inline]
            fn write_element_value_unchecked(
                &mut self,
                cx: Context,
                array_index: u64,
                value: Handle<Value>,
            ) -> EvalResult<()> {
                // May allocate, so call before accessing array buffer
                let element_value = $to_element(cx, value)?;

                // Call to `$to_element` may invoke user code. Check if array has become detached
                // or out of bounds.
                let typed_array_record =
                    make_typed_array_with_buffer_witness_record(self.as_typed_array());
                if is_typed_array_out_of_bounds(&typed_array_record) {
                    return Ok(());
                }

                // Then check if index has become out of bounds
                let length = typed_array_length(&typed_array_record);
                if array_index >= length as u64 {
                    return Ok(());
                }

                let array_buffer_ptr = self.viewed_array_buffer_ptr();
                let byte_index = (array_index as usize) * element_size!() + self.byte_offset;

                $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                Ok(())
            }
        }

        impl HeapItem for $typed_array {
            fn byte_size(_: HeapPtr<Self>) -> usize {
                size_of::<$typed_array>()
            }

            fn visit_pointers(mut typed_array: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
                typed_array.visit_object_pointers(visitor);
                visitor.visit_pointer(&mut typed_array.viewed_array_buffer);
            }
        }
    };
}

pub enum CanonicalIndexType {
    /// Key is not a canonical numeric index
    NotAnIndex,
    /// Key is a canonical numeric index but not valid for some reason.
    Invalid,
    /// Key is a valid canonical numeric index, which is an int
    Valid(usize),
}

/// CanonicalNumericIndexString (https://tc39.es/ecma262/#sec-canonicalnumericindexstring)
///
/// Determines if the given key is a canonical numeric index string, and validates that it can be
/// used as the index into a typed array with a particular length.
pub fn canonical_numeric_index_string(
    cx: Context,
    key: Handle<PropertyKey>,
    typed_array: DynTypedArray,
) -> AllocResult<CanonicalIndexType> {
    let result = if key.is_array_index() {
        // Fast path for array indices
        let array_index = key.as_array_index() as usize;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(CanonicalIndexType::Invalid);
        }
        let array_length = typed_array_length(&typed_array_record);

        if array_index < array_length {
            CanonicalIndexType::Valid(array_index)
        } else {
            CanonicalIndexType::Invalid
        }
    } else if key.is_string() {
        // Otherwise must convert to number then back to string
        let key_string = key.as_string();
        let number_value = must_a!(to_number(cx, key_string.into()));

        // If string representations are equal, must be canonical numeric index
        let number_string = must_a!(to_string(cx, number_value));
        if key_string.equals(&number_string)? {
            if !is_integral_number(*number_value) {
                return Ok(CanonicalIndexType::Invalid);
            }

            let number = number_value.as_number();
            if number.is_sign_negative() {
                return Ok(CanonicalIndexType::Invalid);
            }

            let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
            if is_typed_array_out_of_bounds(&typed_array_record) {
                return Ok(CanonicalIndexType::Invalid);
            }
            let array_length = typed_array_length(&typed_array_record);

            let number = number as usize;
            if number >= array_length {
                return Ok(CanonicalIndexType::Invalid);
            }

            CanonicalIndexType::Valid(number)
        } else if (*key_string)
            .as_flat()
            .eq(&cx.names.negative_zero.as_string().as_flat())
        {
            // The string "-0" is a canonical numeric index but is never valid as an index
            CanonicalIndexType::Invalid
        } else {
            CanonicalIndexType::NotAnIndex
        }
    } else {
        CanonicalIndexType::NotAnIndex
    };

    Ok(result)
}
