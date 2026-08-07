use half::f16;

use crate::{
    common::math::{f64_to_clamped_u8, f64_to_f16, f64_to_wrapping_u32},
    runtime::{
        HeapItemKind, HeapPtr, Value,
        gc::AnyHeapItem,
        intrinsics::{
            array_buffer_object::ArrayBufferObject,
            typed_array::{
                Float16ArrayObject, Float32ArrayObject, Float64ArrayObject, Int8ArrayObject,
                Int16ArrayObject, Int32ArrayObject, UInt8ArrayObject, UInt8ClampedArrayObject,
                UInt16ArrayObject, UInt32ArrayObject,
            },
        },
    },
};

/// Generic fast path element conversion.
trait FastElement: Copy {
    /// Convert the element to a value. Cannot allocate or fail.
    fn to_value(self) -> Value;

    /// Convert the value to an element, or None if the value is not a number. Cannot allocate.
    fn from_value(value: Value) -> Option<Self>;
}

macro_rules! impl_fast_elements {
    ($(($element_type:ty, $from_value:expr),)*) => {
        $(impl FastElement for $element_type {
            #[inline(always)]
            fn to_value(self) -> Value {
                Value::number(self)
            }

            #[inline(always)]
            fn from_value(value: Value) -> Option<Self> {
                $from_value(value)
            }
        })*
    };
}

// Spec's ToIntN/ToUintN is converting to a wrapped u32 then truncating to the element's width.
impl_fast_elements! {
    (i8, |value| Some(value_to_wrapping_u32(value)? as i8)),
    (u8, |value| Some(value_to_wrapping_u32(value)? as u8)),
    (i16, |value| Some(value_to_wrapping_u32(value)? as i16)),
    (u16, |value| Some(value_to_wrapping_u32(value)? as u16)),
    (i32, |value| Some(value_to_wrapping_u32(value)? as i32)),
    (u32, value_to_wrapping_u32),
    (f32, |value| Some(value_to_f64(value)? as f32)),
    (f64, value_to_f64),
}

impl FastElement for f16 {
    #[inline(always)]
    fn to_value(self) -> Value {
        Value::number(self.to_f64())
    }

    #[inline(always)]
    fn from_value(value: Value) -> Option<Self> {
        Some(f64_to_f16(value_to_f64(value)?))
    }
}

/// Uint8Clamped elements are identical to u8 except with custom clamping logic.
#[derive(Clone, Copy)]
#[repr(transparent)]
struct ClampedU8(u8);

impl FastElement for ClampedU8 {
    #[inline(always)]
    fn to_value(self) -> Value {
        Value::number(self.0)
    }

    #[inline(always)]
    fn from_value(value: Value) -> Option<Self> {
        if value.is_smi() {
            Some(ClampedU8(value.as_smi().clamp(0, u8::MAX as i32) as u8))
        } else if value.is_double() {
            Some(ClampedU8(f64_to_clamped_u8(value.as_double())))
        } else {
            None
        }
    }
}

/// Convert value to an f64 if it is a number, or None if value is not a number.
#[inline(always)]
fn value_to_f64(value: Value) -> Option<f64> {
    if value.is_smi() {
        Some(value.as_smi() as f64)
    } else if value.is_double() {
        Some(value.as_double())
    } else {
        None
    }
}

/// Convert value to a u32 (following the spec's ToInt32/ToUint32) if it is a number, or None if
/// value is not a number.
#[inline(always)]
fn value_to_wrapping_u32(value: Value) -> Option<u32> {
    if value.is_smi() {
        Some(value.as_smi() as u32)
    } else if value.is_double() {
        Some(f64_to_wrapping_u32(value.as_double()))
    } else {
        None
    }
}

/// Return the pointer to the element at `index` in the typed array with the given fields.
///
/// Return None if the buffer is detached or the index is out of bounds (possibly due to resizing).
#[inline(always)]
fn fast_element_ptr<T>(
    buffer: HeapPtr<ArrayBufferObject>,
    byte_offset: usize,
    array_length: Option<usize>,
    index: usize,
) -> Option<*mut T> {
    let mut data = buffer.data_ptr()?;
    let data = data.as_mut_slice();

    debug_assert!(data.len() == buffer.byte_length());

    let array_length = match array_length {
        Some(array_length) => {
            // The entire fixed length typed array is invalid if the underlying buffer has shrunk
            // below the necessary length for the typed array.
            if byte_offset + array_length * size_of::<T>() > data.len() {
                return None;
            }

            array_length
        }
        // Resizable typed arrays use the underlying buffer size to determine their length
        None => data.len().checked_sub(byte_offset)? / size_of::<T>(),
    };

    if index >= array_length {
        return None;
    }

    let element_offset = byte_offset + index * size_of::<T>();
    let element_ptr = unsafe { data.as_mut_ptr().add(element_offset) }.cast::<T>();

    Some(element_ptr)
}

/// Fast path for getting an element from a specific typed array kind.
#[inline(always)]
fn fast_get_element<T: FastElement>(
    buffer: HeapPtr<ArrayBufferObject>,
    byte_offset: usize,
    array_length: Option<usize>,
    index: usize,
) -> Value {
    match fast_element_ptr::<T>(buffer, byte_offset, array_length, index) {
        Some(element_ptr) => unsafe { element_ptr.read() }.to_value(),
        None => Value::undefined(),
    }
}

/// Fast path for setting an element on a specific typed array kind.
///
/// Returns true if the store was handled, or false if the slow path must be taken.
#[inline(always)]
fn fast_set_element<T: FastElement>(
    buffer: HeapPtr<ArrayBufferObject>,
    byte_offset: usize,
    array_length: Option<usize>,
    index: usize,
    value: Value,
) -> bool {
    let Some(element) = T::from_value(value) else {
        return false;
    };

    if let Some(element_ptr) = fast_element_ptr::<T>(buffer, byte_offset, array_length, index) {
        unsafe { element_ptr.write(element) };
    }

    true
}

macro_rules! create_typed_array_fast_paths {
    ($(($kind:ident, $element_type:ty),)*) => {
        /// Fast path for getting an element from any kind of typed array.
        ///
        /// Returns the value if the load was handled, None if the slow path must be taken.
        #[inline(always)]
        pub fn typed_array_fast_get(
            object: HeapPtr<AnyHeapItem>,
            kind: HeapItemKind,
            index: usize,
        ) -> Option<Value> {
            match kind {
                $(HeapItemKind::$kind => {
                    let typed_array = object.cast::<$kind>();
                    Some(fast_get_element::<$element_type>(
                        typed_array.viewed_array_buffer_ptr(),
                        typed_array.byte_offset(),
                        typed_array.array_length(),
                        index,
                    ))
                })*
                _ => None,
            }
        }

        /// Fast path for setting an element on any kind of typed array.
        ///
        /// Returns true if the store was handled, false if the slow path must be taken.
        #[inline(always)]
        pub fn typed_array_fast_set(
            object: HeapPtr<AnyHeapItem>,
            kind: HeapItemKind,
            index: usize,
            value: Value,
        ) -> bool {
            match kind {
                $(HeapItemKind::$kind => {
                    let typed_array = object.cast::<$kind>();
                    fast_set_element::<$element_type>(
                        typed_array.viewed_array_buffer_ptr(),
                        typed_array.byte_offset(),
                        typed_array.array_length(),
                        index,
                        value,
                    )
                })*
                _ => false,
            }
        }
    };
}

create_typed_array_fast_paths!(
    (Int8ArrayObject, i8),
    (UInt8ArrayObject, u8),
    (UInt8ClampedArrayObject, ClampedU8),
    (Int16ArrayObject, i16),
    (UInt16ArrayObject, u16),
    (Int32ArrayObject, i32),
    (UInt32ArrayObject, u32),
    (Float16ArrayObject, f16),
    (Float32ArrayObject, f32),
    (Float64ArrayObject, f64),
);
