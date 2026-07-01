use crate::runtime::{
    Context, EvalResult, Value,
    collections::{ArrayInstance, array::ByteArray},
    value::Smi,
};

/// A trait for types that can be used as a bitmap.
trait Bitmap {
    /// Get the value of the bit at `index`, returning false if the index is out of bounds.
    fn get_bit(&self, index: usize) -> bool;

    /// Set the bit at `index`. Noop if the index is out of bounds.
    fn set_bit(&mut self, index: usize);

    /// Clear the bit at `index`. Noop if the index is out of bounds.
    fn clear_bit(&mut self, index: usize);
}

impl Bitmap for u32 {
    fn get_bit(&self, index: usize) -> bool {
        if index >= u32::BITS as usize {
            return false;
        }

        *self & (1 << index) != 0
    }

    fn set_bit(&mut self, index: usize) {
        if index >= u32::BITS as usize {
            return;
        }

        *self |= 1 << index;
    }

    fn clear_bit(&mut self, index: usize) {
        if index >= u32::BITS as usize {
            return;
        }

        *self &= !(1 << index);
    }
}

impl Bitmap for [u8] {
    fn get_bit(&self, index: usize) -> bool {
        let byte_index = index / 8;
        if byte_index >= self.len() {
            return false;
        }

        self[byte_index] & (1 << (index % 8)) != 0
    }

    fn set_bit(&mut self, index: usize) {
        let byte_index = index / 8;
        if byte_index >= self.len() {
            return;
        }

        self[byte_index] |= 1 << (index % 8);
    }

    fn clear_bit(&mut self, index: usize) {
        let byte_index = index / 8;
        if byte_index >= self.len() {
            return;
        }

        self[byte_index] &= !(1 << (index % 8));
    }
}

/// A bitmap stored in a Value. Bits are stored inline in a smi if there is enough room, otherwise
/// in a heap ByteArray.
///
/// Supports any size of bitmap but does not resize after creation.
#[derive(Clone, Copy)]
pub struct ValueBitmap(Value);

impl ValueBitmap {
    /// Max number of bits that fit in the inline smi representation.
    const INLINE_BITS: usize = Smi::BITS as usize;

    pub fn from_value(value: Value) -> Self {
        Self(value)
    }

    /// Create a bitmap of the given length with a callback that determines if the i'th bit is set.
    pub fn new(
        cx: Context,
        len: usize,
        mut is_set: impl FnMut(usize) -> bool,
    ) -> EvalResult<Value> {
        if len > Self::INLINE_BITS {
            let mut bytes = ByteArray::new(cx, len.div_ceil(8), 0)?.to_handle();
            for i in 0..len {
                if is_set(i) {
                    bytes.as_mut_slice().set_bit(i);
                }
            }

            Ok(Value::heap_item((*bytes).cast()))
        } else {
            let mut bits = 0u32;
            for i in 0..len {
                if is_set(i) {
                    bits.set_bit(i);
                }
            }

            Ok(Value::raw_smi(bits.cast_signed()))
        }
    }

    /// Whether the bit at `index` is set. Returns false if the index is out of bounds.
    #[inline]
    pub fn get(self, index: usize) -> bool {
        if self.0.is_smi() {
            (self.0.as_smi() as u32).get_bit(index)
        } else {
            let bytes = self.0.as_pointer().cast::<ByteArray>();
            bytes.as_slice().get_bit(index)
        }
    }

    /// Clear the bit at `index`, returning the backing value to store back (either the updated
    /// smi bitmap or the same heap pointer to a modified ByteArray).
    ///
    /// Noop if the index is out of bounds.
    #[inline]
    pub fn clear(self, index: usize) -> Value {
        if self.0.is_smi() {
            let mut bits = self.0.as_smi() as u32;
            bits.clear_bit(index);
            Value::raw_smi(bits.cast_signed())
        } else {
            let mut bytes = self.0.as_pointer().cast::<ByteArray>();
            bytes.as_mut_slice().clear_bit(index);
            self.0
        }
    }
}
