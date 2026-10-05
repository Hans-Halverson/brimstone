use num_bigint::{BigInt, Sign};

use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr,
    alloc_error::AllocResult,
    collections::InlineArray,
    debug_print::{DebugPrint, DebugPrinter},
    gc::{HeapItem, HeapVisitor},
    shape::Shape,
};

#[repr(C)]
pub struct BigIntValue {
    shape: HeapPtr<Shape>,
    /// Sign of the BigInt
    sign: Sign,
    /// Inline array of the BigInt's digits
    digits: InlineArray<u32>,
}

impl BigIntValue {
    const DIGITS_OFFSET: usize = std::mem::offset_of!(BigIntValue, digits);

    pub fn new(cx: Context, value: BigInt) -> AllocResult<Handle<BigIntValue>> {
        Ok(Self::new_ptr(cx, value)?.to_handle())
    }

    pub fn new_ptr(cx: Context, value: BigInt) -> AllocResult<HeapPtr<BigIntValue>> {
        // Extract sign and digits from BigInt
        let (sign, digits) = value.to_u32_digits();
        let len = digits.len();

        let size = Self::calculate_size_in_bytes(len);
        let mut bigint = cx.alloc_uninit_with_size::<BigIntValue>(size)?;

        // Copy raw parts of BigInt into BigIntValue
        bigint.shape = cx.shapes.get(HeapItemKind::BigIntValue);
        bigint.sign = sign;
        bigint.digits.init_from_slice(&digits);

        Ok(bigint)
    }

    pub fn calculate_size_in_bytes(num_u32_digits: usize) -> usize {
        // Calculate size of BigIntValue with inlined digits
        Self::DIGITS_OFFSET + InlineArray::<u32>::calculate_size_in_bytes(num_u32_digits)
    }

    pub fn bigint(&self) -> BigInt {
        // Recreate BigInt from stored raw parts
        BigInt::from_slice(self.sign, self.digits.as_slice())
    }
}

impl DebugPrint for HeapPtr<BigIntValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        printer.write_heap_item_with_context(self.cast(), &self.bigint().to_string())
    }
}

impl HeapItem for BigIntValue {
    fn byte_size(big_int_value: HeapPtr<Self>) -> usize {
        BigIntValue::calculate_size_in_bytes(big_int_value.digits.len())
    }

    fn visit_pointers(mut big_int_value: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut big_int_value.shape);
    }
}
