use num_bigint::{BigInt, Sign};

use crate::{
    field_offset,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{HeapItem, HeapVisitor},
        shape::Shape,
    },
    set_uninit,
};

#[repr(C)]
pub struct BigIntValue {
    shape: HeapPtr<Shape>,
    // Number of u32 digits in the BigInt
    len: usize,
    // Sign of the BigInt
    sign: Sign,
    // Start of the BigInt's array of digits. Variable sized but array has a single item for
    // alignment.
    digits: [u32; 1],
}

impl BigIntValue {
    const DIGITS_OFFSET: usize = field_offset!(BigIntValue, digits);

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
        set_uninit!(bigint.shape, cx.shapes.get(HeapItemKind::BigIntValue));
        set_uninit!(bigint.len, digits.len());
        set_uninit!(bigint.sign, sign);

        unsafe { std::ptr::copy_nonoverlapping(digits.as_ptr(), bigint.digits.as_mut_ptr(), len) };

        Ok(bigint)
    }

    pub fn calculate_size_in_bytes(num_u32_digits: usize) -> usize {
        // Calculate size of BigIntValue with inlined digits
        Self::DIGITS_OFFSET + num_u32_digits * size_of::<u32>()
    }

    pub fn bigint(&self) -> BigInt {
        // Recreate BigInt from stored raw parts
        let slice = unsafe { std::slice::from_raw_parts(self.digits.as_ptr(), self.len) };
        BigInt::from_slice(self.sign, slice)
    }
}

impl DebugPrint for HeapPtr<BigIntValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        printer.write_heap_item_with_context(self.cast(), &self.bigint().to_string())
    }
}

impl HeapItem for BigIntValue {
    fn byte_size(big_int_value: HeapPtr<Self>) -> usize {
        BigIntValue::calculate_size_in_bytes(big_int_value.len)
    }

    fn visit_pointers(mut big_int_value: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut big_int_value.shape);
    }
}
