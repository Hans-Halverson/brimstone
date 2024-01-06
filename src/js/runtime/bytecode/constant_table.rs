use crate::{
    field_offset,
    js::runtime::{
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrintMode, DebugPrinter},
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

#[repr(C)]
pub struct ConstantTable {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Array of constants
    constants: InlineArray<Value>,
    /// Compressed metadata about constants. One bit per constant, rounded up to the nearest byte.
    /// 0 if constant is a value, 1 if constant is a raw jump offset.
    _metadata: [u64; 1],
}

impl ConstantTable {
    pub fn new(
        cx: Context,
        constants: Vec<Handle<Value>>,
        metadata: Vec<u8>,
    ) -> Handle<ConstantTable> {
        let size = Self::calculate_size_in_bytes(constants.len());
        let mut object = cx.alloc_uninit_with_size::<ConstantTable>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::ConstantTable));

        // Copy constants into inline constants array
        object.constants.init_with_uninit(constants.len());
        for (i, constant) in constants.iter().enumerate() {
            set_uninit!(object.constants.as_mut_slice()[i], constant.get());
        }

        // Copy metadata into metadata section
        let metadata_ptr = object.get_metadata_ptr() as *mut u8;
        unsafe { std::ptr::copy_nonoverlapping(metadata.as_ptr(), metadata_ptr, metadata.len()) };

        object.to_handle()
    }

    const CONSTANTS_BYTE_OFFSET: usize = field_offset!(ConstantTable, constants);

    fn calculate_size_in_bytes(num_constants: usize) -> usize {
        Self::metadata_offset(num_constants) + Self::calculate_metadata_size(num_constants)
    }

    fn metadata_offset(num_constants: usize) -> usize {
        Self::CONSTANTS_BYTE_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_constants)
    }

    /// One bit per constant, rounded up to the nearest byte.
    pub fn calculate_metadata_size(num_constants: usize) -> usize {
        (num_constants + 7) / 8
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants.as_slice()[index]
    }

    pub fn set_constant(&mut self, index: usize, value: Value) {
        self.constants.as_mut_slice()[index] = value;
    }

    pub fn as_slice(&self) -> &[Value] {
        self.constants.as_slice()
    }

    fn get_metadata_ptr(&self) -> *const u8 {
        let ptr = self as *const ConstantTable as *const u8;
        unsafe { ptr.add(Self::metadata_offset(self.constants.len())) }
    }

    pub fn is_value(&self, index: usize) -> bool {
        // Determine the containing byte and bit
        let byte_index = index / 8;
        let bit_mask = 1 << (index % 8);

        let byte = unsafe { *self.get_metadata_ptr().add(byte_index) };

        byte & bit_mask == 0
    }
}

impl DebugPrint for HeapPtr<ConstantTable> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        if printer.is_short_mode() {
            printer.write_heap_item_default(self.cast());
            return;
        }

        // Constant table items are indented and all written in short mode
        printer.write("Constant Table:\n");

        printer.inc_indent();
        printer.push_mode(DebugPrintMode::Short);

        for (i, constant) in self.constants.as_slice().iter().enumerate() {
            printer.write_indent();
            printer.write(&format!("{}: ", i));

            if self.is_value(i) {
                constant.debug_format(printer);
            } else {
                let raw_offset = constant.as_raw_bits() as usize as isize;
                printer.write(&raw_offset.to_string());
            }

            printer.write("\n");
        }

        printer.pop_mode();
        printer.dec_indent();
    }
}

impl HeapObject for HeapPtr<ConstantTable> {
    fn byte_size(&self) -> usize {
        ConstantTable::calculate_size_in_bytes(self.constants.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        // Only visit constants that are values, not raw offsets
        for i in 0..self.constants.len() {
            if self.is_value(i) {
                let constant = self.constants.get_unchecked_mut(i);
                visitor.visit_value(constant);
            }
        }
    }
}
