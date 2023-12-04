use crate::{
    field_offset,
    js::runtime::{
        collections::InlineArray,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

#[repr(C)]
pub struct ConstantTable {
    descriptor: HeapPtr<ObjectDescriptor>,
    constants: InlineArray<Value>,
}

impl ConstantTable {
    pub fn new(cx: Context, constants: Vec<Handle<Value>>) -> Handle<ConstantTable> {
        let size = Self::calculate_size_in_bytes(constants.len());
        let mut object = cx.alloc_uninit_with_size::<ConstantTable>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::ConstantTable));

        // Copy constants into inline constants array
        object.constants.init_with_uninit(constants.len());
        for (i, constant) in constants.iter().enumerate() {
            set_uninit!(object.constants.as_mut_slice()[i], constant.get());
        }

        object.to_handle()
    }

    const CONSTANTS_BYTE_OFFSET: usize = field_offset!(ConstantTable, constants);

    fn calculate_size_in_bytes(num_constants: usize) -> usize {
        Self::CONSTANTS_BYTE_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_constants)
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants.as_slice()[index]
    }

    pub fn set_constant(&mut self, index: usize, value: Value) {
        self.constants.as_mut_slice()[index] = value;
    }
}

impl HeapObject for HeapPtr<ConstantTable> {
    fn byte_size(&self) -> usize {
        ConstantTable::calculate_size_in_bytes(self.constants.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        // TODO: Add GC metadata for which fields are values and which are raw jump offsets
        for constant in self.constants.as_mut_slice() {
            visitor.visit_value(constant);
        }
    }
}
