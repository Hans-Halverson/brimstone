use crate::{
    field_offset,
    js::runtime::{
        collections::InlineArray,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        string_value::FlatString,
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

#[repr(C)]
pub struct ImportAttributes {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Map of all key-value pairs in the import attributes. Both keys and values must be interned
    /// strings. The keys are sorted in lexicographic order.
    ///
    /// | key1 | value1 | key2 | value2 | ... | keyN | valueN |
    attribute_pairs: InlineArray<HeapPtr<FlatString>>,
}

impl ImportAttributes {
    const ATTRIBUTE_PAIRS_OFFSET: usize = field_offset!(ImportAttributes, attribute_pairs);

    pub fn new(
        cx: Context,
        attribute_pairs: &[(Handle<FlatString>, Handle<FlatString>)],
    ) -> Handle<ImportAttributes> {
        let num_entries = attribute_pairs.len() * 2;
        let size = Self::calculate_size_in_bytes(num_entries);
        let mut object = cx.alloc_uninit_with_size::<ImportAttributes>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::ImportAttributes));

        object.attribute_pairs.init_with_uninit(num_entries);
        for (i, (key, value)) in attribute_pairs.iter().enumerate() {
            object.attribute_pairs.as_mut_slice()[i * 2] = **key;
            object.attribute_pairs.as_mut_slice()[i * 2 + 1] = **value;
        }

        object.to_handle()
    }

    #[inline]
    fn calculate_size_in_bytes(num_entries: usize) -> usize {
        let attributes_size =
            InlineArray::<HeapPtr<FlatString>>::calculate_size_in_bytes(num_entries);
        Self::ATTRIBUTE_PAIRS_OFFSET + attributes_size
    }

    pub fn has_attribute_with_value(&self, key: &str, value: &str) -> bool {
        for attribute_pair in self.attribute_pairs.as_slice().chunks_exact(2) {
            if attribute_pair[0].eq_str(key) && attribute_pair[1].eq_str(value) {
                return true;
            }
        }

        false
    }
}

impl PartialEq for HeapPtr<ImportAttributes> {
    fn eq(&self, other: &Self) -> bool {
        if self.attribute_pairs.len() != other.attribute_pairs.len() {
            return false;
        }

        // Attribute pairs are interned strings and sorted by key, so testing map equality should
        // reduce to equality of all pointers in the arrays.
        self.attribute_pairs
            .as_slice()
            .iter()
            .zip(other.attribute_pairs.as_slice())
            .all(|(entry1, entry2)| entry1.ptr_eq(entry2))
    }
}

impl Eq for HeapPtr<ImportAttributes> {}

impl HeapObject for HeapPtr<ImportAttributes> {
    fn byte_size(&self) -> usize {
        ImportAttributes::calculate_size_in_bytes(self.attribute_pairs.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for entry in self.attribute_pairs.as_mut_slice() {
            visitor.visit_pointer(entry);
        }
    }
}
