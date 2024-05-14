use crate::{
    field_offset,
    js::{parser::source::Source, runtime::object_descriptor::ObjectKind},
    set_uninit,
};

use super::{
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    string_value::StringValue,
    Context, Handle, HeapPtr,
};

#[repr(C)]
pub struct SourceFile {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// The name of the source file
    name: HeapPtr<StringValue>,
    /// Inlined source file contents as a WTF8 string
    contents: InlineArray<u8>,
}

impl SourceFile {
    #[inline]
    pub fn new(mut cx: Context, source: &Source) -> Handle<SourceFile> {
        let name = cx.alloc_string(&source.file_path);

        let size = Self::calculate_size_in_bytes(source.contents.len());
        let mut scope = cx.alloc_uninit_with_size::<SourceFile>(size);

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::SourceFile));
        set_uninit!(scope.name, name.get_());

        scope.contents.init_from_slice(source.contents.as_bytes());

        scope.to_handle()
    }

    const CONTENTS_OFFSET: usize = field_offset!(SourceFile, contents);

    #[inline]
    fn calculate_size_in_bytes(contents_len: usize) -> usize {
        Self::CONTENTS_OFFSET + InlineArray::<u8>::calculate_size_in_bytes(contents_len)
    }

    #[inline]
    pub fn name(&self) -> Handle<StringValue> {
        self.name.to_handle()
    }

    #[inline]
    pub fn contents_as_slice(&self) -> &[u8] {
        self.contents.as_slice()
    }
}

impl HeapObject for HeapPtr<SourceFile> {
    fn byte_size(&self) -> usize {
        SourceFile::calculate_size_in_bytes(self.contents.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.name);
    }
}
