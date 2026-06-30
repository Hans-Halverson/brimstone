use crate::{
    field_offset, must_a,
    parser::{loc::calculate_line_offsets, source::Source},
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        collections::{ArrayInstance, InlineArray, array::U32Array},
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemDescriptor,
        string_value::FlatString,
    },
    set_uninit,
};

#[repr(C)]
pub struct SourceFile {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// The path to the source file
    path: HeapPtr<FlatString>,
    /// The display name of the source file, if it is different from the path
    display_name: Option<HeapPtr<FlatString>>,
    /// Lazily generated array of line offsets for the source file
    line_offsets: Option<HeapPtr<U32Array>>,
    /// Inlined source file contents as a WTF-8 string
    contents: InlineArray<u8>,
}

impl SourceFile {
    #[inline]
    pub fn new(mut cx: Context, source: &Source) -> AllocResult<Handle<SourceFile>> {
        // Source guarantees that file path and display name are guaranteed to be within allowed
        // string length.
        let path = must_a!(cx.alloc_flat_string(source.file_path()));
        let display_name = if source.has_display_name() {
            Some(must_a!(cx.alloc_flat_string(source.display_name())))
        } else {
            None
        };

        let size = Self::calculate_size_in_bytes(source.contents.len());
        let mut scope = cx.alloc_uninit_with_size::<SourceFile>(size)?;

        set_uninit!(scope.descriptor, cx.descriptors.get(HeapItemKind::SourceFile));
        set_uninit!(scope.line_offsets, None);
        set_uninit!(scope.path, *path);
        set_uninit!(scope.display_name, display_name.map(|n| *n));

        scope.contents.init_from_slice(source.contents.as_bytes());

        Ok(scope.to_handle())
    }

    const CONTENTS_OFFSET: usize = field_offset!(SourceFile, contents);

    #[inline]
    fn calculate_size_in_bytes(contents_len: usize) -> usize {
        Self::CONTENTS_OFFSET + InlineArray::<u8>::calculate_size_in_bytes(contents_len)
    }

    #[inline]
    pub fn path(&self) -> Handle<FlatString> {
        self.path.to_handle()
    }

    #[inline]
    pub fn display_name(&self) -> Handle<FlatString> {
        self.display_name.as_ref().unwrap_or(&self.path).to_handle()
    }

    #[inline]
    pub fn line_offsets_ptr_raw(&self) -> Option<HeapPtr<U32Array>> {
        self.line_offsets
    }

    #[inline]
    pub fn contents_as_slice(&self) -> &[u8] {
        self.contents.as_slice()
    }
}

impl Handle<SourceFile> {
    pub fn line_offsets_ptr(&mut self, cx: Context) -> AllocResult<HeapPtr<U32Array>> {
        if let Some(line_offsets) = self.line_offsets {
            return Ok(line_offsets);
        }

        // Lazily generate line offsets when first requested
        let raw_line_offsets = calculate_line_offsets(self.contents_as_slice());
        let line_offsets_object = U32Array::new_from_slice(cx, &raw_line_offsets)?;

        self.line_offsets = Some(line_offsets_object);

        Ok(line_offsets_object)
    }

    pub fn get_line(&mut self, cx: Context, line: usize) -> AllocResult<String> {
        let offsets = self.line_offsets_ptr(cx)?;
        let start = offsets.as_slice()[line] as usize;

        let end = if line + 1 < offsets.len() {
            // Exclude the newline character if one exists
            offsets.as_slice()[line + 1] as usize - 1
        } else {
            self.contents.len()
        };

        let line_contents = &self.contents.as_slice()[start..end];
        Ok(String::from_utf8_lossy(line_contents).to_string())
    }
}

impl HeapItem for SourceFile {
    fn byte_size(source_file: HeapPtr<Self>) -> usize {
        SourceFile::calculate_size_in_bytes(source_file.contents.len())
    }

    fn visit_pointers(mut source_file: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut source_file.descriptor);
        visitor.visit_pointer(&mut source_file.path);
        visitor.visit_pointer_opt(&mut source_file.display_name);
        visitor.visit_pointer_opt(&mut source_file.line_offsets);
    }
}
