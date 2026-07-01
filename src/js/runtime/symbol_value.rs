use std::hash;

use rand::Rng;

use crate::{
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemDescriptor,
        string_value::{FlatString, StringValue},
    },
    set_uninit,
};

#[repr(C)]
pub struct SymbolValue {
    descriptor: HeapPtr<HeapItemDescriptor>,
    description: Option<HeapPtr<FlatString>>,
    /// Stable hash code for this symbol, since symbol can be moved by GC
    hash_code: u32,
    /// Whether this symbol is for a private name
    is_private: bool,
}

impl SymbolValue {
    pub fn new(
        mut cx: Context,
        description: Option<Handle<StringValue>>,
        is_private: bool,
    ) -> AllocResult<Handle<SymbolValue>> {
        let description = description.map(|d| d.flatten()).transpose()?;
        let mut symbol = cx.alloc_uninit::<SymbolValue>()?;

        set_uninit!(symbol.descriptor, cx.descriptors.get(HeapItemKind::SymbolValue));
        set_uninit!(symbol.description, description.map(|desc| *desc));
        set_uninit!(symbol.hash_code, cx.rand.r#gen::<u32>());
        set_uninit!(symbol.is_private, is_private);

        Ok(symbol.to_handle())
    }

    pub fn description_ptr(&self) -> Option<HeapPtr<FlatString>> {
        self.description
    }

    pub fn description(&self) -> Option<Handle<FlatString>> {
        self.description.map(|d| d.to_handle())
    }

    pub fn is_private(&self) -> bool {
        self.is_private
    }
}

impl DebugPrint for HeapPtr<SymbolValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        if let Some(description) = self.description_ptr() {
            printer.write_heap_item_with_context(self.cast(), &description.to_string())
        } else {
            printer.write_heap_item_default(self.cast())
        }
    }
}

impl hash::Hash for SymbolValue {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code.hash(state)
    }
}

impl hash::Hash for HeapPtr<SymbolValue> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code.hash(state)
    }
}

impl PartialEq for HeapPtr<SymbolValue> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}

impl Eq for HeapPtr<SymbolValue> {}

impl hash::Hash for Handle<SymbolValue> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl PartialEq for Handle<SymbolValue> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl Eq for Handle<SymbolValue> {}

impl HeapItem for SymbolValue {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<SymbolValue>()
    }

    fn visit_pointers(mut symbol_value: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut symbol_value.descriptor);
        visitor.visit_pointer_opt(&mut symbol_value.description);
    }
}
