use crate::{
    field_offset,
    js::runtime::{
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::{
    generator::GenRegister,
    operand::{min_width_for_unsigned, Operand, Register},
    width::{ExtraWide, WidthEnum},
};

pub struct ExceptionHandlerBuilder {
    /// Byte offset of the start of the instruction range that is covered (inclusive).
    pub start: usize,
    /// Byte offset of the end of the instruction range that is covered (exclusive).
    pub end: usize,
    /// Byte offset of the handler block that is run when an exception in this range occurs.
    pub handler: usize,
    /// Register in which to the place the error value.
    pub error_register: Option<GenRegister>,
}

impl ExceptionHandlerBuilder {
    /// Create a new exception handler with start and end offsets. The handler offset and register
    /// index will be filled in later.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end, handler: 0, error_register: None }
    }
}

pub struct ExceptionHandlersBuilder {
    /// Collection fo all handlers generated so far in the function.
    handlers: Vec<ExceptionHandlerBuilder>,
    /// The minimum width that fits all numbers in the handlers generated so far.
    width: WidthEnum,
}

impl ExceptionHandlersBuilder {
    pub fn new() -> Self {
        Self { handlers: vec![], width: WidthEnum::Narrow }
    }

    pub fn add(&mut self, handler: ExceptionHandlerBuilder) {
        // Determine the min width that fits all numbers in the handler
        self.width = self
            .width
            .max(min_width_for_unsigned(handler.start))
            .max(min_width_for_unsigned(handler.end))
            .max(min_width_for_unsigned(handler.handler));

        if let Some(reg) = handler.error_register {
            self.width = self.width.max(reg.min_width());
        }

        self.handlers.push(handler);
    }

    pub fn finish(&self, cx: Context) -> Option<Handle<ExceptionHandlers>> {
        if self.handlers.is_empty() {
            return None;
        }

        let mut buffer = vec![];
        for handler in &self.handlers {
            self.write_operand(&mut buffer, handler.start);
            self.write_operand(&mut buffer, handler.end);
            self.write_operand(&mut buffer, handler.handler);

            // The `this` register is used as a sigil value to represent a missing register
            let register = handler.error_register.unwrap_or(Register::this());
            self.write_operand(&mut buffer, register.signed() as isize as usize);
        }

        Some(ExceptionHandlers::new(cx, buffer, self.width))
    }

    fn write_operand(&self, buffer: &mut Vec<u8>, value: usize) {
        match self.width {
            WidthEnum::Narrow => {
                buffer.push(value as u8);
            }
            WidthEnum::Wide => {
                buffer.extend_from_slice(&u16::to_ne_bytes(value as u16));
            }
            WidthEnum::ExtraWide => {
                buffer.extend_from_slice(&usize::to_ne_bytes(value));
            }
        }
    }
}

#[repr(C)]
pub struct ExceptionHandlers {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Width of the encoded handler data. A narrow or wide width means all values are encoded as
    /// one or two bytes, respectively. An extra wide width means all values are encoded as a full
    /// eight bytes.
    width: WidthEnum,
    /// Encoded handlers data.
    handlers: InlineArray<u8>,
}

impl ExceptionHandlers {
    fn new(cx: Context, handlers: Vec<u8>, width: WidthEnum) -> Handle<ExceptionHandlers> {
        let size = Self::calculate_size_in_bytes(handlers.len());
        let mut object = cx.alloc_uninit_with_size::<ExceptionHandlers>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::ExceptionHandlers));
        set_uninit!(object.width, width);
        object.handlers.init_from_vec(handlers);

        object.to_handle()
    }

    const HANDLERS_BYTE_OFFSET: usize = field_offset!(ExceptionHandlers, handlers);

    fn calculate_size_in_bytes(handlers_len: usize) -> usize {
        Self::HANDLERS_BYTE_OFFSET + InlineArray::<u8>::calculate_size_in_bytes(handlers_len)
    }

    /// A zero-copy GC-unsafe iterator over the exception handlers.
    pub fn iter(&self) -> ExceptionHandlersIterator {
        let range = self.handlers.as_slice().as_ptr_range();
        ExceptionHandlersIterator { current: range.start, end: range.end, width: self.width }
    }
}

/// A zero-copy GC-unsafe iterator over the exception handlers.
pub struct ExceptionHandlersIterator {
    current: *const u8,
    end: *const u8,
    width: WidthEnum,
}

/// A view of an exception handler entry in the exception handler table.
#[derive(Clone, Copy)]
pub struct ExceptionHandler {
    /// Pointer to the start of the exception handler entry.
    ptr: *const u8,
    /// Byte width of the values in this entry.
    width: WidthEnum,
}

impl ExceptionHandler {
    fn get_value_at(&self, index: usize) -> usize {
        unsafe {
            match self.width {
                WidthEnum::Narrow => *self.ptr.add(index) as usize,
                WidthEnum::Wide => *self.ptr.add(index * 2).cast::<u16>() as usize,
                WidthEnum::ExtraWide => *self.ptr.add(index * 8).cast::<usize>(),
            }
        }
    }

    pub fn start(&self) -> usize {
        self.get_value_at(0)
    }

    pub fn end(&self) -> usize {
        self.get_value_at(1)
    }

    pub fn handler(&self) -> usize {
        self.get_value_at(2)
    }

    pub fn error_register(&self) -> Option<Register<ExtraWide>> {
        let raw_value = unsafe {
            match self.width {
                WidthEnum::Narrow => *self.ptr.add(3).cast::<i8>() as isize,
                WidthEnum::Wide => *self.ptr.add(6).cast::<i16>() as isize,
                WidthEnum::ExtraWide => *self.ptr.add(24).cast::<isize>(),
            }
        };

        // The `this` register is a sigil for no register
        let register = Register::from_signed(raw_value as i32);
        if register.is_this() {
            None
        } else {
            Some(register)
        }
    }
}

impl Iterator for ExceptionHandlersIterator {
    type Item = ExceptionHandler;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.end {
            None
        } else {
            let view = ExceptionHandler { ptr: self.current, width: self.width };

            let entry_size = match self.width {
                WidthEnum::Narrow => 1,
                WidthEnum::Wide => 2,
                WidthEnum::ExtraWide => 4,
            };
            self.current = unsafe { self.current.add(entry_size * 4) };

            Some(view)
        }
    }
}

impl DebugPrint for HeapPtr<ExceptionHandlers> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        if printer.is_short_mode() {
            printer.write_heap_item_default(self.cast());
            return;
        }

        // Exception handlers are indented
        printer.write("Exception Handlers:\n");
        printer.inc_indent();

        for handler in self.iter() {
            printer.write_indent();
            printer.write(&format!(
                "{}-{} -> {}",
                handler.start(),
                handler.end(),
                handler.handler()
            ));

            if let Some(register) = handler.error_register() {
                printer.write(&format!(" ({})", register.to_string()));
            }

            printer.write("\n");
        }

        printer.dec_indent();
    }
}

impl HeapObject for HeapPtr<ExceptionHandlers> {
    fn byte_size(&self) -> usize {
        ExceptionHandlers::calculate_size_in_bytes(self.handlers.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}
