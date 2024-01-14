use super::{
    bytecode::{
        constant_table::ConstantTable, exception_handlers::ExceptionHandlers,
        function::BytecodeFunction,
    },
    gc::HeapItem,
    object_descriptor::ObjectKind,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue},
    HeapPtr,
};

#[derive(Clone, Copy, PartialEq)]
pub enum DebugPrintMode {
    /// Print value's type and basic context, all on the same line.
    Short,
    /// Print additional context about the value, which may include recursive members.
    Verbose,
}

/// Trait for debug printing values and heap items.
pub trait DebugPrint {
    /// Add the debug representation of this value to the printer.
    fn debug_format(&self, printer: &mut DebugPrinter);

    /// Return the debug representation of this value.
    fn debug_print(&self, mode: DebugPrintMode) -> String {
        let mut printer = DebugPrinter::new(mode);
        self.debug_format(&mut printer);
        printer.finish()
    }
}

pub struct DebugPrinter {
    result: String,
    current_indent: usize,
    /// Stack of print modes, top of stack is the current mode.
    mode_stack: Vec<DebugPrintMode>,
    /// Whether to ignore raw bytes when printing bytecode
    ignore_raw_bytes: bool,
}

impl DebugPrinter {
    pub fn new(mode: DebugPrintMode) -> Self {
        Self {
            mode_stack: vec![mode],
            result: String::new(),
            current_indent: 0,
            ignore_raw_bytes: false,
        }
    }

    pub fn finish(self) -> String {
        self.result
    }

    pub fn is_empty(&self) -> bool {
        self.result.is_empty()
    }

    pub fn push_mode(&mut self, mode: DebugPrintMode) {
        self.mode_stack.push(mode);
    }

    pub fn pop_mode(&mut self) {
        self.mode_stack.pop();
    }

    pub fn is_short_mode(&self) -> bool {
        self.mode_stack.last() == Some(&DebugPrintMode::Short)
    }

    pub fn set_ignore_raw_bytes(&mut self, ignore: bool) {
        self.ignore_raw_bytes = ignore;
    }

    pub fn ignore_raw_bytes(&self) -> bool {
        self.ignore_raw_bytes
    }

    pub fn inc_indent(&mut self) {
        self.current_indent += 1;
    }

    pub fn dec_indent(&mut self) {
        self.current_indent -= 1;
    }

    pub fn write_indent(&mut self) {
        self.result.push_str(&"  ".repeat(self.current_indent));
    }

    pub fn write(&mut self, s: &str) {
        self.result.push_str(s);
    }

    pub fn write_default(&mut self, name: &str) {
        self.write(&format!("[{}]", name));
    }

    pub fn write_default_with_context(&mut self, name: &str, context: &str) {
        self.write(&format!("[{}: {}]", name, context));
    }

    pub fn write_heap_item_default(&mut self, heap_item: HeapPtr<HeapItem>) {
        self.write_default(&format!("{:?}", heap_item.descriptor().kind()))
    }

    pub fn write_heap_item_with_context(&mut self, heap_item: HeapPtr<HeapItem>, context: &str) {
        self.write_default_with_context(&format!("{:?}", heap_item.descriptor().kind()), context)
    }
}

impl DebugPrint for HeapPtr<HeapItem> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        match self.descriptor().kind() {
            ObjectKind::String => self.cast::<StringValue>().debug_format(printer),
            ObjectKind::Symbol => self.cast::<SymbolValue>().debug_format(printer),
            ObjectKind::BigInt => self.cast::<BigIntValue>().debug_format(printer),
            ObjectKind::BytecodeFunction => self.cast::<BytecodeFunction>().debug_format(printer),
            ObjectKind::ConstantTable => self.cast::<ConstantTable>().debug_format(printer),
            ObjectKind::ExceptionHandlers => self.cast::<ExceptionHandlers>().debug_format(printer),
            _ => printer.write_heap_item_default(*self),
        }
    }
}
