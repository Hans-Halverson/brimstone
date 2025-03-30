use std::sync::{Mutex, MutexGuard};

use clap::Parser;

use super::{
    constants::DEFAULT_HEAP_SIZE,
    serialized_heap::{get_default_serialized_heap, SerializedHeap},
};

/// Raw command line arguments.
#[derive(Parser)]
#[command(about)]
pub struct Args {
    /// Print the AST the console
    #[arg(long, default_value_t = false)]
    pub print_ast: bool,

    /// Print the bytecode to the console
    #[arg(long, default_value_t = false)]
    pub print_bytecode: bool,

    /// Print the bytecode for all RegExps to the console
    #[arg(long, default_value_t = false)]
    pub print_regexp_bytecode: bool,

    /// Parse as module instead of script
    #[arg(short, long, default_value_t = false)]
    pub module: bool,

    /// Enable Annex B extensions
    #[arg(long, default_value_t = false)]
    pub annex_b: bool,

    /// Expose global gc methods
    #[arg(long, default_value_t = false)]
    pub expose_gc: bool,

    /// Expose the test262 object
    #[arg(long, default_value_t = false)]
    pub expose_test_262: bool,

    /// The starting heap size, in bytes.
    #[arg(long)]
    pub min_heap_size: Option<usize>,

    /// Do not use colors when printing to terminal. Otherwise use colors if supported.
    #[arg(long, default_value_t = false)]
    pub no_color: bool,

    /// Print statistics about the parse phase
    #[arg(long, default_value_t = false)]
    pub parse_stats: bool,

    #[arg(required = true)]
    pub files: Vec<String>,
}

/// Options passed throughout the program.
pub struct Options {
    /// Whether Annex B extensions are enabled
    pub annex_b: bool,

    /// Print each AST to the console
    pub print_ast: bool,

    /// Print the bytecode to the console
    pub print_bytecode: bool,

    /// Print the bytecode for all RegExps to the console
    pub print_regexp_bytecode: bool,

    /// Buffer to write all dumped output into instead of stdout
    pub dump_buffer: Option<Mutex<String>>,

    /// The starting heap size in bytes, if set.
    pub min_heap_size: usize,

    /// Whether to use colors when printing to the terminal
    pub no_color: bool,

    /// Print statistics about the parse phase
    pub parse_stats: bool,

    /// Create the heap from this SerializedHeap if set, otherwise create heap from scratch.
    pub serialized_heap: Option<&'static SerializedHeap<'static>>,
}

impl Options {
    /// Create a new options struct from command line arguments.
    pub fn new_from_args(args: &Args) -> Self {
        OptionsBuilder::new_from_args(args).build()
    }

    pub fn dump_buffer(&self) -> Option<MutexGuard<'_, String>> {
        self.dump_buffer
            .as_ref()
            .map(|buffer| buffer.lock().unwrap())
    }
}

impl Default for Options {
    /// Create a new options struct with default values.
    fn default() -> Self {
        OptionsBuilder::new().build()
    }
}

pub struct OptionsBuilder(Options);

impl OptionsBuilder {
    /// Create new options with default values.
    pub fn new() -> Self {
        Self(Options {
            annex_b: false,
            print_ast: false,
            print_bytecode: false,
            print_regexp_bytecode: false,
            dump_buffer: None,
            min_heap_size: DEFAULT_HEAP_SIZE,
            no_color: false,
            parse_stats: false,
            serialized_heap: get_default_serialized_heap(),
        })
    }

    /// Create new options from command line arguments.
    pub fn new_from_args(args: &Args) -> Self {
        OptionsBuilder::new()
            .annex_b(args.annex_b)
            .print_ast(args.print_ast)
            .print_bytecode(args.print_bytecode)
            .print_regexp_bytecode(args.print_regexp_bytecode)
            .min_heap_size(args.min_heap_size.unwrap_or(DEFAULT_HEAP_SIZE))
            .no_color(args.no_color)
            .parse_stats(args.parse_stats)
    }

    /// Return the options that have been built, consuming the builder.
    pub fn build(self) -> Options {
        self.0
    }

    pub fn annex_b(mut self, annex_b: bool) -> Self {
        self.0.annex_b = annex_b;
        self
    }

    pub fn print_ast(mut self, print_ast: bool) -> Self {
        self.0.print_ast = print_ast;
        self
    }

    pub fn print_bytecode(mut self, print_bytecode: bool) -> Self {
        self.0.print_bytecode = print_bytecode;
        self
    }

    pub fn print_regexp_bytecode(mut self, print_regexp_bytecode: bool) -> Self {
        self.0.print_regexp_bytecode = print_regexp_bytecode;
        self
    }

    pub fn min_heap_size(mut self, min_heap_size: usize) -> Self {
        self.0.min_heap_size = min_heap_size;
        self
    }

    pub fn dump_buffer(mut self, dump_buffer: Option<Mutex<String>>) -> Self {
        self.0.dump_buffer = dump_buffer;
        self
    }

    pub fn no_color(mut self, no_color: bool) -> Self {
        self.0.no_color = no_color;
        self
    }

    pub fn parse_stats(mut self, parse_stats: bool) -> Self {
        self.0.parse_stats = parse_stats;
        self
    }

    pub fn serialized_heap(
        mut self,
        serialized_heap: Option<&'static SerializedHeap<'static>>,
    ) -> Self {
        self.0.serialized_heap = serialized_heap;
        self
    }
}
