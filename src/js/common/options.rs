use std::sync::{Mutex, MutexGuard};

use clap::Parser;

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
    pub expose_test262: bool,

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
}

impl Options {
    /// Create a new options struct from the command line arguments.
    pub fn new_from_args(args: &Args) -> Self {
        Self {
            annex_b: args.annex_b,
            print_ast: args.print_ast,
            print_bytecode: args.print_bytecode,
            print_regexp_bytecode: args.print_regexp_bytecode,
            dump_buffer: None,
        }
    }

    #[allow(unused)]
    pub fn dump_buffer(&self) -> Option<MutexGuard<'_, String>> {
        self.dump_buffer
            .as_ref()
            .map(|buffer| buffer.lock().unwrap())
    }
}

impl Default for Options {
    /// Create a new options struct with default values.
    fn default() -> Self {
        Self {
            annex_b: false,
            print_ast: false,
            print_bytecode: false,
            print_regexp_bytecode: false,
            dump_buffer: None,
        }
    }
}
