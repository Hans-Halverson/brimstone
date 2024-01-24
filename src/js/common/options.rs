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
    #[arg(long, default_value_t = false)]
    pub module: bool,

    /// Expose global gc methods
    #[arg(long, default_value_t = false)]
    pub expose_gc: bool,

    /// Use the bytecode interpreter
    #[arg(long, default_value_t = false)]
    pub bytecode: bool,

    pub file: String,
}

/// Options passed throughout the program.
pub struct Options {
    /// Use the bytecode interpreter
    pub bytecode: bool,
    /// Print the bytecode to the console
    pub print_bytecode: bool,
    /// Print the bytecode for all RegExps to the console
    pub print_regexp_bytecode: bool,
}

impl Options {
    /// Create a new options struct from the command line arguments.
    pub fn new_from_args(args: &Args) -> Self {
        Self {
            bytecode: args.bytecode || args.print_bytecode,
            print_bytecode: args.print_bytecode,
            print_regexp_bytecode: args.print_regexp_bytecode,
        }
    }
}

impl Default for Options {
    /// Create a new options struct with default values.
    fn default() -> Self {
        Self {
            bytecode: false,
            print_bytecode: false,
            print_regexp_bytecode: false,
        }
    }
}
