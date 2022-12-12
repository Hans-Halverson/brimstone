mod js;

use clap::Parser;

use std::error::Error;
use std::rc::Rc;

#[derive(Parser)]
#[command(about)]
struct Args {
    /// Print the AST the console
    #[arg(long, default_value_t = false)]
    print_ast: bool,

    file: String,
}

fn main_impl() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let source = js::parser::source::Source::new(&args.file)?;
    let ast = js::parser::parse_file(&source)?;

    if args.print_ast {
        println!("{}", js::parser::print_program(&ast, &source));
    }

    let mut cx = js::runtime::Context::new();
    let realm = js::runtime::initialize_host_defined_realm(&mut cx);
    js::runtime::evaluate_script(&mut cx, Rc::new(ast), realm);

    return Ok(());
}

/// Wrapper to pretty print errors
fn main() -> () {
    match main_impl() {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
