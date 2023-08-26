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

    /// Parse as module instead of script
    #[arg(long, default_value_t = false)]
    module: bool,

    /// Expose global gc methods
    #[arg(long, default_value_t = false)]
    expose_gc: bool,

    file: String,
}

fn main_impl() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let source = Rc::new(js::parser::source::Source::new_from_file(&args.file)?);
    let mut ast = if args.module {
        js::parser::parse_module(&source)?
    } else {
        js::parser::parse_script(&source)?
    };

    js::parser::analyze::analyze(&mut ast, source.clone())?;

    let ast = Rc::new(ast);

    if args.print_ast {
        println!("{}", js::parser::print_program(&ast, &source));
    }

    let (cx, realm) = js::runtime::Context::new(|cx| {
        // Allocate the realm's built-ins in the permanent heap
        js::runtime::initialize_host_defined_realm(cx, args.expose_gc)
    });

    #[cfg(feature = "gc_stress_test")]
    {
        let mut cx = cx;
        cx.enable_gc_stress_test();
    }

    cx.execute_then_drop(|cx| js::runtime::evaluate(cx, ast, realm))?;

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
