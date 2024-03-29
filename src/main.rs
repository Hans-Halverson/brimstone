mod js;

use clap::Parser;

use std::error::Error;
use std::rc::Rc;

use js::{
    common::options::{Args, Options},
    runtime::{Context, Handle, Realm},
};

fn main_impl() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let options = Rc::new(Options::new_from_args(&args));

    let (cx, realm) = Context::new(options.clone(), |cx| {
        // Allocate the realm's built-ins in the permanent heap
        js::runtime::initialize_host_defined_realm(cx, args.expose_gc, args.expose_test262)
    });

    #[cfg(feature = "gc_stress_test")]
    {
        let mut cx = cx;
        cx.enable_gc_stress_test();
    }

    cx.execute_then_drop(|cx| {
        for file in &args.files {
            evaluate_file(cx, realm, &args, options.clone(), file)?;
        }

        Ok(())
    })
}

fn evaluate_file(
    mut cx: Context,
    realm: Handle<Realm>,
    args: &Args,
    options: Rc<Options>,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    let source = Rc::new(js::parser::source::Source::new_from_file(file)?);
    let mut parse_result = if args.module {
        js::parser::parse_module(&source)?
    } else {
        js::parser::parse_script(&source)?
    };

    js::parser::analyze::analyze(&mut parse_result, source.clone())?;

    let parse_result = Rc::new(parse_result);

    if args.print_ast {
        println!("{}", js::parser::print_program(&parse_result.program, &source));
    }

    if options.bytecode {
        use js::runtime::{
            bytecode::generator::BytecodeProgramGenerator, console::to_console_string,
        };

        // Generate bytecode for the program
        let program_closure =
            BytecodeProgramGenerator::generate_from_program_parse_result(cx, &parse_result, realm)?;

        if options.print_bytecode {
            println!("{}", program_closure.function().debug_print_recursive(false));
        }

        // Execute in the bytecode interpreter
        if let Err(err) = cx.execute_bytecode(program_closure, &[]) {
            let error_string = to_console_string(cx, err);
            print_error_message_and_exit(&error_string);
        }
    } else {
        // Use the tree walk interpreter
        js::runtime::evaluate(cx, parse_result, realm)?;
    }

    Ok(())
}

fn print_error_message_and_exit(message: &str) {
    eprintln!("{}", message);
    std::process::exit(1);
}

/// Wrapper to pretty print errors
fn main() -> () {
    match main_impl() {
        Ok(_) => (),
        Err(err) => print_error_message_and_exit(&err.to_string()),
    }
}
