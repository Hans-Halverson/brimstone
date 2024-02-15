mod js;

use clap::Parser;

use std::error::Error;
use std::rc::Rc;

fn main_impl() -> Result<(), Box<dyn Error>> {
    let args = js::common::options::Args::parse();
    let options = Rc::new(js::common::options::Options::new_from_args(&args));

    let source = Rc::new(js::parser::source::Source::new_from_file(&args.file)?);
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

    let (cx, realm) = js::runtime::Context::new(options.clone(), |cx| {
        // Allocate the realm's built-ins in the permanent heap
        js::runtime::initialize_host_defined_realm(cx, args.expose_gc)
    });

    #[cfg(feature = "gc_stress_test")]
    {
        let mut cx = cx;
        cx.enable_gc_stress_test();
    }

    if options.bytecode {
        use js::runtime::{
            bytecode::{function::Closure, generator::BytecodeProgramGenerator},
            console::to_console_string,
        };

        // Generate bytecode for the program
        let bytecode_program =
            BytecodeProgramGenerator::generate_from_program_parse_result(cx, &parse_result, realm)?;

        if options.print_bytecode {
            println!("{}", bytecode_program.debug_print_recursive(false));
        }

        // Execute in the bytecode interpreter
        cx.execute_then_drop(|mut cx| {
            let global_scope = cx.current_realm().global_scope();
            let closure = Closure::new(cx, bytecode_program, global_scope);
            if let Err(err) = cx.execute_bytecode(closure, &[]) {
                let error_string = to_console_string(cx, err);
                print_error_message_and_exit(&error_string);
            }
        });
    } else {
        // Use the tree walk interpreter
        cx.execute_then_drop(|cx| js::runtime::evaluate(cx, parse_result, realm))?;
    }

    return Ok(());
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
