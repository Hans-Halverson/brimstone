mod js;

use clap::Parser;

use std::error::Error;
use std::rc::Rc;

use js::{
    common::options::{Args, Options},
    runtime::{
        bytecode::generator::BytecodeProgramGenerator, console::to_console_string, Context, Handle,
        Realm, Value,
    },
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

    // Generate bytecode for the program
    let bytecode_program =
        BytecodeProgramGenerator::generate_from_program_parse_result(cx, &parse_result, realm)?;

    if options.print_bytecode {
        println!(
            "{}",
            bytecode_program
                .script_function
                .debug_print_recursive(false)
        );
    }

    // Execute in the bytecode interpreter
    if let Err(err) = cx.run_program(bytecode_program) {
        print_eval_error_and_exit(cx, err);
    }

    Ok(())
}

fn print_error_message_and_exit(message: &str) {
    eprintln!("{}", message);
    std::process::exit(1);
}

fn print_eval_error_and_exit(cx: Context, error: Handle<Value>) {
    let error_string = to_console_string(cx, error);
    print_error_message_and_exit(&error_string);
}

/// Wrapper to pretty print errors
fn main() -> () {
    match main_impl() {
        Ok(_) => (),
        Err(err) => print_error_message_and_exit(&err.to_string()),
    }
}
