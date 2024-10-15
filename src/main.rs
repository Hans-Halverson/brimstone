mod js;

use clap::Parser;

use std::error::Error;
use std::rc::Rc;

use js::{
    common::{
        error::print_error_message_and_exit,
        options::{Args, Options},
    },
    runtime::{
        bytecode::generator::BytecodeProgramGenerator, error::print_eval_error_and_exit, Context,
        Handle, Realm,
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
            evaluate_file(cx, realm, &args, file)?;
        }

        Ok(())
    })
}

fn evaluate_file(
    mut cx: Context,
    realm: Handle<Realm>,
    args: &Args,
    file: &str,
) -> Result<(), Box<dyn Error>> {
    let source = Rc::new(js::parser::source::Source::new_from_file(file)?);
    let mut parse_result = if args.module {
        js::parser::parse_module(&source, cx.options.as_ref())?
    } else {
        js::parser::parse_script(&source, cx.options.as_ref())?
    };

    js::parser::analyze::analyze(&mut parse_result)?;

    let parse_result = Rc::new(parse_result);

    if args.print_ast {
        println!("{}", js::parser::print_program(&parse_result.program));
    }

    // Generate bytecode for the program
    if args.module {
        let module =
            BytecodeProgramGenerator::generate_from_parse_module_result(cx, &parse_result, realm)?;

        // Load modules and execute in the bytecode interpreter
        if let Err(err) = cx.run_module(module) {
            print_eval_error_and_exit(cx, err);
        }
    } else {
        let bytecode_script =
            BytecodeProgramGenerator::generate_from_parse_script_result(cx, &parse_result, realm)?;

        // Execute in the bytecode interpreter
        if let Err(err) = cx.run_script(bytecode_script) {
            print_eval_error_and_exit(cx, err);
        }
    }

    Ok(())
}

/// Wrapper to pretty print errors
fn main() {
    match main_impl() {
        Ok(_) => (),
        Err(err) => print_error_message_and_exit(&err.to_string()),
    }
}
