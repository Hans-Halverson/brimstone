use clap::Parser;

use std::rc::Rc;

use brimstone_core::{
    common::{
        error::{print_error_message_and_exit, FormatOptions},
        options::{Args, Options},
        terminal::stderr_should_use_colors,
    },
    parser::source::Source,
    runtime::{
        gc_object::GcObject, test_262_object::Test262Object, BsResult, Context, ContextBuilder,
    },
};

fn create_context(args: &Args) -> Context {
    let options = Rc::new(Options::new_from_args(args));

    let cx = ContextBuilder::new().set_options(options).build();

    if args.expose_gc {
        GcObject::install(cx, cx.initial_realm());
    }

    if args.expose_test_262 {
        Test262Object::install(cx, cx.initial_realm());
    }

    #[cfg(feature = "gc_stress_test")]
    {
        let mut cx = cx;
        cx.enable_gc_stress_test();
    }

    cx
}

fn evaluate(mut cx: Context, args: &Args) -> BsResult<()> {
    for file in &args.files {
        let source = Rc::new(Source::new_from_file(file)?);

        if args.module {
            cx.evaluate_module(source)?;
        } else {
            cx.evaluate_script(source)?;
        }
    }

    Ok(())
}

/// Wrapper to pretty print errors
fn main() {
    let args = Args::parse();
    let cx = create_context(&args);

    cx.execute_then_drop(|cx| {
        let result = evaluate(cx, &args);

        #[cfg(feature = "handle_stats")]
        println!("{:?}", cx.heap.info().handle_context().handle_stats());

        match result {
            Ok(_) => (),
            Err(err) => {
                let supports_color = stderr_should_use_colors(&cx.options);
                let format_options = FormatOptions::new(supports_color);

                print_error_message_and_exit(&err.format(cx, &format_options));
            }
        }
    })
}
