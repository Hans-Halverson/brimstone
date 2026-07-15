use std::{
    fs::File,
    io::{Read, Write, stderr, stdout},
    mem::forget,
    os::fd::{FromRawFd, RawFd},
    panic,
    process::exit,
    rc::Rc,
};

use brimstone_core::{
    common::{options::OptionsBuilder, wtf_8::Wtf8String},
    handle_scope, must_a,
    parser::source::Source,
    runtime::{
        Arguments, Context, ContextBuilder, EvalResult, Handle, PropertyDescriptor, PropertyFlags,
        PropertyKey, Value, abstract_operations::define_property_or_throw,
        alloc_error::AllocResult, builtin_function::BuiltinFunction, error::type_error,
    },
};

#[link(name = "coverage", kind = "dylib")]
unsafe extern "C" {
    fn __sanitizer_cov_reset_edgeguards();
}

// File descriptors described by fuzzilli
const REPRL_CRFD: RawFd = 100;
const REPRL_CWFD: RawFd = 101;
const REPRL_DRFD: RawFd = 102;
const REPRL_DWFD: RawFd = 103;

fn main() {
    unsafe {
        let mut cr_file = File::from_raw_fd(REPRL_CRFD);
        let mut cw_file = File::from_raw_fd(REPRL_CWFD);
        let mut dr_file = File::from_raw_fd(REPRL_DRFD);

        // Send a handshake to the fuzzer
        cw_file.write("HELO".as_bytes()).unwrap();

        // Wait for a handshake from the fuzzer
        let response = &mut [0u8; 4];
        cr_file.read_exact(response).unwrap();
        assert_eq!(response, b"HELO");

        loop {
            // Signal for start of test case
            let response = &mut [0u8; 4];
            cr_file.read_exact(response).unwrap();
            assert_eq!(response, b"exec");

            // Read test case size
            let response = &mut [0u8; 8];
            cr_file.read_exact(response).unwrap();
            let size = u64::from_le_bytes(*response) as usize;

            // Read test case
            let mut test_case = vec![0u8; size + 1];
            dr_file.read_exact(&mut test_case).unwrap();

            let test_str = str::from_utf8(&test_case[..size]).unwrap();
            let test_wtf8_string = Wtf8String::from_str(test_str);

            // Set up context for test
            let completion_or_panic = panic::catch_unwind(|| {
                let options = Rc::new(OptionsBuilder::new().expose_gc(true).build().unwrap());
                let cx = ContextBuilder::new().set_options(options).build()?;

                cx.initial_realm().install_optional_globals(cx).unwrap();
                install_fuzzilli_function(cx)?;

                // Execute test case
                let source = Rc::new(Source::new_for_string("", test_wtf8_string).unwrap());

                cx.execute_then_drop(|mut cx| cx.evaluate_script(source))
            });

            // Intentionally cause segfault on panic to signal failure to harness
            let completion = match completion_or_panic {
                Ok(completion) => completion,
                Err(_) => segfault(),
            };

            // Gracefully error on abnormal completion or on allocation errors
            let is_error = completion.is_err();

            // Flush output as they may be buffered
            stdout().flush().unwrap();
            stderr().flush().unwrap();

            // Respond with status code
            let status_response: u32 = if is_error { 1 << 8 } else { 0 };
            cw_file.write(&status_response.to_le_bytes()).unwrap();

            // Reset coverage edgeguards for next test case
            __sanitizer_cov_reset_edgeguards();
        }
    }
}

fn install_fuzzilli_function(mut cx: Context) -> AllocResult<()> {
    handle_scope!(cx, {
        let realm = cx.initial_realm();

        // Register the Rust runtime function
        let fuzzili_id = cx.rust_runtime_functions.register(fuzzilli).unwrap();

        let fuzzilli_string = cx.alloc_static_string("fuzzilli")?;
        let fuzzilli_key = PropertyKey::string_handle(cx, fuzzilli_string)?;
        let fuzzilli_function =
            BuiltinFunction::create_custom(cx, fuzzili_id, 2, fuzzilli_key, realm, None)?;

        let desc = PropertyDescriptor::data(
            fuzzilli_function.as_value(),
            PropertyFlags::empty().writable().configurable(),
        );
        let global_object = realm.global_object().as_object();
        must_a!(define_property_or_throw(cx, global_object, fuzzilli_key, desc));

        Ok(())
    })
}

fn fuzzilli(cx: Context, _: Handle<Value>, arguments: Arguments) -> EvalResult<Handle<Value>> {
    if arguments.len() < 2 {
        return type_error(cx, "fuzzilli requires at least one argument");
    }

    let command_arg = arguments.get(cx, 0);
    if !command_arg.is_string() {
        return type_error(cx, "fuzzilli argument must be a string");
    }

    let command = command_arg.as_string();

    match command.to_wtf8_string()?.as_bytes() {
        b"FUZZILLI_CRASH" => {
            let kind = arguments.get(cx, 1);
            if !kind.is_smi() {
                return type_error(cx, "fuzzilli crash argument must be an integer");
            }

            match kind.as_smi() {
                // Intentionally segfault
                0 => segfault(),
                // Intentionally fail assertion
                1 => assert!(false),
                _ => {}
            }
        }
        b"FUZZILLI_PRINT" => {
            let printed = arguments.get(cx, 1);
            if !printed.is_string() {
                return type_error(cx, "fuzzilli print argument must be a string");
            }

            let mut dw_file = unsafe { File::from_raw_fd(REPRL_DWFD) };
            dw_file
                .write_all(printed.as_string().to_wtf8_string()?.as_bytes())
                .unwrap();

            // Flush output but do not close the file
            dw_file.flush().unwrap();
            forget(dw_file);
        }
        _ => {}
    }

    Ok(cx.undefined())
}

fn segfault() -> ! {
    unsafe {
        let ptr = 0x41414141 as *mut u32;
        *ptr = 0x1337;
    }

    exit(-1);
}
