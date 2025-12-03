use std::{
    fs::File,
    io::{stderr, stdout, Read, Write},
    mem::forget,
    os::fd::{FromRawFd, RawFd},
    panic,
    process::exit,
    rc::Rc,
};

use brimstone_core::{
    common::wtf_8::Wtf8String,
    handle_scope, must,
    parser::source::Source,
    runtime::{
        abstract_operations::define_property_or_throw, builtin_function::BuiltinFunction,
        error::type_error, function::get_argument, gc_object::GcObject, Context, EvalResult,
        Handle, PropertyDescriptor, PropertyKey, Value,
    },
};

#[link(name = "coverage", kind = "dylib")]
extern "C" {
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
            let reuslt = panic::catch_unwind(|| {
                let cx = Context::default();
                install_fuzzilli_function(cx);
                GcObject::install(cx, cx.initial_realm());

                // Execute test case
                let source = Rc::new(Source::new_for_string("", test_wtf8_string).unwrap());

                cx.execute_then_drop(|mut cx| cx.evaluate_script(source).is_err())
            });

            // Intentionally cause segfault on panic to signal failure to harness
            let is_error = match reuslt {
                Ok(result) => result,
                Err(_) => segfault(),
            };

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

fn install_fuzzilli_function(mut cx: Context) {
    handle_scope!(cx, {
        let realm = cx.initial_realm();

        // Register the rust runtime function
        cx.rust_runtime_functions.register(fuzzilli);

        let fuzzilli_string = cx.alloc_string("fuzzilli");
        let fuzzilli_key = PropertyKey::string(cx, fuzzilli_string.as_string()).to_handle(cx);
        let fuzzilli_function = BuiltinFunction::create(cx, fuzzilli, 2, fuzzilli_key, realm, None);

        let desc = PropertyDescriptor::data(fuzzilli_function.as_value(), true, false, true);
        must!(define_property_or_throw(cx, realm.global_object(), fuzzilli_key, desc))
    });
}

fn fuzzilli(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    if arguments.len() < 2 {
        return type_error(cx, "fuzzilli requires at least one argument");
    }

    let command_arg = get_argument(cx, arguments, 0);
    if !command_arg.is_string() {
        return type_error(cx, "fuzzilli argument must be a string");
    }

    let command = command_arg.as_string();

    match command.to_wtf8_string().as_bytes() {
        b"FUZZILLI_CRASH" => {
            let kind = get_argument(cx, arguments, 1);
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
            let printed = get_argument(cx, arguments, 1);
            if !printed.is_string() {
                return type_error(cx, "fuzzilli print argument must be a string");
            }

            let mut dw_file = unsafe { File::from_raw_fd(REPRL_DWFD) };
            dw_file
                .write_all(printed.as_string().to_wtf8_string().as_bytes())
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
