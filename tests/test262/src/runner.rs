use serde_json::{self, json};
use threadpool::ThreadPool;

use std::{
    fs,
    panic::{self, AssertUnwindSafe},
    path::Path,
    rc::Rc,
    sync::mpsc::channel,
    time::{Duration, SystemTime},
};

use crate::{
    ignored::IgnoredIndex,
    index::{ExpectedResult, Test, TestIndex, TestMode, TestPhase},
    utils::GenericResult,
};

use brimstone::js::{
    self,
    common::{options::Options, wtf_8::Wtf8String},
    runtime::{
        bytecode::generator::BytecodeProgramGenerator, get, initialize_host_defined_realm,
        module::execute::ExecuteOnReject, object_value::ObjectValue,
        test_262_object::Test262Object, to_console_string, to_string, Context, EvalResult, Handle,
        Realm, Value,
    },
};

pub struct TestRunner {
    index: TestIndex,
    ignored: IgnoredIndex,
    thread_pool: ThreadPool,
    filter: Option<String>,
    feature: Option<String>,
}

// Runner threads have an 8MB stack
const RUNNER_THREAD_STACK_SIZE: usize = 1 << 23;

impl TestRunner {
    pub fn new(
        index: TestIndex,
        ignored: IgnoredIndex,
        num_threads: u8,
        filter: Option<String>,
        feature: Option<String>,
    ) -> TestRunner {
        let thread_pool = threadpool::Builder::new()
            .num_threads(num_threads.into())
            .thread_stack_size(RUNNER_THREAD_STACK_SIZE)
            .build();
        TestRunner { index, ignored, thread_pool, filter, feature }
    }

    pub fn run(&mut self, verbose: bool) -> TestResults {
        let (sender, receiver) = channel::<TestResult>();
        let mut num_jobs = 0;
        let mut num_skipped = 0;
        let mut ignored_failures = vec![];

        let all_tests_start_timestamp = SystemTime::now();

        for (i, test) in self.index.tests.values().enumerate() {
            // If a filter was provided then skip all tests that do not match the filter
            if let Some(filter) = &self.filter {
                if !test.path.contains(filter) {
                    num_skipped += 1;
                    continue;
                }
            }

            // If a feature was specified then skip all tests that do not have that feature
            if let Some(feature) = &self.feature {
                if !test.features.contains(feature) {
                    num_skipped += 1;
                    continue;
                }
            }

            if self.ignored.should_fail(test) {
                ignored_failures.push(TestResult::failure(
                    test,
                    "Ignored test counted as failure".to_owned(),
                    Duration::ZERO,
                ));
                continue;
            } else if self.ignored.should_ignore(test) {
                num_skipped += 1;
                continue;
            }

            let test262_root = self.index.test262_root.clone();

            let test = test.clone();
            let sender = sender.clone();
            num_jobs += 1;

            self.thread_pool.execute(move || {
                let start_timestamp = SystemTime::now();

                let panic_result = panic::catch_unwind(|| {
                    if verbose {
                        println!("{i}: {}", test.path);
                    }

                    run_full_test(&test, &test262_root, start_timestamp)
                });

                let duration = start_timestamp.elapsed().unwrap();

                match panic_result {
                    Ok(result) => sender.send(result).unwrap(),
                    Err(err) => {
                        // Attempt to extract string message from panic
                        let message = err.downcast_ref::<&str>();
                        let message = match message {
                            Some(message) => message.to_string(),
                            None => String::from("<panic message not found>"),
                        };

                        let result = TestResult::failure(
                            &test,
                            format!("Thread panicked:\n{}", message),
                            duration,
                        );

                        sender.send(result).unwrap()
                    }
                }
            });
        }

        let mut results: Vec<TestResult> = receiver.iter().take(num_jobs).collect();

        if !ignored_failures.is_empty() {
            results.extend(ignored_failures);
        }

        TestResults::collate(results, num_skipped, all_tests_start_timestamp.elapsed().unwrap())
    }
}

fn run_full_test(test: &Test, test262_root: &str, start_timestamp: SystemTime) -> TestResult {
    match test.mode {
        TestMode::StrictScript => run_single_test(test, test262_root, true, start_timestamp),
        TestMode::NonStrictScript | TestMode::Module | TestMode::Raw => {
            run_single_test(test, test262_root, false, start_timestamp)
        }
        // Run in both strict and non strict mode, both must pass for this test to be successful
        TestMode::Script => {
            let non_strict_result = run_single_test(test, test262_root, false, start_timestamp);
            if let TestResultCompletion::Success = non_strict_result.result {
                run_single_test(test, test262_root, true, start_timestamp)
            } else {
                non_strict_result
            }
        }
    }
}

fn run_single_test(
    test: &Test,
    test262_root: &str,
    force_strict_mode: bool,
    start_timestamp: SystemTime,
) -> TestResult {
    // Set up options
    let options = Rc::new(Options::default());

    // Each test is executed in its own realm
    let (cx, realm) = Context::new(options, |cx| {
        // Allocate the realm's built-ins in the permanent heap
        initialize_host_defined_realm(
            cx, /* expose_gc */ false, /* expose_test262 */ true,
        )
    });

    // Wrap in catch_unwind so that we can clean up the context in the event of a panic
    let panic_result = panic::catch_unwind(AssertUnwindSafe(|| {
        // Add $262 object to the realm's global object
        let test_262_object = Test262Object::new(cx, realm);
        Test262Object::install(cx, realm, test_262_object);

        #[cfg(feature = "gc_stress_test")]
        {
            let mut cx = cx;
            cx.enable_gc_stress_test();
        }

        // Default harness files are loaded unless running in raw mode
        if test.mode != TestMode::Raw {
            load_harness_test_file(cx, realm, test262_root, "assert.js");
            load_harness_test_file(cx, realm, test262_root, "sta.js");
        }

        // Load all other specified harness files
        for include in &test.includes {
            load_harness_test_file(cx, realm, test262_root, include);
        }

        // Try to parse file
        let parse_result = parse_file(&test.path, Some(test), test262_root, force_strict_mode);
        let mut ast = match parse_result {
            Ok(ast) => ast,
            // An error during parse may be a success or failure depending on the expected result of
            // the test.
            Err(err) => {
                // Do not count IO errors as negative tests results
                let is_parse_error = match err.error {
                    js::parser::ParseError::Io(_) => false,
                    _ => true,
                };

                let duration = start_timestamp.elapsed().unwrap();

                return match test.expected_result {
                    ExpectedResult::Negative { phase: TestPhase::Parse, .. } if is_parse_error => {
                        TestResult::success(test, duration)
                    }
                    _ => TestResult::failure(
                        test,
                        format!("Unexpected error during parsing:\n{}", err.to_string()),
                        duration,
                    ),
                };
            }
        };

        // Perform static analysis on file
        let analyze_result = js::parser::analyze::analyze(&mut ast);
        match analyze_result {
            Ok(_) => {}
            // An error during analysis may be a success or failure depending on the expected result
            // of the test.
            Err(err) => {
                let duration = start_timestamp.elapsed().unwrap();

                return match test.expected_result {
                    ExpectedResult::Negative { phase: TestPhase::Parse, .. } => {
                        TestResult::success(test, duration)
                    }
                    _ => TestResult::failure(
                        test,
                        format!("Unexpected error during analysis:\n{}", err.to_string()),
                        duration,
                    ),
                };
            }
        }

        let completion = if test.mode == TestMode::Module {
            execute_module_as_bytecode(cx, realm, &ast)
        } else {
            execute_script_as_bytecode(cx, realm, &ast)
        };

        let duration = start_timestamp.elapsed().unwrap();

        let global_object = realm.global_object();
        check_expected_completion(cx, test, completion, global_object, duration)
    }));

    cx.drop();

    match panic_result {
        Ok(test_result) => test_result,
        Err(err) => panic::resume_unwind(err),
    }
}

fn parse_file(
    file: &str,
    test: Option<&Test>,
    test262_root: &str,
    force_strict_mode: bool,
) -> js::parser::ParseResult<js::parser::parser::ParseProgramResult> {
    let full_path = Path::new(test262_root).join("test").join(file);

    let mut source = js::parser::source::Source::new_from_file(full_path.to_str().unwrap())?;

    if let Some(Test { mode: TestMode::Module, .. }) = test {
        let source = Rc::new(source);
        let parse_result = js::parser::parse_module(&source)?;

        Ok(parse_result)
    } else {
        // Manually insert use strict directive when forcing strict mode
        if force_strict_mode {
            let mut source_with_directive = Wtf8String::from_str("\"use strict\";\n");
            source_with_directive.push_wtf8_str(&source.contents);

            source.contents = source_with_directive;
        }

        let source = Rc::new(source);
        let parse_result = js::parser::parse_script(&source)?;

        Ok(parse_result)
    }
}

fn load_harness_test_file(cx: Context, realm: Handle<Realm>, test262_root: &str, file: &str) {
    let full_path = Path::new(test262_root).join("harness").join(file);

    let mut ast = parse_file(full_path.to_str().unwrap(), None, test262_root, false)
        .expect(&format!("Failed to parse test harness file {}", full_path.display()));

    js::parser::analyze::analyze(&mut ast)
        .expect(&format!("Failed to parse test harness file {}", full_path.display()));

    let eval_result = execute_script_as_bytecode(cx, realm, &ast);

    if let EvalResult::Throw(_) = eval_result {
        panic!("Failed to evaluate test harness file {}", full_path.display())
    }
}

fn execute_script_as_bytecode(
    mut cx: Context,
    realm: Handle<Realm>,
    parse_result: &js::parser::parser::ParseProgramResult,
) -> EvalResult<()> {
    let generate_result =
        BytecodeProgramGenerator::generate_from_parse_script_result(cx, parse_result, realm);
    let bytecode_script = match generate_result {
        Ok(bytecode_script) => bytecode_script,
        Err(err) => {
            let err_string = cx.alloc_string(&err.to_string());
            return EvalResult::Throw(err_string.into());
        }
    };

    match cx.run_script(bytecode_script) {
        Ok(_) => EvalResult::Ok(()),
        Err(error_value) => EvalResult::Throw(error_value),
    }
}

fn execute_module_as_bytecode(
    mut cx: Context,
    realm: Handle<Realm>,
    parse_result: &js::parser::parser::ParseProgramResult,
) -> EvalResult<()> {
    let generate_result =
        BytecodeProgramGenerator::generate_from_parse_module_result(cx, parse_result, realm);
    let module = match generate_result {
        Ok(module) => module,
        Err(err) => {
            let err_string = cx.alloc_string(&err.to_string());
            return EvalResult::Throw(err_string.into());
        }
    };

    // Panic on rejection, which will be caught by the test runner
    match cx.run_module(module, ExecuteOnReject::Panic) {
        Ok(_) => EvalResult::Ok(()),
        Err(error_value) => EvalResult::Throw(error_value),
    }
}

/// Determine whether the evaluation completion for a test corresponds to that test passing or
/// failing, depending on the expected result of the test.
fn check_expected_completion(
    cx: Context,
    test: &Test,
    completion: EvalResult<()>,
    global_object: Handle<ObjectValue>,
    duration: Duration,
) -> TestResult {
    match completion {
        // A normal completion is a success only if the test was expected to not throw
        EvalResult::Ok(_) => match &test.expected_result {
            ExpectedResult::Positive => {
                // Async tests determine success by looking at the output logged to print()
                if test.is_async {
                    // Get the print log stored on the global object
                    let print_log = Test262Object::get_print_log(cx, global_object);
                    let print_log = match print_log {
                        EvalResult::Ok(log) => log.to_string(),
                        EvalResult::Throw(_) => {
                            return TestResult::failure(
                                test,
                                "failed to access print log".to_owned(),
                                duration,
                            );
                        }
                    };

                    // Fail if the print log contains the fail sequence
                    if print_log.contains("Test262:AsyncTestFailure:") {
                        return TestResult::failure(
                            test,
                            format!("Async test failed with the error: {}", print_log),
                            duration,
                        );
                    }

                    // Only succeed if the print log containst he success sequence
                    if !print_log.contains("Test262:AsyncTestComplete") {
                        return TestResult::failure(
                            test,
                            format!("print log does not contain Test262:AsyncTestComplete, instead contains:\n{}", print_log),
                            duration,
                        );
                    }
                }

                TestResult::success(test, duration)
            }
            other => TestResult::failure(
                test,
                format!("Test completed without throwing, but expected {}", other.to_string()),
                duration,
            ),
        },
        // Throw completions are a success if the expected result is negative, expected during
        // during runtime, and with the same expected error.
        EvalResult::Throw(thrown_value) => match &test.expected_result {
            ExpectedResult::Negative { phase: TestPhase::Runtime, type_ } => {
                // Check that the thrown error matches the expected error type
                let is_expected_error = if thrown_value.is_object() {
                    let thrown_object = thrown_value.as_object();
                    if thrown_object.is_error() {
                        // Check if thrown error type has the same name as the expected error
                        match get(cx, thrown_object, cx.names.name()) {
                            EvalResult::Ok(name_value) if name_value.is_string() => {
                                &name_value.as_string().to_string() == type_
                            }
                            _ => false,
                        }
                    } else if type_ == "Test262Error" {
                        // The Test262Error does not extend the Error type or have a name property,
                        // so check the if the message starts with "Test262Error".
                        match to_string(cx, thrown_value) {
                            EvalResult::Ok(message_value) => {
                                message_value.to_string().starts_with("Test262Error")
                            }
                            _ => false,
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if is_expected_error {
                    TestResult::success(test, duration)
                } else {
                    let thrown_string = to_console_string_test262(cx, thrown_value);
                    TestResult::failure(
                        test,
                        format!(
                            "Test threw the following error during runtime, but expected {}:\n{}",
                            test.expected_result.to_string(),
                            thrown_string,
                        ),
                        duration,
                    )
                }
            }
            other => {
                let thrown_string = to_console_string_test262(cx, thrown_value);
                TestResult::failure(
                    test,
                    format!(
                        "Test threw the following error during runtime, but expected {}:\n{}",
                        other.to_string(),
                        thrown_string,
                    ),
                    duration,
                )
            }
        },
    }
}

fn to_console_string_test262(cx: Context, value: Handle<Value>) -> String {
    // Extract message for Test262Error, otherwise print to console normally
    if value.is_object() {
        match to_string(cx, value) {
            EvalResult::Ok(message_value) => {
                let message = message_value.to_string();
                if message.starts_with("Test262Error") {
                    return String::from(message);
                }
            }
            _ => {}
        }
    }

    to_console_string(cx, value)
}

struct TestResult {
    path: String,
    result: TestResultCompletion,
    // Total time this test took to run
    time: Duration,
}

#[derive(PartialEq)]
enum TestResultCompletion {
    Success,
    Failure(String),
    #[allow(dead_code)]
    Skipped,
}

impl TestResult {
    fn success(test: &Test, time: Duration) -> TestResult {
        TestResult {
            path: test.path.clone(),
            result: TestResultCompletion::Success,
            time,
        }
    }

    fn failure(test: &Test, message: String, time: Duration) -> TestResult {
        TestResult {
            path: test.path.clone(),
            result: TestResultCompletion::Failure(message),
            time,
        }
    }

    #[allow(dead_code)]
    fn skipped(test: &Test) -> TestResult {
        TestResult {
            path: test.path.clone(),
            result: TestResultCompletion::Skipped,
            time: Duration::ZERO,
        }
    }
}

pub struct TestResults {
    succeeded: Vec<TestResult>,
    failed: Vec<TestResult>,
    num_skipped: u64,
    // Total duration of the entire test run
    total_duration: Duration,
}

// ANSII codes for pretty printing
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const WHITE: &str = "\x1b[37m";
const RESET: &str = "\x1b[0m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";

impl TestResults {
    fn collate(
        results: Vec<TestResult>,
        num_skipped: u64,
        total_duration: Duration,
    ) -> TestResults {
        let mut collated = TestResults {
            failed: vec![],
            succeeded: vec![],
            num_skipped,
            total_duration,
        };

        for result in results {
            match result.result {
                TestResultCompletion::Success => {
                    collated.succeeded.push(result);
                }
                TestResultCompletion::Failure(_) => {
                    collated.failed.push(result);
                }
                TestResultCompletion::Skipped => {
                    collated.num_skipped += 1;
                }
            }
        }

        collated.failed.sort_by(|a, b| a.path.cmp(&b.path));
        collated.succeeded.sort_by(|a, b| a.path.cmp(&b.path));

        collated
    }

    pub fn is_successful(&self) -> bool {
        self.failed.is_empty()
    }

    pub fn print_to_console(&self, test262_root: &Path) {
        let test262_prefix = test262_root.join("test");

        for failed in &self.failed {
            if let TestResultCompletion::Failure(message) = &failed.result {
                // Strip file path from message to reduce length
                let file_path = test262_prefix.join(&failed.path);
                let cleaned_message = message.replace(file_path.to_str().unwrap(), "<file>");

                println!("{}{}Failed{}: {}\n{}\n", BOLD, RED, RESET, failed.path, cleaned_message);
            }
        }

        println!(
            "{}Tests completed in {:.2} seconds\n\n{}{}Succeeded: {}\n{}Failed: {}\n{}{}Skipped: {}{}",
            BOLD,
            self.total_duration.as_secs_f64(),
            BOLD,
            GREEN,
            self.succeeded.len(),
            RED,
            self.failed.len(),
            WHITE,
            DIM,
            self.num_skipped,
            RESET
        );
    }

    pub fn print_test262_progress(&self) {
        let succeeded = self.succeeded.len();
        let failed = self.failed.len();
        let total = succeeded + failed;
        let percent = (succeeded as f64 / total as f64) * 100.0;

        println!("\n{}Test262 progress: {:.2}%{}", BOLD, percent, RESET);
        println!("{}/{} tests passed", succeeded, total);
    }

    pub fn save_to_result_files(&self, result_files_path: String) -> GenericResult {
        let succeeded_paths: Vec<&String> =
            self.succeeded.iter().map(|result| &result.path).collect();
        let failed_paths: Vec<&String> = self.failed.iter().map(|result| &result.path).collect();

        let succeeded_string = serde_json::to_string_pretty(&succeeded_paths).unwrap();
        let failed_string = serde_json::to_string_pretty(&failed_paths).unwrap();

        let succeeded_file_path = format!("{}_success.json", result_files_path);
        let failed_file_path = format!("{}_failed.json", result_files_path);

        fs::write(succeeded_file_path, &succeeded_string)?;
        fs::write(failed_file_path, &failed_string)?;

        Ok(())
    }

    pub fn save_to_time_file(&self, tile_file_path: String) -> GenericResult {
        let mut all_test_results = vec![];
        all_test_results.extend(self.succeeded.iter());
        all_test_results.extend(self.failed.iter());

        all_test_results.sort_by(|a, b| a.time.cmp(&b.time).reverse());

        let mut test_result_jsons = vec![];
        for test_result in all_test_results {
            let test_result_json = json!({
                "path": test_result.path,
                "time": test_result.time.as_secs_f64(),
                "succeeded": test_result.result == TestResultCompletion::Success,
            });

            test_result_jsons.push(test_result_json);
        }

        let time_file_string = serde_json::to_string_pretty(&test_result_jsons).unwrap();

        fs::write(tile_file_path, &time_file_string)?;

        Ok(())
    }
}
