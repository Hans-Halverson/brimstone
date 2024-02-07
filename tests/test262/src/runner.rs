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
        bytecode::{function::Closure, generator::BytecodeProgramGenerator},
        eval_module, eval_script, get, initialize_host_defined_realm,
        test_262_object::Test262Object,
        to_console_string, to_string, Completion, CompletionKind, Context, EvalResult, Handle,
        Realm, Value,
    },
};

pub struct TestRunner {
    index: TestIndex,
    ignored: IgnoredIndex,
    thread_pool: ThreadPool,
    filter: Option<String>,
    feature: Option<String>,
    bytecode: bool,
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
        bytecode: bool,
    ) -> TestRunner {
        let thread_pool = threadpool::Builder::new()
            .num_threads(num_threads.into())
            .thread_stack_size(RUNNER_THREAD_STACK_SIZE)
            .build();
        TestRunner { index, ignored, thread_pool, filter, feature, bytecode }
    }

    pub fn run(&mut self, verbose: bool) -> TestResults {
        let (sender, receiver) = channel::<TestResult>();
        let mut num_jobs = 0;
        let mut num_skipped = 0;
        let bytecode = self.bytecode;

        let all_tests_start_timestamp = SystemTime::now();

        for (i, test) in self.index.tests.values().enumerate() {
            if !self.should_run_test(test) {
                num_skipped += 1;
                continue;
            }

            let test262_root = self.index.test262_root.clone();
            let ignore_async_generator = self.ignored.ignore_async_generator();

            let test = test.clone();
            let sender = sender.clone();
            num_jobs += 1;

            self.thread_pool.execute(move || {
                let start_timestamp = SystemTime::now();

                let panic_result = panic::catch_unwind(|| {
                    if verbose {
                        println!("{i}: {}", test.path);
                    }

                    run_full_test(&test, &test262_root, start_timestamp, bytecode)
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

                        let result = if ignore_async_generator
                            && (message == "not implemented: async and generator functions"
                             || message == "not implemented: bytecode for async and generator functions")
                        {
                            TestResult::skipped(&test)
                        } else {
                            TestResult::failure(
                                &test,
                                format!("Thread panicked:\n{}", message),
                                duration,
                            )
                        };

                        sender.send(result).unwrap()
                    }
                }
            });
        }

        let results: Vec<TestResult> = receiver.iter().take(num_jobs).collect();

        TestResults::collate(results, num_skipped, all_tests_start_timestamp.elapsed().unwrap())
    }

    fn should_run_test(&self, test: &Test) -> bool {
        if let Some(filter) = &self.filter {
            if !test.path.contains(filter) {
                return false;
            }
        }

        if let Some(feature) = &self.feature {
            if !test.features.contains(feature) {
                return false;
            }
        }

        !self.ignored.should_ignore(test)
    }
}

fn run_full_test(
    test: &Test,
    test262_root: &str,
    start_timestamp: SystemTime,
    bytecode: bool,
) -> TestResult {
    match test.mode {
        TestMode::StrictScript => {
            run_single_test(test, test262_root, true, start_timestamp, bytecode)
        }
        TestMode::NonStrictScript | TestMode::Module | TestMode::Raw => {
            run_single_test(test, test262_root, false, start_timestamp, bytecode)
        }
        // Run in both strict and non strict mode, both must pass for this test to be successful
        TestMode::Script => {
            let non_strict_result =
                run_single_test(test, test262_root, false, start_timestamp, bytecode);
            if let TestResultCompletion::Success = non_strict_result.result {
                run_single_test(test, test262_root, true, start_timestamp, bytecode)
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
    bytecode: bool,
) -> TestResult {
    // Set up options
    let options = {
        let mut options = Options::default();
        options.bytecode = bytecode;
        Rc::new(options)
    };

    // Each test is executed in its own realm
    let (cx, realm) = Context::new(options, |cx| {
        // Allocate the realm's built-ins in the permanent heap
        initialize_host_defined_realm(cx, false)
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
        let mut ast_and_source = match parse_result {
            Ok(ast_and_source) => ast_and_source,
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
        let analyze_result = js::parser::analyze::analyze(&mut ast_and_source.0, ast_and_source.1);
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

        let completion = if bytecode {
            if test.mode == TestMode::Module {
                unimplemented!("module evaluation")
            } else {
                execute_as_bytecode(cx, &ast_and_source.0)
            }
        } else {
            if test.mode == TestMode::Module {
                eval_module(cx, Rc::new(ast_and_source.0), realm)
            } else {
                eval_script(cx, Rc::new(ast_and_source.0), realm)
            }
        };

        let duration = start_timestamp.elapsed().unwrap();

        check_expected_completion(cx, test, completion, duration)
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
) -> js::parser::ParseResult<(js::parser::parser::ParseProgramResult, Rc<js::parser::source::Source>)>
{
    let full_path = Path::new(test262_root).join("test").join(file);

    let mut source = js::parser::source::Source::new_from_file(full_path.to_str().unwrap())?;

    if let Some(Test { mode: TestMode::Module, .. }) = test {
        let source = Rc::new(source);
        let parese_result = js::parser::parse_module(&source)?;

        Ok((parese_result, source))
    } else {
        // Manually insert use strict directive when forcing strict mode
        if force_strict_mode {
            let mut source_with_directive = Wtf8String::from_str("\"use strict\";\n");
            source_with_directive.push_wtf8_str(&source.contents);

            source.contents = source_with_directive;
        }

        let source = Rc::new(source);
        let parse_result = js::parser::parse_script(&source)?;

        Ok((parse_result, source))
    }
}

fn load_harness_test_file(cx: Context, realm: Handle<Realm>, test262_root: &str, file: &str) {
    let full_path = Path::new(test262_root).join("harness").join(file);

    let mut ast_and_source = parse_file(full_path.to_str().unwrap(), None, test262_root, false)
        .expect(&format!("Failed to parse test harness file {}", full_path.display()));

    js::parser::analyze::analyze(&mut ast_and_source.0, ast_and_source.1)
        .expect(&format!("Failed to parse test harness file {}", full_path.display()));

    let eval_result = if cx.options.bytecode {
        execute_as_bytecode(cx, &ast_and_source.0)
    } else {
        eval_script(cx, Rc::new(ast_and_source.0), realm)
    };

    match eval_result.kind() {
        CompletionKind::Normal => {}
        CompletionKind::Throw => {
            panic!("Failed to evaluate test harness file {}", full_path.display())
        }
        _ => panic!(
            "Unexpected abnormal completion when evaluating test harness file {}",
            full_path.display()
        ),
    }
}

fn execute_as_bytecode(
    mut cx: Context,
    parse_result: &js::parser::parser::ParseProgramResult,
) -> Completion {
    let generate_result = BytecodeProgramGenerator::generate_from_program_parse_result(
        cx,
        parse_result,
        cx.current_realm(),
    );
    let bytecode_program = match generate_result {
        Ok(bytecode_program) => bytecode_program,
        Err(err) => {
            let err_string = cx.alloc_string(&err.to_string());
            return Completion::throw(err_string.into());
        }
    };

    let closure = Closure::new(cx, bytecode_program);

    match cx.execute_bytecode(closure, &[]) {
        Ok(value) => Completion::normal(value),
        Err(error_value) => Completion::throw(error_value),
    }
}

/// Determine whether the evaluation completion for a test corresponds to that test passing or
/// failing, depending on the expected result of the test.
fn check_expected_completion(
    cx: Context,
    test: &Test,
    completion: Completion,
    duration: Duration,
) -> TestResult {
    match completion.kind() {
        // A normal completion is a success only if the test was expected to not throw
        CompletionKind::Normal => match &test.expected_result {
            ExpectedResult::Positive => TestResult::success(test, duration),
            other => TestResult::failure(
                test,
                format!("Test completed without throwing, but expected {}", other.to_string()),
                duration,
            ),
        },
        // Throw completions are a success if the expected result is negative, expected during
        // during runtime, and with the same expected error.
        CompletionKind::Throw => match &test.expected_result {
            ExpectedResult::Negative { phase: TestPhase::Runtime, type_ } => {
                // Check that the thrown error matches the expected error type
                let thrown_value = completion.value();
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
                let thrown_string = to_console_string_test262(cx, completion.value());
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
        // Other abnormal completions are never valid
        _ => TestResult::failure(
            test,
            String::from("Unexpected abnormal completion when evaluating file"),
            duration,
        ),
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
