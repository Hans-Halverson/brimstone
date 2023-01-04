use serde_json;
use threadpool::ThreadPool;

use std::{fs, panic, path::Path, rc::Rc, sync::mpsc::channel};

use crate::{
    index::{ExpectedResult, Test, TestIndex, TestMode, TestPhase},
    utils::GenericResult,
};

use brimstone::js::{
    self,
    runtime::{
        eval_script, get, initialize_host_defined_realm, to_console_string, Completion,
        CompletionKind, Context, EvalResult, Gc, Realm,
    },
};

pub struct TestRunner {
    index: TestIndex,
    thread_pool: ThreadPool,
    filter: Option<String>,
    feature: Option<String>,
}

impl TestRunner {
    pub fn new(
        index: TestIndex,
        num_threads: u8,
        filter: Option<String>,
        feature: Option<String>,
    ) -> TestRunner {
        let thread_pool = ThreadPool::new(num_threads.into());
        TestRunner {
            index,
            thread_pool,
            filter,
            feature,
        }
    }

    pub fn run(&mut self, verbose: bool) -> TestResults {
        let (sender, receiver) = channel::<TestResult>();
        let mut num_jobs = 0;
        let mut num_skipped = 0;

        for test in self.index.tests.values() {
            if !self.should_run_test(test) {
                num_skipped += 1;
                continue;
            }

            let test262_root = self.index.test262_root.clone();
            let test = test.clone();
            let sender = sender.clone();
            num_jobs += 1;

            self.thread_pool.execute(move || {
                let panic_result = panic::catch_unwind(|| {
                    if verbose {
                        println!("{}", test.path);
                    }

                    run_full_test(&test, &test262_root)
                });

                match panic_result {
                    Ok(result) => sender.send(result).unwrap(),
                    Err(err) => {
                        // Attempt to extract string message from panic
                        let message = err.downcast_ref::<String>();
                        let message = match message {
                            Some(message) => message.clone(),
                            None => String::from("<panic message not found>"),
                        };

                        sender
                            .send(TestResult::failure(
                                &test,
                                format!("Thread panicked:\n{}", message),
                            ))
                            .unwrap()
                    }
                }
            });
        }

        let results: Vec<TestResult> = receiver.iter().take(num_jobs).collect();

        TestResults::collate(results, num_skipped)
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

        true
    }
}

fn run_full_test(test: &Test, test262_root: &str) -> TestResult {
    match test.mode {
        TestMode::StrictScript => run_single_test(test, test262_root, true),
        TestMode::NonStrictScript | TestMode::Module | TestMode::Raw => {
            run_single_test(test, test262_root, false)
        }
        // Run in both strict and non strict mode, both must pass for this test to be successful
        TestMode::Script => {
            let non_strict_result = run_single_test(test, test262_root, false);
            if let TestResultCompletion::Success = non_strict_result.result {
                run_single_test(test, test262_root, true)
            } else {
                non_strict_result
            }
        }
    }
}

fn run_single_test(test: &Test, test262_root: &str, force_strict_mode: bool) -> TestResult {
    // Each test is executed in its own realm
    let mut cx = Context::new();
    let realm = initialize_host_defined_realm(&mut cx);

    // Default harness files are loaded unless running in raw mode
    if test.mode != TestMode::Raw {
        load_harness_test_file(&mut cx, realm, test262_root, "assert.js");
        load_harness_test_file(&mut cx, realm, test262_root, "sta.js");
    }

    // Load all other specified harness files
    for include in &test.includes {
        load_harness_test_file(&mut cx, realm, test262_root, include);
    }

    let parse_result = parse_and_analyze(&test.path, test262_root, force_strict_mode);
    let ast = match parse_result {
        Ok(ast) => ast,
        // An error during parse may be a success or failure depending on the expected result of
        // the test.
        Err(err) => {
            // Do not count IO errors as negative tests results
            let is_parse_error = match err.error {
                js::parser::ParseError::Io(_) => false,
                _ => true,
            };

            return match test.expected_result {
                ExpectedResult::Negative {
                    phase: TestPhase::Parse,
                    ..
                } if is_parse_error => TestResult::success(test),
                _ => TestResult::failure(
                    test,
                    format!("Unexpected error during parsing:\n{}", err.to_string()),
                ),
            };
        }
    };

    let completion = eval_script(&mut cx, Rc::new(ast), realm);
    check_expected_completion(&mut cx, test, completion)
}

fn parse_and_analyze(
    file: &str,
    test262_root: &str,
    force_strict_mode: bool,
) -> js::parser::ParseResult<js::parser::ast::Program> {
    let full_path = Path::new(test262_root).join("test").join(file);

    let mut source = js::parser::source::Source::new(full_path.to_str().unwrap())?;

    // Manually insert use strict directive when forcing strict mode
    if force_strict_mode {
        source.contents.insert_str(0, "use strict;\n");
    }

    let mut ast = js::parser::parse_file(&Rc::new(source))?;
    js::parser::analyze::analyze(&mut ast);

    Ok(ast)
}

fn load_harness_test_file(cx: &mut Context, realm: Gc<Realm>, test262_root: &str, file: &str) {
    let full_path = Path::new(test262_root).join("harness").join(file);

    let ast = parse_and_analyze(full_path.to_str().unwrap(), test262_root, false).expect(&format!(
        "Failed to parse test harness file {}",
        full_path.display()
    ));

    let eval_result = eval_script(cx, Rc::new(ast), realm);

    match eval_result.kind() {
        CompletionKind::Normal => {}
        CompletionKind::Throw => panic!(
            "Failed to evaluate test harness file {}",
            full_path.display()
        ),
        _ => panic!(
            "Unexpected abnormal completion when evaluating test harness file {}",
            full_path.display()
        ),
    }
}

/// Determine whether the evaluation completion for a test corresponds to that test passing or
/// failing, depending on the expected result of the test.
fn check_expected_completion(cx: &mut Context, test: &Test, completion: Completion) -> TestResult {
    match completion.kind() {
        // A normal completion is a success only if the test was expected to not throw
        CompletionKind::Normal => match &test.expected_result {
            ExpectedResult::Positive => TestResult::success(test),
            other => TestResult::failure(
                test,
                format!(
                    "Test completed without throwing, but expected {}",
                    other.to_string()
                ),
            ),
        },
        // Throw completions are a success if the expected result is negative, expected during
        // during runtime, and with the same expected error.
        CompletionKind::Throw => match &test.expected_result {
            ExpectedResult::Negative {
                phase: TestPhase::Runtime,
                type_,
            } => {
                // Check that the thrown error matches the expected error type
                let thrown_value = completion.value();
                let is_expected_error = if thrown_value.is_object() {
                    let thrown_object = thrown_value.as_object();
                    if thrown_object.is_error() {
                        // Check if thrown error type has the same name as the expected error
                        match get(cx, thrown_object, "name") {
                            EvalResult::Ok(name_value) if name_value.is_string() => {
                                name_value.as_string().str() == type_
                            }
                            _ => false,
                        }
                    } else if type_ == "Test262Error" {
                        // The Test262Error does not extend the Error type or have a name property,
                        // so check the if the message property starts with "Test262Error".
                        match get(cx, thrown_object, "message") {
                            EvalResult::Ok(message_value) if message_value.is_string() => {
                                message_value.as_string().str().starts_with("Test262Error")
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
                    TestResult::success(test)
                } else {
                    let thrown_string = to_console_string(cx, thrown_value);
                    TestResult::failure(
                        test,
                        format!(
                            "Test threw the following error during runtime, but expected {}:\n{}",
                            test.expected_result.to_string(),
                            thrown_string,
                        ),
                    )
                }
            }
            other => {
                let thrown_string = to_console_string(cx, completion.value());
                TestResult::failure(
                    test,
                    format!(
                        "Test threw the following error during runtime, but expected {}:\n{}",
                        other.to_string(),
                        thrown_string,
                    ),
                )
            }
        },
        // Other abnormal completions are never valid
        _ => TestResult::failure(
            test,
            String::from("Unexpected abnormal completion when evaluating file"),
        ),
    }
}

struct TestResult {
    path: String,
    result: TestResultCompletion,
}

enum TestResultCompletion {
    Success,
    Failure(String),
}

impl TestResult {
    fn success(test: &Test) -> TestResult {
        TestResult {
            path: test.path.clone(),
            result: TestResultCompletion::Success,
        }
    }

    fn failure(test: &Test, message: String) -> TestResult {
        TestResult {
            path: test.path.clone(),
            result: TestResultCompletion::Failure(message),
        }
    }
}

pub struct TestResults {
    succeeded: Vec<TestResult>,
    failed: Vec<TestResult>,
    num_skipped: u64,
}

// ANSII codes for pretty printing
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const WHITE: &str = "\x1b[37m";
const RESET: &str = "\x1b[0m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";

impl TestResults {
    fn collate(results: Vec<TestResult>, num_skipped: u64) -> TestResults {
        let mut collated = TestResults {
            failed: vec![],
            succeeded: vec![],
            num_skipped,
        };

        for result in results {
            match result.result {
                TestResultCompletion::Success => {
                    collated.succeeded.push(result);
                }
                TestResultCompletion::Failure(_) => {
                    collated.failed.push(result);
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

                println!(
                    "{}{}Failed{}: {}\n{}\n",
                    BOLD, RED, RESET, failed.path, cleaned_message
                );
            }
        }

        println!(
            "{}{}Succeeded: {}\n{}Failed: {}\n{}{}Skipped: {}{}",
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
}
