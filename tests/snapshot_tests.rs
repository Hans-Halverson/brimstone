use brimstone::js::{
    common::options::Options,
    parser,
    runtime::{bytecode::generator::BytecodeProgramGenerator, Context},
};

use std::cmp::min;
use std::path::Path;
use std::rc::Rc;
use std::{env, error, fs};

type GenericResult<T> = Result<T, Box<dyn error::Error>>;

const RECORD_ENV_VAR: &str = "RECORD";

struct TestEnv {
    errors: Vec<String>,
}

#[test]
fn js_parser_snapshot_tests() -> GenericResult<()> {
    let parser_tests_dir = Path::new(file!()).parent().unwrap().join("js_parser");
    run_snapshot_tests(&parser_tests_dir, &mut |path| print_ast(path))
}

fn print_ast(path: &str) -> GenericResult<String> {
    let parse_result = parse_script_or_module(path)?;
    Ok(parser::print_program(&parse_result.program, &parse_result.program.source))
}

#[test]
fn js_bytecode_snapshot_tests() -> GenericResult<()> {
    // Context is shared between all tests
    let options = Rc::new(Options::default());
    let (cx, _) = Context::new(options, |_| ());

    let bytecode_tests_dir = Path::new(file!()).parent().unwrap().join("js_bytecode");
    run_snapshot_tests(&bytecode_tests_dir, &mut |path| print_bytecode(cx, path))
}

fn print_bytecode(cx: Context, path: &str) -> GenericResult<String> {
    let mut parse_result = parse_script_or_module(path)?;
    let source = parse_result.program.source.clone();
    parser::analyze::analyze(&mut parse_result, source)?;

    let bytecode_program =
        BytecodeProgramGenerator::generate_from_program_parse_result(cx, &Rc::new(parse_result))?;

    Ok(bytecode_program.debug_print_recursive())
}

fn parse_script_or_module(path: &str) -> GenericResult<parser::parser::ParseProgramResult> {
    let source = Rc::new(parser::source::Source::new_from_file(path)?);

    let parse_result = if path.contains("module") {
        parser::parse_module(&source)?
    } else {
        parser::parse_script(&source)?
    };

    Ok(parse_result)
}

/// Run snapshot tests for all js files under the given directory.
///
/// Individual tests implemeneted as a function takes in a path and returns the output of the test.
fn run_snapshot_tests(
    root_path: &Path,
    test_fn: &mut impl FnMut(&str) -> GenericResult<String>,
) -> GenericResult<()> {
    let mut env = TestEnv { errors: vec![] };
    visit_directory(&mut env, root_path, test_fn)?;

    if !env.errors.is_empty() {
        assert_eq!("actual", "expected", "\n{}\n", env.errors.join(""))
    }

    Ok(())
}

/// Recursively visit all subdirectories under the target directory, searching for js files.
fn visit_directory(
    env: &mut TestEnv,
    path: &Path,
    test_fn: &mut impl FnMut(&str) -> GenericResult<String>,
) -> GenericResult<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            visit_directory(env, &path, test_fn)?
        } else if path.is_file() {
            match path.extension() {
                Some(extension) => match extension.to_str() {
                    Some("js") => process_snapshot_test_file(env, &path, test_fn)?,
                    _ => (),
                },
                _ => (),
            }
        }
    }

    Ok(())
}

/// Compare acutal vs expected output for the test file with the given path.
fn process_snapshot_test_file(
    env: &mut TestEnv,
    path: &Path,
    test_fn: &mut impl FnMut(&str) -> GenericResult<String>,
) -> GenericResult<()> {
    let path_str = path.to_str().unwrap();
    let exp_path = path.with_extension("exp");

    let actual = test_fn(path_str)?;

    let expected = if exp_path.exists() {
        fs::read_to_string(&exp_path)?
    } else {
        String::new()
    };

    if actual != expected {
        if env::var(RECORD_ENV_VAR).is_ok() {
            fs::write(&exp_path, &actual)?;
        }

        env.errors
            .push(find_diff_snippet(&path, &actual, &expected))
    }

    Ok(())
}

const RED: &str = "\u{001B}[31m";
const GREEN: &str = "\u{001B}[32m";
const RESET: &str = "\u{001B}[0m";
const BOLD: &str = "\u{001B}[1m";

/// Find and format a snippet showing the difference between the two strings.
fn find_diff_snippet(path: &Path, actual: &str, expected: &str) -> String {
    let actual_lines = actual.lines().collect::<Vec<&str>>();
    let expected_lines = expected.lines().collect::<Vec<&str>>();

    // Find the first line that differs between the strings
    let mut i = 0;
    while i < actual_lines.len() && i < expected_lines.len() && actual_lines[i] == expected_lines[i]
    {
        i += 1;
    }

    let actual_snippet = if i == actual_lines.len() {
        String::new()
    } else {
        let snippet_end = min(i + 10, actual_lines.len());
        let lines = actual_lines[i..snippet_end].join("\n+ ");
        format!("{}{}+ {}{}", RED, BOLD, lines, RESET)
    };

    let expected_snippet = if i == expected_lines.len() {
        String::new()
    } else {
        let snippet_end = min(i + 10, expected_lines.len());
        let lines = expected_lines[i..snippet_end].join("\n- ");
        format!("{}{}- {}{}", GREEN, BOLD, lines, RESET)
    };

    format!(
        "\n{}\nActual and expected differ on line {}\n{}\n{}\n",
        path.to_str().unwrap(),
        i,
        expected_snippet,
        actual_snippet
    )
}
