use brimstone::js;

use std::cmp::min;
use std::path::Path;
use std::{env, error, fs};

type GenericResult = Result<(), Box<dyn error::Error>>;

const RECORD_ENV_VAR: &str = "RECORD";

struct TestEnv {
    errors: Vec<String>,
}

#[test]
fn js_parser_snapshot_tests() -> GenericResult {
    // Find path of parser tests directory
    let parser_tests_dir = Path::new(file!()).parent().unwrap().join("js_parser");

    let mut env = TestEnv { errors: vec![] };
    visit_directory(&mut env, &parser_tests_dir)?;

    if !env.errors.is_empty() {
        assert_eq!("actual", "expected", "\n{}\n", env.errors.join(""))
    }

    Ok(())
}

/// Recursively visit all subdirectories under the target directory, searching for js files.
fn visit_directory(env: &mut TestEnv, path: &Path) -> GenericResult {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            visit_directory(env, &path)?
        } else if path.is_file() {
            match path.extension() {
                Some(extension) => match extension.to_str() {
                    Some("js") => process_snapshot_test_file(env, &path)?,
                    _ => (),
                },
                _ => (),
            }
        }
    }

    Ok(())
}

/// Compare acutal vs expected output for the test file with the given path.
fn process_snapshot_test_file(env: &mut TestEnv, path: &Path) -> GenericResult {
    let exp_path = path.with_extension("exp");

    let source = js::source::Source::new(path.to_str().unwrap())?;
    let ast = js::parser::parse_file(&source)?;
    let actual = js::printer::print_program(&ast, &source);

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
