use serde::{Deserialize, Serialize};
use serde_json;
use yaml_rust::YamlLoader;

use std::{collections::HashMap, fs, path::Path};

use crate::utils::{GenericError, GenericResult};

/// A single test262 test file along with its metadata
#[derive(Clone, Serialize, Deserialize)]
pub struct Test {
    // Path to the test file. Relative to the test262/test directory.
    pub path: String,
    pub expected_result: ExpectedResult,
    pub mode: TestMode,
    pub is_async: bool,
    // Run test without modifying the source file or evaluating any other scripts from test harness.
    // For scripts the test is run once, in non-strict mode.
    pub is_raw: bool,
    // Files that must be evaluated in the global scope prior to test execution. Paths are
    // relative to the test262/harness directory.
    pub includes: Vec<String>,
    // Tags for categorizing tests by feature, allows easy filtering by feature
    pub features: Vec<String>,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ExpectedResult {
    // Test that are expected to succeed without throwing an error
    Positive,
    // Tests that are expected to throw an uncaught error
    Negative { phase: TestPhase, type_: String },
}

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum TestPhase {
    Parse,
    Resolution,
    Runtime,
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum TestMode {
    // Run test as script, both in strict and non-strict mode
    Script,
    // Run test as script in strict mode only
    StrictScript,
    // Run test as script in non-strict mode only
    NonStrictScript,
    // Run test as module, which is always in strict mode
    Module,
}

impl ExpectedResult {
    pub fn to_string(&self) -> String {
        match self {
            ExpectedResult::Positive => String::from("no error to be thrown"),
            ExpectedResult::Negative { phase, type_ } => {
                let phase_string = match phase {
                    TestPhase::Parse => "parsing",
                    TestPhase::Resolution => "module resolution",
                    TestPhase::Runtime => "evaluation",
                };

                format!("{} error to be thrown during {}", type_, phase_string)
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct TestIndex {
    // Tests indexed by test path
    pub tests: HashMap<String, Test>,

    // Path to the test262 repo
    pub test262_root: String,
}

impl TestIndex {
    pub fn new(test262_root: &Path) -> Result<TestIndex, GenericError> {
        let mut indexer = TestIndex {
            tests: HashMap::new(),
            test262_root: String::from(test262_root.to_str().unwrap()),
        };

        let test262_test_dir = test262_root.join("test");
        indexer.visit_directory(&test262_test_dir)?;

        Ok(indexer)
    }

    pub fn write_to_file(&self, index_path: &Path) -> GenericResult {
        let index_string = serde_json::to_string_pretty(self).unwrap();
        fs::write(index_path, &index_string)?;

        Ok(())
    }

    pub fn load_from_file(index_path: &Path) -> Result<TestIndex, GenericError> {
        let index_string = fs::read_to_string(index_path)?;
        let index = serde_json::from_str(&index_string)?;

        Ok(index)
    }

    /// Recursively visit all subdirectories under the target directory, searching for js files.
    fn visit_directory(&mut self, path: &Path) -> GenericResult {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                self.visit_directory(&path)?
            } else if path.is_file() {
                let path_string = path.to_str().unwrap();
                if path_string.ends_with(".js") && !path_string.contains("_FIXTURE") {
                    self.index_test_file(&path)?;
                }
            }
        }

        Ok(())
    }

    fn index_test_file(&mut self, test_path: &Path) -> GenericResult {
        let file_contents = fs::read_to_string(&test_path)?;

        let comment_start = file_contents.find("/*---").unwrap();
        let comment_end = (&file_contents[comment_start..]).find("---*/").unwrap();

        let comment = &file_contents[comment_start + 5..comment_start + comment_end];

        // The YAML parser doesn't consider CR to be a newline, so replace CR with LF
        let metadata = if comment.contains("\r") {
            YamlLoader::load_from_str(&comment.replace("\r", "\n")).unwrap()
        } else {
            YamlLoader::load_from_str(comment).unwrap()
        };

        let raw_negative = &metadata[0]["negative"];
        let expected_result = if raw_negative.is_badvalue() {
            ExpectedResult::Positive
        } else {
            let raw_phase = raw_negative["phase"].as_str().unwrap();
            let phase = match raw_phase {
                "parse" => TestPhase::Parse,
                "resolution" => TestPhase::Resolution,
                "runtime" => TestPhase::Runtime,
                _ => panic!("Unexpected phase {}", raw_phase),
            };

            let type_ = String::from(raw_negative["type"].as_str().unwrap());

            ExpectedResult::Negative { phase, type_ }
        };

        let mut is_async = false;
        let mut is_raw = false;
        let mut includes = vec![];

        let mut mode = TestMode::Script;
        if let Some(raw_features) = metadata[0]["flags"].as_vec() {
            for raw_feature in raw_features {
                match raw_feature.as_str().unwrap() {
                    "onlyStrict" => {
                        mode = TestMode::StrictScript;
                    }
                    "noStrict" => {
                        mode = TestMode::NonStrictScript;
                    }
                    "module" => {
                        mode = TestMode::Module;
                    }
                    "raw" => {
                        is_raw = true;
                    }
                    "async" => {
                        is_async = true;
                        includes.push("doneprintHandle.js".to_owned());
                    }
                    _ => {}
                }
            }
        }

        if let Some(raw_includes) = metadata[0]["includes"].as_vec() {
            for raw_include in raw_includes {
                includes.push(String::from(raw_include.as_str().unwrap()));
            }
        }

        let mut features = vec![];
        if let Some(raw_features) = metadata[0]["features"].as_vec() {
            for raw_feature in raw_features {
                features.push(String::from(raw_feature.as_str().unwrap()));
            }
        }

        let path = String::from(test_path.to_str().unwrap());

        // Make path relative to test262/test directory
        let path = path.strip_prefix(&self.test262_root).unwrap();
        let path = path.strip_prefix("/test/").unwrap();

        let test = Test {
            path: String::from(path),
            expected_result,
            mode,
            is_async,
            is_raw,
            includes,
            features,
        };

        self.tests.insert(String::from(path), test);

        Ok(())
    }
}
