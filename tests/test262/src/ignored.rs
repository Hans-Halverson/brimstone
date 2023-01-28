use regex::Regex;
use serde_json;

use std::{collections::HashSet, fs, path::Path};

use crate::{index::Test, utils::GenericError};

pub struct IgnoredIndex {
    ignored_tests: HashSet<String>,
    ignored_features: HashSet<String>,
}

impl IgnoredIndex {
    pub fn load_from_file(ignored_path: &Path) -> Result<IgnoredIndex, GenericError> {
        let ignored_string = fs::read_to_string(ignored_path)?;

        // Strip line comments so that serde_json can parse the file
        let strip_comments_regex = Regex::new(r"//.*\n")?;
        let ignored_string_no_comments = strip_comments_regex.replace_all(&ignored_string, "\n");

        let ignored_json: serde_json::Value = serde_json::from_str(&ignored_string_no_comments)?;

        let always_ignored = &ignored_json["always"];

        let mut ignored_tests = HashSet::new();
        for test in always_ignored["tests"].as_array().unwrap() {
            ignored_tests.insert(String::from(test.as_str().unwrap()));
        }

        let mut ignored_features = HashSet::new();
        for feature in always_ignored["features"].as_array().unwrap() {
            ignored_features.insert(String::from(feature.as_str().unwrap()));
        }

        Ok(IgnoredIndex { ignored_tests, ignored_features })
    }

    pub fn should_ignore(&self, test: &Test) -> bool {
        if self.ignored_tests.contains(&test.path) {
            return true;
        }

        for feature in &test.features {
            if self.ignored_features.contains(feature) {
                return true;
            }
        }

        return false;
    }
}
