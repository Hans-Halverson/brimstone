use regex::Regex;
use serde_json;

use std::{collections::HashSet, fs, path::Path};

use crate::{index::Test, utils::GenericError};

pub struct IgnoredIndex {
    ignored_tests_regex: Regex,
    ignored_features: HashSet<String>,
    ignore_async_generator: bool,
    ignore_module: bool,
    ignore_annex_b: bool,
}

impl IgnoredIndex {
    pub fn load_from_file(
        ignored_path: &Path,
        run_all_tests: bool,
        ignore_async_generator: bool,
        ignore_module: bool,
        ignore_annex_b: bool,
    ) -> Result<IgnoredIndex, GenericError> {
        let ignored_string = fs::read_to_string(ignored_path)?;

        // Strip line comments so that serde_json can parse the file
        let strip_comments_regex = Regex::new(r"//.*\n")?;
        let ignored_string_no_comments = strip_comments_regex.replace_all(&ignored_string, "\n");

        let ignored_json: serde_json::Value = serde_json::from_str(&ignored_string_no_comments)?;

        // Create ignored tests regex
        let mut ignored_tests_strings = vec![];
        let mut ignored_features = HashSet::new();

        let always_ignored = &ignored_json["always"];
        Self::add_ignored_config(always_ignored, &mut ignored_tests_strings, &mut ignored_features);

        if !run_all_tests {
            let slow_ignored = &ignored_json["slow"];
            Self::add_ignored_config(
                slow_ignored,
                &mut ignored_tests_strings,
                &mut ignored_features,
            );
        }

        if cfg!(feature = "gc_stress_test") {
            let gc_stress_test_ignored = &ignored_json["gc_stress_test"];
            Self::add_ignored_config(
                gc_stress_test_ignored,
                &mut ignored_tests_strings,
                &mut ignored_features,
            );
        }

        let ignored_tests_regex = Regex::new(&format!("^({})$", ignored_tests_strings.join("|")))?;

        Ok(IgnoredIndex {
            ignored_tests_regex,
            ignored_features,
            ignore_async_generator,
            ignore_module,
            ignore_annex_b,
        })
    }

    fn add_ignored_config(
        ignored_config: &serde_json::Value,
        ignored_tests_strings: &mut Vec<String>,
        ignored_features: &mut HashSet<String>,
    ) {
        for test in ignored_config["tests"].as_array().unwrap() {
            // Convert from glob pattern to regex
            let ignored_test_string = String::from(test.as_str().unwrap())
                // Escape period characters
                .replace(".", "\\.")
                // Convert glob wildcard to regex
                .replace("*", ".*");

            ignored_tests_strings.push(ignored_test_string);
        }

        let features = ignored_config["features"].as_array();

        if let Some(features) = features {
            for feature in features {
                ignored_features.insert(String::from(feature.as_str().unwrap()));
            }
        }
    }

    pub fn should_ignore(&self, test: &Test) -> bool {
        if self.ignored_tests_regex.is_match(&test.path) {
            return true;
        }

        // Crudely ignore tests with certain keywords in name for some filters
        if self.ignore_async_generator
            && (test.path.contains("async")
                || test.path.contains("await")
                || test.path.contains("generator")
                || test.path.contains("yield"))
        {
            return true;
        }

        if self.ignore_module
            && (test.path.contains("module")
                || test.path.contains("import")
                || test.path.contains("export"))
        {
            return true;
        }

        if self.ignore_annex_b && test.path.contains("annexB/") {
            return true;
        }

        for feature in &test.features {
            if self.ignored_features.contains(feature) {
                return true;
            }
        }

        return false;
    }

    pub fn ignore_async_generator(&self) -> bool {
        self.ignore_async_generator
    }
}
