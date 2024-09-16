use regex::Regex;
use serde_json;

use std::{collections::HashSet, fs, path::Path};

use crate::{
    index::{Test, TestMode},
    utils::GenericError,
};

pub struct IgnoredIndex {
    ignored_tests_regex: Regex,
    ignored_features: HashSet<String>,
    ignore_module: bool,
}

impl IgnoredIndex {
    pub fn load_from_file(
        ignored_path: &Path,
        run_all_tests: bool,
        ignore_unimplemented: bool,
    ) -> Result<IgnoredIndex, GenericError> {
        let ignored_string = fs::read_to_string(ignored_path)?;

        // Strip line comments so that serde_json can parse the file
        let strip_comments_regex = Regex::new(r"//.*\n")?;
        let ignored_string_no_comments = strip_comments_regex.replace_all(&ignored_string, "\n");

        let ignored_json: serde_json::Value = serde_json::from_str(&ignored_string_no_comments)?;

        let mut builder = IgnoredIndexBuilder::new();

        let known_failures = &ignored_json["known_failures"];
        builder.add_config(known_failures);

        if !run_all_tests {
            let slow_ignored = &ignored_json["slow"];
            builder.add_config(slow_ignored);

            let non_standard = &ignored_json["non_standard"];
            builder.add_config(non_standard);
        }

        if ignore_unimplemented {
            let unimplemented = &ignored_json["unimplemented"];
            builder.add_config(unimplemented);
        }

        if cfg!(feature = "gc_stress_test") {
            let gc_stress_test_ignored = &ignored_json["gc_stress_test"];
            builder.add_config(gc_stress_test_ignored);
        }

        let ignore_module = ignore_unimplemented;

        builder.finish(ignore_module)
    }

    pub fn should_ignore(&self, test: &Test) -> bool {
        if self.ignored_tests_regex.is_match(&test.path) {
            return true;
        }

        if self.ignore_module && test.mode == TestMode::Module {
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

struct IgnoredIndexBuilder {
    ignored_tests_strings: Vec<String>,
    ignored_features: HashSet<String>,
}

impl IgnoredIndexBuilder {
    fn new() -> Self {
        Self {
            ignored_tests_strings: vec![],
            ignored_features: HashSet::new(),
        }
    }

    fn finish(self, ignore_module: bool) -> Result<IgnoredIndex, GenericError> {
        let ignored_tests_regex =
            Regex::new(&format!("^({})$", self.ignored_tests_strings.join("|")))?;
        Ok(IgnoredIndex {
            ignored_tests_regex,
            ignored_features: self.ignored_features,
            ignore_module,
        })
    }

    fn add_config(&mut self, config_value: &serde_json::Value) {
        if let Some(tests) = config_value["tests"].as_array() {
            for test in tests {
                // Convert from glob pattern to regex
                let ignored_test_string = String::from(test.as_str().unwrap())
                    // Escape period characters
                    .replace(".", "\\.")
                    // Convert glob wildcard to regex
                    .replace("*", ".*");

                self.ignored_tests_strings.push(ignored_test_string);
            }
        }

        if let Some(features) = config_value["features"].as_array() {
            for feature in features {
                self.ignored_features
                    .insert(String::from(feature.as_str().unwrap()));
            }
        }
    }
}
