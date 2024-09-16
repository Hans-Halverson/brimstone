use regex::Regex;
use serde_json;

use std::{collections::HashSet, fs, path::Path};

use crate::{
    index::{Test, TestMode},
    utils::GenericError,
};

pub struct IgnoredIndex {
    /// Matcher for tests which should be ignored
    ignore: Matcher,
    /// Matcher for tests that should count as failures
    fail: Option<Matcher>,
}

struct Matcher {
    /// Regexp that matches tests
    tests_regex: Regex,
    /// Features to match
    features: HashSet<String>,
    /// Whether all module tests should match
    module: bool,
}

impl IgnoredIndex {
    pub fn load_from_file(
        ignored_path: &Path,
        run_all_tests: bool,
        ignore_unimplemented: bool,
        report_test_262_progress: bool,
    ) -> Result<IgnoredIndex, GenericError> {
        let ignored_string = fs::read_to_string(ignored_path)?;

        // Strip line comments so that serde_json can parse the file
        let strip_comments_regex = Regex::new(r"//.*\n")?;
        let ignored_string_no_comments = strip_comments_regex.replace_all(&ignored_string, "\n");

        let ignored_json: serde_json::Value = serde_json::from_str(&ignored_string_no_comments)?;

        let mut builder = IgnoredIndexBuilder::new();

        // Known failures are ignored, but treated as failures when reporting test262 progress
        let known_failures = &ignored_json["known_failures"];
        if report_test_262_progress {
            builder.add_fail_config(known_failures);
        } else {
            builder.add_ignore_config(known_failures);
        }

        let unimplemented = &ignored_json["unimplemented"];
        if ignore_unimplemented {
            // By default unimplemented tests are run but can be ignored with a flag
            builder.add_ignore_config(unimplemented);
            builder.set_ignore_module(true);
        } else if report_test_262_progress {
            // Unimplemented features are treated as failures when reporting test262 progress
            builder.add_fail_config(unimplemented);
            builder.set_fail_module(true);
        }

        // By default non-standard tests are ignored
        if !run_all_tests {
            let non_standard = &ignored_json["non_standard"];
            builder.add_ignore_config(non_standard);
        }

        // Slow tests are ignored by default, but always run when reporting test262 progress
        if !run_all_tests && !report_test_262_progress {
            let slow_ignored = &ignored_json["slow"];
            builder.add_ignore_config(slow_ignored);
        }

        // Ignore some additional tests when in GC stress test mode
        if cfg!(feature = "gc_stress_test") {
            let gc_stress_test_ignored = &ignored_json["gc_stress_test"];
            builder.add_ignore_config(gc_stress_test_ignored);
        }

        builder.finish()
    }

    pub fn should_ignore(&self, test: &Test) -> bool {
        self.ignore.matches(test)
    }

    pub fn should_fail(&self, test: &Test) -> bool {
        self.fail
            .as_ref()
            .map(|matcher| matcher.matches(test))
            .unwrap_or(false)
    }
}

impl Matcher {
    fn matches(&self, test: &Test) -> bool {
        if self.tests_regex.is_match(&test.path) {
            return true;
        }

        if self.module && test.mode == TestMode::Module {
            return true;
        }

        for feature in &test.features {
            if self.features.contains(feature) {
                return true;
            }
        }

        false
    }
}

struct MatcherBuilder {
    test_strings: Vec<String>,
    features: HashSet<String>,
    module: bool,
}

impl MatcherBuilder {
    fn new() -> Self {
        Self {
            test_strings: vec![],
            features: HashSet::new(),
            module: false,
        }
    }

    fn finish(self) -> Result<Matcher, GenericError> {
        let tests_regex = Self::build_regexp(&self.test_strings)?;
        Ok(Matcher { tests_regex, features: self.features, module: self.module })
    }

    fn build_regexp(strings: &[String]) -> Result<Regex, GenericError> {
        let regex_string = format!("^({})$", strings.join("|"));
        let regex = Regex::new(&regex_string)?;
        Ok(regex)
    }
}

struct IgnoredIndexBuilder {
    ignore: MatcherBuilder,
    fail: MatcherBuilder,
    has_fail_matcher: bool,
}

impl IgnoredIndexBuilder {
    fn new() -> Self {
        Self {
            ignore: MatcherBuilder::new(),
            fail: MatcherBuilder::new(),
            has_fail_matcher: false,
        }
    }

    fn finish(self) -> Result<IgnoredIndex, GenericError> {
        Ok(IgnoredIndex {
            ignore: self.ignore.finish()?,
            fail: if self.has_fail_matcher {
                Some(self.fail.finish()?)
            } else {
                None
            },
        })
    }

    fn add_ignore_config(&mut self, config_value: &serde_json::Value) {
        Self::add_config(&mut self.ignore, config_value);
    }

    fn add_fail_config(&mut self, config_value: &serde_json::Value) {
        self.has_fail_matcher = true;
        Self::add_config(&mut self.fail, config_value);
    }

    fn add_config(match_builder: &mut MatcherBuilder, config_value: &serde_json::Value) {
        if let Some(tests) = config_value["tests"].as_array() {
            for test in tests {
                // Convert from glob pattern to regex
                let ignored_test_string = String::from(test.as_str().unwrap())
                    // Escape period characters
                    .replace(".", "\\.")
                    // Convert glob wildcard to regex
                    .replace("*", ".*");

                match_builder.test_strings.push(ignored_test_string);
            }
        }

        if let Some(features) = config_value["features"].as_array() {
            for feature in features {
                match_builder
                    .features
                    .insert(String::from(feature.as_str().unwrap()));
            }
        }
    }

    fn set_ignore_module(&mut self, ignore_module: bool) {
        self.ignore.module = ignore_module;
    }

    fn set_fail_module(&mut self, fail_module: bool) {
        self.fail.module = fail_module;
    }
}
