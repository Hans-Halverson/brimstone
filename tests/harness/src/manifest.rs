use std::{
    collections::HashMap,
    fmt, fs,
    path::{Path, PathBuf},
};

use clap::ValueEnum;
use serde::{Deserialize, Serialize};

use crate::utils::GenericError;

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize, ValueEnum)]
pub enum Suite {
    #[serde(rename = "integration")]
    Integration,
    #[serde(rename = "test262")]
    Test262,
}

/// A collection of tests under the same root directory. For example the `test262` test suite.
#[derive(Deserialize)]
pub struct SuiteConfig {
    /// Identifier for the test suite.
    #[serde(rename = "name")]
    pub suite: Suite,
    /// Path to the root directory of the test suite. Relative to the manifest directory.
    pub path: String,
    /// Path to the ignored tests file. Relative to the manifest directory.
    pub ignored_file: String,
}

/// Configuration for all integration test suites.
#[derive(Deserialize)]
pub struct TestManifest {
    /// Directory which contains this test manifest.
    #[serde(skip_deserializing)]
    pub manifest_dir: PathBuf,
    /// Path to the test index file. Relative to the manifest directory.
    index_path: String,
    /// Path to the test262 repo. Relative to the manifest directory.
    test262_repo_path: String,
    /// Configuration for all integration test suites.
    pub suites: Vec<SuiteConfig>,
}

impl TestManifest {
    pub fn load_from_file(manifest_path: &Path) -> Result<TestManifest, GenericError> {
        let manifest_string = fs::read_to_string(manifest_path)?;
        let mut manifest = serde_json::from_str::<TestManifest>(&manifest_string)?;

        // Fill the manifest directory field
        manifest.manifest_dir = manifest_path.parent().unwrap().to_path_buf();

        Ok(manifest)
    }

    pub fn index_path(&self) -> PathBuf {
        self.manifest_dir.as_path().join(&self.index_path)
    }

    pub fn test262_repo_path(&self) -> PathBuf {
        self.manifest_dir.as_path().join(&self.test262_repo_path)
    }

    pub fn map_for_suites<F, T>(&self, f: F) -> HashMap<Suite, T>
    where
        F: Fn(&SuiteConfig) -> T,
    {
        self.suites
            .iter()
            .map(|config| (config.suite, f(config)))
            .collect()
    }
}

impl fmt::Display for Suite {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Suite::Integration => write!(f, "integration"),
            Suite::Test262 => write!(f, "test262"),
        }
    }
}

#[derive(Clone)]
pub struct SuiteFilter {
    /// The set of all suites that should be run.
    suites: Vec<Suite>,
}

impl SuiteFilter {
    pub fn new(suites_arg: Option<Vec<Suite>>) -> SuiteFilter {
        let mut suites = suites_arg.unwrap_or_else(|| vec![Suite::Integration, Suite::Test262]);
        suites.sort();

        SuiteFilter { suites }
    }

    /// Whether the given suite should be included in the run.
    pub fn should_include(&self, suite: Suite) -> bool {
        self.suites.contains(&suite)
    }

    /// Returns an iterator over all suites that should be run.
    pub fn iter(&self) -> impl Iterator<Item = Suite> + '_ {
        self.suites.iter().copied()
    }

    /// Create a HashMap with keys for all suites that should be run and values computed with the
    /// given function.
    pub fn map_for_suites<F, T>(&self, f: F) -> HashMap<Suite, T>
    where
        F: Fn(Suite) -> T,
    {
        self.iter().map(|suite| (suite, f(suite))).collect()
    }
}
