use std::path::{Path, PathBuf};

use crate::report::{BenchResult, SuiteRun};
use crate::runner::Flamegraph;
use crate::suites;

/// Case-insensitive substring filter over benchmark names; an empty filter matches all.
#[derive(Default)]
pub struct BenchFilter {
    terms: Vec<String>,
}

impl BenchFilter {
    pub fn new(terms: &[String]) -> Self {
        BenchFilter { terms: terms.iter().map(|t| t.to_lowercase()).collect() }
    }

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn matches(&self, name: &str) -> bool {
        if self.is_empty() {
            return true;
        }
        let name = name.to_lowercase();
        self.terms.iter().any(|t| name.contains(t))
    }
}

/// Inputs shared by every suite run.
pub struct RunContext<'a> {
    pub bs_path: &'a Path,
    pub vendor_dir: &'a Path,
    pub shims_dir: &'a Path,
    /// Which benchmarks to run; empty = all.
    pub filter: &'a BenchFilter,
    pub flamegraph: Option<&'a Flamegraph>,
}

impl RunContext<'_> {
    pub fn suite_dir(&self, subdir: &str) -> PathBuf {
        self.vendor_dir.join(subdir)
    }

    pub fn shim(&self, file: &str) -> PathBuf {
        self.shims_dir.join(file)
    }

    /// The flamegraph spec for a profiled `bs` invocation, or `None` if profiling is off.
    /// Pass a `label` (e.g. a benchmark name) when a suite spawns `bs` more than once.
    pub fn flamegraph(&self, label: Option<&str>) -> Option<Flamegraph> {
        self.flamegraph.map(|base| match label {
            Some(label) => base.labeled(label),
            None => base.clone(),
        })
    }

    pub fn suite_run(
        &self,
        suite: &impl SuiteSpec,
        wall_clock_ms: f64,
        results: Vec<BenchResult>,
        summary: Option<BenchResult>,
    ) -> SuiteRun {
        SuiteRun {
            suite: suite.name().to_string(),
            bs_path: self.bs_path.display().to_string(),
            wall_clock_ms,
            results,
            summary,
            pretty_score_precision: suite.pretty_score_precision(),
        }
    }

    pub fn single_error_run(
        &self,
        suite: &impl SuiteSpec,
        unit: &str,
        message: impl Into<String>,
    ) -> SuiteRun {
        self.suite_run(suite, 0.0, vec![BenchResult::error("<run>", unit, message)], None)
    }
}

pub trait SuiteSpec {
    fn name(&self) -> &'static str;

    fn vendor_subdir(&self) -> &'static str;

    fn is_available(&self, ctx: &RunContext) -> bool;

    fn pretty_score_precision(&self) -> usize;

    fn run(&self, ctx: &RunContext) -> SuiteRun;
}

pub fn all_suites() -> Vec<Box<dyn SuiteSpec>> {
    vec![
        Box::new(suites::octane::Octane),
        Box::new(suites::web_tooling::WebTooling),
        Box::new(suites::jetstream::JetStream),
    ]
}

pub fn find_suite(name: &str) -> Option<Box<dyn SuiteSpec>> {
    all_suites().into_iter().find(|s| s.name() == name)
}

/// The first non-blank line of `s` (trimmed), if any.
pub fn first_nonempty_line(s: &str) -> Option<&str> {
    s.lines().map(str::trim).find(|l| !l.is_empty())
}

/// Geometric mean of the scored results (the suites' own scoring), or `None` if nothing
/// scored. Reported as a "Total" carrying the unit of the first result.
pub fn geomean_summary(results: &[BenchResult]) -> Option<BenchResult> {
    let scores: Vec<f64> = results.iter().filter_map(|r| r.score).collect();
    if scores.is_empty() {
        return None;
    }
    let unit = results.first().map_or("", |r| r.unit.as_str());
    let sum_ln: f64 = scores.iter().map(|s| s.ln()).sum();
    let geomean = (sum_ln / scores.len() as f64).exp();
    Some(BenchResult::ok("Total", geomean, unit))
}
