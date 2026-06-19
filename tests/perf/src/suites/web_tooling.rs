use std::path::Path;

use regex::Regex;

use crate::report::{BenchResult, SuiteRun};
use crate::runner::run_bs;
use crate::suite::{RunContext, SuiteSpec, first_nonempty_line, geomean_summary};

const UNIT: &str = "runs/s";

/// All individual benchmarks in this suite.
const BENCHMARKS: &[&str] = &[
    "acorn",
    "babel",
    "babel-minify",
    "babylon",
    "buble",
    "chai",
    "coffeescript",
    "espree",
    "esprima",
    "jshint",
    "lebab",
    "postcss",
    "prepack",
    "prettier",
    "source-map",
    "terser",
    "typescript",
    "uglify-js",
];

pub struct WebTooling;

impl SuiteSpec for WebTooling {
    fn name(&self) -> &'static str {
        "web-tooling"
    }

    fn vendor_subdir(&self) -> &'static str {
        "web-tooling-benchmark"
    }

    fn is_available(&self, ctx: &RunContext) -> bool {
        // The per-tool bundles are what we run; check the first as a sentinel.
        ctx.suite_dir(self.vendor_subdir())
            .join(bundle_name(BENCHMARKS[0]))
            .is_file()
    }

    fn pretty_score_precision(&self) -> usize {
        2
    }

    fn run(&self, ctx: &RunContext) -> SuiteRun {
        let suite_dir = ctx.suite_dir(self.vendor_subdir());

        // Run each benchmark in its own bundle (a fresh `bs` process). The combined bundle
        // runs all 18 tools in one process, whose cumulative memory exceeds bs's heap by
        // `typescript` and fatally OOMs, dropping the rest; per-tool runs avoid that and
        // isolate failures.
        let mut results = Vec::new();
        let mut wall_clock_ms = 0.0;
        for bench in BENCHMARKS
            .iter()
            .copied()
            .filter(|bench| ctx.filter.matches(bench))
        {
            let bundle = suite_dir.join(bundle_name(bench));
            let (result, ms) = run_one(ctx, bench, &bundle);
            wall_clock_ms += ms;
            results.push(result);
        }

        // Overall geometric mean only for a full run; a --bench subset isn't the suite metric.
        let summary = if ctx.filter.is_empty() {
            geomean_summary(&results)
        } else {
            None
        };
        ctx.suite_run(self, wall_clock_ms, results, summary)
    }
}

/// Path (relative to the vendored dir) of the standalone bundle for `bench`.
fn bundle_name(bench: &str) -> String {
    format!("dist/cli-{bench}.js")
}

fn run_one(ctx: &RunContext, bench: &str, bundle: &Path) -> (BenchResult, f64) {
    if !bundle.is_file() {
        return (
            BenchResult::error(bench, UNIT, "standalone bundle missing; re-run install.sh"),
            0.0,
        );
    }

    let output = match run_bs(
        ctx.bs_path,
        &flags(),
        &[bundle.to_path_buf()],
        ctx.flamegraph(Some(bench)).as_ref(),
    ) {
        Ok(o) => o,
        Err(e) => {
            return (BenchResult::error(bench, UNIT, format!("failed to spawn bs: {e}")), 0.0);
        }
    };

    // Only one benchmark is run so take the single parsed result.
    let (results, _) = parse_web_tooling_output(&output.stdout);
    let result = results.into_iter().next().unwrap_or_else(|| {
        let detail = first_nonempty_line(&output.stderr).unwrap_or("no result produced");
        BenchResult::error(bench, UNIT, detail.to_string())
    });
    (result, output.wall_clock_ms)
}

/// Parse lines of the form `<tool>: <n> runs/s` plus the trailing geometric mean.
fn parse_web_tooling_output(stdout: &str) -> (Vec<BenchResult>, Option<BenchResult>) {
    let regex = Regex::new(r"^\s*(.+?):\s+([\d.]+)\s+runs/s").unwrap();
    let mut results = Vec::new();
    let mut summary = None;

    for line in stdout.lines() {
        if let Some(captures) = regex.captures(line) {
            let name = captures[1].trim().to_string();
            let score: f64 = match captures[2].parse() {
                Ok(s) => s,
                Err(_) => continue,
            };
            if name.eq_ignore_ascii_case("geometric mean") {
                summary = Some(BenchResult::ok("Total", score, UNIT));
            } else {
                results.push(BenchResult::ok(name, score, UNIT));
            }
        }
    }

    (results, summary)
}

fn flags() -> Vec<String> {
    vec!["--expose-test-shell-compat".to_string()]
}
