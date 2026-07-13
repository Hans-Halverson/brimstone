use std::{path::Path, time::Duration};

use serde_json::Value as Json;

use crate::report::{BenchResult, SuiteRun};
use crate::runner::run_bs_with_timeout;
use crate::suite::{RunContext, SuiteSpec, first_nonempty_line, geomean_summary};

const UNIT: &str = "jetstream";

/// JetStream's shell entrypoint.
const CLI: &str = "cli.js";

/// `iteration-count` must exceed `worst-case-count`; kept modest so a full run is tractable.
const ITERATION_COUNT: &str = "--iteration-count=8";
const WORST_CASE_COUNT: &str = "--worst-case-count=2";

const BENCH_TIMEOUT: Duration = Duration::from_secs(90);

/// All individual benchmarks in this suite.
const BENCHMARKS: &[&str] = &[
    "8bitbench-wasm",
    "acorn-wtb",
    "ai-astar",
    "Air",
    "argon2-wasm",
    "async-fs",
    "babel-minify-wtb",
    "babel-wtb",
    "Babylon",
    "babylon-wtb",
    "babylonjs-scene-es6",
    "babylonjs-startup-es6",
    "Basic",
    "bigint-noble-ed25519",
    "Box2D",
    "cdjs",
    "chai-wtb",
    "crypto",
    "Dart-flute-todomvc-wasm",
    "delta-blue",
    "dotnet-aot-wasm",
    "dotnet-interp-wasm",
    "doxbee-async",
    "doxbee-promise",
    "earley-boyer",
    "espree-wtb",
    "esprima-next-wtb",
    "first-inspector-code-load",
    "FlightPlanner",
    "gaussian-blur",
    "gbemu",
    "hash-map",
    "j2cl-box2d-wasm",
    "js-tokens",
    "jsdom-d3-startup",
    "json-parse-inspector",
    "json-stringify-inspector",
    "Kotlin-compose-wasm",
    "lazy-collections",
    "mandreel",
    "ML",
    "mobx-startup",
    "multi-inspector-code-load",
    "navier-stokes",
    "octane-code-load",
    "OfflineAssembler",
    "pdfjs",
    "postcss-wtb",
    "prettier-wtb",
    "prismjs-startup-es6",
    "proxy-mobx",
    "proxy-vue",
    "raytrace",
    "raytrace-private-class-fields",
    "raytrace-public-class-fields",
    "regexp-octane",
    "richards",
    "richards-wasm",
    "source-map-wtb",
    "splay",
    "sqlite3-wasm",
    "stanford-crypto-aes",
    "stanford-crypto-pbkdf2",
    "stanford-crypto-sha256",
    "Sunspider",
    "sync-fs",
    "threejs",
    "transformersjs-bert-wasm",
    "tsf-wasm",
    "typescript-lib",
    "UniPoker",
    "validatorjs",
    "web-ssr",
    "WSL",
    "zlib-wasm",
];

pub struct JetStream;

impl SuiteSpec for JetStream {
    fn name(&self) -> &'static str {
        "jetstream"
    }

    fn vendor_subdir(&self) -> &'static str {
        "jetstream"
    }

    fn is_available(&self, ctx: &RunContext) -> bool {
        ctx.suite_dir(self.vendor_subdir()).join(CLI).is_file()
    }

    fn pretty_score_precision(&self) -> usize {
        2
    }

    fn run(&self, ctx: &RunContext) -> SuiteRun {
        let cli = ctx.suite_dir(self.vendor_subdir()).join(CLI);

        let selected: Vec<&str> = BENCHMARKS
            .iter()
            .copied()
            .filter(|name| ctx.filter.matches(name))
            .collect();
        let mut results = Vec::new();
        let mut wall_clock_ms = 0.0;
        for (index, name) in selected.iter().enumerate() {
            eprintln!("  [{}/{}] {name}", index + 1, selected.len());
            let (result, ms) = run_one(ctx, &cli, name);
            wall_clock_ms += ms;
            results.push(result);
        }

        let summary = geomean_summary(&results);

        ctx.suite_run(self, wall_clock_ms, results, summary)
    }
}

fn run_one(ctx: &RunContext, cli: &Path, name: &str) -> (BenchResult, f64) {
    let script_args = [
        format!("--test={name}"),
        "--no-prefetch".to_string(),
        "--dump-json-results".to_string(),
        "--force-gc".to_string(),
        ITERATION_COUNT.to_string(),
        WORST_CASE_COUNT.to_string(),
    ];

    let flamegraph = ctx.flamegraph(Some(name));
    let output = match run_bs_with_timeout(
        ctx.bs_path,
        &shell_flags(),
        &[cli.to_path_buf()],
        &script_args,
        Some(BENCH_TIMEOUT),
        flamegraph.as_ref(),
    ) {
        Ok(output) => output,
        Err(e) => return (BenchResult::error(name, UNIT, format!("failed to spawn bs: {e}")), 0.0),
    };

    if let Some(score) = parse_score(&output.stdout, name) {
        return (BenchResult::ok(name, score, UNIT), output.wall_clock_ms);
    }

    if output.timed_out {
        return (
            BenchResult::error(name, UNIT, format!("timed out after {BENCH_TIMEOUT:?}")),
            output.wall_clock_ms,
        );
    }

    // WebAssembly benchmarks are skipped, anything else is a hard error.
    let detail = failure_detail(&output.stdout, &output.stderr);
    let result = if name.ends_with("-wasm") || detail.contains("WebAssembly") {
        BenchResult::skipped(name, UNIT, "requires WebAssembly")
    } else {
        BenchResult::error(name, UNIT, detail)
    };
    (result, output.wall_clock_ms)
}

/// Score lives at `<JetStreamVersion>.tests.<name>.metrics.Score.current[0]`.
fn parse_score(stdout: &str, name: &str) -> Option<f64> {
    let line = stdout
        .lines()
        .map(str::trim)
        .find(|line| line.starts_with('{') && line.contains("JetStream"))?;
    let json: Json = serde_json::from_str(line).ok()?;

    // The top-level key carries a version (e.g. "JetStream3.0"); take the first object.
    let root = json.as_object()?.values().next()?;
    root.get("tests")?
        .get(name)?
        .get("metrics")?
        .get("Score")?
        .get("current")?
        .get(0)?
        .as_f64()
}

fn failure_detail(stdout: &str, stderr: &str) -> String {
    stdout
        .lines()
        .map(str::trim)
        .find(|l| l.contains("failed:") || l.contains("Error"))
        .or_else(|| first_nonempty_line(stderr))
        .or_else(|| first_nonempty_line(stdout))
        .unwrap_or("no score produced")
        .to_string()
}

fn shell_flags() -> Vec<String> {
    // babylonjs-scene-es6 requires 2GB of heap to run
    vec!["--expose-test-shell-compat".to_string(), "--max-heap-size=2GB".to_string()]
}
