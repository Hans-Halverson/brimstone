use std::path::PathBuf;

use crate::report::{BenchResult, SuiteRun};
use crate::runner::run_bs;
use crate::suite::{RunContext, SuiteSpec, first_nonempty_line};

const UNIT: &str = "octane";

/// Octane 2.0 benchmark groups and their source files, in load order (mirrors Octane's
/// `run.js`). A group runs only if all its files are present in the vendored checkout.
const BENCHMARKS: &[(&str, &[&str])] = &[
    ("Richards", &["richards.js"]),
    ("DeltaBlue", &["deltablue.js"]),
    ("Crypto", &["crypto.js"]),
    ("RayTrace", &["raytrace.js"]),
    ("EarleyBoyer", &["earley-boyer.js"]),
    ("RegExp", &["regexp.js"]),
    ("Splay", &["splay.js"]),
    ("NavierStokes", &["navier-stokes.js"]),
    ("PdfJS", &["pdfjs.js"]),
    ("Mandreel", &["mandreel.js"]),
    ("Gameboy", &["gbemu-part1.js", "gbemu-part2.js"]),
    ("CodeLoad", &["code-load.js"]),
    ("Box2D", &["box2d.js"]),
    ("zlib", &["zlib.js", "zlib-data.js"]),
    (
        "Typescript",
        &[
            "typescript.js",
            "typescript-input.js",
            "typescript-compiler.js",
        ],
    ),
];

pub struct Octane;

impl SuiteSpec for Octane {
    fn name(&self) -> &'static str {
        "octane"
    }

    fn vendor_subdir(&self) -> &'static str {
        "octane"
    }

    fn is_available(&self, ctx: &RunContext) -> bool {
        ctx.suite_dir(self.vendor_subdir())
            .join("base.js")
            .is_file()
    }

    fn pretty_score_precision(&self) -> usize {
        0
    }

    fn run(&self, ctx: &RunContext) -> SuiteRun {
        let suite_dir = ctx.suite_dir(self.vendor_subdir());

        // Loaded in order: base.js, the selected benchmark files, then the driver.
        let mut files: Vec<PathBuf> = vec![suite_dir.join("base.js")];
        let mut skipped: Vec<BenchResult> = Vec::new();
        let mut loaded_groups = 0;

        for (group, group_files) in BENCHMARKS {
            if !ctx.filter.matches(group) {
                continue;
            }

            let paths: Vec<PathBuf> = group_files.iter().map(|f| suite_dir.join(f)).collect();
            if paths.iter().all(|p| p.is_file()) {
                files.extend(paths);
                loaded_groups += 1;
            } else {
                skipped.push(BenchResult::skipped(
                    *group,
                    UNIT,
                    "benchmark source not found in vendored checkout",
                ));
            }
        }

        // Don't spawn bs with no benchmarks; the driver would run zero suites.
        if loaded_groups == 0 {
            return ctx.suite_run(self, 0.0, skipped, None);
        }

        files.push(ctx.shim("octane.js"));

        let flags = vec!["--expose-test-shell-compat".to_string()];
        let output = match run_bs(ctx.bs_path, &flags, &files, ctx.flamegraph(None).as_ref()) {
            Ok(o) => o,
            Err(e) => {
                return ctx.single_error_run(self, UNIT, format!("failed to spawn bs: {e}"));
            }
        };

        let (mut results, summary) = parse_octane_output(&output.stdout);

        if summary.is_none() && !output.success {
            let detail = first_nonempty_line(&output.stderr)
                .unwrap_or("bs exited unsuccessfully with no score");
            results.push(BenchResult::error("<run>", UNIT, detail.to_string()));
        }

        results.extend(skipped);

        ctx.suite_run(self, output.wall_clock_ms, results, summary)
    }
}

/// Parse the `RESULT`/`ERROR`/`SCORE` lines emitted by octane.js
fn parse_octane_output(stdout: &str) -> (Vec<BenchResult>, Option<BenchResult>) {
    let mut results = Vec::new();
    let mut summary = None;

    for line in stdout.lines() {
        let mut parts = line.splitn(3, ' ');
        match parts.next() {
            Some("RESULT") => {
                if let (Some(name), Some(score)) = (parts.next(), parts.next()) {
                    if let Ok(score) = score.trim().parse::<f64>() {
                        results.push(BenchResult::ok(name, score, UNIT));
                    }
                }
            }
            Some("ERROR") => {
                if let Some(name) = parts.next() {
                    let msg = parts.next().unwrap_or("").to_string();
                    results.push(BenchResult::error(name, UNIT, msg));
                }
            }
            Some("SCORE") => {
                if let Some(score) = parts.next() {
                    if let Ok(score) = score.trim().parse::<f64>() {
                        summary = Some(BenchResult::ok("Total", score, UNIT));
                    }
                }
            }
            _ => {}
        }
    }

    (results, summary)
}
