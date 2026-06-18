mod report;
mod runner;
mod suite;
mod suites;

use std::{
    path::{Path, PathBuf},
    process::Command,
};

use clap::{Parser, ValueEnum};

use crate::report::SuiteRun;
use crate::runner::Flamegraph;
use crate::suite::{BenchFilter, RunContext, all_suites, find_suite};

#[derive(Clone, Copy, ValueEnum)]
enum Format {
    Pretty,
    Json,
}

#[derive(Parser)]
#[command(about = "Run standard JS performance suites against the brimstone `bs` engine")]
struct Args {
    /// Suite(s) to run: octane, web-tooling, jetstream, or all. Repeatable.
    #[arg(long, default_values_t = vec!["octane".to_string()])]
    suite: Vec<String>,

    /// Run only the benchmarks with this name. Repeatable.
    #[arg(long)]
    bench: Vec<String>,

    /// Output format.
    #[arg(long, value_enum, default_value_t = Format::Pretty)]
    format: Format,

    /// Write output to this file instead of stdout.
    #[arg(long)]
    out: Option<PathBuf>,

    /// Path to the brimstone executable. If omitted, builds and uses a release build.
    #[arg(long)]
    bs_path: Option<PathBuf>,

    /// Directory where suites are installed. Defaults to the crate's vendor/ directory.
    #[arg(long)]
    vendor_dir: Option<PathBuf>,

    /// Profile each `bs` run with the `flamegraph` CLI (must be on PATH), writing an SVG to
    /// this path (default: flamegraph.svg)
    #[arg(long, num_args = 0..=1, default_missing_value = "flamegraph.svg")]
    flamegraph: Option<PathBuf>,

    /// Extra raw argument forwarded to the `flamegraph` CLI, before the `--` separator. Repeatable.
    #[arg(long)]
    flamegraph_arg: Vec<String>,
}

fn main() {
    let args = Args::parse();

    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(Path::parent)
        .unwrap_or(manifest_dir)
        .to_path_buf();

    let shims_dir = manifest_dir.join("shims");
    let vendor_dir = args
        .vendor_dir
        .clone()
        .unwrap_or_else(|| manifest_dir.join("vendor"));

    let requested = resolve_suites(&args.suite);
    let suite_specs = match requested {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(2);
        }
    };

    let filter = BenchFilter::new(&args.bench);

    // Validate the flamegraph CLI up front rather than failing on the first `bs` spawn.
    let flamegraph_config = match build_flamegraph_config(&args) {
        Ok(cfg) => cfg,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    };

    // Locate or build bs (with debug symbols when profiling, for named frames).
    let bs_path = match resolve_bs_path(&args, &workspace_root, flamegraph_config.is_some()) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    };

    let ctx = RunContext {
        bs_path: &bs_path,
        vendor_dir: &vendor_dir,
        shims_dir: &shims_dir,
        filter: &filter,
        flamegraph: flamegraph_config.as_ref(),
    };

    let mut runs: Vec<SuiteRun> = Vec::new();
    for spec in &suite_specs {
        if !spec.is_available(&ctx) {
            eprintln!(
                "skipping {}: not installed under {} (run tests/perf/install.sh)",
                spec.name(),
                vendor_dir.join(spec.vendor_subdir()).display()
            );
            continue;
        }
        eprintln!("running {}...", spec.name());
        runs.push(spec.run(&ctx));
    }

    if runs.is_empty() {
        eprintln!("no suites ran; nothing to report");
        std::process::exit(1);
    }

    let rendered = match args.format {
        Format::Pretty => report::to_pretty(&runs),
        Format::Json => report::to_json(&runs),
    };

    match &args.out {
        Some(path) => {
            if let Err(e) = std::fs::write(path, rendered) {
                eprintln!("error: failed to write {}: {e}", path.display());
                std::process::exit(1);
            }
            eprintln!("wrote results to {}", path.display());
        }
        None => println!("{rendered}"),
    }
}

/// Resolve suite names (including "all") to specs, in order and de-duplicated.
fn resolve_suites(names: &[String]) -> Result<Vec<Box<dyn suite::SuiteSpec>>, String> {
    let mut selected: Vec<Box<dyn suite::SuiteSpec>> = Vec::new();
    let mut seen: Vec<&str> = Vec::new();

    let push = |spec: Box<dyn suite::SuiteSpec>,
                seen: &mut Vec<&'static str>,
                out: &mut Vec<Box<dyn suite::SuiteSpec>>| {
        if !seen.contains(&spec.name()) {
            seen.push(spec.name());
            out.push(spec);
        }
    };

    for name in names {
        if name == "all" {
            for spec in all_suites() {
                push(spec, &mut seen, &mut selected);
            }
        } else {
            match find_suite(name) {
                Some(spec) => push(spec, &mut seen, &mut selected),
                None => {
                    let known: Vec<&str> = all_suites().iter().map(|s| s.name()).collect();
                    return Err(format!(
                        "unknown suite '{name}'; known suites: {}, all",
                        known.join(", ")
                    ));
                }
            }
        }
    }

    Ok(selected)
}

/// Path to the `bs` binary, building it in release if needed. `with_debug_symbols` adds
/// debug info (for readable flamegraph frames) while keeping release optimizations.
fn resolve_bs_path(
    args: &Args,
    workspace_root: &Path,
    with_debug_symbols: bool,
) -> Result<PathBuf, String> {
    if let Some(path) = &args.bs_path {
        if path.is_file() {
            if with_debug_symbols {
                eprintln!(
                    "note: profiling a pre-built --bs-path; for readable frames it should be \
                     built with debug symbols (e.g. CARGO_PROFILE_RELEASE_DEBUG=true)"
                );
            }
            return Ok(path.clone());
        }
        return Err(format!("--bs-path {} does not exist", path.display()));
    }

    let default = workspace_root.join("target/release/bs");

    let mut cmd = Command::new("cargo");
    cmd.current_dir(workspace_root)
        .args(["build", "--release", "-p", "brimstone"]);
    if with_debug_symbols {
        // Keep release optimizations but emit debug info, so flamegraph frames are named.
        eprintln!("building bs (release + debug symbols)...");
        cmd.env("CARGO_PROFILE_RELEASE_DEBUG", "true");
    } else {
        eprintln!("building bs (release)...");
    }
    let status = cmd
        .status()
        .map_err(|e| format!("failed to run cargo build: {e}"))?;
    if !status.success() {
        return Err("cargo build --release -p brimstone failed".to_string());
    }

    if default.is_file() {
        Ok(default)
    } else {
        Err(format!(
            "bs binary not found at {}; build it or pass --bs-path",
            default.display()
        ))
    }
}

fn build_flamegraph_config(args: &Args) -> Result<Option<Flamegraph>, String> {
    let Some(output) = args.flamegraph.clone() else {
        if !args.flamegraph_arg.is_empty() {
            eprintln!("note: --flamegraph-arg ignored without --flamegraph");
        }
        return Ok(None);
    };

    if !runner::flamegraph_available() {
        return Err("--flamegraph requires the `flamegraph` CLI on PATH; install it with \
             `cargo install flamegraph` (it uses perf on Linux, dtrace on macOS)"
            .to_string());
    }

    Ok(Some(Flamegraph { output, extra_args: args.flamegraph_arg.clone() }))
}
