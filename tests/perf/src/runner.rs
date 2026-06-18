use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, Instant},
};

pub struct BsOutput {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
    pub wall_clock_ms: f64,
    pub timed_out: bool,
}

/// Profiles `bs` via the [`flamegraph`](https://github.com/flamegraph-rs/flamegraph) CLI,
/// writing an SVG to `output`.
#[derive(Clone)]
pub struct Flamegraph {
    pub output: PathBuf,
    pub extra_args: Vec<String>,
}

impl Flamegraph {
    pub fn labeled(&self, label: &str) -> Flamegraph {
        let sanitized: String = label
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '-'
                }
            })
            .collect();
        let stem = self
            .output
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("flamegraph");
        let ext = self
            .output
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("svg");
        Flamegraph {
            output: self
                .output
                .with_file_name(format!("{stem}.{sanitized}.{ext}")),
            extra_args: self.extra_args.clone(),
        }
    }
}

pub fn flamegraph_available() -> bool {
    Command::new("flamegraph")
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

pub fn run_bs(
    bs_path: &Path,
    flags: &[String],
    files: &[PathBuf],
    flamegraph: Option<&Flamegraph>,
) -> std::io::Result<BsOutput> {
    run_bs_with_timeout(bs_path, flags, files, &[], None, flamegraph)
}

pub fn run_bs_with_timeout(
    bs_path: &Path,
    flags: &[String],
    files: &[PathBuf],
    script_args: &[String],
    timeout: Option<Duration>,
    flamegraph: Option<&Flamegraph>,
) -> std::io::Result<BsOutput> {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    let base = std::env::temp_dir().join(format!("bs-perf-{}-{}", std::process::id(), id));
    let out_path = base.with_extension("out");
    let err_path = base.with_extension("err");

    // `flamegraph -- bs <args>`: flamegraph forwards everything after `--` to bs verbatim,
    // including the second `--` that hands `script_args` to bs.
    let mut cmd = match flamegraph {
        Some(fg) => {
            eprintln!("    profiling -> {}", fg.output.display());
            let mut cmd = Command::new("flamegraph");
            cmd.arg("-o").arg(&fg.output);
            cmd.args(&fg.extra_args);
            cmd.arg("--");
            cmd.arg(bs_path);
            cmd
        }
        None => Command::new(bs_path),
    };
    cmd.args(flags);
    for file in files {
        cmd.arg(file);
    }
    if !script_args.is_empty() {
        cmd.arg("--");
        cmd.args(script_args);
    }
    cmd.stdout(std::fs::File::create(&out_path)?)
        .stderr(std::fs::File::create(&err_path)?);

    let start = Instant::now();
    let mut child = cmd.spawn()?;

    let mut timed_out = false;
    let status = loop {
        if let Some(status) = child.try_wait()? {
            break status;
        }
        if let Some(timeout) = timeout {
            if start.elapsed() > timeout {
                let _ = child.kill();
                timed_out = true;
                break child.wait()?;
            }
        }
        std::thread::sleep(Duration::from_millis(50));
    };

    let wall_clock_ms = start.elapsed().as_secs_f64() * 1000.0;
    let stdout = std::fs::read_to_string(&out_path).unwrap_or_default();
    let mut stderr = std::fs::read_to_string(&err_path).unwrap_or_default();
    let _ = std::fs::remove_file(&out_path);
    let _ = std::fs::remove_file(&err_path);

    if timed_out {
        stderr.push_str(&format!("\nkilled: exceeded {:?} timeout", timeout.unwrap()));
    }

    Ok(BsOutput {
        stdout,
        stderr,
        success: status.success() && !timed_out,
        wall_clock_ms,
        timed_out,
    })
}
