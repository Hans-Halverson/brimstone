use serde::Serialize;

#[derive(Clone, Copy, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    Ok,
    /// Intentionally not run (e.g. needs an engine feature brimstone lacks).
    Skipped,
    /// Attempted but failed (crash, exception, or missing score in output).
    Error,
}

#[derive(Clone, Serialize)]
pub struct BenchResult {
    pub name: String,
    /// Score as reported by the suite. `None` for skipped/errored.
    pub score: Option<f64>,
    /// Suite-defined unit, e.g. "octane", "runs/s".
    pub unit: String,
    pub status: Status,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
}

impl BenchResult {
    pub fn ok(name: impl Into<String>, score: f64, unit: impl Into<String>) -> Self {
        BenchResult {
            name: name.into(),
            score: Some(score),
            unit: unit.into(),
            status: Status::Ok,
            note: None,
        }
    }

    pub fn skipped(
        name: impl Into<String>,
        unit: impl Into<String>,
        note: impl Into<String>,
    ) -> Self {
        BenchResult {
            name: name.into(),
            score: None,
            unit: unit.into(),
            status: Status::Skipped,
            note: Some(note.into()),
        }
    }

    pub fn error(
        name: impl Into<String>,
        unit: impl Into<String>,
        note: impl Into<String>,
    ) -> Self {
        BenchResult {
            name: name.into(),
            score: None,
            unit: unit.into(),
            status: Status::Error,
            note: Some(note.into()),
        }
    }
}

#[derive(Clone, Serialize)]
pub struct SuiteRun {
    pub suite: String,
    pub bs_path: String,
    pub wall_clock_ms: f64,
    pub results: Vec<BenchResult>,
    /// Overall score for the suite, as reported by the suite itself.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<BenchResult>,
    #[serde(skip)]
    pub pretty_score_precision: usize,
}

pub fn to_json(runs: &[SuiteRun]) -> String {
    serde_json::to_string_pretty(runs).expect("suite runs are serializable")
}

/// Render the suite runs as a human-readable aligned table.
pub fn to_pretty(runs: &[SuiteRun]) -> String {
    let mut out = String::new();
    for run in runs {
        out.push_str(&format!("=== {} ===\n", run.suite));
        out.push_str(&format!("wall clock: {:.0} ms\n\n", run.wall_clock_ms));

        let name_width = run
            .results
            .iter()
            .map(|r| r.name.len())
            .chain(std::iter::once("Benchmark".len()))
            .max()
            .unwrap_or(9)
            .max(9);

        out.push_str(&format!(
            "{:<width$}  {:>14}  {:<8}  {}\n",
            "Benchmark",
            "Score",
            "Status",
            "Note",
            width = name_width
        ));
        out.push_str(&format!("{}\n", "-".repeat(name_width + 14 + 8 + 10)));

        for r in &run.results {
            let status = match r.status {
                Status::Ok => "ok",
                Status::Skipped => "skipped",
                Status::Error => "error",
            };
            out.push_str(&format!(
                "{:<width$}  {:>14}  {:<8}  {}\n",
                r.name,
                format_score(r.score, run.pretty_score_precision),
                status,
                r.note.as_deref().unwrap_or(""),
                width = name_width
            ));
        }

        if let Some(summary) = &run.summary {
            out.push_str(&format!("{}\n", "-".repeat(name_width + 14 + 8 + 10)));
            out.push_str(&format!(
                "{:<width$}  {:>14}  ({})\n",
                summary.name,
                format_score(summary.score, run.pretty_score_precision),
                summary.unit,
                width = name_width
            ));
        }
        out.push('\n');
    }
    out
}

fn format_score(score: Option<f64>, precision: usize) -> String {
    match score {
        None => "-".to_string(),
        Some(score) => format!("{score:.precision$}"),
    }
}
