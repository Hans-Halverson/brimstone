//! A complete formatter for brimstone: rustfmt the whole tree like cargo fmt,
//! then format the top-level `{ }` blocks inside `MACRO_NAMES` invocations that
//! rustfmt leaves untouched. `--install` installs the pinned toolchain first;
//! `--check` reports instead of writing.

use std::{collections::HashSet, io::Write, path::{Path, PathBuf}, process::{Command, Stdio}};

use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};

/// Pinned toolchain.
const RUSTFMT_TOOLCHAIN: &str = "nightly-2026-06-02";

/// Macros whose top-level `{ }` blocks we format.
const MACRO_NAMES: &[&str] = &["runtime_fn"];

/// Throwaway fn wrapping each block so it can be recovered from rustfmt's output.
const MARKER: &str = "__runtime_fmt_block_";

fn main() {
    let mut check = false;
    let mut install = false;
    let mut paths = Vec::new();
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--check" => check = true,
            "--install" => install = true,
            _ => paths.push(PathBuf::from(arg)),
        }
    }
    if paths.is_empty() {
        paths.push(PathBuf::from("src"));
        paths.push(PathBuf::from("tests"));
    }

    if install && let Err(e) = install_toolchain() {
        eprintln!("error: {e}");
        std::process::exit(1);
    }

    let config = find_rustfmt_toml();
    let ignored = ignored_dirs();
    let mut files = Vec::new();
    for p in &paths {
        collect_rs_files(p, &ignored, &mut files);
    }
    if files.is_empty() {
        return;
    }

    // Pass 1: rustfmt the whole files like cargo fmt (also surfaces syntax errors).
    let mut failed = !run_fmt(&files, config.as_deref(), check);

    // Pass 2: format the macro blocks rustfmt leaves alone, in the now-formatted
    // files. Files that don't lex were already reported by pass 1.
    let mut entries: Vec<(PathBuf, String, Vec<Block>)> = Vec::new();
    for file in files {
        let Ok(src) = std::fs::read_to_string(&file) else { continue };
        let Ok(ts) = src.parse::<TokenStream>() else { continue };
        let mut blocks = Vec::new();
        find_blocks(ts, &src, &mut blocks);
        if !blocks.is_empty() {
            blocks.sort_by_key(|b| b.inner.0);
            entries.push((file, src, blocks));
        }
    }
    if !entries.is_empty() {
        match format_and_write(&entries, config.as_deref(), check) {
            Ok(changed) => failed |= changed,
            Err(e) => {
                eprintln!("error: {e}");
                failed = true;
            }
        }
    }

    if failed {
        std::process::exit(1);
    }
}

/// Format every block (one rustfmt pass) and write each changed file, or report
/// it under `--check`. Returns whether anything would change (for `--check`).
fn format_and_write(
    entries: &[(PathBuf, String, Vec<Block>)],
    config: Option<&Path>,
    check: bool,
) -> Result<bool, String> {
    let all: Vec<&Block> = entries.iter().flat_map(|(_, _, b)| b).collect();
    let formatted = format_blocks(&all, config)?;

    let mut base = 0;
    let mut changed = false;
    for (path, src, blocks) in entries {
        let mut out = src.clone();
        for (j, b) in blocks.iter().enumerate().rev() {
            out.replace_range(b.inner.0..b.inner.1, &render_block(&formatted[base + j], &b.indent));
        }
        base += blocks.len();
        if out == *src {
            continue;
        }
        changed = true;
        if check {
            println!("would reformat: {}", path.display());
        } else {
            std::fs::write(path, &out).map_err(|e| format!("{}: {e}", path.display()))?;
            println!("reformatted: {}", path.display());
        }
    }
    Ok(changed)
}

/// A top-level `{ }` block of a matched macro invocation.
struct Block {
    /// Byte range between the braces.
    inner: (usize, usize),
    /// Indent of the opening brace's line; the body is formatted at `indent + 4`.
    indent: String,
    /// Verbatim inner source (rustfmt input).
    content: String,
}

impl Block {
    fn new(src: &str, g: &Group) -> Self {
        let inner = (g.span_open().byte_range().end, g.span_close().byte_range().start);
        let indent = line_indent(src, g.span_open().byte_range().start);
        Self { content: src[inner.0..inner.1].to_string(), inner, indent }
    }

    /// 4-space indentation levels to format the body at.
    fn levels(&self) -> usize {
        self.indent.len() / 4
    }
}

/// Record the top-level `{ }` blocks of every matched macro invocation,
/// recursing into all other groups.
fn find_blocks(ts: TokenStream, src: &str, out: &mut Vec<Block>) {
    let toks: Vec<TokenTree> = ts.into_iter().collect();
    let mut i = 0;
    while i < toks.len() {
        if let TokenTree::Ident(id) = &toks[i]
            && MACRO_NAMES.contains(&id.to_string().as_str())
            && let (Some(TokenTree::Punct(p)), Some(TokenTree::Group(args))) =
                (toks.get(i + 1), toks.get(i + 2))
            && p.as_char() == '!'
        {
            // Each top-level brace group is a block; skip macro_rules! fragments
            // ($-metavariables can't be formatted as standalone Rust).
            for tt in args.stream() {
                if let TokenTree::Group(g) = tt
                    && g.delimiter() == Delimiter::Brace
                    && !contains_dollar(g.stream())
                {
                    out.push(Block::new(src, &g));
                }
            }
            i += 3;
            continue;
        }
        if let TokenTree::Group(g) = &toks[i] {
            find_blocks(g.stream(), src, out);
        }
        i += 1;
    }
}

/// Format all blocks in one rustfmt pass, each wrapped in a throwaway fn nested
/// in `levels` `mod`s so it sits at its true column.
fn format_blocks(blocks: &[&Block], config: Option<&Path>) -> Result<Vec<Vec<String>>, String> {
    let mut input = String::new();
    for (i, b) in blocks.iter().enumerate() {
        for l in 0..b.levels() {
            input.push_str(&format!("mod __w{i}_{l} {{\n"));
        }
        input.push_str(&format!("fn {MARKER}{i}() {{\n{}\n}}\n", b.content));
        for _ in 0..b.levels() {
            input.push_str("}\n");
        }
    }
    Ok(split(&run_rustfmt(&input, config)?, blocks.len()))
}

/// Recover each block's formatted body lines from rustfmt's output by marker.
fn split(stdout: &str, n: usize) -> Vec<Vec<String>> {
    let prefix = format!("fn {MARKER}");
    let lines: Vec<&str> = stdout.lines().collect();
    let mut out = vec![Vec::new(); n];
    let mut i = 0;
    while i < lines.len() {
        let trimmed = lines[i].trim_start();
        if let Some(rest) = trimmed.strip_prefix(&prefix) {
            let idx: usize = rest.split('(').next().unwrap().parse().unwrap();
            // An empty body collapses to `fn ...() {}`; otherwise gather lines up
            // to the closing brace at the fn's own indentation.
            if !lines[i].ends_with("{}") {
                let close = format!("{}}}", &lines[i][..lines[i].len() - trimmed.len()]);
                let start = i + 1;
                i = start;
                while i < lines.len() && lines[i] != close {
                    i += 1;
                }
                out[idx] = lines[start..i].iter().map(|s| s.to_string()).collect();
            }
        }
        i += 1;
    }
    out
}

/// Block replacement `\n<body>\n<indent>` between the kept braces; empty → `{}`.
fn render_block(lines: &[String], indent: &str) -> String {
    if lines.is_empty() {
        String::new()
    } else {
        format!("\n{}\n{indent}", lines.join("\n"))
    }
}

/// Whether any token (recursively) is a `$` macro metavariable.
fn contains_dollar(ts: TokenStream) -> bool {
    ts.into_iter().any(|t| match t {
        TokenTree::Punct(p) => p.as_char() == '$',
        TokenTree::Group(g) => contains_dollar(g.stream()),
        _ => false,
    })
}

/// Leading whitespace of the line containing byte offset `pos`.
fn line_indent(src: &str, pos: usize) -> String {
    let start = src[..pos].rfind('\n').map_or(0, |n| n + 1);
    src[start..pos].chars().take_while(|&c| c == ' ' || c == '\t').collect()
}

/// Install the pinned toolchain and its rustfmt component (matching CI).
fn install_toolchain() -> Result<(), String> {
    let ok = Command::new("rustup")
        .args(["toolchain", "install", RUSTFMT_TOOLCHAIN, "--profile", "minimal", "--component", "rustfmt"])
        .status()
        .map_err(|e| format!("running rustup: {e}"))?
        .success();
    if ok { Ok(()) } else { Err(format!("rustup could not install {RUSTFMT_TOOLCHAIN}")) }
}

/// The pinned rustfmt command with the project + CI config applied.
fn rustfmt(config: Option<&Path>) -> Command {
    let mut cmd = Command::new("rustup");
    cmd.args(["run", RUSTFMT_TOOLCHAIN, "rustfmt", "--edition", "2024"]);
    if let Some(p) = config {
        cmd.arg("--config-path").arg(p);
    }
    cmd.arg("--config").arg("group_imports=StdExternalCrate");
    cmd
}

/// rustfmt the whole files like cargo fmt — writing in place, or `--check`. Its
/// output (diffs, plus benign config warnings) is shown only on failure.
fn run_fmt(files: &[PathBuf], config: Option<&Path>, check: bool) -> bool {
    let mut cmd = rustfmt(config);
    if check {
        cmd.arg("--check");
    }
    match cmd.args(files).output() {
        Ok(o) if o.status.success() => true,
        Ok(o) => {
            print!("{}", String::from_utf8_lossy(&o.stdout));
            eprint!("{}", String::from_utf8_lossy(&o.stderr));
            false
        }
        Err(e) => {
            eprintln!("error: running rustfmt: {e}");
            false
        }
    }
}

/// Pipe `input` through the pinned rustfmt and return its formatted output.
fn run_rustfmt(input: &str, config: Option<&Path>) -> Result<String, String> {
    let mut cmd = rustfmt(config);
    cmd.args(["--emit", "stdout"]);
    cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = cmd.spawn().map_err(|e| format!("spawn rustfmt: {e}"))?;

    // A write failure just means rustfmt already exited (parse error, missing
    // toolchain); ignore it so its real stderr below isn't masked by "broken pipe".
    let _ = child.stdin.take().unwrap().write_all(input.as_bytes());

    let out = child.wait_with_output().map_err(|e| format!("running rustfmt: {e}"))?;
    if out.status.success() {
        Ok(String::from_utf8_lossy(&out.stdout).into_owned())
    } else {
        Err(format!("rustfmt failed:\n{}", String::from_utf8_lossy(&out.stderr).trim()))
    }
}

/// The nearest `rustfmt.toml` at or above the current directory.
fn find_rustfmt_toml() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    loop {
        if dir.join("rustfmt.toml").exists() {
            return Some(dir.join("rustfmt.toml"));
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Collect `.rs` files under `path` (or `path` itself if it's a file), skipping
/// `target`, hidden, and git-ignored directories (e.g. the vendored test262
/// checkout and fuzz build output under `tests`).
fn collect_rs_files(path: &Path, ignored: &HashSet<PathBuf>, out: &mut Vec<PathBuf>) {
    if path.is_dir() {
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if name == "target"
            || name.starts_with('.')
            || std::fs::canonicalize(path).is_ok_and(|c| ignored.contains(&c))
        {
            return;
        }

        let Ok(entries) = std::fs::read_dir(path) else { return };
        for entry in entries.flatten() {
            collect_rs_files(&entry.path(), ignored, out);
        }
    } else if path.extension().is_some_and(|e| e == "rs") {
        out.push(path.to_path_buf());
    }
}

/// Paths of all directories the formatter should ignore.
fn ignored_dirs() -> HashSet<PathBuf> {
    let mut set = HashSet::new();
    let Ok(out) = Command::new("git")
        .args(["ls-files", "--others", "--ignored", "--exclude-standard", "--directory", "-z"])
        .output()
    else {
        return set;
    };
    if !out.status.success() {
        return set;
    }
    for entry in out.stdout.split(|&b| b == 0) {
        // `--directory` reports each fully-ignored dir once, with a trailing `/`.
        let s = String::from_utf8_lossy(entry);
        if let Some(dir) = s.strip_suffix('/')
            && let Ok(canon) = std::fs::canonicalize(dir)
        {
            set.insert(canon);
        }
    }
    set
}
