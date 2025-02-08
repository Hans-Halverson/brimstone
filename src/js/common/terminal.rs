use super::options::Options;

/// Detect if the terminal supports colors on stdout.
fn detect_stdout_supports_colors() -> bool {
    if let Some(support) = supports_color::on_cached(supports_color::Stream::Stdout) {
        support.has_basic
    } else {
        false
    }
}

/// Whether we should use colors when printing to stdout. Based on the CLI flags and detecting if
/// the terminal supports colors.
pub fn stdout_should_use_colors(options: &Options) -> bool {
    !options.no_color && detect_stdout_supports_colors()
}

/// Detect if the terminal supports colors on stderr.
fn detect_stderr_supports_colors() -> bool {
    if let Some(support) = supports_color::on_cached(supports_color::Stream::Stderr) {
        support.has_basic
    } else {
        false
    }
}

/// Whether we should use colors when printing to stderr. Based on the CLI flags and detecting if
/// the terminal supports colors.
pub fn stderr_should_use_colors(options: &Options) -> bool {
    !options.no_color && detect_stderr_supports_colors()
}

/// Reset all color and style attributes
pub const RESET: &str = "\x1b[0m";

/// Styles
pub const BOLD: &str = "\x1b[1m";
pub const DIM: &str = "\x1b[2m";

/// Colors
pub const RED: &str = "\x1b[31m";
pub const DEFAULT_COLOR: &str = "\x1b[39m";
