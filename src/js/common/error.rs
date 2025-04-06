use std::rc::Rc;

use crate::parser::{
    loc::{find_line_col_for_pos, Pos},
    source::Source,
};

use super::terminal::{BOLD, DEFAULT_COLOR, DIM, RED, RESET};

pub fn print_error_message_and_exit(message: &str) {
    eprintln!("{}", message);
    std::process::exit(1);
}

/// A configurable formatter for creating error messages.
pub struct ErrorFormatter {
    /// Type of this error (e.g. TypeError, SyntaxError, Error, etc.)
    kind: String,
    /// Error message (e.g. "Unknown token")
    message: Option<String>,
    /// Formatted stack frames, if any exist
    stack_trace: Option<String>,
    /// Source info for this error, including source file, line, column, and snippet.
    source_info: Option<SourceInfo>,
    /// Whether to use color in the output
    use_color: bool,
    /// The error message builder
    builder: String,
}

pub struct SourceInfo {
    /// Name of the source to display.
    name: String,
    /// Line number in the source file.
    line: usize,
    /// Column number in the source file.
    col: usize,
    /// Extracted snippet from the source file.
    snippet: String,
}

impl SourceInfo {
    pub fn new(name: String, line: usize, col: usize, snippet: String) -> Self {
        Self { name, line, col, snippet }
    }

    pub fn new_static(source: &Rc<Source>, pos: Pos) -> Self {
        let name = source.display_name().to_string();

        // Find line and column number for the start of the source location
        let offsets = source.line_offsets();
        let (line, col) = find_line_col_for_pos(pos, offsets);

        // Extract the full line as a snippet
        let snippet = source.get_line((line - 1) as u32);

        Self::new(name, line, col, snippet)
    }
}

impl ErrorFormatter {
    pub fn new(kind: String, opts: &FormatOptions) -> Self {
        Self {
            kind,
            message: None,
            stack_trace: None,
            source_info: None,
            use_color: opts.use_color,
            builder: String::new(),
        }
    }

    pub fn set_message(&mut self, message: String) {
        self.message = Some(message);
    }

    pub fn set_stack_trace(&mut self, stack_trace: String) {
        self.stack_trace = Some(stack_trace);
    }

    pub fn set_source_info(&mut self, source_info: SourceInfo) {
        self.source_info = Some(source_info);
    }

    fn reset(&mut self) {
        if self.use_color {
            self.builder.push_str(RESET);
        }
    }

    fn bold(&mut self) {
        if self.use_color {
            self.builder.push_str(BOLD);
        }
    }

    fn dim(&mut self) {
        if self.use_color {
            self.builder.push_str(DIM);
        }
    }

    fn red(&mut self) {
        if self.use_color {
            self.builder.push_str(RED);
        }
    }

    fn default_color(&mut self) {
        if self.use_color {
            self.builder.push_str(DEFAULT_COLOR);
        }
    }

    fn indent(&mut self, indent: u32) {
        for _ in 0..indent {
            self.builder.push(' ');
        }
    }

    pub fn build(mut self) -> String {
        // Kind is in bold and red
        self.bold();
        self.red();
        self.builder.push_str(&self.kind);

        // Message is in bold and default color, if one exists
        if self.message.is_some() {
            self.default_color();
            self.builder.push_str(": ");
            self.builder.push_str(self.message.as_ref().unwrap());
        }

        self.reset();
        self.builder.push('\n');

        // If there is a source location then print it
        let has_source_info = self.source_info.is_some();
        if let Some(SourceInfo { name, line, col, snippet }) = self.source_info.take() {
            // Calculate indent containing the line number with a space of padding on the right
            let indent = line.ilog10() + 2;

            // Write source name and line/col numbers, after the indent and a divider
            self.indent(indent);
            self.dim();
            self.builder.push_str("┌ ");
            self.reset();

            self.builder
                .push_str(&format!("{}:{}:{}\n", name, line, col));

            // Write a line of padding above the snippet
            self.snippet_padding(indent);
            self.builder.push('\n');

            // Write the line number and separator before the snippet
            self.dim();
            self.builder.push_str(&format!("{} | ", line));
            self.reset();

            // Write the snippet itself
            self.builder.push_str(&format!("{}\n", snippet));

            // Write a line of padding below the snippet
            self.snippet_padding(indent);

            // Write the column indicator
            self.indent(col as u32);
            self.bold();
            self.red();
            self.builder.push('^');
            self.reset();
        }

        // Finally write the stack trace if one exists
        if let Some(stack_trace) = self.stack_trace {
            if has_source_info {
                self.builder.push_str("\nStack trace:\n");
            }

            self.builder.push_str(&stack_trace);
        }

        self.builder
    }

    fn snippet_padding(&mut self, indent: u32) {
        self.indent(indent);
        self.dim();
        self.builder.push('|');
        self.reset();
    }
}

#[derive(Default)]
pub struct FormatOptions {
    // Whether to use color in the output
    pub use_color: bool,
}

impl FormatOptions {
    pub fn new(use_color: bool) -> Self {
        Self { use_color }
    }
}
