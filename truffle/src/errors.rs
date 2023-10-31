use std::{fmt, path::Path, slice::Iter};

#[cfg(feature = "lsp")]
use crate::engine::LineLookupTable;
#[cfg(feature = "lsp")]
use lsp_types::Diagnostic;

use crate::parser::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct ScriptError {
    pub message: String,
    pub span: Span,
}

fn write_error(
    fname: &Path,
    script_error: &ScriptError,
    contents: &[u8],
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    let ScriptError { message, span } = script_error;

    let span_start = span.start;
    let span_end = span.end;

    let filename = fname.display();
    let file_span_start = 0;
    let file_span_end = contents.len();

    let (line_number, line_start, line_end) =
        line_extents(contents, span_start, file_span_start, file_span_end);

    let line_number_width = format!("{}", line_number).len();

    let max_number_width = if (line_end + 1) < file_span_end {
        let (next_line_number, _, _) =
            line_extents(contents, line_end + 1, file_span_start, file_span_end);
        format!("{}", next_line_number).len()
    } else {
        line_number_width
    };

    for _ in 0..(max_number_width + 2) {
        write!(f, "─")?;
    }
    writeln!(
        f,
        "┬─ \x1b[0;36m{}:{}:{}\x1b[0m",
        filename,
        line_number,
        span_start - line_start + 1
    )?;

    // Previous line in the source code, if available
    if line_start > (file_span_start + 1) {
        let (prev_line_number, prev_line_start, prev_line_end) =
            line_extents(contents, line_start - 1, file_span_start, file_span_end);
        let prev_line_number_str = format!("{}", prev_line_number);

        for _ in 0..(max_number_width - prev_line_number_str.len()) {
            write!(f, " ")?;
        }

        writeln!(
            f,
            " {} │ {}",
            prev_line_number_str,
            String::from_utf8_lossy(&contents[prev_line_start..prev_line_end])
        )?;
    }

    // Line being highlighted
    for _ in 0..(max_number_width - line_number_width) {
        write!(f, " ")?;
    }

    writeln!(
        f,
        " {} │ {}",
        line_number,
        String::from_utf8_lossy(&contents[line_start..line_end])
    )?;

    for _ in 0..(max_number_width + 2) {
        write!(f, " ")?;
    }
    write!(f, "│")?;
    for _ in 0..(span_start - line_start + 1) {
        write!(f, " ")?;
    }

    write!(f, "\x1b[0;31m")?;
    for _ in span_start..span_end {
        write!(f, "╍")?;
    }
    writeln!(f, " error: {}", message)?;
    write!(f, "\x1b[0m")?;

    // Next line after error, for context
    if (line_end + 1) < file_span_end {
        let (next_line_number, next_line_start, next_line_end) =
            line_extents(contents, line_end + 1, file_span_start, file_span_end);

        writeln!(
            f,
            " {} │ {}",
            next_line_number,
            String::from_utf8_lossy(&contents[next_line_start..next_line_end])
        )?;
    }

    for _ in 0..(max_number_width + 2) {
        write!(f, "─")?;
    }
    writeln!(f, "┴─")?;
    Ok(())
}

impl ScriptError {
    pub fn print_with(&self, fname: &Path, contents: &[u8]) {
        eprintln!("{}", self.display_with(fname, contents));
    }

    pub fn display_with<'a>(
        &'a self,
        fname: &'a Path,
        contents: &'a [u8],
    ) -> ResolvedScriptError<'a> {
        ResolvedScriptError {
            error: self,
            fname,
            contents,
        }
    }
}

pub struct ResolvedScriptError<'a> {
    error: &'a ScriptError,
    fname: &'a Path,
    contents: &'a [u8],
}

impl fmt::Display for ResolvedScriptError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_error(self.fname, self.error, self.contents, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorBatch {
    errors: Vec<ScriptError>,
}

impl ErrorBatch {
    pub fn empty() -> ErrorBatch {
        ErrorBatch { errors: vec![] }
    }

    pub fn one(error: ScriptError) -> ErrorBatch {
        ErrorBatch {
            errors: vec![error],
        }
    }

    pub fn push(&mut self, error: ScriptError) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn assert_contains(&self, message: &str) {
        assert!(
            self.errors.iter().any(|err| err.message.contains(message)),
            "One of the errors contained in this error batch should contain the text \"{}\"",
            message
        );
    }

    pub fn print_with(&self, fname: &Path, contents: &[u8]) {
        eprintln!("{}", self.display_with(fname, contents));
    }

    fn display_with<'a>(&'a self, fname: &'a Path, contents: &'a [u8]) -> ResolvedErrorBatch<'a> {
        ResolvedErrorBatch {
            errors: self,
            fname,
            contents,
        }
    }

    #[cfg(feature = "lsp")]
    pub fn as_diagnostics_with(&self, fname: &Path, contents: &[u8]) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        let lookup = LineLookupTable::from_bytes(contents);
        for error in self {
            let range = lookup.to_range(error.span);
            let message = error.display_with(fname, contents).to_string();
            let diagnostic = Diagnostic::new_simple(range, message);
            diagnostics.push(diagnostic);
        }
        diagnostics
    }
}

pub struct ResolvedErrorBatch<'a> {
    errors: &'a ErrorBatch,
    fname: &'a Path,
    contents: &'a [u8],
}

impl ResolvedErrorBatch<'_> {}

impl fmt::Display for ResolvedErrorBatch<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for script_error in self.errors {
            let resolved = script_error.display_with(self.fname, self.contents);
            writeln!(f, "{}", resolved)?;
        }

        Ok(())
    }
}

impl From<ScriptError> for ErrorBatch {
    fn from(value: ScriptError) -> Self {
        let mut batch = Self::empty();
        batch.push(value);
        batch
    }
}

impl<'a> IntoIterator for &'a ErrorBatch {
    type Item = &'a ScriptError;

    type IntoIter = Iter<'a, ScriptError>;

    fn into_iter(self) -> Self::IntoIter {
        (self.errors).iter()
    }
}

// line number, line start, line_end
pub fn line_extents(
    contents: &[u8],
    span_position: usize,
    file_span_start: usize,
    file_span_end: usize,
) -> (usize, usize, usize) {
    let line_number = contents[0..span_position].split(|x| *x == b'\n').count();

    let mut line_start = span_position;
    if line_start > file_span_start && contents[line_start] == b'\n' {
        line_start -= 1;
    }

    while line_start > file_span_start && contents[line_start] != b'\n' {
        line_start -= 1;
    }
    if contents[line_start] == b'\n' {
        line_start += 1;
    }

    let mut line_end = span_position;
    while line_end < file_span_end && contents[line_end] != b'\n' {
        line_end += 1;
    }

    (line_number, line_start, line_end)
}
