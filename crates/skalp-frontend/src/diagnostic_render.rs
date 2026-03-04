//! Shared diagnostic rendering using codespan-reporting.
//!
//! Provides rustc-style error output with source context, line numbers,
//! and underlined spans for both the `.sk` and VHDL frontends.

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{Buffer, ColorChoice, StandardStream};

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

/// A single diagnostic to render.
pub struct DiagnosticMessage<'a> {
    pub severity: Severity,
    pub message: &'a str,
    pub start: usize,
    pub end: usize,
    /// Additional labeled spans: (start, end, label_text)
    pub labels: &'a [(usize, usize, &'a str)],
}

/// Thin wrapper around `codespan-reporting` that renders diagnostics
/// with source context, line numbers, and underlined spans.
pub struct DiagnosticRenderer {
    files: SimpleFiles<String, String>,
    file_id: usize,
}

impl DiagnosticRenderer {
    /// Create a renderer for a single source file.
    pub fn new(filename: &str, source: &str) -> Self {
        let mut files = SimpleFiles::new();
        let file_id = files.add(filename.to_string(), source.to_string());
        Self { files, file_id }
    }

    /// Render a single diagnostic to stderr.
    pub fn emit(&self, diag: &DiagnosticMessage) {
        let d = self.build_diagnostic(diag);
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        // Ignore write errors (stderr may be closed)
        let _ = term::emit(&mut writer.lock(), &config, &self.files, &d);
    }

    /// Render a single diagnostic to a `String` (for embedding in `anyhow` errors or tests).
    pub fn render_to_string(&self, diag: &DiagnosticMessage) -> String {
        let d = self.build_diagnostic(diag);
        let mut buf = Buffer::no_color();
        let config = term::Config::default();
        let _ = term::emit(&mut buf, &config, &self.files, &d);
        String::from_utf8_lossy(buf.as_slice()).into_owned()
    }

    /// Render multiple diagnostics to a single `String`.
    pub fn render_all_to_string(&self, diags: &[DiagnosticMessage]) -> String {
        let mut buf = Buffer::no_color();
        let config = term::Config::default();
        for diag in diags {
            let d = self.build_diagnostic(diag);
            let _ = term::emit(&mut buf, &config, &self.files, &d);
        }
        String::from_utf8_lossy(buf.as_slice()).into_owned()
    }

    fn build_diagnostic(&self, diag: &DiagnosticMessage) -> Diagnostic<usize> {
        let mut labels = Vec::with_capacity(1 + diag.labels.len());

        // Primary label at the span
        let primary = if diag.start == diag.end {
            // Point span — highlight at least one byte so codespan shows something
            let end = if diag.start < self.source_len() {
                diag.start + 1
            } else {
                diag.start
            };
            Label::primary(self.file_id, diag.start..end)
        } else {
            Label::primary(self.file_id, diag.start..diag.end)
        };
        labels.push(primary);

        // Secondary labels
        for &(s, e, text) in diag.labels {
            labels.push(Label::secondary(self.file_id, s..e).with_message(text));
        }

        let base = match diag.severity {
            Severity::Error => Diagnostic::error(),
            Severity::Warning => Diagnostic::warning(),
        };
        base.with_message(diag.message).with_labels(labels)
    }

    fn source_len(&self) -> usize {
        use codespan_reporting::files::Files;
        self.files
            .source(self.file_id)
            .map(|s| s.len())
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_error_contains_line_info() {
        let source = "line one\nline two\nline three\n";
        let renderer = DiagnosticRenderer::new("test.vhd", source);
        let diag = DiagnosticMessage {
            severity: Severity::Error,
            message: "something went wrong",
            start: 9, // start of "line two"
            end: 17,  // end of "line two"
            labels: &[],
        };
        let output = renderer.render_to_string(&diag);
        assert!(
            output.contains("something went wrong"),
            "should contain message"
        );
        assert!(output.contains("test.vhd"), "should contain filename");
        assert!(output.contains("line two"), "should contain source snippet");
    }

    #[test]
    fn render_warning() {
        let source = "abc\ndef\n";
        let renderer = DiagnosticRenderer::new("f.sk", source);
        let diag = DiagnosticMessage {
            severity: Severity::Warning,
            message: "be careful",
            start: 4,
            end: 7,
            labels: &[],
        };
        let output = renderer.render_to_string(&diag);
        assert!(output.contains("warning"), "should say warning");
        assert!(output.contains("be careful"));
    }

    #[test]
    fn render_point_span() {
        let source = "hello world\n";
        let renderer = DiagnosticRenderer::new("t.vhd", source);
        let diag = DiagnosticMessage {
            severity: Severity::Error,
            message: "unexpected",
            start: 5,
            end: 5, // point span
            labels: &[],
        };
        let output = renderer.render_to_string(&diag);
        assert!(output.contains("unexpected"));
        assert!(output.contains("t.vhd"));
    }
}
