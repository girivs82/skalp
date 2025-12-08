//! Source span and location types for error reporting
//!
//! This module provides types for tracking source locations with
//! line and column information for better error diagnostics.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Source location with full context for error reporting
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    /// File path (if available)
    pub file: Option<PathBuf>,
    /// Byte offset in source (start)
    pub start: usize,
    /// Byte offset in source (end)
    pub end: usize,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
    /// End line (for multi-line spans)
    pub end_line: usize,
    /// End column
    pub end_column: usize,
}

impl SourceSpan {
    /// Create a new span with just start offset (for simple errors)
    pub fn at_offset(offset: usize, line_index: &LineIndex) -> Self {
        let (line, column) = line_index.line_col(offset);
        Self {
            file: None,
            start: offset,
            end: offset,
            line,
            column,
            end_line: line,
            end_column: column,
        }
    }

    /// Create span from byte offset range using line index
    pub fn from_offset_range(start: usize, end: usize, line_index: &LineIndex) -> Self {
        let (line, column) = line_index.line_col(start);
        let (end_line, end_column) = line_index.line_col(end);
        Self {
            file: None,
            start,
            end,
            line,
            column,
            end_line,
            end_column,
        }
    }

    /// Create span with file path
    pub fn with_file(mut self, file: PathBuf) -> Self {
        self.file = Some(file);
        self
    }

    /// Format as "file:line:column" for error messages
    pub fn display(&self) -> String {
        let file_str = self
            .file
            .as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "<unknown>".to_string());
        format!("{}:{}:{}", file_str, self.line, self.column)
    }

    /// Format as just "line:column" (without file)
    pub fn line_col_display(&self) -> String {
        format!("{}:{}", self.line, self.column)
    }
}

impl Default for SourceSpan {
    fn default() -> Self {
        Self {
            file: None,
            start: 0,
            end: 0,
            line: 1,
            column: 1,
            end_line: 1,
            end_column: 1,
        }
    }
}

/// Index for converting byte offsets to line:column positions
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset of start of each line
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Create a new line index from source text
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert byte offset to (line, column), both 1-indexed
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        // Binary search for the line containing this offset
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line + 1, // Exact match on line start
            Err(line) => line,    // Between lines, use the previous line
        };

        let line_start = self.line_starts.get(line.saturating_sub(1)).copied().unwrap_or(0);
        let column = offset.saturating_sub(line_start) + 1;
        (line, column)
    }

    /// Get the byte offset for start of a given line (1-indexed)
    pub fn line_start(&self, line: usize) -> Option<usize> {
        if line == 0 {
            return None;
        }
        self.line_starts.get(line - 1).copied()
    }

    /// Get total number of lines
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_index_single_line() {
        let source = "hello world";
        let index = LineIndex::new(source);

        assert_eq!(index.line_col(0), (1, 1)); // 'h'
        assert_eq!(index.line_col(5), (1, 6)); // ' '
        assert_eq!(index.line_col(10), (1, 11)); // 'd'
    }

    #[test]
    fn test_line_index_multiple_lines() {
        let source = "line1\nline2\nline3";
        let index = LineIndex::new(source);

        // line1
        assert_eq!(index.line_col(0), (1, 1)); // 'l'
        assert_eq!(index.line_col(4), (1, 5)); // '1'
        assert_eq!(index.line_col(5), (1, 6)); // '\n'

        // line2
        assert_eq!(index.line_col(6), (2, 1)); // 'l'
        assert_eq!(index.line_col(10), (2, 5)); // '2'

        // line3
        assert_eq!(index.line_col(12), (3, 1)); // 'l'
        assert_eq!(index.line_col(16), (3, 5)); // '3'
    }

    #[test]
    fn test_line_index_empty_lines() {
        let source = "line1\n\nline3";
        let index = LineIndex::new(source);

        assert_eq!(index.line_col(5), (1, 6)); // '\n'
        assert_eq!(index.line_col(6), (2, 1)); // '\n' (empty line)
        assert_eq!(index.line_col(7), (3, 1)); // 'l' of line3
    }

    #[test]
    fn test_source_span_display() {
        let source = "fn foo() {\n    error here\n}";
        let index = LineIndex::new(source);
        let span = SourceSpan::from_offset_range(15, 20, &index)
            .with_file(PathBuf::from("test.sk"));

        assert_eq!(span.display(), "test.sk:2:5");
        assert_eq!(span.line_col_display(), "2:5");
    }
}
