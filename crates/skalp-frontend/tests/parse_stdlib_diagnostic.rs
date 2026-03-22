//! Diagnostic test: parse stdlib component files and print detailed error info.
//!
//! Run with: cargo test -p skalp-frontend --test parse_stdlib_diagnostic -- --nocapture

use skalp_frontend::parse::{parse_with_errors, ParseErrorKind};

/// Convert a byte offset in source to (line, col) — both 1-based.
fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Extract the source line containing the given byte offset.
fn source_line_at(source: &str, offset: usize) -> &str {
    let start = source[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let end = source[offset..]
        .find('\n')
        .map(|i| offset + i)
        .unwrap_or(source.len());
    &source[start..end]
}

fn diagnose_file(path: &str) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Cannot read {}: {}", path, e);
            return;
        }
    };

    let (_tree, errors) = parse_with_errors(&source);

    eprintln!("\n{}", "=".repeat(72));
    eprintln!("FILE: {}", path);
    eprintln!("Source length: {} bytes, {} lines", source.len(), source.lines().count());
    eprintln!("Parse errors: {}", errors.len());
    eprintln!("{}", "=".repeat(72));

    if errors.is_empty() {
        eprintln!("  (no errors)");
        return;
    }

    for (i, err) in errors.iter().enumerate() {
        let (line, col) = offset_to_line_col(&source, err.position);
        let src_line = source_line_at(&source, err.position);

        eprintln!("\n--- Error {} ---", i + 1);
        eprintln!("  Location:  line {}, col {} (byte offset {})", line, col, err.position);
        eprintln!("  Kind:      {:?}", err.kind);
        eprintln!("  Message:   {}", err.message);
        if let Some(ref expected) = err.expected {
            eprintln!("  Expected:  {}", expected);
        }
        if let Some(ref found) = err.found {
            eprintln!("  Found:     {}", found);
        }
        eprintln!("  Source:    {}", src_line.trim());

        // Show pointer to the error column
        let trimmed_offset = src_line.len() - src_line.trim_start().len();
        let pointer_col = if col > trimmed_offset + 1 {
            col - trimmed_offset - 1
        } else {
            0
        };
        eprintln!("             {}^", " ".repeat(pointer_col));
    }

    // Summary: group errors by message
    eprintln!("\n--- Error Summary ---");
    let mut counts: std::collections::BTreeMap<String, usize> = std::collections::BTreeMap::new();
    for err in &errors {
        *counts.entry(err.message.clone()).or_insert(0) += 1;
    }
    for (msg, count) in &counts {
        eprintln!("  {:>3}x  {}", count, msg);
    }

    // Show first 5 unique error locations with context
    eprintln!("\n--- First 10 errors with context ---");
    for err in errors.iter().take(10) {
        let (line, col) = offset_to_line_col(&source, err.position);
        let src_line = source_line_at(&source, err.position);
        eprintln!(
            "  L{}:{} [{}] {} | found: {} | source: `{}`",
            line,
            col,
            match err.kind {
                ParseErrorKind::MissingToken => "MISSING",
                ParseErrorKind::UnexpectedToken => "UNEXPECTED",
                ParseErrorKind::InvalidSyntax => "INVALID",
                ParseErrorKind::UnknownConstruct => "UNKNOWN",
            },
            err.message,
            err.found.as_deref().unwrap_or("?"),
            src_line.trim()
        );
    }
}

#[test]
fn diagnose_adder_parse() {
    diagnose_file(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../skalp-stdlib/components/adder.sk"
    ));
}

#[test]
fn diagnose_multiplier_parse() {
    // Run in a thread with explicit stack size to catch stack overflow
    // (multiplier.sk may trigger infinite recursion in the parser)
    let builder = std::thread::Builder::new()
        .name("multiplier-parse".to_string())
        .stack_size(8 * 1024 * 1024); // 8MB stack

    let handle = builder
        .spawn(|| {
            diagnose_file(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../skalp-stdlib/components/multiplier.sk"
            ));
        })
        .expect("failed to spawn thread");

    // Wait with a timeout to avoid getting SIGKILLed
    let result = handle.join();
    if result.is_err() {
        eprintln!("\n!!! multiplier.sk parse CRASHED (likely stack overflow / infinite recursion) !!!");
    }
}
