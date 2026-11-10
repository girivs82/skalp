//! Initial contents of a memory, read from a hex file.
//!
//! The file format is the one `$readmemh` reads: hex words separated by
//! whitespace, `//` line comments and `/* */` block comments, and `@addr`
//! (hex) to move to a word address. Underscores inside a word are ignored.

use std::path::Path;

/// The words of a hex memory file, indexed by address. Words the file skips
/// over with `@addr` are zero.
pub fn parse_hex_words(text: &str) -> Result<Vec<u64>, String> {
    let mut words = Vec::new();
    let mut address = 0usize;
    for (line_index, token) in tokens(text)? {
        let line = line_index + 1;
        if let Some(target) = token.strip_prefix('@') {
            address = usize::from_str_radix(&target.replace('_', ""), 16)
                .map_err(|_| format!("line {line}: bad address `{token}`"))?;
            continue;
        }
        let digits = token.replace('_', "");
        if digits.is_empty() || digits.len() > 16 {
            return Err(format!(
                "line {line}: `{token}` is not a hex word of at most 64 bits"
            ));
        }
        let value = u64::from_str_radix(&digits, 16)
            .map_err(|_| format!("line {line}: `{token}` is not a hex word"))?;
        if words.len() <= address {
            words.resize(address + 1, 0);
        }
        words[address] = value;
        address += 1;
    }
    Ok(words)
}

/// Read a hex memory file named relative to the source file that names it.
pub fn load_hex_file(name: &str, source_file: Option<&Path>) -> Result<Vec<u64>, String> {
    let path = match source_file.and_then(Path::parent) {
        Some(dir) if Path::new(name).is_relative() => dir.join(name),
        _ => Path::new(name).to_path_buf(),
    };
    let text = std::fs::read_to_string(&path)
        .map_err(|e| format!("cannot read `{}`: {e}", path.display()))?;
    parse_hex_words(&text).map_err(|e| format!("{}: {e}", path.display()))
}

/// The tokens of `text` outside comments, each with its 0-based line.
fn tokens(text: &str) -> Result<Vec<(usize, String)>, String> {
    let mut out = Vec::new();
    let mut line = 0usize;
    let mut current = String::new();
    let mut chars = text.chars().peekable();
    let flush = |current: &mut String, out: &mut Vec<(usize, String)>, line: usize| {
        if !current.is_empty() {
            out.push((line, std::mem::take(current)));
        }
    };
    while let Some(c) = chars.next() {
        match c {
            '/' if chars.peek() == Some(&'/') => {
                flush(&mut current, &mut out, line);
                for c in chars.by_ref() {
                    if c == '\n' {
                        line += 1;
                        break;
                    }
                }
            }
            '/' if chars.peek() == Some(&'*') => {
                flush(&mut current, &mut out, line);
                chars.next();
                let start = line;
                let mut closed = false;
                while let Some(c) = chars.next() {
                    if c == '\n' {
                        line += 1;
                    } else if c == '*' && chars.peek() == Some(&'/') {
                        chars.next();
                        closed = true;
                        break;
                    }
                }
                if !closed {
                    return Err(format!("line {}: unclosed `/*` comment", start + 1));
                }
            }
            c if c.is_whitespace() => {
                flush(&mut current, &mut out, line);
                if c == '\n' {
                    line += 1;
                }
            }
            c => current.push(c),
        }
    }
    flush(&mut current, &mut out, line);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn words_comments_and_addresses() {
        let text = "// boot image\n0a 0B\n/* skip\n a block */ ff_ff\n@8 12 // tail\n34";
        assert_eq!(
            parse_hex_words(text).unwrap(),
            vec![0x0a, 0x0b, 0xffff, 0, 0, 0, 0, 0, 0x12, 0x34]
        );
    }

    #[test]
    fn bad_words_name_their_line() {
        let err = parse_hex_words("01\n02\nzz\n").unwrap_err();
        assert!(err.contains("line 3"), "{err}");
        let err = parse_hex_words("1_0000_0000_0000_0000").unwrap_err();
        assert!(err.contains("64 bits"), "{err}");
        assert!(parse_hex_words("/* open").is_err());
    }

    #[test]
    fn files_are_found_beside_their_source() {
        let dir = std::env::temp_dir().join(format!("skalp_meminit_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("rom.hex"), "1 2 3\n").unwrap();
        let words = load_hex_file("rom.hex", Some(&dir.join("design.sk"))).unwrap();
        assert_eq!(words, vec![1, 2, 3]);
        assert!(load_hex_file("missing.hex", Some(&dir.join("design.sk"))).is_err());
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
