//! Name patterns with `*` wildcards, as constraint and calibration files
//! use them

/// Whether `text` matches `pattern`, where `*` stands for any run of
/// characters (none included) and every other character for itself.
pub fn glob_matches(pattern: &str, text: &str) -> bool {
    let mut parts = pattern.split('*');
    let first = parts.next().unwrap_or_default();
    let Some(mut rest) = text.strip_prefix(first) else {
        return false;
    };
    let middle: Vec<&str> = parts.collect();
    let Some((&last, middle)) = middle.split_last() else {
        // No `*`: the whole text must be the pattern
        return rest.is_empty();
    };
    for part in middle {
        match rest.find(part) {
            Some(at) => rest = &rest[at + part.len()..],
            None => return false,
        }
    }
    rest.len() >= last.len() && rest.ends_with(last)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn patterns_take_any_number_of_wildcards() {
        assert!(glob_matches("top.*.ring_*.lat0", "top.Probe.ring_12.lat0"));
        assert!(glob_matches("*", ""));
        assert!(glob_matches("a*b*c", "abc"));
        assert!(glob_matches("a*b*c", "a-b-b-c"));
        assert!(!glob_matches("a*b*c", "a-c-b"));
        assert!(!glob_matches("a*a", "a"));
        assert!(glob_matches("exact", "exact"));
        assert!(!glob_matches("exact", "exactly"));
    }
}
