//! File fingerprinting for change detection

use crate::IncrementalResult;

pub fn calculate_target_fingerprint(target: &str) -> IncrementalResult<String> {
    // Simplified fingerprint calculation
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(target.as_bytes());
    let result = hasher.finalize();
    Ok(format!("{:x}", result))
}

pub fn calculate_file_fingerprint(path: &std::path::Path) -> IncrementalResult<String> {
    use sha2::{Digest, Sha256};
    let content = std::fs::read(path)?;
    let mut hasher = Sha256::new();
    hasher.update(&content);
    let result = hasher.finalize();
    Ok(format!("{:x}", result))
}
