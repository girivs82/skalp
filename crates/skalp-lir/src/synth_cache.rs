//! On-disk cache of synthesized module netlists
//!
//! Synthesizing a module depends only on its LIR, the technology library,
//! the preset and the synthesizer itself. [`SynthCache`] stores each
//! synthesized module netlist under a hash of all four, so a rebuild in
//! which only some modules changed (a new top-level wrapper around the same
//! blocks, say) re-synthesizes only those.
//!
//! The synthesizer is identified by a hash of the running executable: any
//! rebuild of the tool gives new keys, so a cache never hands back a
//! netlist made by different synthesis code. Entries are written to a
//! temporary file and renamed into place, so concurrent builds sharing a
//! cache directory never read a half-written entry. A missing, unreadable or
//! corrupt entry is treated as a miss.

use crate::gate_netlist::GateNetlist;
use crate::lir::Lir;
use crate::synth::SynthPreset;
use crate::tech_library::TechLibrary;
use std::path::{Path, PathBuf};

/// Directory-backed cache of synthesized module netlists
#[derive(Debug, Clone)]
pub struct SynthCache {
    dir: PathBuf,
    /// Hash identifying the synthesizer (the running executable)
    tool: blake3::Hash,
}

impl SynthCache {
    /// A cache in `dir`, keyed for the running executable. `None` when the
    /// executable cannot be read (then nothing could tell builds by
    /// different synthesizers apart) or `dir` cannot be created.
    pub fn new(dir: impl Into<PathBuf>) -> Option<Self> {
        let exe = std::env::current_exe().ok()?;
        let tool = hash_file(&exe)?;
        Self::with_tool_hash(dir, tool)
    }

    /// A cache in `dir` for a synthesizer identified by `tool`
    pub fn with_tool_hash(dir: impl Into<PathBuf>, tool: blake3::Hash) -> Option<Self> {
        let dir = dir.into();
        std::fs::create_dir_all(&dir).ok()?;
        Some(Self { dir, tool })
    }

    /// The directory the cache lives in
    pub fn dir(&self) -> &Path {
        &self.dir
    }

    /// Key of one module's synthesis. `library_hash` comes from
    /// [`SynthCache::library_hash`], computed once per build.
    pub fn key(
        &self,
        lir: &Lir,
        library_hash: &blake3::Hash,
        preset: SynthPreset,
    ) -> Option<blake3::Hash> {
        let lir_bytes = bincode::serialize(lir).ok()?;
        let mut hasher = blake3::Hasher::new();
        hasher.update(b"skalp synth cache v1\0");
        hasher.update(self.tool.as_bytes());
        hasher.update(library_hash.as_bytes());
        hasher.update(format!("{preset:?}").as_bytes());
        hasher.update(&(lir_bytes.len() as u64).to_le_bytes());
        hasher.update(&lir_bytes);
        Some(hasher.finalize())
    }

    /// Hash of a technology library, for [`SynthCache::key`]
    pub fn library_hash(library: &TechLibrary) -> Option<blake3::Hash> {
        let bytes = bincode::serialize(library).ok()?;
        Some(blake3::hash(&bytes))
    }

    fn path_of(&self, key: &blake3::Hash) -> PathBuf {
        self.dir.join(format!("{}.json", key.to_hex()))
    }

    /// The netlist stored under `key`, if any
    pub fn get(&self, key: &blake3::Hash) -> Option<GateNetlist> {
        let text = std::fs::read(self.path_of(key)).ok()?;
        match serde_json::from_slice(&text) {
            Ok(netlist) => Some(netlist),
            Err(e) => {
                tracing::warn!(
                    "synthesis cache entry {} unreadable ({e}); synthesizing again",
                    key.to_hex()
                );
                None
            }
        }
    }

    /// Store `netlist` under `key`. Failures only cost a later miss, so
    /// they are logged, not returned.
    pub fn put(&self, key: &blake3::Hash, netlist: &GateNetlist) {
        let path = self.path_of(key);
        let tmp = self
            .dir
            .join(format!("{}.{}.tmp", key.to_hex(), std::process::id()));
        let result = serde_json::to_vec(netlist)
            .map_err(std::io::Error::other)
            .and_then(|bytes| std::fs::write(&tmp, bytes))
            .and_then(|()| std::fs::rename(&tmp, &path));
        if let Err(e) = result {
            let _ = std::fs::remove_file(&tmp);
            tracing::warn!("could not store synthesis cache entry {path:?}: {e}");
        }
    }
}

fn hash_file(path: &Path) -> Option<blake3::Hash> {
    let mut hasher = blake3::Hasher::new();
    let mut file = std::fs::File::open(path).ok()?;
    std::io::copy(&mut file, &mut hasher).ok()?;
    Some(hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tool(byte: u8) -> blake3::Hash {
        blake3::hash(&[byte])
    }

    #[test]
    fn stores_and_returns_a_netlist_by_key() {
        let dir = tempfile::tempdir().unwrap();
        let cache = SynthCache::with_tool_hash(dir.path(), tool(1)).unwrap();
        let lir = Lir::new("m".to_string());
        let lib = blake3::hash(b"library");
        let key = cache.key(&lir, &lib, SynthPreset::Quick).unwrap();
        assert!(cache.get(&key).is_none());
        let netlist = GateNetlist::new("m".to_string(), "lib".to_string());
        cache.put(&key, &netlist);
        let back = cache.get(&key).expect("stored entry");
        assert_eq!(back.name, "m");
        // no temporary files left behind
        let names: Vec<_> = std::fs::read_dir(dir.path())
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect();
        assert_eq!(names, vec![format!("{}.json", key.to_hex())]);
    }

    #[test]
    fn key_changes_with_lir_library_preset_and_tool() {
        let dir = tempfile::tempdir().unwrap();
        let a = SynthCache::with_tool_hash(dir.path(), tool(1)).unwrap();
        let b = SynthCache::with_tool_hash(dir.path(), tool(2)).unwrap();
        let m = Lir::new("m".to_string());
        let n = Lir::new("n".to_string());
        let lib1 = blake3::hash(b"library 1");
        let lib2 = blake3::hash(b"library 2");
        let base = a.key(&m, &lib1, SynthPreset::Quick).unwrap();
        assert_eq!(base, a.key(&m, &lib1, SynthPreset::Quick).unwrap());
        assert_ne!(base, a.key(&n, &lib1, SynthPreset::Quick).unwrap());
        assert_ne!(base, a.key(&m, &lib2, SynthPreset::Quick).unwrap());
        assert_ne!(base, a.key(&m, &lib1, SynthPreset::Balanced).unwrap());
        assert_ne!(base, b.key(&m, &lib1, SynthPreset::Quick).unwrap());
    }

    #[test]
    fn corrupt_entry_is_a_miss() {
        let dir = tempfile::tempdir().unwrap();
        let cache = SynthCache::with_tool_hash(dir.path(), tool(1)).unwrap();
        let key = cache
            .key(
                &Lir::new("m".to_string()),
                &blake3::hash(b"l"),
                SynthPreset::Quick,
            )
            .unwrap();
        std::fs::write(dir.path().join(format!("{}.json", key.to_hex())), b"{").unwrap();
        assert!(cache.get(&key).is_none());
    }
}
