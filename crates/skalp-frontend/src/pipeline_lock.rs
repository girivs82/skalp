//! The pipeline lock file: latencies synthesis chose for `#[pipeline]`
//! entities that don't name one.
//!
//! A bare `#[pipeline]` (or `#[pipeline(max_stages = N)]`) leaves the
//! entity's latency to synthesis, which picks the fewest cycles that meet
//! the clock. The latency changes what the design does cycle by cycle, so
//! the choice is recorded beside the design's top file, as
//! `<top>.pipeline.lock`, and every flow (simulation, SystemVerilog,
//! synthesis) reads it from there. Commit it with the design, as a
//! `Cargo.lock` is committed.
//!
//! The format is one `Entity = cycles` line per entity; `#` starts a
//! comment.

use crate::hir::{Hir, PipelineConfig};
use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use std::path::{Path, PathBuf};

/// Latency in cycles, per entity
pub type PipelineLock = IndexMap<String, u32>;

/// The lock file of the design whose top file is `source`
pub fn lock_path(source: &Path) -> PathBuf {
    source.with_extension("pipeline.lock")
}

/// Parse lock file text
pub fn parse(text: &str) -> Result<PipelineLock> {
    let mut lock = PipelineLock::new();
    for (number, line) in text.lines().enumerate() {
        let line = line.split('#').next().unwrap_or("").trim();
        if line.is_empty() {
            continue;
        }
        let Some((name, cycles)) = line.split_once('=') else {
            bail!("line {}: expected `Entity = cycles`", number + 1);
        };
        let cycles = cycles.trim().parse().with_context(|| {
            format!(
                "line {}: `{}` is not a cycle count",
                number + 1,
                cycles.trim()
            )
        })?;
        lock.insert(name.trim().to_string(), cycles);
    }
    Ok(lock)
}

/// Read the lock file beside `source`; empty when there is none
pub fn read(source: &Path) -> Result<PipelineLock> {
    let path = lock_path(source);
    match std::fs::read_to_string(&path) {
        Ok(text) => parse(&text).with_context(|| format!("in {}", path.display())),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(PipelineLock::new()),
        Err(e) => Err(e).with_context(|| format!("reading {}", path.display())),
    }
}

/// Lock file text for `lock`; `note` (such as the clock it was chosen for)
/// goes in the header
pub fn render(lock: &PipelineLock, note: &str) -> String {
    let mut text = String::from(
        "# Latencies (in clock cycles) SKALP chose for #[pipeline] entities that\n\
         # don't name one. Written by synthesis, read by every flow; commit it\n\
         # with the design.\n",
    );
    if !note.is_empty() {
        text.push_str(&format!("# {note}\n"));
    }
    for (name, cycles) in lock {
        text.push_str(&format!("{name} = {cycles}\n"));
    }
    text
}

/// Write `lock` beside `source`
pub fn write(source: &Path, lock: &PipelineLock, note: &str) -> Result<PathBuf> {
    let path = lock_path(source);
    std::fs::write(&path, render(lock, note))
        .with_context(|| format!("writing {}", path.display()))?;
    Ok(path)
}

/// Every `#[pipeline]` config in `hir` that leaves the latency to synthesis,
/// by entity (or function) name
pub fn auto_configs(hir: &mut Hir) -> Vec<(String, &mut PipelineConfig)> {
    let mut configs = Vec::new();
    for entity in &mut hir.entities {
        if let Some(config) = entity.pipeline_config.as_mut().filter(|c| c.auto) {
            configs.push((entity.name.clone(), config));
        }
    }
    let functions = hir.functions.iter_mut().chain(
        hir.implementations
            .iter_mut()
            .flat_map(|i| i.functions.iter_mut()),
    );
    for function in functions {
        if let Some(config) = function.pipeline_config.as_mut().filter(|c| c.auto) {
            configs.push((function.name.clone(), config));
        }
    }
    configs
}

/// Give each `auto` entity in `hir` the latency `lock` records for it.
/// Returns the entities it has none for (they keep their own latency).
pub fn apply(hir: &mut Hir, lock: &PipelineLock) -> Vec<String> {
    let mut missing = Vec::new();
    for (name, config) in auto_configs(hir) {
        match lock.get(&name) {
            Some(&cycles) => config.stages = cycles,
            None => {
                config.stages = 0;
                missing.push(name);
            }
        }
    }
    missing
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lock_round_trip() {
        let mut lock = PipelineLock::new();
        lock.insert("MulAdd".to_string(), 4);
        lock.insert("Divider".to_string(), 9);
        let text = render(&lock, "for 100 MHz");
        assert!(text.contains("# for 100 MHz\n"));
        assert_eq!(parse(&text).unwrap(), lock);
    }

    #[test]
    fn test_lock_parse_errors_name_the_line() {
        let error = parse("A = 2\nB = x\n").unwrap_err();
        assert!(format!("{error:#}").contains("line 2"), "{error:#}");
        let error = parse("A 2\n").unwrap_err();
        assert!(format!("{error:#}").contains("line 1"), "{error:#}");
    }
}
