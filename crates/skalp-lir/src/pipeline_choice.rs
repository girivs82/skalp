//! Choosing the latency of `#[pipeline]` entities that don't name one.
//!
//! The design is synthesized with each such entity at its own latency, then
//! for every instance of the entity [`extra_stages_for_period`] finds the
//! fewest registers to add at its inputs for forward retiming to meet the
//! clock period. The entity gets its own latency plus the most any of its
//! instances needs. The added registers may move into the entity and back
//! into retimable logic feeding it (`#[retime]` or `#[pipeline]` entities
//! around it), so a slow path in front of the entity can be cut too, as long
//! as it is retimable. The caller records the result in the pipeline lock file
//! (`skalp_frontend::pipeline_lock`) and compiles again with it.

use crate::retime::{extra_stages_for_period, BlockedPath, RetimeDelays};
use crate::synth::SynthPreset;
use crate::tech_library::TechLibrary;
use skalp_mir::Mir;

/// Most cycles added to an entity without `max_stages`
pub const DEFAULT_MAX_EXTRA: u32 = 16;

/// The latency chosen for one entity
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageChoice {
    /// Entity (module) name
    pub entity: String,
    /// Its latency without added stages
    pub own_latency: u32,
    /// The latency that meets the period, or `None` when no latency up to
    /// the limit does
    pub latency: Option<u32>,
    /// The most latency it may be given
    pub limit: u32,
    /// When no latency meets the period: the path that stays too long
    pub blocked: Option<BlockedPath>,
    /// Paths through or next to the entity that are too long for the
    /// period whatever its latency (logic retiming can't reach): they limit
    /// the clock, and are reported, but are not the entity's to fix
    pub limits: Vec<BlockedPath>,
}

/// Choose a latency for every `#[pipeline]` entity in `mir` that leaves it
/// to synthesis. `mir` must have those entities at their own latency (no
/// recorded choice applied). `period_ps` is the clock period to meet.
pub fn choose_pipeline_stages(
    mir: &Mir,
    library: &TechLibrary,
    preset: SynthPreset,
    delays: &dyn RetimeDelays,
    period_ps: i64,
) -> Vec<StageChoice> {
    let auto: Vec<&skalp_mir::Module> = mir
        .modules
        .iter()
        .filter(|m| m.pipeline_config.as_ref().is_some_and(|c| c.auto))
        .collect();
    if auto.is_empty() {
        return Vec::new();
    }
    let (hier_lir, _) = crate::lower_mir_hierarchical_for_optimize_first(mir);
    let mut hier = crate::synthesize_hierarchical(&hier_lir, library, preset);
    // Measure the logic as written: no retiming yet
    hier.retime_regions.clear();
    let netlist = hier.flatten_for_library(library);
    // Every retimable instance: the added registers may move through them
    let regions = hier_lir.retime_regions.clone();
    // Debug aid: SKALP_PIPELINE_DUMP=<dir> saves the netlist and regions the
    // choice is made on, to replay `retime::extra_stages_for_period` on them
    if let Ok(dir) = std::env::var("SKALP_PIPELINE_DUMP") {
        let dir = std::path::Path::new(&dir);
        let _ = std::fs::create_dir_all(dir);
        if let Ok(text) = serde_json::to_string(&netlist) {
            let _ = std::fs::write(dir.join("netlist.json"), text);
        }
        let _ = std::fs::write(dir.join("regions.txt"), regions.join("\n"));
    }

    auto.iter()
        .map(|module| {
            let config = module.pipeline_config.as_ref().expect("filtered on auto");
            let own_latency = skalp_mir::pipelining::module_latency(mir, module).unwrap_or(0);
            let limit = config
                .max_stages
                .unwrap_or(own_latency + DEFAULT_MAX_EXTRA)
                .max(own_latency);
            let mut needed = Some(0u32);
            let mut blocked = None;
            let mut limits: Vec<BlockedPath> = Vec::new();
            for (path, _) in hier_lir
                .instances
                .iter()
                .filter(|(_, i)| i.module_name == module.name)
            {
                let extra = match extra_stages_for_period(
                    &netlist,
                    delays,
                    &regions,
                    path,
                    period_ps,
                    limit - own_latency,
                ) {
                    Ok((extra, found)) => {
                        for path in found {
                            // Instances of the entity often hit the same
                            // path; say it once
                            let same = |p: &BlockedPath| {
                                p.why == path.why
                                    && p.delay_ps == path.delay_ps
                                    && summarize_path(&p.steps) == summarize_path(&path.steps)
                            };
                            if !limits.iter().any(same) {
                                limits.push(path);
                            }
                        }
                        Some(extra)
                    }
                    // A region with nothing to retime can't use added stages
                    Err(None) => (limit == own_latency).then_some(0),
                    Err(Some(path)) => {
                        if blocked.is_none() {
                            blocked = Some(path);
                        }
                        None
                    }
                };
                needed = match (needed, extra) {
                    (Some(a), Some(b)) => Some(a.max(b)),
                    _ => None,
                };
            }
            StageChoice {
                entity: module.name.clone(),
                own_latency,
                latency: needed.map(|extra| own_latency + extra),
                limit,
                blocked: if needed.is_none() { blocked } else { None },
                limits,
            }
        })
        .collect()
}

/// What to tell the designer when `choice` found no latency for the clock
/// period `period_ps`: the path that stays too long, and what would let
/// retiming cut it
pub fn unmet_message(choice: &StageChoice, period_ps: i64) -> String {
    let mut text = format!(
        "`{}` can't meet {:.1} MHz with up to {} cycles of latency (its own is {})",
        choice.entity,
        1e6 / period_ps as f64,
        choice.limit,
        choice.own_latency
    );
    match &choice.blocked {
        Some(path) if path.from_outside => {
            text.push_str(&format!(
                ".\n  This path ({} ps) starts outside the retimable logic, so added stages \
                 can't reach it:\n    {}\n  Mark the entities it runs through #[retime] (latency \
                 unchanged; registers may move through them) or #[pipeline], or shorten it.",
                path.delay_ps,
                summarize_path(&path.steps).join("\n    ")
            ));
        }
        Some(path) => {
            text.push_str(&format!(
                ".\n  This path ({} ps) stays too long:\n    {}\n  Raise max_stages or split \
                 its logic.",
                path.delay_ps,
                summarize_path(&path.steps).join("\n    ")
            ));
        }
        None => text.push_str("; raise max_stages or split its logic"),
    }
    text
}

/// A path's steps grouped by the instance they are in: one line per run of
/// cells in one instance, naming the operations among them
pub fn summarize_path(steps: &[String]) -> Vec<String> {
    // The cell path in a step ("TYPE path (lag n)", "net N from path, ...")
    fn cell_path(step: &str) -> Option<&str> {
        let rest = step.strip_prefix("net ").map_or(step, |r| {
            r.split_once(" from ").map_or("", |(_, from)| from)
        });
        let path = if step.starts_with("net ") {
            rest.split(',').next()?
        } else {
            rest.split_whitespace().nth(1)?
        };
        path.contains('.').then_some(path)
    }
    // The instance a cell is in, and the operation it came from: the
    // instance path ends where a generated name (`aig`, `retime..`) or the
    // entity name (capitalized) starts
    fn split(path: &str) -> (String, Option<String>) {
        let parts: Vec<&str> = path.split('.').collect();
        let cut = parts
            .iter()
            .position(|p| {
                *p == "aig"
                    || p.starts_with("retime")
                    || p.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            })
            .unwrap_or(parts.len().saturating_sub(1))
            .max(1);
        let operation = parts
            .get(cut + 1)
            .filter(|_| {
                parts[cut]
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
            })
            .map(|op| op.to_string());
        (parts[..cut].join("."), operation)
    }
    let mut lines: Vec<String> = Vec::new();
    let mut run: Option<(String, usize, Vec<String>)> = None;
    let flush = |run: &mut Option<(String, usize, Vec<String>)>, lines: &mut Vec<String>| {
        if let Some((instance, cells, ops)) = run.take() {
            let mut line = format!(
                "{instance}: {cells} cell{}",
                if cells == 1 { "" } else { "s" }
            );
            if !ops.is_empty() {
                let shown: Vec<&str> = ops.iter().take(6).map(String::as_str).collect();
                line.push_str(&format!(
                    " ({}{})",
                    shown.join(", "),
                    if ops.len() > 6 { ", ..." } else { "" }
                ));
            }
            lines.push(line);
        }
    };
    for step in steps {
        let is_cell = !step.starts_with("net ") && !step.starts_with("registers ");
        match cell_path(step).filter(|_| is_cell) {
            Some(path) => {
                let (instance, op) = split(path);
                if run.as_ref().is_some_and(|(i, _, _)| *i != instance) {
                    flush(&mut run, &mut lines);
                }
                let entry = run.get_or_insert_with(|| (instance, 0, Vec::new()));
                entry.1 += 1;
                if let Some(op) = op {
                    if !entry.2.contains(&op) {
                        entry.2.push(op);
                    }
                }
                // Where it ends in logic that stays put, how much is left
                if let Some((_, rest)) = step.split_once("(stays put; ") {
                    flush(&mut run, &mut lines);
                    lines.push(format!("then {}", rest.trim_end_matches(')')));
                }
            }
            None => {
                flush(&mut run, &mut lines);
                lines.push(step.clone());
            }
        }
    }
    flush(&mut run, &mut lines);
    lines
}

/// What to tell the designer about a path that limits the clock whatever
/// `choice` latency is
pub fn limit_message(choice: &StageChoice, path: &BlockedPath) -> String {
    let advice = if path.from_outside {
        "it starts outside the retimable logic: mark the entities it runs through \
         #[retime] (latency unchanged) or #[pipeline], or shorten it"
    } else {
        "it ends in logic that stays put: mark the entities after it #[retime] or \
         #[pipeline], or shorten it"
    };
    format!(
        "`{}`: a path of {:.1} ns ({:.1} MHz) is too long for the clock whatever its latency; {advice}:\n    {}",
        choice.entity,
        path.delay_ps as f64 / 1000.0,
        1e6 / path.delay_ps as f64,
        summarize_path(&path.steps).join("\n    ")
    )
}
