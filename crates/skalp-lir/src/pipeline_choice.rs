//! Choosing the latency of `#[pipeline]` entities that don't name one.
//!
//! The design is synthesized with each such entity at its own latency, then
//! for every instance of the entity [`extra_stages_for_period`] finds the
//! fewest registers to add at its inputs for forward retiming to meet the
//! clock period. The entity gets its own latency plus the most any of its
//! instances needs. The caller records the result in the pipeline lock file
//! (`skalp_frontend::pipeline_lock`) and compiles again with it.

use crate::retime::{extra_stages_for_period, RetimeDelays};
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

    auto.iter()
        .map(|module| {
            let config = module.pipeline_config.as_ref().expect("filtered on auto");
            let own_latency = skalp_mir::pipelining::module_latency(mir, module).unwrap_or(0);
            let limit = config
                .max_stages
                .unwrap_or(own_latency + DEFAULT_MAX_EXTRA)
                .max(own_latency);
            let mut needed = Some(0u32);
            for (path, _) in hier_lir
                .instances
                .iter()
                .filter(|(_, i)| i.module_name == module.name)
            {
                let extra =
                    extra_stages_for_period(&netlist, delays, path, period_ps, limit - own_latency)
                        // A region with nothing to retime can't use added stages
                        .or_else(|| (limit == own_latency).then_some(0));
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
            }
        })
        .collect()
}
