//! Pipeline Register Insertion Pass
//!
//! This module implements automatic pipeline register insertion based on the
//! `#[pipeline(stages=N)]` attribute. It analyzes the combinational logic graph,
//! computes logic depths, and inserts FlipFlop nodes at appropriate cut points
//! to achieve the requested pipeline depth.
//!
//! # Algorithm
//!
//! 1. Build a dependency graph from combinational nodes
//! 2. Compute the topological order and logic level of each node
//! 3. Determine cut points based on requested stages
//! 4. Insert pipeline registers (FlipFlop nodes) at cut points
//! 5. Rewire the graph to use the registered signals

use crate::sir::{ClockEdge, SignalRef, SirModule, SirNode, SirNodeKind, SirSignal, SirType};
use skalp_frontend::hir::PipelineConfig;
use std::collections::{HashMap, HashSet};

/// Result of pipeline insertion pass
#[derive(Debug)]
pub struct PipelineResult {
    /// Number of pipeline stages inserted
    pub stages_inserted: usize,
    /// Names of pipeline register signals created
    pub pipeline_registers: Vec<String>,
    /// Whether the pass was successful
    pub success: bool,
    /// Any warnings or info messages
    pub messages: Vec<String>,
}

/// Insert pipeline registers into a SIR module based on its pipeline_config.
///
/// This is the main entry point for the pipeline insertion pass. It modifies
/// the SirModule in place, adding sequential nodes (FlipFlops) and new signals
/// as needed to achieve the requested pipeline depth.
///
/// # Returns
///
/// Returns `Some(PipelineResult)` if pipeline insertion was performed,
/// `None` if no pipelining was needed (stages <= 1 or no config).
pub fn insert_pipeline_registers(sir: &mut SirModule) -> Option<PipelineResult> {
    let config = sir.pipeline_config.as_ref()?;

    // Only pipeline if stages > 1
    if config.stages <= 1 {
        return None;
    }

    let mut inserter = PipelineInserter::new(sir, config.clone());
    inserter.run()
}

/// Pipeline insertion state machine
struct PipelineInserter<'a> {
    sir: &'a mut SirModule,
    config: PipelineConfig,
    /// Maps node ID to its computed logic level (depth from inputs)
    node_levels: HashMap<usize, usize>,
    /// Maximum logic level in the design
    max_level: usize,
    /// Next available node ID
    next_node_id: usize,
    /// Created pipeline register signal names
    pipeline_signals: Vec<String>,
    /// Messages/warnings
    messages: Vec<String>,
}

impl<'a> PipelineInserter<'a> {
    fn new(sir: &'a mut SirModule, config: PipelineConfig) -> Self {
        let next_node_id = sir
            .combinational_nodes
            .iter()
            .chain(sir.sequential_nodes.iter())
            .map(|n| n.id)
            .max()
            .unwrap_or(0)
            + 1;

        PipelineInserter {
            sir,
            config,
            node_levels: HashMap::new(),
            max_level: 0,
            next_node_id,
            pipeline_signals: Vec::new(),
            messages: Vec::new(),
        }
    }

    fn run(&mut self) -> Option<PipelineResult> {
        // Step 1: Compute logic levels for all nodes
        self.compute_logic_levels();

        if self.max_level == 0 {
            self.messages
                .push("No combinational logic found to pipeline".to_string());
            return Some(PipelineResult {
                stages_inserted: 0,
                pipeline_registers: vec![],
                success: true,
                messages: self.messages.clone(),
            });
        }

        // Step 2: Determine cut points
        let cut_levels = self.compute_cut_levels();

        // Step 3: Insert pipeline registers at cut points
        let _registers_inserted = self.insert_registers_at_cuts(&cut_levels);

        Some(PipelineResult {
            stages_inserted: self.config.stages as usize,
            pipeline_registers: self.pipeline_signals.clone(),
            success: true,
            messages: self.messages.clone(),
        })
    }

    /// Compute the logic level (depth from inputs) for each combinational node.
    /// Uses topological traversal starting from input signals.
    fn compute_logic_levels(&mut self) {
        // Build a mapping from signal name to the node that drives it
        let mut signal_to_driver: HashMap<String, usize> = HashMap::new();
        for node in &self.sir.combinational_nodes {
            for output in &node.outputs {
                signal_to_driver.insert(output.signal_id.clone(), node.id);
            }
        }

        // Find input signals (no driver = primary input)
        let input_signals: HashSet<String> =
            self.sir.inputs.iter().map(|p| p.name.clone()).collect();

        // Initialize: nodes driven directly by inputs are level 0
        // All others start as not computed
        let mut computed: HashSet<usize> = HashSet::new();
        let mut worklist: Vec<usize> = Vec::new();

        // First pass: find nodes that are directly driven by inputs
        for node in &self.sir.combinational_nodes {
            let all_inputs_are_primary = node.inputs.iter().all(|input| {
                input_signals.contains(&input.signal_id)
                    || !signal_to_driver.contains_key(&input.signal_id)
            });

            if all_inputs_are_primary {
                self.node_levels.insert(node.id, 0);
                computed.insert(node.id);
                worklist.push(node.id);
            }
        }

        // Propagate levels through the graph
        while !worklist.is_empty() {
            let current_id = worklist.remove(0);
            let _current_level = self.node_levels[&current_id];

            // Find the current node
            let current_node = self
                .sir
                .combinational_nodes
                .iter()
                .find(|n| n.id == current_id);

            if let Some(node) = current_node {
                // For each output signal, find nodes that use it
                for output in &node.outputs {
                    for other_node in &self.sir.combinational_nodes {
                        if computed.contains(&other_node.id) {
                            continue;
                        }

                        // Check if this node uses the output signal
                        let uses_output = other_node
                            .inputs
                            .iter()
                            .any(|i| i.signal_id == output.signal_id);

                        if uses_output {
                            // Check if all inputs to other_node are now computed
                            let all_inputs_computed = other_node.inputs.iter().all(|input| {
                                input_signals.contains(&input.signal_id)
                                    || !signal_to_driver.contains_key(&input.signal_id)
                                    || signal_to_driver
                                        .get(&input.signal_id)
                                        .map(|id| computed.contains(id))
                                        .unwrap_or(true)
                            });

                            if all_inputs_computed {
                                // Compute the level as max of input levels + 1
                                let max_input_level = other_node
                                    .inputs
                                    .iter()
                                    .filter_map(|input| {
                                        signal_to_driver
                                            .get(&input.signal_id)
                                            .and_then(|id| self.node_levels.get(id))
                                    })
                                    .max()
                                    .unwrap_or(&0);

                                let new_level = max_input_level + 1;
                                self.node_levels.insert(other_node.id, new_level);
                                computed.insert(other_node.id);
                                worklist.push(other_node.id);

                                if new_level > self.max_level {
                                    self.max_level = new_level;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Handle any remaining uncomputed nodes (may have cycles or disconnected)
        for node in &self.sir.combinational_nodes {
            if !computed.contains(&node.id) {
                // Conservative: assign to max level
                self.node_levels.insert(node.id, self.max_level);
                self.messages.push(format!(
                    "Warning: Node {} could not be leveled, assigned to max level",
                    node.id
                ));
            }
        }
    }

    /// Compute the levels at which to insert pipeline cuts.
    /// Divides the logic depth evenly among the requested stages.
    fn compute_cut_levels(&self) -> Vec<usize> {
        if self.max_level == 0 || self.config.stages <= 1 {
            return vec![];
        }

        let stages = self.config.stages as usize;
        let levels_per_stage = (self.max_level + 1) / stages;

        // Don't cut if there's not enough logic
        if levels_per_stage == 0 {
            self.messages.clone().push(format!(
                "Warning: Not enough logic depth ({}) for {} stages",
                self.max_level + 1,
                stages
            ));
            // Still try to insert at least some cuts
            return (1..stages).collect();
        }

        // Insert cuts after every levels_per_stage levels
        // For N stages, we need N-1 cuts
        (1..stages)
            .map(|i| i * levels_per_stage)
            .filter(|&level| level <= self.max_level)
            .collect()
    }

    /// Insert pipeline registers at the specified cut levels.
    /// For each signal that crosses a cut boundary, create a pipeline
    /// register (FlipFlop) and rewire the consumers to use the registered signal.
    fn insert_registers_at_cuts(&mut self, cut_levels: &[usize]) -> usize {
        if cut_levels.is_empty() {
            return 0;
        }

        let mut signals_to_register: Vec<(String, usize, usize)> = Vec::new(); // (signal_name, width, cut_level)

        // Find signals that cross cut boundaries
        for node in &self.sir.combinational_nodes {
            let node_level = self.node_levels.get(&node.id).copied().unwrap_or(0);

            for output in &node.outputs {
                // Check if any consumer is at a level that crosses a cut
                for consumer in &self.sir.combinational_nodes {
                    let consumer_level = self.node_levels.get(&consumer.id).copied().unwrap_or(0);

                    let uses_signal = consumer
                        .inputs
                        .iter()
                        .any(|i| i.signal_id == output.signal_id);

                    if uses_signal {
                        // Check if there's a cut between producer and consumer
                        for &cut in cut_levels {
                            if node_level < cut && consumer_level >= cut {
                                // Signal crosses this cut - needs a pipeline register
                                let width = self.get_signal_width(&output.signal_id);
                                signals_to_register.push((output.signal_id.clone(), width, cut));
                            }
                        }
                    }
                }
            }
        }

        // Deduplicate signals
        signals_to_register.sort();
        signals_to_register.dedup();

        // Create pipeline registers for each signal
        for (signal_name, width, cut_level) in &signals_to_register {
            self.create_pipeline_register(signal_name, *width, *cut_level);
        }

        signals_to_register.len()
    }

    /// Get the width of a signal by name
    fn get_signal_width(&self, signal_name: &str) -> usize {
        // Check inputs
        if let Some(port) = self.sir.inputs.iter().find(|p| p.name == signal_name) {
            return port.width;
        }
        // Check outputs
        if let Some(port) = self.sir.outputs.iter().find(|p| p.name == signal_name) {
            return port.width;
        }
        // Check signals
        if let Some(sig) = self.sir.signals.iter().find(|s| s.name == signal_name) {
            return sig.width;
        }
        // Default to 32 if not found
        32
    }

    /// Create a pipeline register for a signal at a given cut level.
    /// This creates:
    /// 1. A new signal for the registered output
    /// 2. A FlipFlop node that registers the original signal
    /// 3. Rewires consumers at/after the cut level to use the registered signal
    fn create_pipeline_register(&mut self, signal_name: &str, width: usize, cut_level: usize) {
        let reg_name = format!("{}_pipe_s{}", signal_name, cut_level);

        // Create the registered signal
        let reg_signal = SirSignal {
            name: reg_name.clone(),
            width,
            sir_type: SirType::Bits(width),
            driver_node: Some(self.next_node_id),
            fanout_nodes: vec![],
            is_state: true,
            span: None,
        };
        self.sir.signals.push(reg_signal);

        // Create the FlipFlop node
        let ff_node = SirNode {
            id: self.next_node_id,
            kind: SirNodeKind::FlipFlop {
                clock_edge: ClockEdge::Rising,
            },
            inputs: vec![SignalRef {
                signal_id: signal_name.to_string(),
                bit_range: None,
            }],
            outputs: vec![SignalRef {
                signal_id: reg_name.clone(),
                bit_range: None,
            }],
            clock_domain: Some("clk".to_string()),
            impl_style_hint: Default::default(),
            span: None,
        };
        self.sir.sequential_nodes.push(ff_node);
        self.next_node_id += 1;

        self.pipeline_signals.push(reg_name.clone());

        // Rewire consumers at or after the cut level to use the registered signal
        // We need to collect the changes first, then apply them
        let consumers_to_rewire: Vec<usize> = self
            .sir
            .combinational_nodes
            .iter()
            .filter(|node| {
                let node_level = self.node_levels.get(&node.id).copied().unwrap_or(0);
                node_level >= cut_level && node.inputs.iter().any(|i| i.signal_id == signal_name)
            })
            .map(|n| n.id)
            .collect();

        for node_id in consumers_to_rewire {
            if let Some(node) = self
                .sir
                .combinational_nodes
                .iter_mut()
                .find(|n| n.id == node_id)
            {
                for input in &mut node.inputs {
                    if input.signal_id == signal_name {
                        input.signal_id = reg_name.clone();
                    }
                }
            }
        }

    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pipeline_config_skips_single_stage() {
        let mut sir = SirModule::new("test".to_string());
        sir.pipeline_config = Some(PipelineConfig {
            stages: 1,
            target_freq: None,
            auto_balance: false,
        });

        let result = insert_pipeline_registers(&mut sir);
        assert!(result.is_none(), "Should skip single-stage pipeline");
    }

    #[test]
    fn test_pipeline_config_none() {
        let mut sir = SirModule::new("test".to_string());
        sir.pipeline_config = None;

        let result = insert_pipeline_registers(&mut sir);
        assert!(result.is_none(), "Should skip when no config");
    }

    #[test]
    fn test_pipeline_empty_module() {
        let mut sir = SirModule::new("test".to_string());
        sir.pipeline_config = Some(PipelineConfig {
            stages: 3,
            target_freq: None,
            auto_balance: false,
        });

        let result = insert_pipeline_registers(&mut sir);
        assert!(result.is_some());
        let result = result.unwrap();
        // stages_inserted is 0 when there's no combinational logic to pipeline
        assert_eq!(result.stages_inserted, 0);
        assert!(result.success);
        // No actual pipeline registers because no combinational logic exists
        assert!(result.pipeline_registers.is_empty(), "No logic to pipeline");
    }
}
