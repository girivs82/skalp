//! Static Timing Analysis (STA)
//!
//! This module implements forward and backward timing propagation for
//! analyzing timing paths in the design.
//!
//! # Algorithm
//!
//! 1. **Forward Propagation**: Compute arrival times from primary inputs
//!    to all nodes in topological order.
//!
//! 2. **Backward Propagation**: Compute required times from primary outputs
//!    back to all nodes in reverse topological order.
//!
//! 3. **Slack Calculation**: slack = required_time - arrival_time
//!
//! # References
//!
//! - Static Timing Analysis for Nanometer Designs (Bhasker & Chadha)
//! - Synopsys PrimeTime User Guide

use super::timing::{
    CapacitanceFf, CellTiming, ClockDefinition, NetTiming, NldmTable, OperatingConditions,
    PinDirection, TimePs, TimingConstraints,
};
use super::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::{HashMap, HashSet, VecDeque};

/// Static Timing Analyzer
pub struct Sta {
    /// Timing information per node
    node_timing: HashMap<AigNodeId, NetTiming>,
    /// Cell library with timing models
    cell_library: HashMap<String, CellTiming>,
    /// Timing constraints
    constraints: TimingConstraints,
    /// Operating conditions
    conditions: OperatingConditions,
    /// Default wire delay per fanout
    wire_delay_per_fanout: TimePs,
    /// Default wire capacitance per fanout
    wire_cap_per_fanout: CapacitanceFf,
    /// Default input slew
    default_input_slew: TimePs,
}

impl Default for Sta {
    fn default() -> Self {
        Self::new()
    }
}

impl Sta {
    /// Create a new STA engine with default settings
    pub fn new() -> Self {
        Self {
            node_timing: HashMap::new(),
            cell_library: HashMap::new(),
            constraints: TimingConstraints::new(),
            conditions: OperatingConditions::typical(),
            wire_delay_per_fanout: 10.0, // 10ps per fanout
            wire_cap_per_fanout: 5.0,    // 5fF per fanout
            default_input_slew: 100.0,   // 100ps default slew
        }
    }

    /// Set timing constraints
    pub fn set_constraints(&mut self, constraints: TimingConstraints) {
        self.constraints = constraints;
    }

    /// Set operating conditions
    pub fn set_conditions(&mut self, conditions: OperatingConditions) {
        self.conditions = conditions;
    }

    /// Add a cell timing model to the library
    pub fn add_cell_timing(&mut self, cell: CellTiming) {
        self.cell_library.insert(cell.name.clone(), cell);
    }

    /// Run full timing analysis on an AIG
    pub fn analyze(&mut self, aig: &Aig) -> StaResult {
        // Clear previous results
        self.node_timing.clear();

        // Compute fanout counts for wire delay estimation
        let fanout_counts = self.compute_fanout_counts(aig);

        // Get topological order
        let topo_order = self.topological_sort(aig);

        // Forward propagation (arrival times)
        self.propagate_forward(aig, &topo_order, &fanout_counts);

        // Backward propagation (required times)
        self.propagate_backward(aig, &topo_order);

        // Collect results
        self.collect_results(aig)
    }

    /// Compute fanout counts for each node
    fn compute_fanout_counts(&self, aig: &Aig) -> HashMap<AigNodeId, usize> {
        let mut counts: HashMap<AigNodeId, usize> = HashMap::new();

        for (_, node) in aig.iter_nodes() {
            for fanin in node.fanins() {
                *counts.entry(fanin.node).or_insert(0) += 1;
            }
        }

        // Count output fanouts
        for (_, lit) in aig.outputs() {
            *counts.entry(lit.node).or_insert(0) += 1;
        }

        counts
    }

    /// Compute topological order of nodes
    fn topological_sort(&self, aig: &Aig) -> Vec<AigNodeId> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut in_stack = HashSet::new();

        fn visit(
            aig: &Aig,
            node: AigNodeId,
            visited: &mut HashSet<AigNodeId>,
            in_stack: &mut HashSet<AigNodeId>,
            order: &mut Vec<AigNodeId>,
        ) {
            if visited.contains(&node) {
                return;
            }
            if in_stack.contains(&node) {
                return; // Cycle detected - skip
            }

            in_stack.insert(node);

            if let Some(n) = aig.get_node(node) {
                for fanin in n.fanins() {
                    visit(aig, fanin.node, visited, in_stack, order);
                }
            }

            in_stack.remove(&node);
            visited.insert(node);
            order.push(node);
        }

        // Visit all nodes
        for (id, _) in aig.iter_nodes() {
            visit(aig, id, &mut visited, &mut in_stack, &mut order);
        }

        order
    }

    /// Forward propagation: compute arrival times
    fn propagate_forward(
        &mut self,
        aig: &Aig,
        topo_order: &[AigNodeId],
        fanout_counts: &HashMap<AigNodeId, usize>,
    ) {
        for &node_id in topo_order {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            let mut timing = NetTiming::new();

            match node {
                AigNode::Input { name, .. } => {
                    // Check for input delay constraint
                    let input_delay = self
                        .constraints
                        .input_delays
                        .get(name)
                        .map(|d| d.delay)
                        .unwrap_or(0.0);

                    timing.arrival_rise = input_delay;
                    timing.arrival_fall = input_delay;
                    timing.slew_rise = self.default_input_slew;
                    timing.slew_fall = self.default_input_slew;
                }

                AigNode::Const => {
                    // Constants have zero arrival time
                    timing.arrival_rise = 0.0;
                    timing.arrival_fall = 0.0;
                    timing.slew_rise = 0.0;
                    timing.slew_fall = 0.0;
                }

                AigNode::Latch { data, .. } => {
                    // Latches break timing paths at clock edge
                    // Use clock-to-Q delay
                    let clk_to_q = self.get_latch_clk_to_q();
                    timing.arrival_rise = clk_to_q;
                    timing.arrival_fall = clk_to_q;

                    // Slew from data input
                    if let Some(data_timing) = self.node_timing.get(&data.node) {
                        timing.slew_rise = data_timing.slew_rise;
                        timing.slew_fall = data_timing.slew_fall;
                    }
                }

                AigNode::And { left, right } => {
                    // Get arrival times from inputs
                    let left_timing = self
                        .node_timing
                        .get(&left.node)
                        .cloned()
                        .unwrap_or_default();
                    let right_timing = self
                        .node_timing
                        .get(&right.node)
                        .cloned()
                        .unwrap_or_default();

                    // Account for inversions in arrival
                    let left_arr_rise = if left.inverted {
                        left_timing.arrival_fall
                    } else {
                        left_timing.arrival_rise
                    };
                    let left_arr_fall = if left.inverted {
                        left_timing.arrival_rise
                    } else {
                        left_timing.arrival_fall
                    };

                    let right_arr_rise = if right.inverted {
                        right_timing.arrival_fall
                    } else {
                        right_timing.arrival_rise
                    };
                    let right_arr_fall = if right.inverted {
                        right_timing.arrival_rise
                    } else {
                        right_timing.arrival_fall
                    };

                    // AND gate: output rises when BOTH inputs are high
                    // So rise arrival = max(left_rise, right_rise) + gate_delay
                    let gate_delay = self.get_and_gate_delay();

                    // Wire delay based on fanout
                    let fanout = fanout_counts.get(&node_id).copied().unwrap_or(1) as f64;
                    let wire_delay = self.wire_delay_per_fanout * fanout;

                    timing.arrival_rise =
                        left_arr_rise.max(right_arr_rise) + gate_delay + wire_delay;
                    timing.arrival_fall =
                        left_arr_fall.min(right_arr_fall) + gate_delay + wire_delay;

                    // Slew degradation
                    let input_slew = left_timing.slew().max(right_timing.slew());
                    let load = self.wire_cap_per_fanout * fanout;
                    timing.slew_rise = self.compute_output_slew(input_slew, load);
                    timing.slew_fall = timing.slew_rise;
                    timing.load = load;
                }

                AigNode::Barrier { data, .. } => {
                    // Barriers are power domain boundary cells (level shifters, etc.)
                    // They have a fixed delay similar to buffers
                    let barrier_delay = 30.0; // Level shifter/isolation cell delay in ps

                    if let Some(data_timing) = self.node_timing.get(&data.node) {
                        let data_arr_rise = if data.inverted {
                            data_timing.arrival_fall
                        } else {
                            data_timing.arrival_rise
                        };
                        let data_arr_fall = if data.inverted {
                            data_timing.arrival_rise
                        } else {
                            data_timing.arrival_fall
                        };

                        // Wire delay based on fanout
                        let fanout = fanout_counts.get(&node_id).copied().unwrap_or(1) as f64;
                        let wire_delay = self.wire_delay_per_fanout * fanout;

                        timing.arrival_rise = data_arr_rise + barrier_delay + wire_delay;
                        timing.arrival_fall = data_arr_fall + barrier_delay + wire_delay;
                        timing.slew_rise = data_timing.slew_rise;
                        timing.slew_fall = data_timing.slew_fall;
                        timing.load = self.wire_cap_per_fanout * fanout;
                    }
                }
            }

            self.node_timing.insert(node_id, timing);
        }
    }

    /// Backward propagation: compute required times
    fn propagate_backward(&mut self, aig: &Aig, topo_order: &[AigNodeId]) {
        // Initialize required times for outputs
        for (name, lit) in aig.outputs() {
            if let Some(timing) = self.node_timing.get_mut(&lit.node) {
                // Get clock period from first defined clock
                let clock_period = self
                    .constraints
                    .clocks
                    .values()
                    .next()
                    .map(|c| c.period)
                    .unwrap_or(10000.0); // Default 10ns

                // Get output delay constraint
                let output_delay = self
                    .constraints
                    .output_delays
                    .get(name)
                    .map(|d| d.delay)
                    .unwrap_or(0.0);

                let required = clock_period - output_delay;
                timing.required_rise = required;
                timing.required_fall = required;
            }
        }

        // Propagate backwards through the graph
        for &node_id in topo_order.iter().rev() {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            // Get current required time for this node
            let current_required = self
                .node_timing
                .get(&node_id)
                .map(|t| (t.required_rise, t.required_fall))
                .unwrap_or((f64::MAX, f64::MAX));

            // Propagate to fanins
            for fanin in node.fanins() {
                let gate_delay = match node {
                    AigNode::And { .. } => self.get_and_gate_delay(),
                    AigNode::Latch { .. } => self.get_latch_setup(),
                    _ => 0.0,
                };

                let new_required_rise = current_required.0 - gate_delay;
                let new_required_fall = current_required.1 - gate_delay;

                if let Some(fanin_timing) = self.node_timing.get_mut(&fanin.node) {
                    // Account for inversion
                    if fanin.inverted {
                        fanin_timing.required_rise =
                            fanin_timing.required_rise.min(new_required_fall);
                        fanin_timing.required_fall =
                            fanin_timing.required_fall.min(new_required_rise);
                    } else {
                        fanin_timing.required_rise =
                            fanin_timing.required_rise.min(new_required_rise);
                        fanin_timing.required_fall =
                            fanin_timing.required_fall.min(new_required_fall);
                    }
                }
            }
        }
    }

    /// Collect timing analysis results
    fn collect_results(&self, aig: &Aig) -> StaResult {
        let mut result = StaResult::new();

        // Find worst negative slack (WNS)
        let mut wns = f64::MAX;
        let mut wns_path: Option<TimingPath> = None;

        // Find total negative slack (TNS)
        let mut tns = 0.0;
        let mut failing_endpoints = 0;

        for (name, lit) in aig.outputs() {
            if let Some(timing) = self.node_timing.get(&lit.node) {
                let slack = timing.slack();

                if slack < 0.0 {
                    tns += slack;
                    failing_endpoints += 1;

                    if slack < wns {
                        wns = slack;
                        wns_path = Some(TimingPath {
                            startpoint: "input".to_string(),
                            endpoint: name.clone(),
                            path_type: PathType::Setup,
                            arrival: timing.arrival(),
                            required: timing.required(),
                            slack,
                            stages: Vec::new(),
                        });
                    }
                }
            }
        }

        result.wns = if wns == f64::MAX { 0.0 } else { wns };
        result.tns = tns;
        result.failing_endpoints = failing_endpoints;
        result.total_endpoints = aig.outputs().len();
        result.worst_path = wns_path;

        // Store all node timing
        result.node_timing = self.node_timing.clone();

        result
    }

    /// Get AND gate delay (default model)
    fn get_and_gate_delay(&self) -> TimePs {
        // Look up in library if available
        if let Some(cell) = self.cell_library.get("AND2_X1") {
            return cell
                .timing_arcs
                .first()
                .and_then(|arc| arc.cell_rise.as_ref())
                .map(|t| t.lookup(self.default_input_slew, 10.0))
                .unwrap_or(20.0);
        }
        20.0 // Default 20ps
    }

    /// Get latch clock-to-Q delay
    fn get_latch_clk_to_q(&self) -> TimePs {
        if let Some(cell) = self.cell_library.get("DFF_X1") {
            return cell
                .timing_arcs
                .iter()
                .find(|arc| arc.from_pin == "CK")
                .and_then(|arc| arc.cell_rise.as_ref())
                .map(|t| t.lookup(self.default_input_slew, 10.0))
                .unwrap_or(50.0);
        }
        50.0 // Default 50ps
    }

    /// Get latch setup time
    fn get_latch_setup(&self) -> TimePs {
        if let Some(cell) = self.cell_library.get("DFF_X1") {
            return cell
                .timing_arcs
                .iter()
                .find(|arc| arc.from_pin == "D")
                .and_then(|arc| arc.setup.as_ref())
                .map(|t| t.lookup(self.default_input_slew, 0.0))
                .unwrap_or(30.0);
        }
        30.0 // Default 30ps
    }

    /// Compute output slew based on input slew and load
    fn compute_output_slew(&self, input_slew: TimePs, load: CapacitanceFf) -> TimePs {
        // Simple linear model: slew = base_slew + k * load
        let base_slew = input_slew * 0.8;
        let load_factor = 5.0; // ps per fF
        base_slew + load_factor * load
    }

    /// Get timing for a specific node
    pub fn get_node_timing(&self, node: AigNodeId) -> Option<&NetTiming> {
        self.node_timing.get(&node)
    }

    /// Get all critical nodes (negative slack)
    pub fn get_critical_nodes(&self) -> Vec<(AigNodeId, &NetTiming)> {
        self.node_timing
            .iter()
            .filter(|(_, t)| t.is_critical())
            .map(|(&id, t)| (id, t))
            .collect()
    }
}

/// Results of static timing analysis
#[derive(Debug, Clone)]
pub struct StaResult {
    /// Worst Negative Slack
    pub wns: TimePs,
    /// Total Negative Slack
    pub tns: TimePs,
    /// Number of failing endpoints
    pub failing_endpoints: usize,
    /// Total number of endpoints
    pub total_endpoints: usize,
    /// Worst timing path
    pub worst_path: Option<TimingPath>,
    /// Per-node timing information
    pub node_timing: HashMap<AigNodeId, NetTiming>,
}

impl StaResult {
    /// Create a new empty result
    fn new() -> Self {
        Self {
            wns: 0.0,
            tns: 0.0,
            failing_endpoints: 0,
            total_endpoints: 0,
            worst_path: None,
            node_timing: HashMap::new(),
        }
    }

    /// Check if timing is met
    pub fn is_timing_met(&self) -> bool {
        self.wns >= 0.0
    }

    /// Get a summary string
    pub fn summary(&self) -> String {
        let status = if self.is_timing_met() {
            "MET"
        } else {
            "VIOLATED"
        };
        format!(
            "Timing {}: WNS={:.2}ps, TNS={:.2}ps, Failing={}/{}",
            status, self.wns, self.tns, self.failing_endpoints, self.total_endpoints
        )
    }
}

/// A timing path
#[derive(Debug, Clone)]
pub struct TimingPath {
    /// Start point name
    pub startpoint: String,
    /// End point name
    pub endpoint: String,
    /// Path type (setup/hold)
    pub path_type: PathType,
    /// Arrival time at endpoint
    pub arrival: TimePs,
    /// Required time at endpoint
    pub required: TimePs,
    /// Slack
    pub slack: TimePs,
    /// Stages in the path
    pub stages: Vec<PathStage>,
}

/// Type of timing path
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathType {
    Setup,
    Hold,
}

/// A stage in a timing path
#[derive(Debug, Clone)]
pub struct PathStage {
    /// Node or cell name
    pub name: String,
    /// Incremental delay
    pub incr: TimePs,
    /// Cumulative arrival time
    pub arrival: TimePs,
    /// Transition type
    pub transition: TransitionType,
}

/// Signal transition type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransitionType {
    Rise,
    Fall,
}

/// Timing-driven optimization hints
#[derive(Debug, Clone)]
pub struct TimingOptHints {
    /// Nodes on critical paths that should be optimized for delay
    pub critical_nodes: Vec<AigNodeId>,
    /// Nodes with slack that can be area-optimized
    pub slack_nodes: Vec<(AigNodeId, TimePs)>,
    /// Suggested buffer insertions
    pub buffer_suggestions: Vec<BufferSuggestion>,
}

/// Buffer insertion suggestion
#[derive(Debug, Clone)]
pub struct BufferSuggestion {
    /// Node to buffer
    pub node: AigNodeId,
    /// Suggested buffer strength
    pub strength: BufferStrength,
    /// Expected improvement
    pub improvement: TimePs,
}

/// Buffer drive strength
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferStrength {
    X1,
    X2,
    X4,
    X8,
}

impl Sta {
    /// Generate timing-driven optimization hints
    pub fn generate_opt_hints(&self, aig: &Aig) -> TimingOptHints {
        let mut hints = TimingOptHints {
            critical_nodes: Vec::new(),
            slack_nodes: Vec::new(),
            buffer_suggestions: Vec::new(),
        };

        for (id, timing) in &self.node_timing {
            let slack = timing.slack();

            if slack < 0.0 {
                hints.critical_nodes.push(*id);

                // Suggest buffering for high-fanout critical nodes
                if timing.load > 20.0 {
                    hints.buffer_suggestions.push(BufferSuggestion {
                        node: *id,
                        strength: if timing.load > 50.0 {
                            BufferStrength::X4
                        } else {
                            BufferStrength::X2
                        },
                        improvement: timing.load * 0.3, // Rough estimate
                    });
                }
            } else if slack > 1000.0 {
                // Lots of slack - can area optimize
                hints.slack_nodes.push((*id, slack));
            }
        }

        // Sort critical nodes by criticality
        hints.critical_nodes.sort_by(|a, b| {
            let slack_a = self.node_timing.get(a).map(|t| t.slack()).unwrap_or(0.0);
            let slack_b = self.node_timing.get(b).map(|t| t.slack()).unwrap_or(0.0);
            slack_a
                .partial_cmp(&slack_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        hints
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::synth::AigLit;

    #[test]
    fn test_sta_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut sta = Sta::new();
        let result = sta.analyze(&aig);

        assert!(result.is_timing_met());
        assert_eq!(result.total_endpoints, 1);
    }

    #[test]
    fn test_sta_chain() {
        let mut aig = Aig::new("chain".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Create a chain: ab -> abc -> abcd
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let c = aig.add_input("c".to_string(), None);
        let abc = aig.add_and(ab, AigLit::new(c));
        let d = aig.add_input("d".to_string(), None);
        let abcd = aig.add_and(abc, AigLit::new(d));

        aig.add_output("y".to_string(), abcd);

        let mut sta = Sta::new();
        let result = sta.analyze(&aig);

        // Check that arrival times increase along the chain
        let timing_ab = sta.get_node_timing(ab.node);
        let timing_abc = sta.get_node_timing(abc.node);
        let timing_abcd = sta.get_node_timing(abcd.node);

        assert!(timing_ab.is_some());
        assert!(timing_abc.is_some());
        assert!(timing_abcd.is_some());

        let arr_ab = timing_ab.unwrap().arrival();
        let arr_abc = timing_abc.unwrap().arrival();
        let arr_abcd = timing_abcd.unwrap().arrival();

        assert!(arr_abc > arr_ab);
        assert!(arr_abcd > arr_abc);
    }

    #[test]
    fn test_sta_with_constraints() {
        let mut aig = Aig::new("constrained".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut sta = Sta::new();

        // Add timing constraints
        let mut constraints = TimingConstraints::new();
        constraints.add_clock("clk", 1000.0); // 1ns clock
        constraints.add_input_delay("a", "clk", 100.0);
        constraints.add_output_delay("y", "clk", 200.0);
        sta.set_constraints(constraints);

        let result = sta.analyze(&aig);

        // Required time should be clock_period - output_delay = 800ps
        if let Some(timing) = sta.get_node_timing(ab.node) {
            assert!((timing.required() - 800.0).abs() < 1.0);
        }
    }

    #[test]
    fn test_sta_violation() {
        let mut aig = Aig::new("tight".to_string());

        // Create a long chain that will violate timing
        let first = aig.add_input("in".to_string(), None);
        let mut prev = AigLit::new(first);
        for i in 0..50 {
            let next = aig.add_input(format!("x{}", i), None);
            prev = aig.add_and(prev, AigLit::new(next));
        }
        aig.add_output("out".to_string(), prev);

        let mut sta = Sta::new();

        // Very tight constraint
        let mut constraints = TimingConstraints::new();
        constraints.add_clock("clk", 100.0); // 100ps = 10GHz (impossibly fast)
        sta.set_constraints(constraints);

        let result = sta.analyze(&aig);

        // Should have timing violations
        assert!(!result.is_timing_met());
        assert!(result.wns < 0.0);
    }

    #[test]
    fn test_sta_result_summary() {
        let result = StaResult {
            wns: -50.0,
            tns: -150.0,
            failing_endpoints: 3,
            total_endpoints: 10,
            worst_path: None,
            node_timing: HashMap::new(),
        };

        let summary = result.summary();
        assert!(summary.contains("VIOLATED"));
        assert!(summary.contains("-50.00"));
        assert!(summary.contains("3/10"));
    }

    #[test]
    fn test_sta_opt_hints() {
        let mut aig = Aig::new("hints".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut sta = Sta::new();
        sta.analyze(&aig);

        let hints = sta.generate_opt_hints(&aig);

        // With relaxed constraints, should have slack nodes
        assert!(hints.critical_nodes.is_empty() || !hints.slack_nodes.is_empty());
    }

    #[test]
    fn test_topological_sort() {
        let mut aig = Aig::new("topo".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let sta = Sta::new();
        let order = sta.topological_sort(&aig);

        // Inputs should come before the AND gate in topological order
        let pos_a = order.iter().position(|&x| x == a);
        let pos_b = order.iter().position(|&x| x == b);
        let pos_ab = order.iter().position(|&x| x == ab.node);

        assert!(pos_a.is_some());
        assert!(pos_b.is_some());
        assert!(pos_ab.is_some());
        assert!(pos_a.unwrap() < pos_ab.unwrap());
        assert!(pos_b.unwrap() < pos_ab.unwrap());
    }
}
