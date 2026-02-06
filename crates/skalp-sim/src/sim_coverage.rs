//! Coverage tracking for simulation-based verification
//!
//! Tracks three types of coverage:
//! - **Toggle coverage**: has each signal bit seen both 0→1 and 1→0 transitions?
//! - **Mux arm coverage**: has each arm of each mux been selected?
//! - **Comparison coverage**: has each comparison node produced both true and false?
//!
//! Coverage can be built from a behavioral SirModule or a gate-level GateNetlist.

use indexmap::IndexMap;
use skalp_sir::{BinaryOperation, SirModule, SirNodeKind};

/// Information about a tracked signal for toggle coverage
#[derive(Debug, Clone)]
struct SignalInfo {
    name: String,
    byte_offset: usize,
    width_bits: usize,
}

/// Per-signal toggle tracking.
/// For each tracked bit: we record whether we've seen a 0→1 transition and a 1→0 transition.
pub struct ToggleCoverage {
    /// Previous signal values as flat bytes (from SimulationState signals)
    prev_values: IndexMap<String, Vec<u8>>,
    /// For each signal, bitvec of which bits have seen a rising edge (0→1)
    seen_rise: IndexMap<String, Vec<bool>>,
    /// For each signal, bitvec of which bits have seen a falling edge (1→0)
    seen_fall: IndexMap<String, Vec<bool>>,
    /// Signal info (name -> width in bits)
    signal_widths: IndexMap<String, usize>,
    /// Total bits tracked
    total_bits: usize,
    /// Whether we have received the first snapshot (can't compute transitions until we have two)
    initialized: bool,
}

impl ToggleCoverage {
    fn new() -> Self {
        Self {
            prev_values: IndexMap::new(),
            seen_rise: IndexMap::new(),
            seen_fall: IndexMap::new(),
            signal_widths: IndexMap::new(),
            total_bits: 0,
            initialized: false,
        }
    }

    /// Register a signal to track
    fn add_signal(&mut self, name: &str, width_bits: usize) {
        self.signal_widths.insert(name.to_string(), width_bits);
        self.seen_rise
            .insert(name.to_string(), vec![false; width_bits]);
        self.seen_fall
            .insert(name.to_string(), vec![false; width_bits]);
        self.total_bits += width_bits;
    }

    /// Update toggle coverage from a signal snapshot.
    /// `signals` maps signal name -> byte value (little-endian).
    pub fn update(&mut self, signals: &IndexMap<String, Vec<u8>>) {
        if !self.initialized {
            // First snapshot: just record values, no transitions yet
            for (name, _width) in &self.signal_widths {
                if let Some(bytes) = signals.get(name) {
                    self.prev_values.insert(name.clone(), bytes.clone());
                }
            }
            self.initialized = true;
            return;
        }

        for (name, width) in &self.signal_widths {
            let width = *width;
            if let Some(curr_bytes) = signals.get(name) {
                let prev_bytes = self
                    .prev_values
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| vec![0u8; (width + 7) / 8]);

                // Check each bit for transitions
                if let Some(rise) = self.seen_rise.get_mut(name) {
                    if let Some(fall) = self.seen_fall.get_mut(name) {
                        for bit in 0..width {
                            let byte_idx = bit / 8;
                            let bit_idx = bit % 8;

                            let prev_bit = prev_bytes
                                .get(byte_idx)
                                .map(|b| (b >> bit_idx) & 1 == 1)
                                .unwrap_or(false);
                            let curr_bit = curr_bytes
                                .get(byte_idx)
                                .map(|b| (b >> bit_idx) & 1 == 1)
                                .unwrap_or(false);

                            if !prev_bit && curr_bit {
                                rise[bit] = true; // 0→1
                            }
                            if prev_bit && !curr_bit {
                                fall[bit] = true; // 1→0
                            }
                        }
                    }
                }

                // Update prev
                self.prev_values.insert(name.clone(), curr_bytes.clone());
            }
        }
    }

    /// Update toggle coverage from gate-level bool signals
    pub fn update_gate(&mut self, signals: &IndexMap<String, Vec<bool>>) {
        // Convert to byte representation for uniform handling
        let byte_signals: IndexMap<String, Vec<u8>> = signals
            .iter()
            .map(|(name, bits)| {
                let num_bytes = (bits.len() + 7) / 8;
                let mut bytes = vec![0u8; num_bytes];
                for (i, &b) in bits.iter().enumerate() {
                    if b {
                        bytes[i / 8] |= 1 << (i % 8);
                    }
                }
                (name.clone(), bytes)
            })
            .collect();
        self.update(&byte_signals);
    }

    /// Count of bits that have seen both transitions
    fn covered_count(&self) -> usize {
        let mut count = 0;
        for (name, width) in &self.signal_widths {
            let rise = self.seen_rise.get(name);
            let fall = self.seen_fall.get(name);
            if let (Some(rise), Some(fall)) = (rise, fall) {
                for bit in 0..*width {
                    if rise[bit] && fall[bit] {
                        count += 1;
                    }
                }
            }
        }
        count
    }

    /// Get uncovered toggle points for reporting
    fn uncovered_items(&self) -> Vec<(String, usize, &'static str)> {
        let mut items = Vec::new();
        for (name, width) in &self.signal_widths {
            let rise = self.seen_rise.get(name);
            let fall = self.seen_fall.get(name);
            if let (Some(rise), Some(fall)) = (rise, fall) {
                for bit in 0..*width {
                    if !rise[bit] && !fall[bit] {
                        items.push((name.clone(), bit, "neither"));
                    } else if !rise[bit] {
                        items.push((name.clone(), bit, "0->1"));
                    } else if !fall[bit] {
                        items.push((name.clone(), bit, "1->0"));
                    }
                }
            }
        }
        items
    }
}

/// A single mux coverage point
#[derive(Debug, Clone)]
struct MuxPoint {
    node_name: String,
    output_signal_id: Option<String>,
    num_arms: usize,
    arms_seen: Vec<bool>,
}

/// Track which arm of each mux was selected
pub struct MuxArmCoverage {
    points: Vec<MuxPoint>,
    /// Map from node name to index for quick lookup
    name_to_idx: IndexMap<String, usize>,
}

impl MuxArmCoverage {
    fn new() -> Self {
        Self {
            points: Vec::new(),
            name_to_idx: IndexMap::new(),
        }
    }

    /// Add a mux coverage point
    fn add_mux(&mut self, name: &str, num_arms: usize, output_signal_id: Option<&str>) {
        let idx = self.points.len();
        self.points.push(MuxPoint {
            node_name: name.to_string(),
            output_signal_id: output_signal_id.map(|s| s.to_string()),
            num_arms,
            arms_seen: vec![false; num_arms],
        });
        self.name_to_idx.insert(name.to_string(), idx);
    }

    /// Record that a specific arm was selected.
    /// For Mux (ternary): arm 0 = false branch, arm 1 = true branch.
    /// For ParallelMux: arm i = case i.
    pub fn record_arm(&mut self, name: &str, arm: usize) {
        if let Some(&idx) = self.name_to_idx.get(name) {
            if arm < self.points[idx].num_arms {
                self.points[idx].arms_seen[arm] = true;
            }
        }
    }

    /// Total mux arms
    fn total_arms(&self) -> usize {
        self.points.iter().map(|p| p.num_arms).sum()
    }

    /// Covered mux arms
    fn covered_arms(&self) -> usize {
        self.points
            .iter()
            .flat_map(|p| p.arms_seen.iter())
            .filter(|&&seen| seen)
            .count()
    }

    /// Get uncovered mux arms: (node_name, list of uncovered arm indices)
    pub fn uncovered_mux_arms(&self) -> Vec<(&str, Vec<usize>)> {
        self.points
            .iter()
            .filter_map(|p| {
                let uncovered: Vec<usize> = p
                    .arms_seen
                    .iter()
                    .enumerate()
                    .filter(|(_, &seen)| !seen)
                    .map(|(i, _)| i)
                    .collect();
                if uncovered.is_empty() {
                    None
                } else {
                    Some((p.node_name.as_str(), uncovered))
                }
            })
            .collect()
    }

    /// Returns (node_name, uncovered_arms, output_signal_id) for cross-referencing
    /// against gate netlist signals.
    pub fn uncovered_mux_arms_with_signals(&self) -> Vec<(&str, Vec<usize>, Option<&str>)> {
        self.points
            .iter()
            .filter_map(|p| {
                let uncovered: Vec<usize> = p
                    .arms_seen
                    .iter()
                    .enumerate()
                    .filter(|(_, &seen)| !seen)
                    .map(|(i, _)| i)
                    .collect();
                if uncovered.is_empty() {
                    None
                } else {
                    Some((
                        p.node_name.as_str(),
                        uncovered,
                        p.output_signal_id.as_deref(),
                    ))
                }
            })
            .collect()
    }
}

/// A single comparison coverage point
#[derive(Debug, Clone)]
struct CmpPoint {
    node_name: String,
    op: String,
    seen_true: bool,
    seen_false: bool,
}

/// Track true/false outcomes for comparisons
pub struct ComparisonCoverage {
    points: Vec<CmpPoint>,
    name_to_idx: IndexMap<String, usize>,
}

impl ComparisonCoverage {
    fn new() -> Self {
        Self {
            points: Vec::new(),
            name_to_idx: IndexMap::new(),
        }
    }

    /// Add a comparison coverage point
    fn add_comparison(&mut self, name: &str, op: &str) {
        let idx = self.points.len();
        self.points.push(CmpPoint {
            node_name: name.to_string(),
            op: op.to_string(),
            seen_true: false,
            seen_false: false,
        });
        self.name_to_idx.insert(name.to_string(), idx);
    }

    /// Record a comparison outcome
    pub fn record_outcome(&mut self, name: &str, result: bool) {
        if let Some(&idx) = self.name_to_idx.get(name) {
            if result {
                self.points[idx].seen_true = true;
            } else {
                self.points[idx].seen_false = true;
            }
        }
    }

    /// Total comparison points (each has 2 outcomes)
    fn total_outcomes(&self) -> usize {
        self.points.len() * 2
    }

    /// Covered outcomes
    fn covered_outcomes(&self) -> usize {
        self.points
            .iter()
            .map(|p| p.seen_true as usize + p.seen_false as usize)
            .sum()
    }

    /// Get uncovered comparisons: (node_name, op, missing_outcome)
    pub fn uncovered_comparisons(&self) -> Vec<(&str, &str, bool, bool)> {
        self.points
            .iter()
            .filter(|p| !p.seen_true || !p.seen_false)
            .map(|p| {
                (
                    p.node_name.as_str(),
                    p.op.as_str(),
                    p.seen_true,
                    p.seen_false,
                )
            })
            .collect()
    }
}

/// Combined coverage database
pub struct SimCoverageDb {
    pub toggle: ToggleCoverage,
    pub mux: MuxArmCoverage,
    pub comparison: ComparisonCoverage,
    pub vectors_applied: usize,
}

/// Coverage metrics summary
#[derive(Debug, Clone)]
pub struct CoverageMetrics {
    pub toggle_pct: f64,
    pub toggle_covered: usize,
    pub toggle_total: usize,
    pub mux_pct: f64,
    pub mux_arms_covered: usize,
    pub mux_arms_total: usize,
    pub comparison_pct: f64,
    pub cmp_covered: usize,
    pub cmp_total: usize,
    pub overall_pct: f64,
    pub vectors_applied: usize,
}

fn is_comparison_op(op: &BinaryOperation) -> bool {
    matches!(
        op,
        BinaryOperation::Eq
            | BinaryOperation::Neq
            | BinaryOperation::Lt
            | BinaryOperation::Lte
            | BinaryOperation::Gt
            | BinaryOperation::Gte
            | BinaryOperation::Slt
            | BinaryOperation::Slte
            | BinaryOperation::Sgt
            | BinaryOperation::Sgte
    )
}

fn op_name(op: &BinaryOperation) -> &'static str {
    match op {
        BinaryOperation::Eq => "Eq",
        BinaryOperation::Neq => "Neq",
        BinaryOperation::Lt => "Lt",
        BinaryOperation::Lte => "Lte",
        BinaryOperation::Gt => "Gt",
        BinaryOperation::Gte => "Gte",
        BinaryOperation::Slt => "Slt",
        BinaryOperation::Slte => "Slte",
        BinaryOperation::Sgt => "Sgt",
        BinaryOperation::Sgte => "Sgte",
        _ => "Unknown",
    }
}

impl SimCoverageDb {
    /// Build coverage database from a behavioral SirModule.
    /// Enumerates all signals for toggle coverage, all Mux/ParallelMux nodes
    /// for mux arm coverage, and all comparison nodes for comparison coverage.
    ///
    /// This version tracks ALL signals including compiler-generated intermediates.
    /// For user-visible signals only, use `from_sir_module_user_visible`.
    pub fn from_sir_module(module: &SirModule) -> Self {
        let mut toggle = ToggleCoverage::new();
        let mut mux = MuxArmCoverage::new();
        let mut comparison = ComparisonCoverage::new();

        // Track internal signals and output ports for toggle coverage.
        // Input ports are excluded — they're driven by the test harness,
        // not the design, so their toggle behavior is irrelevant.
        for signal in &module.signals {
            toggle.add_signal(&signal.name, signal.width);
        }
        for port in &module.outputs {
            toggle.add_signal(&port.name, port.width);
        }

        // Enumerate combinational nodes for mux and comparison coverage
        for node in &module.combinational_nodes {
            let node_name = format!("node_{}", node.id);
            match &node.kind {
                SirNodeKind::Mux => {
                    // Binary mux: 2 arms (true/false)
                    let output_sig = node.outputs.first().map(|r| r.signal_id.as_str());
                    mux.add_mux(&node_name, 2, output_sig);
                }
                SirNodeKind::ParallelMux { num_cases, .. } => {
                    let output_sig = node.outputs.first().map(|r| r.signal_id.as_str());
                    mux.add_mux(&node_name, *num_cases, output_sig);
                }
                SirNodeKind::BinaryOp(op) if is_comparison_op(op) => {
                    comparison.add_comparison(&node_name, op_name(op));
                }
                _ => {}
            }
        }

        SimCoverageDb {
            toggle,
            mux,
            comparison,
            vectors_applied: 0,
        }
    }

    /// Build coverage database tracking only user-visible signals from SKALP source.
    ///
    /// This filters signals using the name_registry to only include signals that
    /// have user-defined names in the source code. Compiler-generated intermediates
    /// (like `_s1`, `node_123_out`) are excluded.
    ///
    /// For behavioral testing at the SKALP abstraction level, this gives more
    /// meaningful coverage metrics since it only tracks signals the user defined.
    pub fn from_sir_module_user_visible(module: &SirModule) -> Self {
        let mut toggle = ToggleCoverage::new();
        let mut mux = MuxArmCoverage::new();
        let mut comparison = ComparisonCoverage::new();

        // Track only user-visible signals (those with entries in name_registry)
        for signal in &module.signals {
            // Check if signal has a user-visible name
            if module.name_registry.reverse_resolve(&signal.name).is_some() {
                toggle.add_signal(&signal.name, signal.width);
            }
        }
        // Output ports are always user-visible
        for port in &module.outputs {
            toggle.add_signal(&port.name, port.width);
        }

        // For mux and comparison coverage, track nodes that involve user-visible signals
        // A mux is relevant if its output signal feeds into a user-visible signal
        // (directly or through the dataflow). For simplicity, we include all muxes
        // since they represent branching decisions in user code.
        for node in &module.combinational_nodes {
            let node_name = format!("node_{}", node.id);
            match &node.kind {
                SirNodeKind::Mux => {
                    // Include all muxes - they represent if/else decisions in user code
                    let output_sig = node.outputs.first().map(|r| r.signal_id.as_str());
                    mux.add_mux(&node_name, 2, output_sig);
                }
                SirNodeKind::ParallelMux { num_cases, .. } => {
                    // Include all parallel muxes - they represent case/match decisions
                    let output_sig = node.outputs.first().map(|r| r.signal_id.as_str());
                    mux.add_mux(&node_name, *num_cases, output_sig);
                }
                SirNodeKind::BinaryOp(op) if is_comparison_op(op) => {
                    // For comparisons, check if any input or output is user-visible
                    let any_visible = node.inputs.iter().any(|r| {
                        module.name_registry.reverse_resolve(&r.signal_id).is_some()
                    }) || node.outputs.iter().any(|r| {
                        module.name_registry.reverse_resolve(&r.signal_id).is_some()
                    });
                    if any_visible {
                        comparison.add_comparison(&node_name, op_name(op));
                    }
                }
                _ => {}
            }
        }

        SimCoverageDb {
            toggle,
            mux,
            comparison,
            vectors_applied: 0,
        }
    }

    /// Build coverage database for gate-level (toggle only).
    /// Gate-level SIR doesn't have high-level Mux/Comparison nodes,
    /// so we only track toggle coverage on nets.
    pub fn from_gate_netlist(
        signal_names: &[(String, usize)], // (name, width_bits)
    ) -> Self {
        let mut toggle = ToggleCoverage::new();
        for (name, width) in signal_names {
            toggle.add_signal(name, *width);
        }

        SimCoverageDb {
            toggle,
            mux: MuxArmCoverage::new(),
            comparison: ComparisonCoverage::new(),
            vectors_applied: 0,
        }
    }

    /// Update toggle coverage from a behavioral signal snapshot
    /// (from SimulationState.signals: IndexMap<String, Vec<u8>>)
    pub fn update_toggle(&mut self, signals: &IndexMap<String, Vec<u8>>) {
        self.toggle.update(signals);
    }

    /// Update toggle coverage from gate-level bool signals (IndexMap)
    pub fn update_toggle_gate(&mut self, signals: &IndexMap<String, Vec<bool>>) {
        self.toggle.update_gate(signals);
    }

    /// Update toggle coverage from gate-level bool signals (Vec of pairs)
    pub fn update_toggle_gate_vec(&mut self, signals: &[(String, Vec<bool>)]) {
        let map: IndexMap<String, Vec<bool>> = signals.iter().cloned().collect();
        self.toggle.update_gate(&map);
    }

    /// Update mux and comparison coverage from behavioral signal snapshot.
    /// For mux nodes, we read the selector signal to determine which arm was taken.
    /// For comparison nodes, we read the output signal to determine true/false.
    pub fn update_nodes(&mut self, signals: &IndexMap<String, Vec<u8>>, module: &SirModule) {
        for node in &module.combinational_nodes {
            let node_name = format!("node_{}", node.id);
            match &node.kind {
                SirNodeKind::Mux => {
                    // Mux: first input is selector, read its value
                    if let Some(sel_ref) = node.inputs.first() {
                        if let Some(sel_bytes) = signals.get(&sel_ref.signal_id) {
                            let sel_val = sel_bytes.first().copied().unwrap_or(0);
                            let arm = if sel_val != 0 { 1 } else { 0 };
                            self.mux.record_arm(&node_name, arm);
                        }
                    }
                }
                SirNodeKind::ParallelMux { match_values, .. } => {
                    // ParallelMux: first input is selector
                    if let Some(sel_ref) = node.inputs.first() {
                        if let Some(sel_bytes) = signals.get(&sel_ref.signal_id) {
                            let sel_val = {
                                let mut v = 0u64;
                                for (i, &b) in sel_bytes.iter().take(8).enumerate() {
                                    v |= (b as u64) << (i * 8);
                                }
                                v
                            };
                            // Find which match value was selected
                            for (i, &mv) in match_values.iter().enumerate() {
                                if sel_val == mv {
                                    self.mux.record_arm(&node_name, i);
                                    break;
                                }
                            }
                        }
                    }
                }
                SirNodeKind::BinaryOp(op) if is_comparison_op(op) => {
                    // Comparison: read the output signal
                    if let Some(out_ref) = node.outputs.first() {
                        if let Some(out_bytes) = signals.get(&out_ref.signal_id) {
                            let out_val = out_bytes.first().copied().unwrap_or(0);
                            self.comparison.record_outcome(&node_name, out_val != 0);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Increment vector count
    pub fn record_vector(&mut self) {
        self.vectors_applied += 1;
    }

    /// Compute current coverage metrics
    pub fn metrics(&self) -> CoverageMetrics {
        let toggle_covered = self.toggle.covered_count();
        let toggle_total = self.toggle.total_bits;
        let toggle_pct = if toggle_total > 0 {
            (toggle_covered as f64 / toggle_total as f64) * 100.0
        } else {
            100.0
        };

        let mux_covered = self.mux.covered_arms();
        let mux_total = self.mux.total_arms();
        let mux_pct = if mux_total > 0 {
            (mux_covered as f64 / mux_total as f64) * 100.0
        } else {
            100.0
        };

        let cmp_covered = self.comparison.covered_outcomes();
        let cmp_total = self.comparison.total_outcomes();
        let cmp_pct = if cmp_total > 0 {
            (cmp_covered as f64 / cmp_total as f64) * 100.0
        } else {
            100.0
        };

        // Weighted overall: toggle has 50% weight, mux 30%, comparison 20%
        let total_all = toggle_total + mux_total + cmp_total;
        let covered_all = toggle_covered + mux_covered + cmp_covered;
        let overall_pct = if total_all > 0 {
            (covered_all as f64 / total_all as f64) * 100.0
        } else {
            100.0
        };

        CoverageMetrics {
            toggle_pct,
            toggle_covered,
            toggle_total,
            mux_pct,
            mux_arms_covered: mux_covered,
            mux_arms_total: mux_total,
            comparison_pct: cmp_pct,
            cmp_covered,
            cmp_total,
            overall_pct,
            vectors_applied: self.vectors_applied,
        }
    }

    /// Get uncovered mux arms (for vector generation feedback)
    pub fn uncovered_mux_arms(&self) -> Vec<(&str, Vec<usize>)> {
        self.mux.uncovered_mux_arms()
    }

    /// Get uncovered mux arms with output signal IDs (for gate netlist cross-referencing)
    pub fn uncovered_mux_arms_with_signals(&self) -> Vec<(&str, Vec<usize>, Option<&str>)> {
        self.mux.uncovered_mux_arms_with_signals()
    }

    /// Get uncovered comparisons
    pub fn uncovered_comparisons(&self) -> Vec<(&str, &str, bool, bool)> {
        self.comparison.uncovered_comparisons()
    }

    /// Get uncovered toggle items (signal, bit, missing_direction)
    pub fn uncovered_toggles(&self) -> Vec<(String, usize, &'static str)> {
        self.toggle.uncovered_items()
    }

    /// Get tracked toggle signal names and their widths (for cross-referencing)
    pub fn tracked_toggle_signals(&self) -> &IndexMap<String, usize> {
        &self.toggle.signal_widths
    }

    /// Count of covered toggle bits for a specific signal
    pub fn toggle_covered_for_signal(&self, name: &str) -> usize {
        if let (Some(rise), Some(fall)) = (
            self.toggle.seen_rise.get(name),
            self.toggle.seen_fall.get(name),
        ) {
            rise.iter()
                .zip(fall.iter())
                .filter(|(&r, &f)| r && f)
                .count()
        } else {
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_toggle_coverage_basic() {
        let mut toggle = ToggleCoverage::new();
        toggle.add_signal("a", 4);
        assert_eq!(toggle.total_bits, 4);

        // First snapshot: 0b0000
        let mut signals = IndexMap::new();
        signals.insert("a".to_string(), vec![0b0000u8]);
        toggle.update(&signals);
        assert_eq!(toggle.covered_count(), 0);

        // Second snapshot: 0b1010 - bits 1 and 3 rose
        signals.insert("a".to_string(), vec![0b1010u8]);
        toggle.update(&signals);
        // No full coverage yet (only rise seen)
        assert_eq!(toggle.covered_count(), 0);

        // Third snapshot: 0b0101 - bits 1 and 3 fell, bits 0 and 2 rose
        signals.insert("a".to_string(), vec![0b0101u8]);
        toggle.update(&signals);
        // Bits 1 and 3 have full coverage (seen rise and fall)
        assert_eq!(toggle.covered_count(), 2);

        // Fourth: 0b0000 - bits 0 and 2 fall
        signals.insert("a".to_string(), vec![0b0000u8]);
        toggle.update(&signals);
        assert_eq!(toggle.covered_count(), 4);
    }

    #[test]
    fn test_mux_coverage() {
        let mut mux = MuxArmCoverage::new();
        mux.add_mux("mux0", 2, None);
        mux.add_mux("mux1", 4, None);

        assert_eq!(mux.total_arms(), 6);
        assert_eq!(mux.covered_arms(), 0);

        mux.record_arm("mux0", 0);
        assert_eq!(mux.covered_arms(), 1);

        mux.record_arm("mux0", 1);
        assert_eq!(mux.covered_arms(), 2);

        mux.record_arm("mux1", 2);
        assert_eq!(mux.covered_arms(), 3);

        let uncovered = mux.uncovered_mux_arms();
        assert_eq!(uncovered.len(), 1); // Only mux1 has uncovered arms
        assert_eq!(uncovered[0].0, "mux1");
        assert_eq!(uncovered[0].1, vec![0, 1, 3]);
    }

    #[test]
    fn test_comparison_coverage() {
        let mut cmp = ComparisonCoverage::new();
        cmp.add_comparison("cmp0", "Eq");

        assert_eq!(cmp.total_outcomes(), 2);
        assert_eq!(cmp.covered_outcomes(), 0);

        cmp.record_outcome("cmp0", true);
        assert_eq!(cmp.covered_outcomes(), 1);

        cmp.record_outcome("cmp0", false);
        assert_eq!(cmp.covered_outcomes(), 2);
    }

    #[test]
    fn test_coverage_metrics() {
        let mut db = SimCoverageDb {
            toggle: ToggleCoverage::new(),
            mux: MuxArmCoverage::new(),
            comparison: ComparisonCoverage::new(),
            vectors_applied: 0,
        };

        // Empty db should report 100%
        let m = db.metrics();
        assert_eq!(m.overall_pct, 100.0);

        // Add some coverage points
        db.toggle.add_signal("x", 2);
        db.mux.add_mux("m", 2, None);
        db.comparison.add_comparison("c", "Eq");

        let m = db.metrics();
        assert_eq!(m.toggle_total, 2);
        assert_eq!(m.mux_arms_total, 2);
        assert_eq!(m.cmp_total, 2);
        assert_eq!(m.overall_pct, 0.0);
    }
}
