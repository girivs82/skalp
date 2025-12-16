//! Structural Pattern Detection for Safety Mechanisms
//!
//! Detects safety mechanism patterns in gate netlists, including:
//! - Triple Modular Redundancy (TMR)
//! - Dual modular redundancy
//! - Voter circuits
//! - Watchdog patterns
//!
//! # Usage
//!
//! ```ignore
//! use skalp_lir::pattern_detector::PatternDetector;
//!
//! let detector = PatternDetector::new();
//! let patterns = detector.detect_patterns(&gate_netlist);
//!
//! for pattern in &patterns.tmr_patterns {
//!     println!("Found TMR: {} with voter {}", pattern.name, pattern.voter_cell);
//! }
//! ```

use crate::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

// ============================================================================
// Pattern Types
// ============================================================================

/// Detected safety mechanism patterns in a gate netlist
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DetectedPatterns {
    /// Triple Modular Redundancy patterns
    pub tmr_patterns: Vec<TmrPattern>,
    /// Dual Modular Redundancy patterns
    pub dmr_patterns: Vec<DmrPattern>,
    /// Standalone voter circuits
    pub voters: Vec<VoterPattern>,
    /// Watchdog/timeout monitor patterns
    pub watchdogs: Vec<WatchdogPattern>,
}

impl DetectedPatterns {
    /// Total number of detected patterns
    pub fn total_count(&self) -> usize {
        self.tmr_patterns.len() + self.dmr_patterns.len() + self.voters.len() + self.watchdogs.len()
    }

    /// Check if any safety patterns were detected
    pub fn has_patterns(&self) -> bool {
        self.total_count() > 0
    }
}

/// Triple Modular Redundancy pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TmrPattern {
    /// Pattern name/identifier
    pub name: String,
    /// Cells in replica A
    pub replica_a: Vec<CellId>,
    /// Cells in replica B
    pub replica_b: Vec<CellId>,
    /// Cells in replica C
    pub replica_c: Vec<CellId>,
    /// Voter cell(s)
    pub voters: Vec<CellId>,
    /// Primary outputs from the voter
    pub outputs: Vec<GateNetId>,
    /// Diagnostic coverage (percentage)
    /// Based on ISO 26262 Table D.8: TMR typically provides 99% DC
    pub diagnostic_coverage: f64,
    /// Whether the TMR is complete (all paths covered)
    pub is_complete: bool,
}

impl TmrPattern {
    /// Calculate total FIT for the TMR structure
    /// TMR reduces effective FIT by (1 - 2p^2) where p is single replica failure probability
    pub fn effective_fit(&self, netlist: &GateNetlist) -> f64 {
        let replica_fit: f64 = self
            .replica_a
            .iter()
            .filter_map(|id| netlist.get_cell(*id))
            .map(|c| c.fit)
            .sum();

        let voter_fit: f64 = self
            .voters
            .iter()
            .filter_map(|id| netlist.get_cell(*id))
            .map(|c| c.fit)
            .sum();

        // Simplified TMR FIT calculation
        // Assumes replicas fail independently
        // Voter is a single point of failure
        let replica_failure_rate = replica_fit / 1e9; // Convert FIT to failures/hour
        let tmr_failure_rate = 3.0 * replica_failure_rate.powi(2) + voter_fit / 1e9;

        tmr_failure_rate * 1e9 // Convert back to FIT
    }

    /// Get total cell count across all replicas
    pub fn cell_count(&self) -> usize {
        self.replica_a.len() + self.replica_b.len() + self.replica_c.len() + self.voters.len()
    }
}

/// Dual Modular Redundancy pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmrPattern {
    /// Pattern name/identifier
    pub name: String,
    /// Cells in replica A
    pub replica_a: Vec<CellId>,
    /// Cells in replica B
    pub replica_b: Vec<CellId>,
    /// Comparator cell(s)
    pub comparators: Vec<CellId>,
    /// Error output signal
    pub error_output: Option<GateNetId>,
    /// Diagnostic coverage
    /// Based on ISO 26262 Table D.8: DMR typically provides 99% DC for detection
    pub diagnostic_coverage: f64,
}

/// Voter circuit pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoterPattern {
    /// Voter name
    pub name: String,
    /// Voter type
    pub voter_type: VoterType,
    /// Cell implementing the voter
    pub cell: CellId,
    /// Input nets
    pub inputs: Vec<GateNetId>,
    /// Output net
    pub output: GateNetId,
}

/// Types of voter circuits
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum VoterType {
    /// 2-of-3 majority voter (standard TMR voter)
    Majority2Of3,
    /// 1-of-2 OR voter (DMR with fail-operational)
    Or2,
    /// 2-of-2 AND voter (DMR with fail-safe)
    And2,
    /// Generic n-of-m voter
    Generic { n: u32, m: u32 },
}

/// Watchdog/timeout monitor pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WatchdogPattern {
    /// Pattern name
    pub name: String,
    /// Counter/timer cells
    pub timer_cells: Vec<CellId>,
    /// Comparator cells
    pub comparator_cells: Vec<CellId>,
    /// Timeout output
    pub timeout_output: Option<GateNetId>,
    /// Diagnostic coverage
    pub diagnostic_coverage: f64,
}

// ============================================================================
// Pattern Detector
// ============================================================================

/// Structural pattern detector for safety mechanisms
pub struct PatternDetector {
    /// Minimum replica similarity threshold (0.0 - 1.0)
    pub similarity_threshold: f64,
    /// Enable verbose detection logging
    pub verbose: bool,
}

impl Default for PatternDetector {
    fn default() -> Self {
        Self::new()
    }
}

impl PatternDetector {
    /// Create a new pattern detector with default settings
    pub fn new() -> Self {
        Self {
            similarity_threshold: 0.90, // 90% structural similarity
            verbose: false,
        }
    }

    /// Create a pattern detector with custom similarity threshold
    pub fn with_threshold(threshold: f64) -> Self {
        Self {
            similarity_threshold: threshold.clamp(0.0, 1.0),
            verbose: false,
        }
    }

    /// Enable verbose logging
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Detect all safety mechanism patterns in a gate netlist
    pub fn detect_patterns(&self, netlist: &GateNetlist) -> DetectedPatterns {
        let mut patterns = DetectedPatterns::default();

        // Step 1: Detect voter circuits
        patterns.voters = self.detect_voters(netlist);

        // Step 2: Detect TMR patterns using voters as anchors
        patterns.tmr_patterns = self.detect_tmr(netlist, &patterns.voters);

        // Step 3: Detect DMR patterns
        patterns.dmr_patterns = self.detect_dmr(netlist);

        // Step 4: Detect watchdog patterns
        patterns.watchdogs = self.detect_watchdogs(netlist);

        if self.verbose {
            eprintln!(
                "[PATTERN_DETECT] Found {} TMR, {} DMR, {} voters, {} watchdogs",
                patterns.tmr_patterns.len(),
                patterns.dmr_patterns.len(),
                patterns.voters.len(),
                patterns.watchdogs.len()
            );
        }

        patterns
    }

    /// Detect majority voter circuits
    fn detect_voters(&self, netlist: &GateNetlist) -> Vec<VoterPattern> {
        let mut voters = Vec::new();

        for cell in &netlist.cells {
            // Look for cells that implement voting logic
            // Common patterns:
            // 1. Cells named with "voter", "maj", "majority"
            // 2. Cells with 3 inputs and 1 output implementing majority function
            // 3. Specific voter library cells

            let cell_name_lower = cell.cell_type.to_lowercase();
            let path_lower = cell.path.to_lowercase();

            if cell_name_lower.contains("voter")
                || cell_name_lower.contains("maj")
                || cell_name_lower.contains("majority")
                || path_lower.contains("voter")
                || path_lower.contains("tmr")
            {
                if let Some(output) = cell.outputs.first() {
                    voters.push(VoterPattern {
                        name: format!("voter_{}", cell.id.0),
                        voter_type: if cell.inputs.len() == 3 {
                            VoterType::Majority2Of3
                        } else if cell.inputs.len() == 2 {
                            VoterType::Or2
                        } else {
                            VoterType::Generic {
                                n: (cell.inputs.len() / 2 + 1) as u32,
                                m: cell.inputs.len() as u32,
                            }
                        },
                        cell: cell.id,
                        inputs: cell.inputs.clone(),
                        output: *output,
                    });
                }
            }

            // Also detect structural majority gates (AOI/OAI combinations)
            if self.is_majority_gate_structure(cell, netlist) {
                if let Some(output) = cell.outputs.first() {
                    voters.push(VoterPattern {
                        name: format!("maj_struct_{}", cell.id.0),
                        voter_type: VoterType::Majority2Of3,
                        cell: cell.id,
                        inputs: cell.inputs.clone(),
                        output: *output,
                    });
                }
            }
        }

        voters
    }

    /// Check if a cell implements majority gate structure
    fn is_majority_gate_structure(&self, cell: &Cell, _netlist: &GateNetlist) -> bool {
        // A majority gate can be implemented as:
        // MAJ(a,b,c) = (a AND b) OR (b AND c) OR (a AND c)
        // = AB + BC + AC

        // Check for OAI22 or AOI22 patterns that could be majority gates
        let cell_type_lower = cell.cell_type.to_lowercase();
        if (cell_type_lower.contains("oai22") || cell_type_lower.contains("aoi22"))
            && cell.inputs.len() >= 3
        {
            return true;
        }

        // Check for explicit majority cell types
        if cell_type_lower.contains("maj3") || cell_type_lower.contains("majority") {
            return true;
        }

        false
    }

    /// Detect TMR patterns using voters as anchors
    fn detect_tmr(&self, netlist: &GateNetlist, voters: &[VoterPattern]) -> Vec<TmrPattern> {
        let mut tmr_patterns = Vec::new();

        for voter in voters {
            if voter.voter_type != VoterType::Majority2Of3 || voter.inputs.len() != 3 {
                continue;
            }

            // Trace back from voter inputs to find triplicated logic
            let mut replicas: [Vec<CellId>; 3] = Default::default();
            let mut is_valid_tmr = true;

            for (i, input_net) in voter.inputs.iter().enumerate() {
                if let Some(driving_cells) = self.trace_cone_back(netlist, *input_net) {
                    replicas[i] = driving_cells;
                } else {
                    is_valid_tmr = false;
                    break;
                }
            }

            if is_valid_tmr {
                // Check if the three replicas are structurally similar
                let similarity_ab = self.structural_similarity(&replicas[0], &replicas[1], netlist);
                let similarity_bc = self.structural_similarity(&replicas[1], &replicas[2], netlist);
                let similarity_ac = self.structural_similarity(&replicas[0], &replicas[2], netlist);

                let avg_similarity = (similarity_ab + similarity_bc + similarity_ac) / 3.0;

                if avg_similarity >= self.similarity_threshold {
                    tmr_patterns.push(TmrPattern {
                        name: format!("tmr_{}", voter.name),
                        replica_a: replicas[0].clone(),
                        replica_b: replicas[1].clone(),
                        replica_c: replicas[2].clone(),
                        voters: vec![voter.cell],
                        outputs: vec![voter.output],
                        diagnostic_coverage: 99.0, // ISO 26262 Table D.8 for TMR
                        is_complete: avg_similarity >= 0.95,
                    });
                }
            }
        }

        // Also detect TMR by naming conventions
        tmr_patterns.extend(self.detect_tmr_by_naming(netlist));

        tmr_patterns
    }

    /// Detect TMR by naming conventions (e.g., _a, _b, _c suffixes)
    fn detect_tmr_by_naming(&self, netlist: &GateNetlist) -> Vec<TmrPattern> {
        let mut patterns = Vec::new();
        let mut processed_groups: HashSet<String> = HashSet::new();

        // Group cells by base name (without replica suffix)
        let mut name_groups: HashMap<String, Vec<&Cell>> = HashMap::new();

        for cell in &netlist.cells {
            // Extract base name by removing common replica suffixes
            let base_name = self.extract_base_name(&cell.path);
            name_groups.entry(base_name).or_default().push(cell);
        }

        // Find groups of 3 with matching suffixes
        for (base_name, cells) in &name_groups {
            if processed_groups.contains(base_name) {
                continue;
            }

            // Look for cells with _a/_b/_c or _0/_1/_2 suffixes
            let mut replicas: [Vec<CellId>; 3] = Default::default();
            let mut found_count = 0;

            for cell in cells {
                let suffix = self.extract_replica_suffix(&cell.path);
                match suffix.as_str() {
                    "a" | "0" | "replica_a" => {
                        replicas[0].push(cell.id);
                        found_count |= 1;
                    }
                    "b" | "1" | "replica_b" => {
                        replicas[1].push(cell.id);
                        found_count |= 2;
                    }
                    "c" | "2" | "replica_c" => {
                        replicas[2].push(cell.id);
                        found_count |= 4;
                    }
                    _ => {}
                }
            }

            // Found all three replicas
            if found_count == 7
                && !replicas[0].is_empty()
                && !replicas[1].is_empty()
                && !replicas[2].is_empty()
            {
                processed_groups.insert(base_name.clone());

                patterns.push(TmrPattern {
                    name: format!("tmr_named_{}", base_name),
                    replica_a: replicas[0].clone(),
                    replica_b: replicas[1].clone(),
                    replica_c: replicas[2].clone(),
                    voters: Vec::new(), // Voter not identified from naming
                    outputs: Vec::new(),
                    diagnostic_coverage: 99.0,
                    is_complete: true,
                });
            }
        }

        patterns
    }

    /// Trace the logic cone back from a net
    fn trace_cone_back(&self, netlist: &GateNetlist, net: GateNetId) -> Option<Vec<CellId>> {
        let mut cells = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = vec![net];

        while let Some(current_net) = queue.pop() {
            // Find cells that drive this net
            for cell in &netlist.cells {
                if cell.outputs.contains(&current_net) && !visited.contains(&cell.id) {
                    visited.insert(cell.id);
                    cells.push(cell.id);

                    // Add input nets to explore
                    for input in &cell.inputs {
                        if !visited.contains(&CellId(input.0)) {
                            queue.push(*input);
                        }
                    }
                }
            }
        }

        if cells.is_empty() {
            None
        } else {
            Some(cells)
        }
    }

    /// Calculate structural similarity between two cell groups
    fn structural_similarity(
        &self,
        group_a: &[CellId],
        group_b: &[CellId],
        netlist: &GateNetlist,
    ) -> f64 {
        if group_a.is_empty() || group_b.is_empty() {
            return 0.0;
        }

        // Count cell types in each group
        let types_a: HashMap<String, usize> = group_a
            .iter()
            .filter_map(|id| netlist.get_cell(*id))
            .fold(HashMap::new(), |mut acc, cell| {
                *acc.entry(cell.cell_type.clone()).or_insert(0) += 1;
                acc
            });

        let types_b: HashMap<String, usize> = group_b
            .iter()
            .filter_map(|id| netlist.get_cell(*id))
            .fold(HashMap::new(), |mut acc, cell| {
                *acc.entry(cell.cell_type.clone()).or_insert(0) += 1;
                acc
            });

        // Calculate Jaccard similarity of cell type distributions
        let all_types: HashSet<_> = types_a.keys().chain(types_b.keys()).collect();

        let mut intersection = 0;
        let mut union = 0;

        for cell_type in all_types {
            let count_a = types_a.get(cell_type).copied().unwrap_or(0);
            let count_b = types_b.get(cell_type).copied().unwrap_or(0);

            intersection += count_a.min(count_b);
            union += count_a.max(count_b);
        }

        if union == 0 {
            0.0
        } else {
            intersection as f64 / union as f64
        }
    }

    /// Extract base name without replica suffix
    fn extract_base_name(&self, path: &str) -> String {
        let path = path.to_lowercase();

        // Remove common replica suffixes
        for suffix in [
            "_a",
            "_b",
            "_c",
            "_0",
            "_1",
            "_2",
            "_replica_a",
            "_replica_b",
            "_replica_c",
        ] {
            if let Some(stripped) = path.strip_suffix(suffix) {
                return stripped.to_string();
            }
        }

        path
    }

    /// Extract replica suffix from path
    fn extract_replica_suffix(&self, path: &str) -> String {
        let path = path.to_lowercase();

        for (suffix, normalized) in [
            ("_a", "a"),
            ("_b", "b"),
            ("_c", "c"),
            ("_0", "0"),
            ("_1", "1"),
            ("_2", "2"),
            ("_replica_a", "replica_a"),
            ("_replica_b", "replica_b"),
            ("_replica_c", "replica_c"),
        ] {
            if path.ends_with(suffix) {
                return normalized.to_string();
            }
        }

        String::new()
    }

    /// Detect DMR (Dual Modular Redundancy) patterns
    fn detect_dmr(&self, netlist: &GateNetlist) -> Vec<DmrPattern> {
        let mut patterns = Vec::new();

        // Look for comparator cells
        for cell in &netlist.cells {
            let cell_name_lower = cell.cell_type.to_lowercase();
            let path_lower = cell.path.to_lowercase();

            // Check for comparator or XOR cells used for comparison
            let is_comparator = cell_name_lower.contains("cmp")
                || cell_name_lower.contains("compare")
                || path_lower.contains("dmr")
                || (cell_name_lower.contains("xor") && path_lower.contains("check"));

            if is_comparator && cell.inputs.len() >= 2 {
                patterns.push(DmrPattern {
                    name: format!("dmr_{}", cell.id.0),
                    replica_a: Vec::new(), // Would need deeper analysis
                    replica_b: Vec::new(),
                    comparators: vec![cell.id],
                    error_output: cell.outputs.first().copied(),
                    diagnostic_coverage: 99.0, // ISO 26262 Table D.8 for comparison
                });
            }
        }

        patterns
    }

    /// Detect watchdog/timeout monitor patterns
    fn detect_watchdogs(&self, netlist: &GateNetlist) -> Vec<WatchdogPattern> {
        let mut patterns = Vec::new();

        // Look for cells named with watchdog-related terms
        let mut watchdog_cells: Vec<&Cell> = Vec::new();

        for cell in &netlist.cells {
            let path_lower = cell.path.to_lowercase();

            if path_lower.contains("watchdog")
                || path_lower.contains("wdt")
                || path_lower.contains("timeout")
                || path_lower.contains("deadline")
            {
                watchdog_cells.push(cell);
            }
        }

        if !watchdog_cells.is_empty() {
            // Group cells into timer and comparator categories
            let timer_cells: Vec<CellId> = watchdog_cells
                .iter()
                .filter(|c| {
                    c.path.to_lowercase().contains("counter")
                        || c.path.to_lowercase().contains("timer")
                })
                .map(|c| c.id)
                .collect();

            let comparator_cells: Vec<CellId> = watchdog_cells
                .iter()
                .filter(|c| {
                    c.path.to_lowercase().contains("compare")
                        || c.path.to_lowercase().contains("cmp")
                })
                .map(|c| c.id)
                .collect();

            patterns.push(WatchdogPattern {
                name: "watchdog_0".to_string(),
                timer_cells,
                comparator_cells,
                timeout_output: None,
                diagnostic_coverage: 90.0, // ISO 26262 Table D.8 for watchdog
            });
        }

        patterns
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_detector_creation() {
        let detector = PatternDetector::new();
        assert!((detector.similarity_threshold - 0.90).abs() < 0.01);
        assert!(!detector.verbose);
    }

    #[test]
    fn test_empty_netlist() {
        let detector = PatternDetector::new();
        let netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());
        let patterns = detector.detect_patterns(&netlist);

        assert!(!patterns.has_patterns());
        assert_eq!(patterns.total_count(), 0);
    }

    #[test]
    fn test_extract_base_name() {
        let detector = PatternDetector::new();

        assert_eq!(detector.extract_base_name("counter_a"), "counter");
        assert_eq!(detector.extract_base_name("counter_b"), "counter");
        assert_eq!(detector.extract_base_name("adder_0"), "adder");
        assert_eq!(detector.extract_base_name("adder_1"), "adder");
        assert_eq!(detector.extract_base_name("normal"), "normal");
    }

    #[test]
    fn test_extract_replica_suffix() {
        let detector = PatternDetector::new();

        assert_eq!(detector.extract_replica_suffix("counter_a"), "a");
        assert_eq!(detector.extract_replica_suffix("counter_b"), "b");
        assert_eq!(detector.extract_replica_suffix("counter_c"), "c");
        assert_eq!(detector.extract_replica_suffix("adder_0"), "0");
        assert_eq!(detector.extract_replica_suffix("normal"), "");
    }

    #[test]
    fn test_voter_type_variants() {
        let v1 = VoterType::Majority2Of3;
        let v2 = VoterType::Or2;
        let v3 = VoterType::And2;
        let v4 = VoterType::Generic { n: 3, m: 5 };

        assert_eq!(v1, VoterType::Majority2Of3);
        assert_ne!(v2, v1);
        assert_ne!(v3, v2);
        assert!(matches!(v4, VoterType::Generic { n: 3, m: 5 }));
    }
}
