//! EC Report generation for the VS Code dashboard.
//!
//! Converts `SymbolicEquivalenceResult` into a JSON report file
//! consumed by the SKALP VS Code extension's EC dashboard.

use serde::Serialize;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use crate::equivalence::SymbolicEquivalenceResult;

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EcReport {
    pub design: String,
    pub status: String,
    pub outputs: Vec<GateResult>,
    pub latches: Vec<GateResult>,
    pub timing: EcTiming,
    pub self_test: Option<EcSelfTest>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GateResult {
    pub name: String,
    pub status: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub time_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counterexample: Option<std::collections::HashMap<String, bool>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EcTiming {
    pub phase1_sim_ms: u64,
    pub phase2_sat_ms: u64,
    pub phase2c_sim_ms: u64,
    pub phase3_selftest_ms: u64,
    pub total_ms: u64,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EcSelfTest {
    pub bugs_injected: u32,
    pub bugs_detected: u32,
    pub detection_rate: f64,
}

impl EcReport {
    /// Build an EcReport from a SymbolicEquivalenceResult
    pub fn from_result(result: &SymbolicEquivalenceResult, design_name: &str) -> Self {
        let mut outputs = Vec::new();
        let mut latches = Vec::new();

        // Proven gates
        for name in &result.proven_gates {
            let gate = GateResult {
                name: name.clone(),
                status: "pass".to_string(),
                time_ms: None,
                counterexample: None,
            };
            if name.contains("output") || name.contains("diff_output") {
                outputs.push(gate);
            } else {
                latches.push(gate);
            }
        }

        // Unresolved gates
        for (_idx, name) in &result.unresolved_gates {
            let gate = GateResult {
                name: name.clone(),
                status: "unknown".to_string(),
                time_ms: None,
                counterexample: None,
            };
            if name.contains("output") || name.contains("diff_output") {
                outputs.push(gate);
            } else {
                latches.push(gate);
            }
        }

        // Counterexample
        if let Some(ref cex) = result.counterexample {
            if let Some(ref diff_sig) = cex.differing_signal {
                let gate = GateResult {
                    name: diff_sig.clone(),
                    status: "fail".to_string(),
                    time_ms: None,
                    counterexample: Some(cex.inputs.clone()),
                };
                if diff_sig.contains("output") {
                    outputs.push(gate);
                } else {
                    latches.push(gate);
                }
            }
        }

        let status = if !result.equivalent {
            "fail"
        } else if result.unresolved_gates.is_empty() {
            "pass"
        } else {
            "unknown"
        };

        EcReport {
            design: design_name.to_string(),
            status: status.to_string(),
            outputs,
            latches,
            timing: EcTiming {
                phase1_sim_ms: 0,
                phase2_sat_ms: result.time_ms,
                phase2c_sim_ms: 0,
                phase3_selftest_ms: 0,
                total_ms: result.time_ms,
            },
            self_test: None,
        }
    }

    /// Write the report as JSON to a file
    pub fn write_json(&self, path: &Path) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = File::create(path)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, self).map_err(std::io::Error::other)
    }
}
