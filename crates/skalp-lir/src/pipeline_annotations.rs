//! Pipeline Annotations
//!
//! This module provides data structures for tracking pipeline stages
//! added during synthesis (e.g., register retiming). These annotations
//! enable behavioral simulation to match gate-level timing.
//!
//! # Usage
//!
//! After synthesis, annotations are written to `pipeline_annotations.toml`.
//! The simulator reads this file to add latency to behavioral models.
//!
//! # Format
//!
//! ```toml
//! [metadata]
//! target_frequency_mhz = 500.0
//! original_critical_path_ps = 3200.0
//! final_critical_path_ps = 1800.0
//!
//! [[modules]]
//! name = "MyALU"
//! original_latency_cycles = 1
//! final_latency_cycles = 3
//!
//! [[modules.pipeline_stages]]
//! location = "multiply_output"
//! signal = "mult_result"
//! cycles_added = 1
//! reason = "Critical path: 2.3ns exceeds 2.0ns budget"
//! source_line = 42
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Pipeline annotations for a complete design
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PipelineAnnotations {
    /// Metadata about the synthesis run
    pub metadata: SynthesisMetadata,
    /// Annotations per module
    pub modules: Vec<ModuleAnnotations>,
}

/// Metadata about the synthesis configuration and results
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SynthesisMetadata {
    /// Target clock frequency in MHz
    pub target_frequency_mhz: f64,
    /// Original critical path delay in picoseconds
    pub original_critical_path_ps: f64,
    /// Final critical path delay after retiming
    pub final_critical_path_ps: f64,
    /// Whether timing was met
    pub timing_met: bool,
    /// Synthesis timestamp
    pub timestamp: String,
    /// SKALP version
    pub skalp_version: String,
}

/// Pipeline annotations for a single module
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ModuleAnnotations {
    /// Module name
    pub name: String,
    /// Original latency in clock cycles (before retiming)
    pub original_latency_cycles: u32,
    /// Final latency in clock cycles (after retiming)
    pub final_latency_cycles: u32,
    /// Individual pipeline stages added
    pub pipeline_stages: Vec<PipelineStage>,
    /// Path-specific latency changes
    pub path_latencies: Vec<PathLatency>,
}

/// A single pipeline stage added during synthesis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStage {
    /// Human-readable location description
    pub location: String,
    /// Signal name where register was inserted
    pub signal: String,
    /// Number of cycles added (usually 1)
    pub cycles_added: u32,
    /// Reason for insertion (e.g., "critical path exceeded target")
    pub reason: String,
    /// Source file line number (if traceable)
    pub source_line: Option<u32>,
    /// Original arrival time at this point (ps)
    pub original_arrival_ps: Option<f64>,
    /// Retiming direction: "forward" or "backward"
    pub direction: String,
}

/// Latency information for a specific path
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathLatency {
    /// Start signal (input or register output)
    pub from_signal: String,
    /// End signal (output or register input)
    pub to_signal: String,
    /// Latency in cycles for behavioral simulation
    pub cycles: u32,
    /// Combinational delay in picoseconds
    pub delay_ps: f64,
}

impl PipelineAnnotations {
    /// Create new empty annotations
    pub fn new() -> Self {
        Self::default()
    }

    /// Create with metadata
    pub fn with_metadata(target_freq_mhz: f64) -> Self {
        Self {
            metadata: SynthesisMetadata {
                target_frequency_mhz: target_freq_mhz,
                timestamp: chrono_lite_timestamp(),
                skalp_version: env!("CARGO_PKG_VERSION").to_string(),
                ..Default::default()
            },
            modules: Vec::new(),
        }
    }

    /// Add module annotations
    pub fn add_module(&mut self, module: ModuleAnnotations) {
        self.modules.push(module);
    }

    /// Get total cycles added across all modules
    pub fn total_cycles_added(&self) -> u32 {
        self.modules
            .iter()
            .flat_map(|m| m.pipeline_stages.iter())
            .map(|s| s.cycles_added)
            .sum()
    }

    /// Check if any retiming was performed
    pub fn has_retiming(&self) -> bool {
        self.modules.iter().any(|m| !m.pipeline_stages.is_empty())
    }

    /// Write annotations to TOML file
    pub fn write_toml(&self, path: &Path) -> std::io::Result<()> {
        let toml_str = toml::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        fs::write(path, toml_str)
    }

    /// Read annotations from TOML file
    pub fn read_toml(path: &Path) -> std::io::Result<Self> {
        let content = fs::read_to_string(path)?;
        toml::from_str(&content)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }

    /// Get latency adjustment for a specific module
    pub fn get_module_latency_adjustment(&self, module_name: &str) -> u32 {
        self.modules
            .iter()
            .find(|m| m.name == module_name)
            .map(|m| {
                m.final_latency_cycles
                    .saturating_sub(m.original_latency_cycles)
            })
            .unwrap_or(0)
    }

    /// Generate summary string
    pub fn summary(&self) -> String {
        let total_stages: usize = self.modules.iter().map(|m| m.pipeline_stages.len()).sum();

        let total_cycles: u32 = self.total_cycles_added();

        format!(
            "Pipeline annotations: {} modules, {} stages, {} cycles added, timing {}",
            self.modules.len(),
            total_stages,
            total_cycles,
            if self.metadata.timing_met {
                "met"
            } else {
                "not met"
            }
        )
    }
}

impl ModuleAnnotations {
    /// Create new module annotations
    pub fn new(name: String) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    /// Add a pipeline stage
    pub fn add_stage(&mut self, stage: PipelineStage) {
        self.pipeline_stages.push(stage);
        // Update final latency
        self.final_latency_cycles = self.original_latency_cycles
            + self
                .pipeline_stages
                .iter()
                .map(|s| s.cycles_added)
                .sum::<u32>();
    }

    /// Add path latency information
    pub fn add_path_latency(&mut self, path: PathLatency) {
        self.path_latencies.push(path);
    }
}

impl PipelineStage {
    /// Create a new pipeline stage annotation
    pub fn new(signal: String, reason: String) -> Self {
        Self {
            location: signal.clone(),
            signal,
            cycles_added: 1,
            reason,
            source_line: None,
            original_arrival_ps: None,
            direction: "forward".to_string(),
        }
    }

    /// Create with full details
    pub fn with_details(
        signal: String,
        reason: String,
        cycles: u32,
        direction: &str,
        arrival_ps: f64,
    ) -> Self {
        Self {
            location: signal.clone(),
            signal,
            cycles_added: cycles,
            reason,
            source_line: None,
            original_arrival_ps: Some(arrival_ps),
            direction: direction.to_string(),
        }
    }
}

/// Simple timestamp without external dependencies
fn chrono_lite_timestamp() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}", duration.as_secs())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_pipeline_annotations_new() {
        let annotations = PipelineAnnotations::new();
        assert!(annotations.modules.is_empty());
        assert!(!annotations.has_retiming());
    }

    #[test]
    fn test_add_module_and_stage() {
        let mut annotations = PipelineAnnotations::with_metadata(500.0);

        let mut module = ModuleAnnotations::new("MyALU".to_string());
        module.original_latency_cycles = 1;

        module.add_stage(PipelineStage::new(
            "mult_result".to_string(),
            "Critical path exceeded".to_string(),
        ));

        annotations.add_module(module);

        assert!(annotations.has_retiming());
        assert_eq!(annotations.total_cycles_added(), 1);
        assert_eq!(annotations.get_module_latency_adjustment("MyALU"), 1);
    }

    #[test]
    fn test_toml_roundtrip() {
        let mut annotations = PipelineAnnotations::with_metadata(250.0);
        annotations.metadata.timing_met = true;
        annotations.metadata.original_critical_path_ps = 5000.0;
        annotations.metadata.final_critical_path_ps = 3500.0;

        let mut module = ModuleAnnotations::new("TestModule".to_string());
        module.original_latency_cycles = 2;
        module.add_stage(PipelineStage::with_details(
            "data_out".to_string(),
            "Fanout reduction".to_string(),
            1,
            "forward",
            4200.0,
        ));
        annotations.add_module(module);

        // Write to temp file
        let temp_file = NamedTempFile::new().unwrap();
        annotations.write_toml(temp_file.path()).unwrap();

        // Read back
        let loaded = PipelineAnnotations::read_toml(temp_file.path()).unwrap();

        assert_eq!(loaded.metadata.target_frequency_mhz, 250.0);
        assert!(loaded.metadata.timing_met);
        assert_eq!(loaded.modules.len(), 1);
        assert_eq!(loaded.modules[0].name, "TestModule");
        assert_eq!(loaded.modules[0].pipeline_stages.len(), 1);
    }

    #[test]
    fn test_summary() {
        let mut annotations = PipelineAnnotations::with_metadata(100.0);
        annotations.metadata.timing_met = true;

        let mut m1 = ModuleAnnotations::new("Mod1".to_string());
        m1.add_stage(PipelineStage::new("sig1".to_string(), "reason".to_string()));
        m1.add_stage(PipelineStage::new("sig2".to_string(), "reason".to_string()));

        let mut m2 = ModuleAnnotations::new("Mod2".to_string());
        m2.add_stage(PipelineStage::new("sig3".to_string(), "reason".to_string()));

        annotations.add_module(m1);
        annotations.add_module(m2);

        let summary = annotations.summary();
        assert!(summary.contains("2 modules"));
        assert!(summary.contains("3 stages"));
        assert!(summary.contains("3 cycles added"));
        assert!(summary.contains("timing met"));
    }
}
