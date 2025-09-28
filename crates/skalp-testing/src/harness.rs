//! Test harness for executing hardware tests

use crate::{TestingResult, Stimulus};
use std::collections::HashMap;

pub struct TestHarness {
    design: skalp_lir::LirDesign,
    stimuli: HashMap<String, Stimulus>,
}

#[derive(Debug)]
pub struct SimulationResult {
    pub errors: Vec<String>,
    pub execution_time_ms: u64,
    pub coverage_data: HashMap<String, f64>,
}

impl TestHarness {
    pub fn new(design: skalp_lir::LirDesign) -> Self {
        Self {
            design,
            stimuli: HashMap::new(),
        }
    }

    pub fn apply_stimulus(&mut self, signal: &str, stimulus: &Stimulus) -> TestingResult<()> {
        self.stimuli.insert(signal.to_string(), stimulus.clone());
        Ok(())
    }

    pub async fn run_simulation(&self, _timeout_ms: u64) -> TestingResult<SimulationResult> {
        Ok(SimulationResult {
            errors: Vec::new(),
            execution_time_ms: 100,
            coverage_data: HashMap::new(),
        })
    }
}