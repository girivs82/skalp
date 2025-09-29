use crate::simulator::{Simulator, SimulationConfig, SimulationResult, SimulationError};
use skalp_sir::SirModule;
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::timeout;

#[derive(Debug, Clone)]
pub struct TestVector {
    pub cycle: u64,
    pub inputs: HashMap<String, Vec<u8>>,
    pub expected_outputs: Option<HashMap<String, Vec<u8>>>,
}

#[derive(Debug)]
pub struct TestResult {
    pub passed: bool,
    pub cycle: u64,
    pub mismatches: Vec<SignalMismatch>,
    pub error: Option<String>,
}

#[derive(Debug)]
pub struct SignalMismatch {
    pub signal_name: String,
    pub expected: Vec<u8>,
    pub actual: Vec<u8>,
    pub cycle: u64,
}

pub struct Testbench {
    simulator: Simulator,
    test_vectors: Vec<TestVector>,
    results: Vec<TestResult>,
}

impl Testbench {
    pub async fn new(config: SimulationConfig) -> SimulationResult<Self> {
        let simulator = Simulator::new(config).await?;

        Ok(Testbench {
            simulator,
            test_vectors: Vec::new(),
            results: Vec::new(),
        })
    }

    pub async fn load_module(&mut self, module: &SirModule) -> SimulationResult<()> {
        self.simulator.load_module(module).await
    }

    pub fn add_test_vector(&mut self, vector: TestVector) {
        self.test_vectors.push(vector);
    }

    pub fn add_test_vectors(&mut self, vectors: Vec<TestVector>) {
        self.test_vectors.extend(vectors);
    }

    pub async fn run_test(&mut self) -> SimulationResult<Vec<TestResult>> {
        // Reset the simulator
        self.simulator.reset().await?;
        self.results.clear();

        // Sort test vectors by cycle
        self.test_vectors.sort_by_key(|v| v.cycle);

        let mut current_cycle = 0u64;
        let mut vector_index = 0;

        while vector_index < self.test_vectors.len() {
            let vector = &self.test_vectors[vector_index];

            // Run simulation until the next test vector cycle
            while current_cycle < vector.cycle {
                self.simulator.step_simulation().await?;
                current_cycle += 1;
            }

            // Apply inputs
            for (name, value) in &vector.inputs {
                self.simulator.set_input(name, value.clone()).await?;
            }

            // Step once to propagate inputs
            self.simulator.step_simulation().await?;
            current_cycle += 1;

            // Check outputs if expected values are provided
            if let Some(expected_outputs) = &vector.expected_outputs {
                let mut mismatches = Vec::new();

                for (name, expected) in expected_outputs {
                    match self.simulator.get_output(name).await {
                        Ok(actual) => {
                            if actual != *expected {
                                mismatches.push(SignalMismatch {
                                    signal_name: name.clone(),
                                    expected: expected.clone(),
                                    actual,
                                    cycle: current_cycle,
                                });
                            }
                        }
                        Err(e) => {
                            self.results.push(TestResult {
                                passed: false,
                                cycle: current_cycle,
                                mismatches: vec![],
                                error: Some(format!("Failed to get output {}: {:?}", name, e)),
                            });
                            return Ok(self.results.clone());
                        }
                    }
                }

                self.results.push(TestResult {
                    passed: mismatches.is_empty(),
                    cycle: current_cycle,
                    mismatches,
                    error: None,
                });
            }

            vector_index += 1;
        }

        Ok(self.results.clone())
    }

    pub async fn run_test_with_timeout(&mut self, timeout_duration: Duration) -> SimulationResult<Vec<TestResult>> {
        match timeout(timeout_duration, self.run_test()).await {
            Ok(result) => result,
            Err(_) => Err(SimulationError::Timeout),
        }
    }

    pub fn generate_report(&self) -> String {
        let mut report = String::new();

        report.push_str("=== Testbench Report ===\n\n");

        let total_tests = self.results.len();
        let passed_tests = self.results.iter().filter(|r| r.passed).count();
        let failed_tests = total_tests - passed_tests;

        report.push_str(&format!("Total Tests: {}\n", total_tests));
        report.push_str(&format!("Passed: {}\n", passed_tests));
        report.push_str(&format!("Failed: {}\n\n", failed_tests));

        if failed_tests > 0 {
            report.push_str("Failed Tests:\n");
            report.push_str("--------------\n");

            for (i, result) in self.results.iter().enumerate() {
                if !result.passed {
                    report.push_str(&format!("\nTest {} (Cycle {}):\n", i + 1, result.cycle));

                    if let Some(error) = &result.error {
                        report.push_str(&format!("  Error: {}\n", error));
                    }

                    for mismatch in &result.mismatches {
                        report.push_str(&format!(
                            "  Signal '{}' mismatch:\n    Expected: {:?}\n    Actual:   {:?}\n",
                            mismatch.signal_name,
                            mismatch.expected,
                            mismatch.actual
                        ));
                    }
                }
            }
        }

        report
    }

    pub fn get_results(&self) -> &[TestResult] {
        &self.results
    }

    pub fn all_tests_passed(&self) -> bool {
        self.results.iter().all(|r| r.passed)
    }
}

// Builder pattern for creating test vectors
pub struct TestVectorBuilder {
    cycle: u64,
    inputs: HashMap<String, Vec<u8>>,
    expected_outputs: Option<HashMap<String, Vec<u8>>>,
}

impl TestVectorBuilder {
    pub fn new(cycle: u64) -> Self {
        TestVectorBuilder {
            cycle,
            inputs: HashMap::new(),
            expected_outputs: None,
        }
    }

    pub fn with_input(mut self, name: &str, value: Vec<u8>) -> Self {
        self.inputs.insert(name.to_string(), value);
        self
    }

    pub fn with_input_u32(mut self, name: &str, value: u32) -> Self {
        self.inputs.insert(name.to_string(), value.to_le_bytes().to_vec());
        self
    }

    pub fn with_expected_output(mut self, name: &str, value: Vec<u8>) -> Self {
        if self.expected_outputs.is_none() {
            self.expected_outputs = Some(HashMap::new());
        }
        if let Some(outputs) = &mut self.expected_outputs {
            outputs.insert(name.to_string(), value);
        }
        self
    }

    pub fn with_expected_output_u32(mut self, name: &str, value: u32) -> Self {
        if self.expected_outputs.is_none() {
            self.expected_outputs = Some(HashMap::new());
        }
        if let Some(outputs) = &mut self.expected_outputs {
            outputs.insert(name.to_string(), value.to_le_bytes().to_vec());
        }
        self
    }

    pub fn build(self) -> TestVector {
        TestVector {
            cycle: self.cycle,
            inputs: self.inputs,
            expected_outputs: self.expected_outputs,
        }
    }
}