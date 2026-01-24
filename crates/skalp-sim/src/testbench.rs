use crate::clock_manager::ClockManager;
use crate::simulator::{SimulationConfig, SimulationError, SimulationResult, Simulator};
use indexmap::IndexMap;
use skalp_mir::name_registry::NameRegistry;
use skalp_sir::SirModule;
use std::time::Duration;
use tokio::time::timeout;

#[derive(Debug, Clone)]
pub struct TestVector {
    pub cycle: u64,
    pub inputs: IndexMap<String, Vec<u8>>,
    pub expected_outputs: Option<IndexMap<String, Vec<u8>>>,
}

#[derive(Debug, Clone)]
pub struct TestResult {
    pub passed: bool,
    pub cycle: u64,
    pub mismatches: Vec<SignalMismatch>,
    pub error: Option<String>,
}

#[derive(Debug, Clone)]
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
    clock_manager: ClockManager,
    auto_clock: bool,
    /// Name registry for resolving user-facing signal names to internal names
    name_registry: NameRegistry,
}

impl Testbench {
    pub async fn new(config: SimulationConfig) -> SimulationResult<Self> {
        let simulator = Simulator::new(config).await?;

        Ok(Testbench {
            simulator,
            test_vectors: Vec::new(),
            results: Vec::new(),
            clock_manager: ClockManager::new(),
            auto_clock: true, // Enable auto-clocking by default
            name_registry: NameRegistry::new(),
        })
    }

    /// Resolve a user-facing signal path to the internal signal name
    fn resolve_path(&self, path: &str) -> String {
        self.name_registry
            .resolve(path)
            .map(|s| s.to_string())
            .unwrap_or_else(|| path.to_string())
    }

    pub fn with_auto_clock(mut self, enabled: bool) -> Self {
        self.auto_clock = enabled;
        self
    }

    pub async fn load_module(&mut self, module: &SirModule) -> SimulationResult<()> {
        // Store the name registry for path resolution
        self.name_registry = module.name_registry.clone();

        // Register clocks from the module
        // Use the name registry to find original names since internal names like "_s0"
        // don't contain "clk" or "clock"
        for input in &module.inputs {
            // Try to find the original user-facing name for this input
            let original_name = self.name_registry
                .reverse_resolve(&input.name)
                .unwrap_or(&input.name);

            // Check if the original name suggests this is a clock signal
            let is_clock = original_name.contains("clk")
                || original_name.contains("clock")
                || input.name.contains("clk")
                || input.name.contains("clock");

            if is_clock {
                // Register using the internal name (used in set_input calls)
                self.clock_manager.add_clock(input.name.clone(), 10_000); // 10ns period
            }
        }

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
        self.clock_manager.reset();

        // Sort test vectors by cycle
        self.test_vectors.sort_by_key(|v| v.cycle);

        let mut current_cycle = 0u64;
        let mut vector_index = 0;

        while vector_index < self.test_vectors.len() {
            let vector = &self.test_vectors[vector_index];

            // Run simulation until the next test vector cycle
            while current_cycle < vector.cycle {
                // Toggle clocks if auto-clock is enabled
                if self.auto_clock {
                    for (clock_name, _) in self.clock_manager.clocks.clone() {
                        let edge = self.clock_manager.toggle_clock(&clock_name);
                        if let Some(_edge) = edge {
                            let value = self
                                .clock_manager
                                .get_clock_value(&clock_name)
                                .unwrap_or(false);
                            self.simulator
                                .set_input(&clock_name, vec![value as u8])
                                .await?;
                        }
                    }
                }

                self.simulator.step_simulation().await?;
                current_cycle += 1;
            }

            // Apply inputs (resolve user-facing names to internal names)
            for (name, value) in &vector.inputs {
                let internal_name = self.resolve_path(name);
                self.simulator.set_input(&internal_name, value.clone()).await?;
            }

            // Step twice to ensure clock edge if auto-clocking
            if self.auto_clock {
                // Rising edge
                for (clock_name, _) in self.clock_manager.clocks.clone() {
                    self.simulator.set_input(&clock_name, vec![1]).await?;
                }
                self.simulator.step_simulation().await?;

                // Falling edge
                for (clock_name, _) in self.clock_manager.clocks.clone() {
                    self.simulator.set_input(&clock_name, vec![0]).await?;
                }
                self.simulator.step_simulation().await?;
                current_cycle += 2;
            } else {
                // Step once to propagate inputs
                self.simulator.step_simulation().await?;
                current_cycle += 1;
            }

            // Check outputs if expected values are provided (resolve user-facing names)
            if let Some(expected_outputs) = &vector.expected_outputs {
                let mut mismatches = Vec::new();

                for (name, expected) in expected_outputs {
                    let internal_name = self.resolve_path(name);
                    match self.simulator.get_output(&internal_name).await {
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

    pub async fn run_test_with_timeout(
        &mut self,
        timeout_duration: Duration,
    ) -> SimulationResult<Vec<TestResult>> {
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
                            mismatch.signal_name, mismatch.expected, mismatch.actual
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
    inputs: IndexMap<String, Vec<u8>>,
    expected_outputs: Option<IndexMap<String, Vec<u8>>>,
}

impl TestVectorBuilder {
    pub fn new(cycle: u64) -> Self {
        TestVectorBuilder {
            cycle,
            inputs: IndexMap::new(),
            expected_outputs: None,
        }
    }

    pub fn with_input(mut self, name: &str, value: Vec<u8>) -> Self {
        self.inputs.insert(name.to_string(), value);
        self
    }

    pub fn with_input_u32(mut self, name: &str, value: u32) -> Self {
        self.inputs
            .insert(name.to_string(), value.to_le_bytes().to_vec());
        self
    }

    pub fn with_expected_output(mut self, name: &str, value: Vec<u8>) -> Self {
        if self.expected_outputs.is_none() {
            self.expected_outputs = Some(IndexMap::new());
        }
        if let Some(outputs) = &mut self.expected_outputs {
            outputs.insert(name.to_string(), value);
        }
        self
    }

    pub fn with_expected_output_u32(mut self, name: &str, value: u32) -> Self {
        if self.expected_outputs.is_none() {
            self.expected_outputs = Some(IndexMap::new());
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
