//! Ergonomic testbench API for hardware verification
//!
//! This module provides a high-level, declarative API for writing testbenches
//! that is much more concise than manually calling set_input/step/get_output.
//!
//! # Example
//! ```rust,no_run
//! use skalp_testing::testbench::*;
//!
//! #[tokio::test]
//! async fn test_alu() {
//!     let mut tb = Testbench::new("examples/alu.sk").await.unwrap();
//!
//!     // Test ADD operation
//!     tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
//!     tb.clock(2).await;
//!     tb.expect("result", 8u32).await;
//!
//!     // Test SUB operation
//!     tb.set("op", 0b001u8);
//!     tb.clock(2).await;
//!     tb.expect("result", 2u32).await;
//! }
//! ```

use anyhow::Result;
use skalp_frontend::parse_and_build_hir;
use skalp_mir::{MirCompiler, OptimizationLevel};
use skalp_sim::{SimulationConfig, Simulator};
use skalp_sir::convert_mir_to_sir;
use std::collections::HashMap;
use std::fs;

/// High-level testbench builder for ergonomic verification
pub struct Testbench {
    sim: Simulator,
    pending_inputs: HashMap<String, Vec<u8>>,
    cycle_count: u64,
}

impl Testbench {
    /// Create a new testbench from a SKALP source file
    pub async fn new(source_path: &str) -> Result<Self> {
        let config = SimulationConfig {
            use_gpu: cfg!(target_os = "macos"), // Use GPU on macOS, CPU elsewhere
            max_cycles: 1_000_000,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        };
        Self::with_config(source_path, config).await
    }

    /// Create a new testbench with custom simulation config
    pub async fn with_config(source_path: &str, config: SimulationConfig) -> Result<Self> {
        let source = fs::read_to_string(source_path)?;

        // Parse and build HIR
        let hir = parse_and_build_hir(&source)?;

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler
            .compile_to_mir(&hir)
            .map_err(|e| anyhow::anyhow!("{}", e))?;

        // Convert to SIR
        if mir.modules.is_empty() {
            anyhow::bail!("No modules found in design");
        }
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Create simulator and load design
        let mut sim = Simulator::new(config).await?;
        sim.load_module(&sir).await?;

        Ok(Self {
            sim,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
        })
    }

    /// Set an input signal value (chainable)
    pub fn set(&mut self, signal: &str, value: impl IntoSignalValue) -> &mut Self {
        self.pending_inputs
            .insert(signal.to_string(), value.into_bytes());
        self
    }

    /// Apply pending inputs and run for N clock cycles (uses default clock "clk")
    ///
    /// # Parameters
    /// - `cycles`: Number of clock cycles to run (NOT a signal value)
    pub async fn clock(&mut self, cycles: usize) -> &mut Self {
        self.clock_signal("clk", cycles).await
    }

    /// Apply pending inputs and run for N cycles on a specific clock signal
    ///
    /// # Parameters
    /// - `clock_name`: Name of the clock signal (e.g., "clk", "wr_clk", "rd_clk")
    /// - `cycles`: Number of clock cycles to run (NOT a signal value)
    ///
    /// # Example
    /// ```rust,ignore
    /// // Run 3 cycles on the write clock
    /// tb.clock_signal("wr_clk", 3).await;
    /// ```
    pub async fn clock_signal(&mut self, clock_name: &str, cycles: usize) -> &mut Self {
        // Apply all pending inputs
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await.unwrap();
        }

        // Run clock cycles
        for _ in 0..cycles {
            self.sim.set_input(clock_name, vec![0]).await.unwrap();
            self.sim.step_simulation().await.unwrap();
            self.sim.set_input(clock_name, vec![1]).await.unwrap();
            self.sim.step_simulation().await.unwrap();
            self.cycle_count += 1;
        }

        self
    }

    /// Toggle multiple clocks independently - useful for CDC testing
    ///
    /// # Parameters
    /// - `clocks`: Array of (clock_name, num_cycles) tuples
    ///   - Each tuple specifies a clock and how many cycles to run it
    ///   - The numbers are cycle counts, NOT signal values
    ///
    /// # Example
    /// ```rust,ignore
    /// // Run 1 cycle on wr_clk AND 2 cycles on rd_clk simultaneously
    /// // This simulates rd_clk being 2x faster than wr_clk
    /// tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 2)]).await;
    /// //                           ^               ^
    /// //                    1 = num cycles   2 = num cycles
    /// ```
    pub async fn clock_multi(&mut self, clocks: &[(&str, usize)]) -> &mut Self {
        // Apply all pending inputs first
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await.unwrap();
        }

        // Find the maximum number of cycles across all clocks
        let max_cycles = clocks.iter().map(|(_, c)| c).max().copied().unwrap_or(0);

        for cycle in 0..max_cycles {
            // For each clock, toggle if we're within its cycle count
            for (clock_name, clock_cycles) in clocks {
                if cycle < *clock_cycles {
                    // Low phase
                    self.sim.set_input(clock_name, vec![0]).await.unwrap();
                }
            }
            self.sim.step_simulation().await.unwrap();

            for (clock_name, clock_cycles) in clocks {
                if cycle < *clock_cycles {
                    // High phase
                    self.sim.set_input(clock_name, vec![1]).await.unwrap();
                }
            }
            self.sim.step_simulation().await.unwrap();
            self.cycle_count += 1;
        }

        self
    }

    /// Expect an output signal to have a specific value
    pub async fn expect(&mut self, signal: &str, expected: impl IntoSignalValue) -> &mut Self {
        let actual = self.sim.get_output(signal).await.unwrap();
        let expected_bytes = expected.into_bytes();

        assert_eq!(
            actual, expected_bytes,
            "Signal '{}' mismatch at cycle {}: expected {:?}, got {:?}",
            signal, self.cycle_count, expected_bytes, actual
        );

        self
    }

    /// Get the current value of an output signal
    pub async fn get(&mut self, signal: &str) -> Vec<u8> {
        self.sim.get_output(signal).await.unwrap()
    }

    /// Get the current value as a specific type
    pub async fn get_as<T: FromSignalValue>(&mut self, signal: &str) -> T {
        let bytes = self.get(signal).await;
        T::from_bytes(&bytes)
    }

    /// Apply reset for N cycles
    pub async fn reset(&mut self, cycles: usize) -> &mut Self {
        self.set("rst", 1u8);
        self.clock(cycles).await;
        self.set("rst", 0u8);
        self.clock(1).await;
        self
    }

    /// Run simulation until a condition is met (with timeout)
    pub async fn wait_until<F>(&mut self, mut condition: F, max_cycles: usize) -> Result<&mut Self>
    where
        F: FnMut(&[u8]) -> bool,
    {
        for _ in 0..max_cycles {
            self.clock(1).await;
            let value = self.get("valid").await; // TODO: Make signal name configurable
            if condition(&value) {
                return Ok(self);
            }
        }

        anyhow::bail!("Timeout waiting for condition after {} cycles", max_cycles)
    }

    /// Get the current cycle count
    pub fn cycles(&self) -> u64 {
        self.cycle_count
    }

    /// Check multiple signals at once
    /// Note: Use tuples with concrete types rather than trait objects
    /// Example: tb.expect_many(&[("a", 5u32), ("b", 3u32)]).await;
    pub async fn expect_many<T: IntoSignalValue + Clone>(
        &mut self,
        expectations: &[(&str, T)],
    ) -> &mut Self {
        for (signal, expected) in expectations {
            let actual = self.sim.get_output(signal).await.unwrap();
            let expected_bytes = expected.clone().into_bytes();

            assert_eq!(
                actual, expected_bytes,
                "Signal '{}' mismatch at cycle {}: expected {:?}, got {:?}",
                signal, self.cycle_count, expected_bytes, actual
            );
        }

        self
    }
}

/// Trait for converting Rust types to signal byte values
pub trait IntoSignalValue {
    fn into_bytes(self) -> Vec<u8>;
}

impl IntoSignalValue for u8 {
    fn into_bytes(self) -> Vec<u8> {
        vec![self]
    }
}

impl IntoSignalValue for u16 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for u32 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for u64 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for Vec<u8> {
    fn into_bytes(self) -> Vec<u8> {
        self
    }
}

impl IntoSignalValue for &[u8] {
    fn into_bytes(self) -> Vec<u8> {
        self.to_vec()
    }
}

/// Trait for converting signal byte values to Rust types
pub trait FromSignalValue {
    fn from_bytes(bytes: &[u8]) -> Self;
}

impl FromSignalValue for u8 {
    fn from_bytes(bytes: &[u8]) -> Self {
        bytes[0]
    }
}

impl FromSignalValue for u16 {
    fn from_bytes(bytes: &[u8]) -> Self {
        u16::from_le_bytes([bytes[0], bytes[1]])
    }
}

impl FromSignalValue for u32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }
}

impl FromSignalValue for u64 {
    fn from_bytes(bytes: &[u8]) -> Self {
        u64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ])
    }
}

impl FromSignalValue for Vec<u8> {
    fn from_bytes(bytes: &[u8]) -> Self {
        bytes.to_vec()
    }
}

/// Builder for creating testbench sequences
pub struct TestSequence {
    steps: Vec<TestStep>,
}

#[derive(Clone)]
enum TestStep {
    SetInput(String, Vec<u8>),
    Clock(usize),
    ExpectOutput(String, Vec<u8>),
    Reset(usize),
}

impl TestSequence {
    pub fn new() -> Self {
        Self { steps: Vec::new() }
    }

    pub fn set(mut self, signal: &str, value: impl IntoSignalValue) -> Self {
        self.steps
            .push(TestStep::SetInput(signal.to_string(), value.into_bytes()));
        self
    }

    pub fn clock(mut self, cycles: usize) -> Self {
        self.steps.push(TestStep::Clock(cycles));
        self
    }

    pub fn expect(mut self, signal: &str, value: impl IntoSignalValue) -> Self {
        self.steps.push(TestStep::ExpectOutput(
            signal.to_string(),
            value.into_bytes(),
        ));
        self
    }

    pub fn reset(mut self, cycles: usize) -> Self {
        self.steps.push(TestStep::Reset(cycles));
        self
    }

    pub async fn run(self, tb: &mut Testbench) {
        for step in self.steps {
            match step {
                TestStep::SetInput(signal, value) => {
                    tb.set(&signal, value);
                }
                TestStep::Clock(cycles) => {
                    tb.clock(cycles).await;
                }
                TestStep::ExpectOutput(signal, value) => {
                    tb.expect(&signal, value).await;
                }
                TestStep::Reset(cycles) => {
                    tb.reset(cycles).await;
                }
            }
        }
    }
}

impl Default for TestSequence {
    fn default() -> Self {
        Self::new()
    }
}

/// Macro for creating test vectors (stimulus/response pairs)
#[macro_export]
macro_rules! test_vector {
    (
        inputs: { $($in_name:ident: $in_val:expr),* $(,)? },
        outputs: { $($out_name:ident: $out_val:expr),* $(,)? }
    ) => {
        {
            let mut inputs = std::collections::HashMap::new();
            $(
                inputs.insert(stringify!($in_name).to_string(), $in_val);
            )*
            let mut outputs = std::collections::HashMap::new();
            $(
                outputs.insert(stringify!($out_name).to_string(), $out_val);
            )*
            (inputs, outputs)
        }
    };
}
