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
use skalp_frontend::{parse_and_build_hir, parse_and_build_hir_from_file};
use skalp_mir::{MirCompiler, OptimizationLevel};
use skalp_sim::{SimulationConfig, Simulator};
use skalp_sir::convert_mir_to_sir_with_hierarchy;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

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
            // Force CPU mode until SSA conversion is implemented
            // GPU mode fails on mutable variable reassignment (x = f(x)) which creates
            // combinational cycles. The CPU works despite cycles because signals persist.
            // TODO: Implement SSA conversion to fix GPU mode
            use_gpu: false,
            max_cycles: 1_000_000,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        };
        eprintln!("🚨 DEBUG: use_gpu = {}", config.use_gpu);
        Self::with_config(source_path, config).await
    }

    /// Create a new testbench with custom simulation config
    pub async fn with_config(source_path: &str, config: SimulationConfig) -> Result<Self> {
        use std::time::Instant;
        let start_total = Instant::now();
        eprintln!("⏱️  [TESTBENCH] Starting compilation of '{}'", source_path);

        // Parse and build HIR with full module resolution support (Bug #84 fix)
        // This handles imports like "mod async_fifo; use async_fifo::AsyncFifo"
        let start_hir = Instant::now();
        let path = Path::new(source_path);
        let context = skalp_frontend::parse_and_build_compilation_context(path)?;
        eprintln!("⏱️  [TESTBENCH] HIR parsing completed in {:?}", start_hir.elapsed());

        // Compile to MIR with optimizations and module scope resolution
        // The proper HIR→MIR fix ensures array assignments are expanded correctly
        // Bug #84 fix: Pass module HIRs for proper transitive import support
        let start_mir = Instant::now();
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .map_err(|e| anyhow::anyhow!("{}", e))?;
        eprintln!("⏱️  [TESTBENCH] MIR compilation completed in {:?}", start_mir.elapsed());

        // Convert to SIR with hierarchical elaboration
        let start_sir = Instant::now();
        if mir.modules.is_empty() {
            anyhow::bail!("No modules found in design");
        }

        // Find the top-level module: the module that is NOT instantiated by any other module
        // Collect all module IDs that are instantiated
        eprintln!("🔍 DEBUG: Total modules in MIR: {}", mir.modules.len());
        for module in &mir.modules {
            eprintln!(
                "  - Module '{}' (ID={:?}): {} ports, {} instances",
                module.name,
                module.id,
                module.ports.len(),
                module.instances.len()
            );
        }

        let mut instantiated_modules = std::collections::HashSet::new();
        for module in &mir.modules {
            for instance in &module.instances {
                instantiated_modules.insert(instance.module);
                eprintln!(
                    "  - Module '{}' instantiates module ID {:?}",
                    module.name, instance.module
                );
            }
        }

        // Find the module that is not instantiated (the top-level)
        // IMPORTANT: When multiple modules are uninstantiated (e.g., generic templates
        // and their specializations), prefer the module that HAS instances (is a parent),
        // since that's more likely to be the actual top-level testbench/design.
        let uninstantiated: Vec<_> = mir
            .modules
            .iter()
            .filter(|m| {
                let is_uninstantiated = !instantiated_modules.contains(&m.id);
                eprintln!(
                    "  - Module '{}' (ID={:?}): is_uninstantiated={}, has_instances={}",
                    m.name,
                    m.id,
                    is_uninstantiated,
                    !m.instances.is_empty()
                );
                is_uninstantiated
            })
            .collect();

        if uninstantiated.is_empty() {
            anyhow::bail!(
                "Could not find top-level module (no module is uninstantiated). \
                 This might indicate a circular dependency or all modules are instantiated."
            );
        }

        // Prefer modules with instances (parent modules) over leaf modules
        let top_module = uninstantiated
            .iter()
            .max_by_key(|m| m.instances.len())
            .unwrap();

        eprintln!(
            "✅ Selected top module: '{}' ({} instances)",
            top_module.name,
            top_module.instances.len()
        );

        let sir = convert_mir_to_sir_with_hierarchy(&mir, top_module);
        eprintln!("⏱️  [TESTBENCH] SIR conversion completed in {:?}", start_sir.elapsed());

        // Create simulator and load design
        let start_sim = Instant::now();
        eprintln!("⏱️  [TESTBENCH] Creating simulator and loading design...");
        let mut sim = Simulator::new(config).await?;
        sim.load_module(&sir).await?;
        eprintln!("⏱️  [TESTBENCH] Simulator initialization completed in {:?}", start_sim.elapsed());

        eprintln!("⏱️  [TESTBENCH] ✅ Total testbench creation time: {:?}", start_total.elapsed());

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

    /// Apply pending inputs and step the simulation once (for combinational logic)
    ///
    /// This is useful for purely combinational designs or when you need to
    /// evaluate combinational logic without clock edges.
    ///
    /// # Example
    /// ```rust,ignore
    /// tb.set("a", 5u32).set("b", 3u32);
    /// tb.step().await;
    /// let result: u32 = tb.get_as("sum").await;
    /// ```
    pub async fn step(&mut self) -> &mut Self {
        // Apply all pending inputs
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await.unwrap();
        }

        // Step the simulation once
        self.sim.step_simulation().await.unwrap();

        self
    }

    /// Get the current value of an output signal
    ///
    /// NOTE: This automatically applies pending inputs and steps the simulation
    /// if there are any pending inputs. This ensures combinational logic is
    /// evaluated correctly.
    pub async fn get(&mut self, signal: &str) -> Vec<u8> {
        // If there are pending inputs, apply them and step
        if !self.pending_inputs.is_empty() {
            self.step().await;
        }

        self.sim.get_output(signal).await.unwrap()
    }

    /// Get the current value as a specific type
    ///
    /// NOTE: This automatically applies pending inputs and steps the simulation
    /// if there are any pending inputs. This ensures combinational logic is
    /// evaluated correctly.
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
