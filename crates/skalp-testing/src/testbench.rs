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

use crate::cache::{collect_dependencies, CompilationCache};
use anyhow::Result;
use skalp_mir::MirCompiler;
use skalp_sim::{
    convert_gate_netlist_to_sir, HwAccel, SimLevel, SimulationConfig, Simulator, UnifiedSimConfig,
    UnifiedSimulator,
};
use skalp_sir::convert_mir_to_sir_with_hierarchy;
use std::collections::HashMap;
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
        // Check SKALP_SIM_MODE environment variable - "cpu" disables GPU
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true); // Default to GPU if not set

        let config = SimulationConfig {
            // SSA conversion is now implemented in skalp-mir to eliminate combinational cycles
            // from mutable variable reassignment (x = f(x) -> x_0 = value, x_1 = f(x_0), etc.)
            use_gpu,
            max_cycles: 1_000_000,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        };
        eprintln!(
            "🚨 DEBUG: use_gpu = {} (SKALP_SIM_MODE={:?})",
            config.use_gpu,
            std::env::var("SKALP_SIM_MODE").ok()
        );
        Self::with_config(source_path, config).await
    }

    /// Create a new testbench with custom simulation config
    pub async fn with_config(source_path: &str, config: SimulationConfig) -> Result<Self> {
        use std::time::Instant;
        let start_total = Instant::now();
        eprintln!("⏱️  [TESTBENCH] Starting compilation of '{}'", source_path);

        let path = Path::new(source_path);

        // Initialize cache (defaults to target/skalp-cache/, override with SKALP_CACHE_DIR)
        let cache = CompilationCache::new();
        let dependencies = collect_dependencies(path).unwrap_or_default();
        let cache_key = cache.compute_cache_key(path, &dependencies).ok();

        // Try to load from cache
        let sir = if let Some(ref key) = cache_key {
            if let Ok(Some(cached_sir)) = cache.load(key) {
                eprintln!("⏱️  [TESTBENCH] Using cached SIR (skipped HIR→MIR→SIR)");
                cached_sir
            } else {
                // Cache miss - compile from source
                Self::compile_to_sir(path, &cache, Some(key))?
            }
        } else {
            // No cache key - compile without caching
            Self::compile_to_sir(path, &cache, None)?
        };

        // Create simulator and load design
        let start_sim = Instant::now();
        eprintln!("⏱️  [TESTBENCH] Creating simulator and loading design...");
        let mut sim = Simulator::new(config).await?;
        sim.load_module(&sir).await?;
        eprintln!(
            "⏱️  [TESTBENCH] Simulator initialization completed in {:?}",
            start_sim.elapsed()
        );

        eprintln!(
            "⏱️  [TESTBENCH] ✅ Total testbench creation time: {:?}",
            start_total.elapsed()
        );

        Ok(Self {
            sim,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
        })
    }

    /// Compile source to SIR (HIR → MIR → SIR), optionally caching the result
    fn compile_to_sir(
        path: &Path,
        cache: &CompilationCache,
        cache_key: Option<&String>,
    ) -> Result<skalp_sir::SirModule> {
        use std::time::Instant;

        // Parse and build HIR with full module resolution support (Bug #84 fix)
        // This handles imports like "mod async_fifo; use async_fifo::AsyncFifo"
        let start_hir = Instant::now();
        let context = skalp_frontend::parse_and_build_compilation_context(path)?;
        eprintln!(
            "⏱️  [TESTBENCH] HIR parsing completed in {:?}",
            start_hir.elapsed()
        );

        // Compile to MIR with optimizations and module scope resolution
        // The proper HIR→MIR fix ensures array assignments are expanded correctly
        // Bug #84 fix: Pass module HIRs for proper transitive import support
        let start_mir = Instant::now();
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .map_err(|e| anyhow::anyhow!("{}", e))?;
        eprintln!(
            "⏱️  [TESTBENCH] MIR compilation completed in {:?}",
            start_mir.elapsed()
        );

        // Convert to SIR with hierarchical elaboration
        let start_sir = Instant::now();
        if mir.modules.is_empty() {
            anyhow::bail!("No modules found in design");
        }

        // Find the top-level module: the module that is NOT instantiated by any other module
        // Collect all module IDs that are instantiated
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

        // Get the source file basename for matching
        let source_basename = path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string());

        // BUG #180 FIX: Improved top module selection
        // Priority order:
        // 1. Module whose name matches the source file basename (search ALL modules first!)
        // 2. Among uninstantiated modules: Module that is NOT a trait specialization
        // 3. Among uninstantiated modules: Module with the most instances (original logic)
        let top_module = if let Some(ref basename) = source_basename {
            // Convert basename to PascalCase for matching (e.g., "fp_sqrt_simple" -> "FpSqrtSimple")
            let pascal_basename: String = basename
                .split('_')
                .map(|s| {
                    let mut chars: Vec<char> = s.chars().collect();
                    if !chars.is_empty() {
                        chars[0] = chars[0].to_ascii_uppercase();
                    }
                    chars.into_iter().collect::<String>()
                })
                .collect();

            // BUG #183 FIX: First search ALL modules for exact match, not just uninstantiated
            // This handles cases where a module may be incorrectly marked as instantiated
            // due to module ID assignment issues during monomorphization
            if let Some(m) = mir.modules.iter().find(|m| m.name == pascal_basename) {
                eprintln!(
                    "  ✓ Found module matching source file name: '{}' (searching all modules)",
                    m.name
                );
                m
            } else if let Some(m) = uninstantiated.iter().find(|m| m.name == pascal_basename) {
                eprintln!("  ✓ Found module matching source file name: '{}'", m.name);
                *m
            } else {
                // Filter out trait specializations (modules with _fp32, _fp64, _fp16 suffixes)
                let non_specialized: Vec<_> = uninstantiated
                    .iter()
                    .filter(|m| {
                        !m.name.ends_with("_fp32")
                            && !m.name.ends_with("_fp64")
                            && !m.name.ends_with("_fp16")
                    })
                    .collect();

                if !non_specialized.is_empty() {
                    // Prefer non-specialized modules with the most instances
                    non_specialized
                        .iter()
                        .max_by_key(|m| m.instances.len())
                        .map(|m| **m)
                        .unwrap()
                } else {
                    // Fallback to original logic
                    *uninstantiated
                        .iter()
                        .max_by_key(|m| m.instances.len())
                        .unwrap()
                }
            }
        } else {
            // No source basename - use original logic
            *uninstantiated
                .iter()
                .max_by_key(|m| m.instances.len())
                .unwrap()
        };

        eprintln!(
            "✅ Selected top module: '{}' ({} instances)",
            top_module.name,
            top_module.instances.len()
        );

        let sir = convert_mir_to_sir_with_hierarchy(&mir, top_module);
        eprintln!(
            "⏱️  [TESTBENCH] SIR conversion completed in {:?}",
            start_sir.elapsed()
        );

        // Store in cache if we have a key
        if let Some(key) = cache_key {
            if let Err(e) = cache.store(key, &sir) {
                eprintln!("⚠️  [CACHE] Failed to store in cache: {}", e);
            }
        }

        Ok(sir)
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

    /// Expect an fp32 (float) output signal with a tolerance for comparison
    ///
    /// This method is useful for floating-point signals where exact bit-level
    /// comparison may fail due to rounding differences.
    ///
    /// # Parameters
    /// - `signal`: Name of the output signal
    /// - `expected`: Expected fp32 value
    /// - `tolerance`: Maximum allowed difference between expected and actual
    ///
    /// # Example
    /// ```rust,ignore
    /// tb.expect_fp32("result", 3.14159, 0.001).await;
    /// ```
    pub async fn expect_fp32(&mut self, signal: &str, expected: f32, tolerance: f32) -> &mut Self {
        let actual_bytes = self.sim.get_output(signal).await.unwrap();

        // Convert bytes to f32 (assuming little-endian IEEE 754)
        let actual = if actual_bytes.len() >= 4 {
            f32::from_le_bytes([
                actual_bytes[0],
                actual_bytes[1],
                actual_bytes[2],
                actual_bytes[3],
            ])
        } else {
            panic!(
                "Signal '{}' has {} bytes, expected at least 4 for fp32",
                signal,
                actual_bytes.len()
            );
        };

        let diff = (actual - expected).abs();
        assert!(
            diff <= tolerance,
            "Signal '{}' fp32 mismatch at cycle {}: expected {}, got {} (diff: {}, tolerance: {})",
            signal,
            self.cycle_count,
            expected,
            actual,
            diff,
            tolerance
        );

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

impl IntoSignalValue for f32 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for f64 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
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

impl FromSignalValue for f32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }
}

impl FromSignalValue for f64 {
    fn from_bytes(bytes: &[u8]) -> Self {
        f64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ])
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

// ============================================================================
// Gate-Level Testbench
// ============================================================================

/// Gate-level testbench for technology-mapped verification
///
/// This testbench compiles designs through the full synthesis flow:
/// HIR → MIR → LIR → GateNetlist → SIR
///
/// The simulation uses primitive gates (AND, OR, NOT, DFF, etc.) instead of
/// behavioral constructs, providing more accurate timing and area verification.
///
/// # Example
/// ```rust,no_run
/// use skalp_testing::testbench::*;
///
/// #[test]
/// fn test_counter_gates() {
///     let mut tb = GateLevelTestbench::new("examples/counter.sk").unwrap();
///
///     // Reset
///     tb.set("rst", 1u8);
///     tb.clock(2);
///     tb.set("rst", 0u8);
///
///     // Count up
///     tb.clock(5);
///     assert_eq!(tb.get_u32("count"), 5);
/// }
/// ```
pub struct GateLevelTestbench {
    sim: UnifiedSimulator,
    pending_inputs: HashMap<String, u64>,
    cycle_count: u64,
}

impl GateLevelTestbench {
    /// Create a new gate-level testbench from a SKALP source file
    pub fn new(source_path: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_cycles: 1_000_000,
            capture_waveforms: true,
            ..Default::default()
        };

        Self::with_config(source_path, config)
    }

    /// Create a new gate-level testbench with custom config
    pub fn with_config(source_path: &str, config: UnifiedSimConfig) -> Result<Self> {
        use skalp_frontend::parse_and_build_hir_from_file;
        use skalp_lir::{
            get_stdlib_library, lower_mir_hierarchical, lower_mir_module_to_lir,
            map_hierarchical_to_gates, map_lir_to_gates_optimized,
        };
        use std::time::Instant;

        let start_total = Instant::now();
        eprintln!(
            "⏱️  [GATE-LEVEL TB] Starting gate-level compilation of '{}'",
            source_path
        );

        let path = Path::new(source_path);

        // Parse and build HIR
        let start_hir = Instant::now();
        let hir = parse_and_build_hir_from_file(path)?;
        eprintln!(
            "⏱️  [GATE-LEVEL TB] HIR parsing completed in {:?}",
            start_hir.elapsed()
        );

        // Compile to MIR
        let start_mir = Instant::now();
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir(&hir)
            .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;
        eprintln!(
            "⏱️  [GATE-LEVEL TB] MIR compilation completed in {:?}",
            start_mir.elapsed()
        );

        // Load technology library
        let library = get_stdlib_library("generic_asic")
            .map_err(|e| anyhow::anyhow!("Failed to load technology library: {:?}", e))?;

        // Check if design has hierarchy
        let has_hierarchy =
            mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

        // Lower to LIR and tech-map to GateNetlist
        let start_lir = Instant::now();
        let netlist = if has_hierarchy {
            eprintln!(
                "⏱️  [GATE-LEVEL TB] Using hierarchical synthesis ({} modules)",
                mir.modules.len()
            );

            let hier_lir = lower_mir_hierarchical(&mir);
            let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
            hier_netlist.flatten()
        } else {
            eprintln!("⏱️  [GATE-LEVEL TB] Using flat synthesis (single module)");

            let top_module = mir
                .modules
                .first()
                .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?;

            let lir_result = lower_mir_module_to_lir(top_module);
            let tech_result = map_lir_to_gates_optimized(&lir_result.lir, &library);
            tech_result.netlist
        };
        eprintln!(
            "⏱️  [GATE-LEVEL TB] Gate netlist completed in {:?} ({} cells, {} nets)",
            start_lir.elapsed(),
            netlist.cells.len(),
            netlist.nets.len()
        );

        // Convert GateNetlist to SIR for simulation
        let start_sir = Instant::now();
        let sir_result = convert_gate_netlist_to_sir(&netlist);
        eprintln!(
            "⏱️  [GATE-LEVEL TB] SIR conversion completed in {:?} ({} primitives)",
            start_sir.elapsed(),
            sir_result.stats.primitives_created
        );

        // Create and initialize unified simulator
        let start_sim = Instant::now();
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_gate_level(&sir_result.sir)
            .map_err(|e| anyhow::anyhow!("Failed to load design: {}", e))?;

        eprintln!(
            "⏱️  [GATE-LEVEL TB] Simulator initialized in {:?} (device: {})",
            start_sim.elapsed(),
            sim.device_info()
        );
        eprintln!(
            "⏱️  [GATE-LEVEL TB] ✅ Total gate-level testbench creation time: {:?}",
            start_total.elapsed()
        );

        Ok(Self {
            sim,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
        })
    }

    /// Set an input signal value (chainable)
    pub fn set(&mut self, signal: &str, value: impl IntoSignalValue) -> &mut Self {
        let bytes = value.into_bytes();
        // Convert bytes to u64
        let val = bytes_to_u64(&bytes);
        self.pending_inputs.insert(signal.to_string(), val);
        self
    }

    /// Apply pending inputs and run for N clock cycles (uses default clock "clk")
    pub fn clock(&mut self, cycles: usize) -> &mut Self {
        self.clock_signal("clk", cycles)
    }

    /// Apply pending inputs and run for N cycles on a specific clock signal
    pub fn clock_signal(&mut self, clock_name: &str, cycles: usize) -> &mut Self {
        // Apply all pending inputs
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value);
        }

        // Run clock cycles
        for _ in 0..cycles {
            self.sim.set_input(clock_name, 0);
            self.sim.step();
            self.sim.set_input(clock_name, 1);
            self.sim.step();
            self.cycle_count += 1;
        }

        self
    }

    /// Apply pending inputs and step the simulation once
    pub fn step(&mut self) -> &mut Self {
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value);
        }
        self.sim.step();
        self
    }

    /// Get an output value as u32
    ///
    /// Handles both packed signals (e.g., "result") and bit-blasted signals
    /// (e.g., "result[0]", "result[1]", ..., "result[31]").
    pub fn get_u32(&mut self, signal: &str) -> u32 {
        // If there are pending inputs, apply them and step
        if !self.pending_inputs.is_empty() {
            self.step();
        }

        // Try direct lookup first
        if let Some(val) = self.sim.get_output(signal) {
            return val as u32;
        }

        // Try bit-blasted signals
        self.get_bitblasted_value(signal, 32) as u32
    }

    /// Get an output value as u64
    ///
    /// Handles both packed signals and bit-blasted signals.
    pub fn get_u64(&mut self, signal: &str) -> u64 {
        if !self.pending_inputs.is_empty() {
            self.step();
        }

        // Try direct lookup first
        if let Some(val) = self.sim.get_output(signal) {
            return val;
        }

        // Try bit-blasted signals
        self.get_bitblasted_value(signal, 64)
    }

    /// Get a bit-blasted signal value (e.g., "signal[0]", "signal[1]", ...)
    fn get_bitblasted_value(&self, signal: &str, max_bits: usize) -> u64 {
        let mut result = 0u64;

        for bit in 0..max_bits {
            let bit_signal = format!("{}[{}]", signal, bit);
            if let Some(val) = self.sim.get_output(&bit_signal) {
                if val != 0 {
                    result |= 1u64 << bit;
                }
            } else {
                // If we can't find this bit, stop looking for more
                // (handles signals that are less than max_bits wide)
                if bit > 0 {
                    break;
                }
            }
        }

        result
    }

    /// Expect an output signal to have a specific value
    pub fn expect(&mut self, signal: &str, expected: impl IntoSignalValue) -> &mut Self {
        let expected_bytes = expected.into_bytes();
        let expected_u64 = bytes_to_u64(&expected_bytes);
        let actual = self.sim.get_output(signal).unwrap_or(0);

        assert_eq!(
            actual, expected_u64,
            "Signal '{}' mismatch at cycle {}: expected 0x{:x}, got 0x{:x}",
            signal, self.cycle_count, expected_u64, actual
        );

        self
    }

    /// Expect a fp32 output with tolerance
    pub fn expect_fp32(&mut self, signal: &str, expected: f32, tolerance: f32) -> &mut Self {
        let actual_bits = self.sim.get_output(signal).unwrap_or(0) as u32;
        let actual = f32::from_bits(actual_bits);
        let diff = (actual - expected).abs();

        assert!(
            diff <= tolerance,
            "Signal '{}' fp32 mismatch at cycle {}: expected {}, got {} (diff: {}, tolerance: {})",
            signal,
            self.cycle_count,
            expected,
            actual,
            diff,
            tolerance
        );

        self
    }

    /// Apply reset for N cycles
    pub fn reset(&mut self, cycles: usize) -> &mut Self {
        self.set("rst", 1u8);
        self.clock(cycles);
        self.set("rst", 0u8);
        self.clock(1);
        self
    }

    /// Get the current cycle count
    pub fn cycles(&self) -> u64 {
        self.cycle_count
    }

    /// Get device info (CPU or GPU)
    pub fn device_info(&self) -> String {
        self.sim.device_info()
    }

    /// Get input port names
    pub fn get_input_names(&self) -> Vec<String> {
        self.sim.get_input_names()
    }

    /// Get output port names
    pub fn get_output_names(&self) -> Vec<String> {
        self.sim.get_output_names()
    }
}

/// Helper function to convert bytes to u64 (little-endian)
fn bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut result = 0u64;
    for (i, &byte) in bytes.iter().take(8).enumerate() {
        result |= (byte as u64) << (i * 8);
    }
    result
}
