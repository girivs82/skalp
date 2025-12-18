#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP LIR - Low-level Intermediate Representation
//!
//! Gate-level representation for hardware designs. The compilation flow is:
//!
//! ```text
//! HIR → MIR → LIR → SIR (for simulation)
//! ```
//!
//! Key types:
//! - [`Lir`] - Technology-independent gate-level netlist (primitives + nets)
//! - [`Primitive`] - Individual gate/flip-flop/mux with hierarchy path
//! - [`PrimitiveType`] - Type of primitive (AND, OR, DFF, MUX2, etc.)
//! - [`LirNet`] - Wire connecting primitives
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::lower_to_lir;
//!
//! let mir = compile_to_mir(source)?;
//! let lir_results = lower_to_lir(&mir)?;
//! for result in lir_results {
//!     println!("Module: {}", result.lir.name);
//!     println!("Primitives: {}", result.lir.primitives.len());
//!     println!("Total FIT: {}", result.lir.stats.total_fit);
//! }
//! ```

pub mod builtin_libraries;
pub mod gate_netlist;
pub mod gate_optimization;
pub mod lir;
pub mod mir_to_gate_netlist;
pub mod mir_to_word_lir;
pub mod netlist;
pub mod pattern_detector;
pub mod primitives;
pub mod tech_library;
pub mod tech_mapper;
pub mod technology;
pub mod word_lir;

// Primary LIR types
pub use lir::{
    FitOverrides, HierarchyNode, Lir, LirNet, LirSafetyInfo, NetId, NetlistStats, Primitive,
    PrimitiveId, PrimitiveType,
};

// MIR to LIR transformation
pub use mir_to_gate_netlist::{
    lower_mir_module_to_lir, MirToLirResult, MirToLirTransform, TransformStats,
};

// Gate optimization passes (operate on Lir type)
pub use gate_optimization::{
    GateBooleanSimplification, GateBufferRemoval, GateCSE, GateConstantFolding,
    GateDeadCodeElimination, GateFanoutOptimization, GateMuxOptimization, GateOptConfig,
    GateOptimizationPipeline, GateOptimizationResult, LirOptimizationPass, OptTarget,
};

// Other exports
pub use netlist::Netlist;

// Word-level LIR (for technology mapping)
pub use word_lir::{WordLir, WordLirStats, WordNode, WordNodeId, WordOp, WordSignal, WordSignalId};

// MIR to Word-level LIR transformation
pub use mir_to_word_lir::{lower_mir_module_to_word_lir, MirToWordLirResult};

// Gate-level netlist (output of technology mapping)
pub use gate_netlist::{
    Cell, CellFailureMode, CellId, CellSafetyClassification, FaultType, GateNet, GateNetId,
    GateNetlist, GateNetlistStats,
};

// Technology library
pub use tech_library::{
    arrhenius_acceleration_factor,
    process_corner_factor,
    voltage_acceleration_factor,
    CellFunction,
    DecompConnectivity,
    DecompSource,
    DecompositionRule,
    // Derating and operating conditions
    DeratingFactors,
    DeratingPreset,
    LibraryCell,
    LibraryDeratingSummary,
    LibraryFailureMode,
    OperatingConditions,
    ProcessCorner,
    TechLibrary,
};

// Technology mapper
pub use tech_mapper::{map_word_lir_to_gates, TechMapResult, TechMapStats, TechMapper};

// Built-in technology libraries
pub use builtin_libraries::{
    builtin_asic_28nm, builtin_asic_7nm, builtin_fpga_lut4, builtin_fpga_lut6,
    builtin_generic_asic, get_builtin_library, list_builtin_libraries,
};

// Structural pattern detection for safety mechanisms
pub use pattern_detector::{
    DetectedPatterns, DmrPattern, PatternDetector, TmrPattern, VoterPattern, VoterType,
    WatchdogPattern,
};

use anyhow::Result;
use skalp_mir::mir::ModuleId;
use skalp_mir::Mir;
use std::collections::HashMap;

/// Lower MIR to LIR (gate-level netlist)
///
/// This produces a technology-independent gate-level representation with:
/// - Full primitive decomposition (gates, flip-flops, muxes, adders)
/// - Per-bit net representation for multi-bit signals
/// - FIT estimation for each primitive (for ISO 26262 safety analysis)
/// - Hierarchy traceability via `Primitive.path`
///
/// # Example
///
/// ```ignore
/// use skalp_mir::Mir;
/// use skalp_lir::lower_to_lir;
///
/// let mir = compile_to_mir(source)?;
/// let lir_results = lower_to_lir(&mir)?;
/// for result in lir_results {
///     println!("Module: {}", result.lir.name);
///     println!("Primitives: {}", result.lir.primitives.len());
///     println!("Total FIT: {}", result.lir.stats.total_fit);
/// }
/// ```
pub fn lower_to_lir(mir: &Mir) -> Result<Vec<MirToLirResult>> {
    let mut results = Vec::new();

    for module in &mir.modules {
        let result = lower_mir_module_to_lir(module);
        results.push(result);
    }

    Ok(results)
}

/// Lower MIR to a single flattened LIR
///
/// This creates a fully flattened gate-level netlist where all module
/// instances are inlined. This is essential for accurate safety analysis
/// as fault injection needs to cover the entire design.
///
/// # Arguments
///
/// * `mir` - The MIR containing all modules
/// * `top_module` - Optional name of top module (defaults to last module)
///
/// # Returns
///
/// A single MirToLirResult with all instances inlined and hierarchical
/// paths preserved (e.g., "top.ch_a.counter[0]").
pub fn lower_to_flattened_lir(mir: &Mir, top_module: Option<&str>) -> Result<MirToLirResult> {
    // First, lower all modules to LIR individually
    let mut module_lirs: HashMap<ModuleId, MirToLirResult> = HashMap::new();
    let mut module_name_to_id: HashMap<String, ModuleId> = HashMap::new();

    for module in &mir.modules {
        let result = lower_mir_module_to_lir(module);
        module_name_to_id.insert(module.name.clone(), module.id);
        module_lirs.insert(module.id, result);
    }

    // Find top module
    let top_id = if let Some(name) = top_module {
        *module_name_to_id
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("Top module '{}' not found", name))?
    } else {
        // Default to last module
        mir.modules
            .last()
            .map(|m| m.id)
            .ok_or_else(|| anyhow::anyhow!("No modules in MIR"))?
    };

    // Get top module from MIR
    let top_mir_module = mir
        .modules
        .iter()
        .find(|m| m.id == top_id)
        .ok_or_else(|| anyhow::anyhow!("Top module not found in MIR"))?;

    // Start flattening from top module
    let mut flattener = LirFlattener::new(&module_lirs, &module_name_to_id, mir);
    flattener.flatten_module(top_id, "top")?;

    Ok(flattener.into_result(top_mir_module.name.clone()))
}

/// Helper struct for flattening LIR hierarchy
struct LirFlattener<'a> {
    /// Module LIRs indexed by ModuleId
    module_lirs: &'a HashMap<ModuleId, MirToLirResult>,
    /// Module name to ID mapping
    module_name_to_id: &'a HashMap<String, ModuleId>,
    /// Full MIR (for instance info)
    mir: &'a Mir,
    /// Flattened LIR being built
    flattened: Lir,
    /// Next primitive ID
    next_prim_id: u32,
    /// Next net ID
    next_net_id: u32,
    /// Mapping from (module_id, original_net_id) to flattened net_id
    net_remap: HashMap<(ModuleId, NetId), NetId>,
    /// Total stats
    stats: TransformStats,
}

impl<'a> LirFlattener<'a> {
    fn new(
        module_lirs: &'a HashMap<ModuleId, MirToLirResult>,
        module_name_to_id: &'a HashMap<String, ModuleId>,
        mir: &'a Mir,
    ) -> Self {
        Self {
            module_lirs,
            module_name_to_id,
            mir,
            flattened: Lir::new("flattened".to_string()),
            next_prim_id: 0,
            next_net_id: 0,
            net_remap: HashMap::new(),
            stats: TransformStats::default(),
        }
    }

    fn flatten_module(&mut self, module_id: ModuleId, path_prefix: &str) -> Result<()> {
        // Get the LIR for this module
        let lir_result = self
            .module_lirs
            .get(&module_id)
            .ok_or_else(|| anyhow::anyhow!("LIR not found for module {:?}", module_id))?;

        let lir = &lir_result.lir;

        // Copy nets with remapped IDs and prefixed names
        for net in &lir.nets {
            let new_id = NetId(self.next_net_id);
            self.next_net_id += 1;

            let mut new_net = net.clone();
            new_net.id = new_id;
            new_net.name = format!("{}.{}", path_prefix, net.name);

            // Track primary inputs/outputs at top level only
            if path_prefix == "top" {
                if net.is_primary_input {
                    self.flattened.inputs.push(new_id);
                }
                if net.is_primary_output {
                    self.flattened.outputs.push(new_id);
                }
            }

            self.net_remap.insert((module_id, net.id), new_id);
            self.flattened.add_net(new_net);
        }

        // Copy primitives with remapped nets and prefixed paths
        for prim in &lir.primitives {
            let new_id = PrimitiveId(self.next_prim_id);
            self.next_prim_id += 1;

            let mut new_prim = prim.clone();
            new_prim.id = new_id;
            new_prim.path = format!("{}.{}", path_prefix, prim.path.trim_start_matches("top."));

            // Remap input nets
            new_prim.inputs = prim
                .inputs
                .iter()
                .map(|net_id| {
                    self.net_remap
                        .get(&(module_id, *net_id))
                        .copied()
                        .unwrap_or(*net_id)
                })
                .collect();

            // Remap output nets
            new_prim.outputs = prim
                .outputs
                .iter()
                .map(|net_id| {
                    self.net_remap
                        .get(&(module_id, *net_id))
                        .copied()
                        .unwrap_or(*net_id)
                })
                .collect();

            // Remap clock/reset if present
            if let Some(clk) = prim.clock {
                new_prim.clock = self.net_remap.get(&(module_id, clk)).copied();
            }
            if let Some(rst) = prim.reset {
                new_prim.reset = self.net_remap.get(&(module_id, rst)).copied();
            }

            self.flattened.add_primitive(new_prim);
        }

        // Recursively process instances
        let mir_module = self
            .mir
            .modules
            .iter()
            .find(|m| m.id == module_id)
            .ok_or_else(|| anyhow::anyhow!("MIR module not found"))?;

        for instance in &mir_module.instances {
            let child_path = format!("{}.{}", path_prefix, instance.name);

            // Get the child module ID
            let child_id = instance.module;

            // Recursively flatten the child module
            self.flatten_module(child_id, &child_path)?;

            // Connect instance ports to parent nets
            self.connect_instance_ports(module_id, child_id, instance, &child_path)?;
        }

        Ok(())
    }

    fn connect_instance_ports(
        &mut self,
        parent_id: ModuleId,
        child_id: ModuleId,
        instance: &skalp_mir::mir::ModuleInstance,
        child_path: &str,
    ) -> Result<()> {
        use skalp_mir::mir::{ExpressionKind, LValue, PortDirection};

        // Get parent and child MIR modules
        let parent_mir = self
            .mir
            .modules
            .iter()
            .find(|m| m.id == parent_id)
            .ok_or_else(|| anyhow::anyhow!("Parent MIR module not found"))?;

        let child_mir = self
            .mir
            .modules
            .iter()
            .find(|m| m.id == child_id)
            .ok_or_else(|| anyhow::anyhow!("Child MIR module not found"))?;

        let child_lir = self
            .module_lirs
            .get(&child_id)
            .ok_or_else(|| anyhow::anyhow!("Child LIR not found"))?;

        let parent_lir = self
            .module_lirs
            .get(&parent_id)
            .ok_or_else(|| anyhow::anyhow!("Parent LIR not found"))?;

        // For each port connection, create buffer primitives
        for port in &child_mir.ports {
            if let Some(connection_expr) = instance.connections.get(&port.name) {
                // Try to find the parent signal/port being connected
                let parent_signal_name = match &connection_expr.kind {
                    ExpressionKind::Ref(lvalue) => match lvalue {
                        LValue::Port(port_id) => parent_mir
                            .ports
                            .iter()
                            .find(|p| p.id == *port_id)
                            .map(|p| p.name.clone()),
                        LValue::Signal(sig_id) => parent_mir
                            .signals
                            .iter()
                            .find(|s| s.id == *sig_id)
                            .map(|s| s.name.clone()),
                        _ => None,
                    },
                    _ => None, // Complex expressions not yet supported
                };

                if let Some(parent_name) = parent_signal_name {
                    // Find parent net by name (try both port name and with bit suffix)
                    let parent_net_id = parent_lir
                        .lir
                        .nets
                        .iter()
                        .find(|n| {
                            n.name == parent_name
                                || n.name.starts_with(&format!("{}[", parent_name))
                        })
                        .and_then(|n| self.net_remap.get(&(parent_id, n.id)).copied());

                    // Find child port net by name
                    let child_net_id = child_lir
                        .lir
                        .nets
                        .iter()
                        .find(|n| {
                            n.name == port.name || n.name.starts_with(&format!("{}[", port.name))
                        })
                        .and_then(|n| self.net_remap.get(&(child_id, n.id)).copied());

                    if let (Some(parent_net), Some(child_net)) = (parent_net_id, child_net_id) {
                        // Create a buffer primitive to connect parent → child (for inputs)
                        // or child → parent (for outputs)
                        let (from_net, to_net) = match port.direction {
                            PortDirection::Input | PortDirection::InOut => (parent_net, child_net),
                            PortDirection::Output => (child_net, parent_net),
                        };

                        let buf_id = PrimitiveId(self.next_prim_id);
                        self.next_prim_id += 1;

                        let buffer = Primitive {
                            id: buf_id,
                            ptype: PrimitiveType::Buf,
                            path: format!("{}._{}_buf", child_path, port.name),
                            inputs: vec![from_net],
                            outputs: vec![to_net],
                            clock: None,
                            reset: None,
                            enable: None,
                            bit_index: None,
                            safety_info: None,
                            power_domain: None,
                        };

                        self.flattened.add_primitive(buffer);
                    }
                }
            }
        }

        Ok(())
    }

    fn into_result(self, top_name: String) -> MirToLirResult {
        let mut lir = self.flattened;
        lir.name = top_name;
        lir.update_stats();

        MirToLirResult {
            lir,
            stats: self.stats,
            warnings: Vec::new(),
        }
    }
}
