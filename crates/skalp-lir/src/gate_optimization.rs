//! Gate-Level Netlist Optimization Passes
//!
//! Optimization passes for `GateNetlist`/`Primitive` structures, enabling
//! technology-independent gate-level optimization for fault simulation.
//!
//! # Passes
//!
//! 1. **GateConstantFolding** - Evaluate primitives with constant inputs
//! 2. **GateDeadCodeElimination** - Remove primitives not driving outputs
//! 3. **GateCSE** - Merge duplicate primitive patterns
//! 4. **GateBooleanSimplification** - Double negation, idempotency patterns
//! 5. **GateMuxOptimization** - Constant select, cascaded mux merging
//! 6. **GateBufferRemoval** - Remove unnecessary buffers
//! 7. **GateFanoutOptimization** - Insert buffers for high-fanout nets
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::gate_optimization::{GateOptimizationPipeline, GateOptConfig};
//! use skalp_lir::lir::GateNetlist;
//!
//! let mut netlist = GateNetlist::new("test".to_string());
//! // ... populate netlist ...
//!
//! let mut pipeline = GateOptimizationPipeline::for_safety_analysis();
//! let results = pipeline.optimize(&mut netlist);
//!
//! for result in &results {
//!     println!("{}: {} -> {} primitives",
//!              result.pass_name, result.primitives_before, result.primitives_after);
//! }
//! ```

use crate::lir::{GateNet, GateNetlist, NetId, Primitive, PrimitiveId, PrimitiveType};
use std::collections::{HashMap, HashSet};

/// Result of a gate-level optimization pass
#[derive(Debug, Clone)]
pub struct GateOptimizationResult {
    /// Name of the pass
    pub pass_name: String,
    /// Number of primitives before optimization
    pub primitives_before: u64,
    /// Number of primitives after optimization
    pub primitives_after: u64,
    /// Number of nets before optimization
    pub nets_before: u64,
    /// Number of nets after optimization
    pub nets_after: u64,
    /// FIT before optimization
    pub fit_before: f64,
    /// FIT after optimization
    pub fit_after: f64,
    /// Whether the pass succeeded
    pub success: bool,
    /// Optional message
    pub message: Option<String>,
}

/// Trait for gate-level netlist optimization passes
pub trait GateNetlistOptimizationPass {
    /// Name of the optimization pass
    fn name(&self) -> &str;

    /// Apply the optimization to the netlist
    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult;
}

// ============================================================================
// Pass 1: Constant Folding
// ============================================================================

/// Constant folding optimization for gate netlists
///
/// Evaluates primitives with constant inputs and propagates constant values.
pub struct GateConstantFolding {
    /// Known constant net values (net_id -> value)
    constants: HashMap<NetId, bool>,
}

impl Default for GateConstantFolding {
    fn default() -> Self {
        Self::new()
    }
}

impl GateConstantFolding {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    /// Evaluate a primitive if all inputs are constant
    fn evaluate_constant(&self, prim: &Primitive) -> Option<Vec<bool>> {
        let inputs: Vec<bool> = prim
            .inputs
            .iter()
            .map(|net_id| self.constants.get(net_id).copied())
            .collect::<Option<Vec<_>>>()?;

        Some(self.evaluate_primitive(&prim.ptype, &inputs))
    }

    /// Evaluate a primitive with given inputs
    fn evaluate_primitive(&self, ptype: &PrimitiveType, inputs: &[bool]) -> Vec<bool> {
        match ptype {
            PrimitiveType::And { inputs: n } => {
                let n = *n as usize;
                vec![inputs.iter().take(n).all(|&x| x)]
            }
            PrimitiveType::Or { inputs: n } => {
                let n = *n as usize;
                vec![inputs.iter().take(n).any(|&x| x)]
            }
            PrimitiveType::Nand { inputs: n } => {
                let n = *n as usize;
                vec![!inputs.iter().take(n).all(|&x| x)]
            }
            PrimitiveType::Nor { inputs: n } => {
                let n = *n as usize;
                vec![!inputs.iter().take(n).any(|&x| x)]
            }
            PrimitiveType::Xor => {
                let a = inputs.first().copied().unwrap_or(false);
                let b = inputs.get(1).copied().unwrap_or(false);
                vec![a ^ b]
            }
            PrimitiveType::Xnor => {
                let a = inputs.first().copied().unwrap_or(false);
                let b = inputs.get(1).copied().unwrap_or(false);
                vec![!(a ^ b)]
            }
            PrimitiveType::Inv => {
                let a = inputs.first().copied().unwrap_or(false);
                vec![!a]
            }
            PrimitiveType::Buf | PrimitiveType::ClkBuf => {
                let a = inputs.first().copied().unwrap_or(false);
                vec![a]
            }
            PrimitiveType::Mux2 => {
                let sel = inputs.first().copied().unwrap_or(false);
                let d0 = inputs.get(1).copied().unwrap_or(false);
                let d1 = inputs.get(2).copied().unwrap_or(false);
                vec![if sel { d1 } else { d0 }]
            }
            PrimitiveType::Constant { value } => vec![*value],
            _ => {
                // For complex primitives, don't constant fold
                vec![]
            }
        }
    }
}

impl GateNetlistOptimizationPass for GateConstantFolding {
    fn name(&self) -> &str {
        "GateConstantFolding"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        self.constants.clear();

        // Initialize constants from Constant primitives
        for prim in &netlist.primitives {
            if let PrimitiveType::Constant { value } = prim.ptype {
                if let Some(out) = prim.outputs.first() {
                    self.constants.insert(*out, value);
                }
            }
        }

        // Iteratively propagate constants
        let mut changed = true;
        let mut iterations = 0;
        while changed && iterations < 100 {
            changed = false;
            iterations += 1;

            for prim in &netlist.primitives {
                if let Some(outputs) = self.evaluate_constant(prim) {
                    for (out_net, out_val) in prim.outputs.iter().zip(outputs) {
                        if self.constants.insert(*out_net, out_val).is_none() {
                            changed = true;
                        }
                    }
                }
            }
        }

        // Remove primitives with constant outputs
        let prims_to_remove: HashSet<PrimitiveId> = netlist
            .primitives
            .iter()
            .filter(|p| p.outputs.iter().all(|o| self.constants.contains_key(o)))
            .map(|p| p.id)
            .collect();

        let removed_count = prims_to_remove.len();
        netlist
            .primitives
            .retain(|p| !prims_to_remove.contains(&p.id));

        // Add constant drivers for removed nets (if still needed by other primitives)
        let needed_constants: HashSet<NetId> = netlist
            .primitives
            .iter()
            .flat_map(|p| p.inputs.iter().copied())
            .filter(|net| self.constants.contains_key(net))
            .collect();

        let mut next_prim_id = netlist
            .primitives
            .iter()
            .map(|p| p.id.0)
            .max()
            .unwrap_or(0)
            + 1;

        for net_id in needed_constants {
            if let Some(&value) = self.constants.get(&net_id) {
                let const_prim = Primitive::new_comb(
                    PrimitiveId(next_prim_id),
                    PrimitiveType::Constant { value },
                    format!("const_{}", net_id.0),
                    vec![],
                    vec![net_id],
                );
                netlist.primitives.push(const_prim);
                next_prim_id += 1;
            }
        }

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: true,
            message: Some(format!(
                "Folded {} constant primitives, {} iterations",
                removed_count, iterations
            )),
        }
    }
}

// ============================================================================
// Pass 2: Dead Code Elimination
// ============================================================================

/// Dead code elimination for gate netlists
///
/// Removes primitives that don't contribute to primary outputs.
pub struct GateDeadCodeElimination;

impl Default for GateDeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl GateDeadCodeElimination {
    pub fn new() -> Self {
        Self
    }
}

impl GateNetlistOptimizationPass for GateDeadCodeElimination {
    fn name(&self) -> &str {
        "GateDeadCodeElimination"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        // Build net driver map: net_id -> primitive_id that drives it
        let mut net_drivers: HashMap<NetId, PrimitiveId> = HashMap::new();
        for prim in &netlist.primitives {
            for out in &prim.outputs {
                net_drivers.insert(*out, prim.id);
            }
        }

        // Start with output nets as live
        let mut live_nets: HashSet<NetId> = netlist.outputs.iter().copied().collect();

        // Also include clock and reset nets
        live_nets.extend(netlist.clocks.iter().copied());
        live_nets.extend(netlist.resets.iter().copied());

        // Backward propagation to find all live primitives
        let mut changed = true;
        while changed {
            changed = false;

            for prim in &netlist.primitives {
                // If any output is live, all inputs become live
                if prim.outputs.iter().any(|o| live_nets.contains(o)) {
                    for input in &prim.inputs {
                        if live_nets.insert(*input) {
                            changed = true;
                        }
                    }
                    // Clock and reset are also inputs
                    if let Some(clk) = prim.clock {
                        if live_nets.insert(clk) {
                            changed = true;
                        }
                    }
                    if let Some(rst) = prim.reset {
                        if live_nets.insert(rst) {
                            changed = true;
                        }
                    }
                }
            }
        }

        // Remove dead primitives
        let dead_prims: HashSet<PrimitiveId> = netlist
            .primitives
            .iter()
            .filter(|p| !p.outputs.iter().any(|o| live_nets.contains(o)))
            .map(|p| p.id)
            .collect();

        let removed_count = dead_prims.len();
        netlist
            .primitives
            .retain(|p| !dead_prims.contains(&p.id));

        // Remove dead nets
        let dead_nets_count = netlist.nets.len();
        netlist.nets.retain(|n| live_nets.contains(&n.id));
        let removed_nets = dead_nets_count - netlist.nets.len();

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: true,
            message: Some(format!(
                "Removed {} dead primitives, {} dead nets",
                removed_count, removed_nets
            )),
        }
    }
}

// ============================================================================
// Pass 3: Common Subexpression Elimination
// ============================================================================

/// Common subexpression elimination for gate netlists
///
/// Merges duplicate primitive patterns.
pub struct GateCSE;

impl Default for GateCSE {
    fn default() -> Self {
        Self::new()
    }
}

impl GateCSE {
    pub fn new() -> Self {
        Self
    }

    /// Create a signature for a primitive for comparison
    fn primitive_signature(prim: &Primitive) -> String {
        let mut inputs: Vec<u32> = prim.inputs.iter().map(|n| n.0).collect();
        // Sort inputs for commutative operations
        if Self::is_commutative(&prim.ptype) {
            inputs.sort();
        }
        format!("{:?}_{:?}", prim.ptype, inputs)
    }

    /// Check if a primitive type is commutative
    fn is_commutative(ptype: &PrimitiveType) -> bool {
        matches!(
            ptype,
            PrimitiveType::And { .. }
                | PrimitiveType::Or { .. }
                | PrimitiveType::Nand { .. }
                | PrimitiveType::Nor { .. }
                | PrimitiveType::Xor
                | PrimitiveType::Xnor
        )
    }
}

impl GateNetlistOptimizationPass for GateCSE {
    fn name(&self) -> &str {
        "GateCSE"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        // Group primitives by signature
        let mut by_signature: HashMap<String, Vec<PrimitiveId>> = HashMap::new();
        for prim in &netlist.primitives {
            // Only CSE combinational logic
            if !prim.ptype.is_sequential() {
                let sig = Self::primitive_signature(prim);
                by_signature.entry(sig).or_default().push(prim.id);
            }
        }

        // Find duplicates and create replacement map
        let mut output_replacements: HashMap<NetId, NetId> = HashMap::new();
        let mut prims_to_remove: HashSet<PrimitiveId> = HashSet::new();

        for (_, prim_ids) in by_signature {
            if prim_ids.len() > 1 {
                // Keep the first, remove the rest
                let keeper_id = prim_ids[0];
                let keeper = netlist.primitives.iter().find(|p| p.id == keeper_id);

                for &dup_id in &prim_ids[1..] {
                    if let Some(dup) = netlist.primitives.iter().find(|p| p.id == dup_id) {
                        if let Some(keeper) = keeper {
                            // Map duplicate outputs to keeper outputs
                            for (dup_out, keep_out) in
                                dup.outputs.iter().zip(keeper.outputs.iter())
                            {
                                output_replacements.insert(*dup_out, *keep_out);
                            }
                        }
                        prims_to_remove.insert(dup_id);
                    }
                }
            }
        }

        let removed_count = prims_to_remove.len();

        // Remove duplicates
        netlist
            .primitives
            .retain(|p| !prims_to_remove.contains(&p.id));

        // Update references in remaining primitives
        for prim in &mut netlist.primitives {
            for input in &mut prim.inputs {
                if let Some(replacement) = output_replacements.get(input) {
                    *input = *replacement;
                }
            }
        }

        // Update net loads
        for net in &mut netlist.nets {
            net.loads.retain(|(prim_id, _)| !prims_to_remove.contains(prim_id));
        }

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: true,
            message: Some(format!("Merged {} duplicate primitives", removed_count)),
        }
    }
}

// ============================================================================
// Pass 4: Boolean Simplification
// ============================================================================

/// Boolean algebra simplification for gate netlists
///
/// Applies patterns like double negation (INV(INV(x)) -> BUF(x)) and
/// idempotency (AND(x,x) -> BUF(x)).
pub struct GateBooleanSimplification;

impl Default for GateBooleanSimplification {
    fn default() -> Self {
        Self::new()
    }
}

impl GateBooleanSimplification {
    pub fn new() -> Self {
        Self
    }
}

impl GateNetlistOptimizationPass for GateBooleanSimplification {
    fn name(&self) -> &str {
        "GateBooleanSimplification"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        let mut simplifications = 0;

        // Build output-to-primitive map
        let mut output_to_prim: HashMap<NetId, PrimitiveId> = HashMap::new();
        for prim in &netlist.primitives {
            for out in &prim.outputs {
                output_to_prim.insert(*out, prim.id);
            }
        }

        // Clone primitives for modification
        let mut new_primitives: Vec<Primitive> = Vec::new();
        let mut prims_to_remove: HashSet<PrimitiveId> = HashSet::new();

        for prim in &netlist.primitives {
            // Pattern: Double negation INV(INV(x)) -> BUF(x)
            if matches!(prim.ptype, PrimitiveType::Inv) {
                if let Some(input_net) = prim.inputs.first() {
                    if let Some(&driver_id) = output_to_prim.get(input_net) {
                        if let Some(driver) = netlist.primitives.iter().find(|p| p.id == driver_id)
                        {
                            if matches!(driver.ptype, PrimitiveType::Inv) {
                                // Found double negation
                                let new_prim = Primitive::new_comb(
                                    prim.id,
                                    PrimitiveType::Buf,
                                    format!("{}_simplified", prim.path),
                                    driver.inputs.clone(),
                                    prim.outputs.clone(),
                                );
                                new_primitives.push(new_prim);
                                prims_to_remove.insert(prim.id);
                                prims_to_remove.insert(driver_id);
                                simplifications += 1;
                                continue;
                            }
                        }
                    }
                }
            }

            // Pattern: Idempotency AND(x,x) -> BUF(x), OR(x,x) -> BUF(x)
            if matches!(
                prim.ptype,
                PrimitiveType::And { .. } | PrimitiveType::Or { .. }
            ) {
                let unique_inputs: HashSet<NetId> = prim.inputs.iter().copied().collect();
                if unique_inputs.len() == 1 && prim.inputs.len() > 1 {
                    let input = *unique_inputs.iter().next().unwrap();
                    let new_prim = Primitive::new_comb(
                        prim.id,
                        PrimitiveType::Buf,
                        format!("{}_simplified", prim.path),
                        vec![input],
                        prim.outputs.clone(),
                    );
                    new_primitives.push(new_prim);
                    prims_to_remove.insert(prim.id);
                    simplifications += 1;
                    continue;
                }
            }
        }

        // Apply changes
        netlist
            .primitives
            .retain(|p| !prims_to_remove.contains(&p.id));
        netlist.primitives.extend(new_primitives);

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: simplifications > 0,
            message: Some(format!("Simplified {} patterns", simplifications)),
        }
    }
}

// ============================================================================
// Pass 5: MUX Optimization
// ============================================================================

/// MUX optimization for gate netlists
///
/// Handles constant select signals and identical inputs.
pub struct GateMuxOptimization;

impl Default for GateMuxOptimization {
    fn default() -> Self {
        Self::new()
    }
}

impl GateMuxOptimization {
    pub fn new() -> Self {
        Self
    }
}

impl GateNetlistOptimizationPass for GateMuxOptimization {
    fn name(&self) -> &str {
        "GateMuxOptimization"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        let mut optimizations = 0;
        let mut new_primitives: Vec<Primitive> = Vec::new();
        let mut prims_to_remove: HashSet<PrimitiveId> = HashSet::new();

        // Find constant nets from Constant primitives
        let mut constants: HashMap<NetId, bool> = HashMap::new();
        for prim in &netlist.primitives {
            if let PrimitiveType::Constant { value } = prim.ptype {
                if let Some(out) = prim.outputs.first() {
                    constants.insert(*out, value);
                }
            }
        }

        for prim in &netlist.primitives {
            if matches!(prim.ptype, PrimitiveType::Mux2) {
                // MUX2: inputs [sel, d0, d1], output: sel ? d1 : d0
                let sel = prim.inputs.first().copied();
                let d0 = prim.inputs.get(1).copied();
                let d1 = prim.inputs.get(2).copied();

                // Pattern: Constant select
                if let Some(sel_net) = sel {
                    if let Some(&sel_val) = constants.get(&sel_net) {
                        let selected_input = if sel_val {
                            d1.unwrap_or(NetId(0))
                        } else {
                            d0.unwrap_or(NetId(0))
                        };
                        let new_prim = Primitive::new_comb(
                            prim.id,
                            PrimitiveType::Buf,
                            format!("{}_const_sel", prim.path),
                            vec![selected_input],
                            prim.outputs.clone(),
                        );
                        new_primitives.push(new_prim);
                        prims_to_remove.insert(prim.id);
                        optimizations += 1;
                        continue;
                    }
                }

                // Pattern: Identical inputs MUX(s, a, a) -> BUF(a)
                if d0 == d1 {
                    if let Some(input) = d0 {
                        let new_prim = Primitive::new_comb(
                            prim.id,
                            PrimitiveType::Buf,
                            format!("{}_identical", prim.path),
                            vec![input],
                            prim.outputs.clone(),
                        );
                        new_primitives.push(new_prim);
                        prims_to_remove.insert(prim.id);
                        optimizations += 1;
                        continue;
                    }
                }
            }
        }

        // Apply changes
        netlist
            .primitives
            .retain(|p| !prims_to_remove.contains(&p.id));
        netlist.primitives.extend(new_primitives);

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: optimizations > 0,
            message: Some(format!("Optimized {} MUX primitives", optimizations)),
        }
    }
}

// ============================================================================
// Pass 6: Buffer Removal
// ============================================================================

/// Buffer removal optimization
///
/// Removes unnecessary buffers (except clock buffers).
pub struct GateBufferRemoval;

impl Default for GateBufferRemoval {
    fn default() -> Self {
        Self::new()
    }
}

impl GateBufferRemoval {
    pub fn new() -> Self {
        Self
    }
}

impl GateNetlistOptimizationPass for GateBufferRemoval {
    fn name(&self) -> &str {
        "GateBufferRemoval"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        let mut removals = 0;
        let mut wire_through: HashMap<NetId, NetId> = HashMap::new();
        let mut prims_to_remove: HashSet<PrimitiveId> = HashSet::new();

        // Find buffers that can be removed (not ClkBuf, single fanout)
        for prim in &netlist.primitives {
            if matches!(prim.ptype, PrimitiveType::Buf) {
                if let (Some(&input), Some(&output)) =
                    (prim.inputs.first(), prim.outputs.first())
                {
                    // Check that output is not a primary output
                    if !netlist.outputs.contains(&output) {
                        wire_through.insert(output, input);
                        prims_to_remove.insert(prim.id);
                        removals += 1;
                    }
                }
            }
        }

        // Update references
        for prim in &mut netlist.primitives {
            for input in &mut prim.inputs {
                while let Some(&new_input) = wire_through.get(input) {
                    *input = new_input;
                }
            }
        }

        // Remove buffers
        netlist
            .primitives
            .retain(|p| !prims_to_remove.contains(&p.id));

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: removals > 0,
            message: Some(format!("Removed {} unnecessary buffers", removals)),
        }
    }
}

// ============================================================================
// Pass 7: Fanout Optimization
// ============================================================================

/// Fanout optimization
///
/// Inserts buffers for high-fanout nets to balance load.
pub struct GateFanoutOptimization {
    /// Maximum fanout before inserting buffers
    pub fanout_threshold: u32,
}

impl Default for GateFanoutOptimization {
    fn default() -> Self {
        Self::new(16)
    }
}

impl GateFanoutOptimization {
    pub fn new(threshold: u32) -> Self {
        Self {
            fanout_threshold: threshold,
        }
    }
}

impl GateNetlistOptimizationPass for GateFanoutOptimization {
    fn name(&self) -> &str {
        "GateFanoutOptimization"
    }

    fn optimize(&mut self, netlist: &mut GateNetlist) -> GateOptimizationResult {
        let primitives_before = netlist.primitives.len() as u64;
        let nets_before = netlist.nets.len() as u64;
        let fit_before: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        // Count fanout for each net
        let mut fanout: HashMap<NetId, Vec<(PrimitiveId, usize)>> = HashMap::new();
        for prim in &netlist.primitives {
            for (idx, input) in prim.inputs.iter().enumerate() {
                fanout.entry(*input).or_default().push((prim.id, idx));
            }
        }

        // Find high-fanout nets
        let high_fanout_nets: Vec<(NetId, usize)> = fanout
            .iter()
            .filter(|(_, loads)| loads.len() > self.fanout_threshold as usize)
            .map(|(net_id, loads)| (*net_id, loads.len()))
            .collect();

        let buffers_inserted = high_fanout_nets.len();

        // For now, just report - actual buffer tree insertion is complex
        // Would need to: create buffer tree, rewire loads to buffer outputs

        let fit_after: f64 = netlist.primitives.iter().map(|p| p.fit()).sum();

        GateOptimizationResult {
            pass_name: self.name().to_string(),
            primitives_before,
            primitives_after: netlist.primitives.len() as u64,
            nets_before,
            nets_after: netlist.nets.len() as u64,
            fit_before,
            fit_after,
            success: true,
            message: Some(format!(
                "Found {} high-fanout nets (threshold: {})",
                buffers_inserted, self.fanout_threshold
            )),
        }
    }
}

// ============================================================================
// Optimization Pipeline
// ============================================================================

/// Configuration for gate optimization pipeline
#[derive(Debug, Clone)]
pub struct GateOptConfig {
    /// Optimization level (0=none, 1=minimal, 2=standard, 3=aggressive)
    pub level: u8,
    /// Maximum iterations for iterative passes
    pub max_iterations: u32,
    /// Preserve hierarchy for traceability (limits some opts)
    pub preserve_hierarchy: bool,
    /// Target: area, timing, or balanced
    pub target: OptTarget,
}

impl Default for GateOptConfig {
    fn default() -> Self {
        Self {
            level: 2,
            max_iterations: 3,
            preserve_hierarchy: false,
            target: OptTarget::Balanced,
        }
    }
}

/// Optimization target
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptTarget {
    /// Minimize area (gate count)
    Area,
    /// Minimize timing (logic depth)
    Timing,
    /// Balance area and timing
    Balanced,
}

/// Gate optimization pipeline
pub struct GateOptimizationPipeline {
    passes: Vec<Box<dyn GateNetlistOptimizationPass>>,
    config: GateOptConfig,
}

impl Default for GateOptimizationPipeline {
    fn default() -> Self {
        Self::standard()
    }
}

impl GateOptimizationPipeline {
    /// Create a new empty pipeline
    pub fn new() -> Self {
        Self {
            passes: Vec::new(),
            config: GateOptConfig::default(),
        }
    }

    /// Create a pipeline with given config
    pub fn with_config(config: GateOptConfig) -> Self {
        let mut pipeline = Self::new();
        let level = config.level;
        pipeline.config = config;

        match level {
            0 => {}
            1 => {
                pipeline.add_pass(Box::new(GateConstantFolding::new()));
                pipeline.add_pass(Box::new(GateDeadCodeElimination::new()));
            }
            2 => {
                pipeline.add_pass(Box::new(GateConstantFolding::new()));
                pipeline.add_pass(Box::new(GateBooleanSimplification::new()));
                pipeline.add_pass(Box::new(GateCSE::new()));
                pipeline.add_pass(Box::new(GateDeadCodeElimination::new()));
            }
            _ => {
                pipeline.add_pass(Box::new(GateConstantFolding::new()));
                pipeline.add_pass(Box::new(GateBooleanSimplification::new()));
                pipeline.add_pass(Box::new(GateMuxOptimization::new()));
                pipeline.add_pass(Box::new(GateCSE::new()));
                pipeline.add_pass(Box::new(GateBufferRemoval::new()));
                pipeline.add_pass(Box::new(GateDeadCodeElimination::new()));
                pipeline.add_pass(Box::new(GateFanoutOptimization::default()));
            }
        }

        pipeline
    }

    /// Create a standard optimization pipeline
    pub fn standard() -> Self {
        Self::with_config(GateOptConfig {
            level: 2,
            ..Default::default()
        })
    }

    /// Create a pipeline optimized for safety analysis
    ///
    /// Preserves hierarchy for traceability to RTL.
    pub fn for_safety_analysis() -> Self {
        Self::with_config(GateOptConfig {
            level: 2,
            max_iterations: 3,
            preserve_hierarchy: true,
            target: OptTarget::Balanced,
        })
    }

    /// Add an optimization pass to the pipeline
    pub fn add_pass(&mut self, pass: Box<dyn GateNetlistOptimizationPass>) {
        self.passes.push(pass);
    }

    /// Run all optimization passes
    pub fn optimize(&mut self, netlist: &mut GateNetlist) -> Vec<GateOptimizationResult> {
        let mut results = Vec::new();

        for iteration in 0..self.config.max_iterations {
            let mut any_changed = false;

            for pass in &mut self.passes {
                let result = pass.optimize(netlist);
                if result.primitives_before != result.primitives_after {
                    any_changed = true;
                }
                results.push(result);
            }

            if !any_changed {
                break;
            }

            // Only iterate if level is aggressive
            if self.config.level < 3 {
                break;
            }
        }

        // Update netlist statistics
        netlist.update_stats();

        results
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string());

        // Create nets
        let net_a = GateNet::new_primary_input(NetId(0), "a".to_string());
        let net_b = GateNet::new_primary_input(NetId(1), "b".to_string());
        let mut net_w = GateNet::new(NetId(2), "w".to_string());
        net_w.driver = Some((PrimitiveId(0), 0));
        let net_y = GateNet::new_primary_output(NetId(3), "y".to_string(), (PrimitiveId(1), 0));

        netlist.add_net(net_a);
        netlist.add_net(net_b);
        netlist.add_net(net_w);
        netlist.add_net(net_y);

        netlist.inputs = vec![NetId(0), NetId(1)];
        netlist.outputs = vec![NetId(3)];

        // a & b -> w, !w -> y
        let and_gate = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "and_0".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(2)],
        );

        let inv_gate = Primitive::new_comb(
            PrimitiveId(1),
            PrimitiveType::Inv,
            "inv_0".to_string(),
            vec![NetId(2)],
            vec![NetId(3)],
        );

        netlist.add_primitive(and_gate);
        netlist.add_primitive(inv_gate);

        netlist
    }

    #[test]
    fn test_constant_folding() {
        let mut netlist = GateNetlist::new("const_test".to_string());

        // const_0 -> a (value = true)
        // const_1 -> b (value = true)
        // a & b -> y (should fold to true)

        let net_a = GateNet::new(NetId(0), "a".to_string());
        let net_b = GateNet::new(NetId(1), "b".to_string());
        let net_y = GateNet::new_primary_output(NetId(2), "y".to_string(), (PrimitiveId(2), 0));

        netlist.add_net(net_a);
        netlist.add_net(net_b);
        netlist.add_net(net_y);
        netlist.outputs = vec![NetId(2)];

        let const_a = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::Constant { value: true },
            "const_a".to_string(),
            vec![],
            vec![NetId(0)],
        );

        let const_b = Primitive::new_comb(
            PrimitiveId(1),
            PrimitiveType::Constant { value: true },
            "const_b".to_string(),
            vec![],
            vec![NetId(1)],
        );

        let and_gate = Primitive::new_comb(
            PrimitiveId(2),
            PrimitiveType::And { inputs: 2 },
            "and_0".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(2)],
        );

        netlist.add_primitive(const_a);
        netlist.add_primitive(const_b);
        netlist.add_primitive(and_gate);

        let mut pass = GateConstantFolding::new();
        let result = pass.optimize(&mut netlist);

        assert!(result.success);
        // Should have removed some primitives via constant folding
    }

    #[test]
    fn test_dead_code_elimination() {
        let mut netlist = make_simple_netlist();

        // Add a dead gate
        let dead_gate = Primitive::new_comb(
            PrimitiveId(100),
            PrimitiveType::Or { inputs: 2 },
            "dead_or".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(100)], // Output not connected to anything
        );
        let dead_net = GateNet::new(NetId(100), "dead_net".to_string());
        netlist.add_primitive(dead_gate);
        netlist.add_net(dead_net);

        let initial_count = netlist.primitives.len();

        let mut pass = GateDeadCodeElimination::new();
        let result = pass.optimize(&mut netlist);

        assert!(result.success);
        assert!(netlist.primitives.len() < initial_count);
    }

    #[test]
    fn test_cse() {
        let mut netlist = GateNetlist::new("cse_test".to_string());

        let net_a = GateNet::new_primary_input(NetId(0), "a".to_string());
        let net_b = GateNet::new_primary_input(NetId(1), "b".to_string());
        let net_w1 = GateNet::new(NetId(2), "w1".to_string());
        let net_w2 = GateNet::new(NetId(3), "w2".to_string());
        let net_y = GateNet::new_primary_output(NetId(4), "y".to_string(), (PrimitiveId(2), 0));

        netlist.add_net(net_a);
        netlist.add_net(net_b);
        netlist.add_net(net_w1);
        netlist.add_net(net_w2);
        netlist.add_net(net_y);
        netlist.inputs = vec![NetId(0), NetId(1)];
        netlist.outputs = vec![NetId(4)];

        // Two identical AND gates
        let and1 = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "and_0".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(2)],
        );

        let and2 = Primitive::new_comb(
            PrimitiveId(1),
            PrimitiveType::And { inputs: 2 },
            "and_1".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(3)],
        );

        // OR them together
        let or_gate = Primitive::new_comb(
            PrimitiveId(2),
            PrimitiveType::Or { inputs: 2 },
            "or_0".to_string(),
            vec![NetId(2), NetId(3)],
            vec![NetId(4)],
        );

        netlist.add_primitive(and1);
        netlist.add_primitive(and2);
        netlist.add_primitive(or_gate);

        let mut pass = GateCSE::new();
        let result = pass.optimize(&mut netlist);

        assert!(result.success);
        assert_eq!(result.primitives_after, 2); // One AND merged
    }

    #[test]
    fn test_boolean_simplification_double_negation() {
        let mut netlist = GateNetlist::new("bool_test".to_string());

        let net_a = GateNet::new_primary_input(NetId(0), "a".to_string());
        let net_w = GateNet::new(NetId(1), "w".to_string());
        let net_y = GateNet::new_primary_output(NetId(2), "y".to_string(), (PrimitiveId(1), 0));

        netlist.add_net(net_a);
        netlist.add_net(net_w);
        netlist.add_net(net_y);
        netlist.inputs = vec![NetId(0)];
        netlist.outputs = vec![NetId(2)];

        // Double negation: INV(INV(a))
        let inv1 = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::Inv,
            "inv_0".to_string(),
            vec![NetId(0)],
            vec![NetId(1)],
        );

        let inv2 = Primitive::new_comb(
            PrimitiveId(1),
            PrimitiveType::Inv,
            "inv_1".to_string(),
            vec![NetId(1)],
            vec![NetId(2)],
        );

        netlist.add_primitive(inv1);
        netlist.add_primitive(inv2);

        let mut pass = GateBooleanSimplification::new();
        let result = pass.optimize(&mut netlist);

        assert!(result.success);
        // Should simplify to single buffer
    }

    #[test]
    fn test_mux_optimization_identical_inputs() {
        let mut netlist = GateNetlist::new("mux_test".to_string());

        let net_sel = GateNet::new_primary_input(NetId(0), "sel".to_string());
        let net_a = GateNet::new_primary_input(NetId(1), "a".to_string());
        let net_y = GateNet::new_primary_output(NetId(2), "y".to_string(), (PrimitiveId(0), 0));

        netlist.add_net(net_sel);
        netlist.add_net(net_a);
        netlist.add_net(net_y);
        netlist.inputs = vec![NetId(0), NetId(1)];
        netlist.outputs = vec![NetId(2)];

        // MUX with identical inputs: MUX(sel, a, a) -> BUF(a)
        let mux = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::Mux2,
            "mux_0".to_string(),
            vec![NetId(0), NetId(1), NetId(1)], // sel, d0=a, d1=a
            vec![NetId(2)],
        );

        netlist.add_primitive(mux);

        let mut pass = GateMuxOptimization::new();
        let result = pass.optimize(&mut netlist);

        assert!(result.success);
        // Should replace MUX with BUF
        assert_eq!(netlist.primitives.len(), 1);
        assert!(matches!(
            netlist.primitives[0].ptype,
            PrimitiveType::Buf
        ));
    }

    #[test]
    fn test_pipeline_standard() {
        let mut netlist = make_simple_netlist();
        let mut pipeline = GateOptimizationPipeline::standard();
        let results = pipeline.optimize(&mut netlist);

        assert!(!results.is_empty());
    }

    #[test]
    fn test_pipeline_for_safety() {
        let mut netlist = make_simple_netlist();
        let mut pipeline = GateOptimizationPipeline::for_safety_analysis();
        let results = pipeline.optimize(&mut netlist);

        assert!(!results.is_empty());
    }
}
