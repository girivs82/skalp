//! LIR optimization passes
//!
//! Various optimization techniques for gate-level netlists

use crate::lir::{Gate, GateType, Lir};
use std::collections::{HashMap, HashSet};

/// Optimization pass trait
pub trait OptimizationPass {
    /// Name of the optimization pass
    fn name(&self) -> &str;

    /// Apply the optimization to the LIR
    fn optimize(&mut self, lir: &mut Lir) -> OptimizationResult;
}

/// Result of an optimization pass
#[derive(Debug, Clone)]
pub struct OptimizationResult {
    /// Name of the pass
    pub pass_name: String,
    /// Number of gates before optimization
    pub gates_before: usize,
    /// Number of gates after optimization
    pub gates_after: usize,
    /// Number of nets before optimization
    pub nets_before: usize,
    /// Number of nets after optimization
    pub nets_after: usize,
    /// Optimization successful
    pub success: bool,
    /// Optional message
    pub message: Option<String>,
}

/// Constant folding optimization
pub struct ConstantFolding;

impl OptimizationPass for ConstantFolding {
    fn name(&self) -> &str {
        "Constant Folding"
    }

    fn optimize(&mut self, lir: &mut Lir) -> OptimizationResult {
        let gates_before = lir.gates.len();
        let nets_before = lir.nets.len();

        // Find gates with constant inputs
        let mut gates_to_remove = Vec::new();
        let mut constant_nets = HashMap::new();

        for gate in &lir.gates {
            if let Some(const_output) = self.evaluate_constant_gate(gate, &constant_nets) {
                gates_to_remove.push(gate.id.clone());
                for output in &gate.outputs {
                    constant_nets.insert(output.clone(), const_output);
                }
            }
        }

        // Remove constant gates
        lir.gates.retain(|g| !gates_to_remove.contains(&g.id));

        // Replace references to constant nets
        for gate in &mut lir.gates {
            for input in &mut gate.inputs {
                if let Some(const_val) = constant_nets.get(input) {
                    // Replace with tie-high or tie-low
                    *input = if *const_val {
                        "tie_high".to_string()
                    } else {
                        "tie_low".to_string()
                    };
                }
            }
        }

        OptimizationResult {
            pass_name: self.name().to_string(),
            gates_before,
            gates_after: lir.gates.len(),
            nets_before,
            nets_after: lir.nets.len(),
            success: true,
            message: Some(format!("Removed {} constant gates", gates_to_remove.len())),
        }
    }
}

impl ConstantFolding {
    /// Evaluate if a gate has constant output
    fn evaluate_constant_gate(
        &self,
        gate: &Gate,
        constants: &HashMap<String, bool>,
    ) -> Option<bool> {
        match gate.gate_type {
            GateType::And => {
                let result = true;
                for input in &gate.inputs {
                    if input == "tie_low" || constants.get(input) == Some(&false) {
                        return Some(false); // AND with 0 is always 0
                    }
                    if input != "tie_high" && constants.get(input) != Some(&true) {
                        return None; // Non-constant input
                    }
                }
                Some(result)
            }
            GateType::Or => {
                let result = false;
                for input in &gate.inputs {
                    if input == "tie_high" || constants.get(input) == Some(&true) {
                        return Some(true); // OR with 1 is always 1
                    }
                    if input != "tie_low" && constants.get(input) != Some(&false) {
                        return None; // Non-constant input
                    }
                }
                Some(result)
            }
            GateType::Not => {
                if let Some(input) = gate.inputs.first() {
                    if input == "tie_high" || constants.get(input) == Some(&true) {
                        Some(false)
                    } else if input == "tie_low" || constants.get(input) == Some(&false) {
                        Some(true)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Dead code elimination
pub struct DeadCodeElimination;

impl OptimizationPass for DeadCodeElimination {
    fn name(&self) -> &str {
        "Dead Code Elimination"
    }

    fn optimize(&mut self, lir: &mut Lir) -> OptimizationResult {
        let gates_before = lir.gates.len();
        let nets_before = lir.nets.len();

        // Find output nets (nets that drive outputs or are marked as outputs)
        let mut live_nets = HashSet::new();

        // Mark all output ports as live
        for net in &lir.nets {
            // Use proper metadata to identify output nets
            if net.is_output || net.id.starts_with("out_") || net.id.starts_with("output") {
                live_nets.insert(net.id.clone());
            }
        }

        // Backward propagation to find all live gates
        let mut changed = true;
        while changed {
            changed = false;
            for gate in &lir.gates {
                // If any output is live, mark all inputs as live
                if gate.outputs.iter().any(|o| live_nets.contains(o)) {
                    for input in &gate.inputs {
                        if live_nets.insert(input.clone()) {
                            changed = true;
                        }
                    }
                }
            }
        }

        // Remove dead gates
        let dead_gates: Vec<_> = lir
            .gates
            .iter()
            .filter(|g| !g.outputs.iter().any(|o| live_nets.contains(o)))
            .map(|g| g.id.clone())
            .collect();

        lir.gates.retain(|g| !dead_gates.contains(&g.id));

        // Remove dead nets
        lir.nets.retain(|n| live_nets.contains(&n.id));

        OptimizationResult {
            pass_name: self.name().to_string(),
            gates_before,
            gates_after: lir.gates.len(),
            nets_before,
            nets_after: lir.nets.len(),
            success: true,
            message: Some(format!("Removed {} dead gates", dead_gates.len())),
        }
    }
}

/// Common subexpression elimination
pub struct CommonSubexpressionElimination;

impl OptimizationPass for CommonSubexpressionElimination {
    fn name(&self) -> &str {
        "Common Subexpression Elimination"
    }

    fn optimize(&mut self, lir: &mut Lir) -> OptimizationResult {
        let gates_before = lir.gates.len();
        let nets_before = lir.nets.len();

        // Create signatures for gates
        let mut gate_signatures: HashMap<String, Vec<String>> = HashMap::new();

        for gate in &lir.gates {
            let signature = self.gate_signature(gate);
            gate_signatures
                .entry(signature)
                .or_insert_with(Vec::new)
                .push(gate.id.clone());
        }

        // Find duplicate gates
        let mut gate_replacements = HashMap::new();
        let mut gates_to_remove = Vec::new();

        for (signature, gate_ids) in gate_signatures {
            if gate_ids.len() > 1 {
                // Keep the first gate, remove the rest
                let keeper = &gate_ids[0];
                for duplicate in &gate_ids[1..] {
                    gates_to_remove.push(duplicate.clone());

                    // Find the outputs of the duplicate gate
                    if let Some(dup_gate) = lir.gates.iter().find(|g| g.id == *duplicate) {
                        if let Some(keeper_gate) = lir.gates.iter().find(|g| g.id == *keeper) {
                            // Map duplicate outputs to keeper outputs
                            for (dup_out, keep_out) in
                                dup_gate.outputs.iter().zip(keeper_gate.outputs.iter())
                            {
                                gate_replacements.insert(dup_out.clone(), keep_out.clone());
                            }
                        }
                    }
                }
            }
        }

        // Remove duplicate gates
        lir.gates.retain(|g| !gates_to_remove.contains(&g.id));

        // Update references in remaining gates
        for gate in &mut lir.gates {
            for input in &mut gate.inputs {
                if let Some(replacement) = gate_replacements.get(input) {
                    *input = replacement.clone();
                }
            }
        }

        OptimizationResult {
            pass_name: self.name().to_string(),
            gates_before,
            gates_after: lir.gates.len(),
            nets_before,
            nets_after: lir.nets.len(),
            success: true,
            message: Some(format!(
                "Eliminated {} duplicate gates",
                gates_to_remove.len()
            )),
        }
    }
}

impl CommonSubexpressionElimination {
    /// Create a signature for a gate for comparison
    fn gate_signature(&self, gate: &Gate) -> String {
        let mut inputs = gate.inputs.clone();
        inputs.sort(); // Sort for commutative operations

        format!("{:?}_{}", gate.gate_type, inputs.join("_"))
    }
}

/// Boolean algebra simplification
pub struct BooleanSimplification;

impl OptimizationPass for BooleanSimplification {
    fn name(&self) -> &str {
        "Boolean Simplification"
    }

    fn optimize(&mut self, lir: &mut Lir) -> OptimizationResult {
        let gates_before = lir.gates.len();
        let nets_before = lir.nets.len();

        let mut optimized = false;
        let mut gates_to_add = Vec::new();
        let mut gates_to_remove = Vec::new();

        for gate in &lir.gates {
            // Look for patterns like double negation
            if gate.gate_type == GateType::Not {
                if let Some(input_gate) = lir
                    .gates
                    .iter()
                    .find(|g| g.outputs.contains(&gate.inputs[0]) && g.gate_type == GateType::Not)
                {
                    // Double negation found - replace with buffer
                    let new_gate = Gate {
                        id: format!("{}_simplified", gate.id),
                        gate_type: GateType::Buffer,
                        inputs: input_gate.inputs.clone(),
                        outputs: gate.outputs.clone(),
                    };
                    gates_to_add.push(new_gate);
                    gates_to_remove.push(gate.id.clone());
                    gates_to_remove.push(input_gate.id.clone());
                    optimized = true;
                }
            }

            // Look for A & A = A, A | A = A
            if matches!(gate.gate_type, GateType::And | GateType::Or) {
                let unique_inputs: HashSet<_> = gate.inputs.iter().collect();
                if unique_inputs.len() == 1 && gate.inputs.len() > 1 {
                    // All inputs are the same
                    let new_gate = Gate {
                        id: format!("{}_simplified", gate.id),
                        gate_type: GateType::Buffer,
                        inputs: vec![gate.inputs[0].clone()],
                        outputs: gate.outputs.clone(),
                    };
                    gates_to_add.push(new_gate);
                    gates_to_remove.push(gate.id.clone());
                    optimized = true;
                }
            }
        }

        // Apply changes
        lir.gates.retain(|g| !gates_to_remove.contains(&g.id));
        lir.gates.extend(gates_to_add);

        OptimizationResult {
            pass_name: self.name().to_string(),
            gates_before,
            gates_after: lir.gates.len(),
            nets_before,
            nets_after: lir.nets.len(),
            success: optimized,
            message: if optimized {
                Some(format!("Simplified {} gates", gates_to_remove.len()))
            } else {
                Some("No simplifications found".to_string())
            },
        }
    }
}

/// Optimization pipeline that runs multiple passes
pub struct OptimizationPipeline {
    passes: Vec<Box<dyn OptimizationPass>>,
}

impl OptimizationPipeline {
    /// Create a new optimization pipeline
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add an optimization pass to the pipeline
    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(pass);
    }

    /// Create a standard optimization pipeline
    pub fn standard() -> Self {
        let mut pipeline = Self::new();
        pipeline.add_pass(Box::new(ConstantFolding));
        pipeline.add_pass(Box::new(BooleanSimplification));
        pipeline.add_pass(Box::new(CommonSubexpressionElimination));
        pipeline.add_pass(Box::new(DeadCodeElimination));
        pipeline
    }

    /// Run all optimization passes
    pub fn optimize(&mut self, lir: &mut Lir) -> Vec<OptimizationResult> {
        let mut results = Vec::new();

        for pass in &mut self.passes {
            let result = pass.optimize(lir);
            println!(
                "Optimization pass '{}': {} gates -> {} gates",
                result.pass_name, result.gates_before, result.gates_after
            );
            if let Some(msg) = &result.message {
                println!("  {}", msg);
            }
            results.push(result);
        }

        results
    }
}
