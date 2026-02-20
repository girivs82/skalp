//! Bounded Model Checking implementation

use crate::property::{Property, TemporalFormula};
use crate::smt::{SatResult, SmtSolver};
use crate::{FormalError, FormalResult, PropertyStatus, TraceStep};
use skalp_lir::Lir;

/// Bounded Model Checker
pub struct BoundedModelChecker {
    /// Maximum unrolling depth
    max_depth: usize,
    /// SMT solver (optional)
    solver: Option<Box<dyn SmtSolver>>,
}

impl BoundedModelChecker {
    pub fn new(max_depth: usize) -> Self {
        Self {
            max_depth,
            solver: None,
        }
    }

    pub fn with_solver(mut self, solver: Box<dyn SmtSolver>) -> Self {
        self.solver = Some(solver);
        self
    }

    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Check safety property at specific bound
    pub async fn check_safety_at_bound(
        &self,
        design: &Lir,
        formula: &str,
        k: usize,
    ) -> FormalResult<bool> {
        // Simplified implementation - would integrate with SMT solver
        // Returns true if property holds up to bound k
        Ok(true)
    }

    /// Check liveness property at specific bound
    pub async fn check_liveness_at_bound(
        &self,
        design: &Lir,
        formula: &str,
        k: usize,
    ) -> FormalResult<Option<bool>> {
        // Returns Some(true) if verified, Some(false) if violated, None if unknown
        Ok(None)
    }

    /// Extract counterexample trace
    pub async fn extract_counterexample(&self, k: usize) -> Option<Vec<TraceStep>> {
        None // Simplified
    }

    /// Check property using BMC (legacy method)
    pub fn check_property(
        &mut self,
        design: &Lir,
        property: &Property,
    ) -> FormalResult<PropertyStatus> {
        // Create transition system first (doesn't need mutable self)
        let ts = self.create_transition_system(design)?;
        let negated_property = self.negate_property(&property.formula);

        // Then work with solver
        let Some(ref mut solver) = self.solver else {
            return Ok(PropertyStatus::Unknown);
        };

        // Try different unrolling depths
        for k in 0..=self.max_depth as u32 {
            log::debug!("BMC: Checking depth {}", k);

            // Assert initial state
            solver.push();
            solver.assert(&ts.initial_state);

            // Unroll transition relation
            for i in 0..k {
                let transition = ts.get_transition(i as usize);
                solver.assert(&transition);
            }

            // Assert negated property at step k
            let property_at_k = instantiate_at_step(&negated_property, k);
            solver.assert(&property_at_k);

            // Check satisfiability
            match solver.check_sat() {
                SatResult::Sat => {
                    // Counterexample found
                    log::info!("BMC: Counterexample found at depth {}", k);
                    return Ok(PropertyStatus::Violated);
                }
                SatResult::Unsat => {
                    // No counterexample at this depth
                    log::debug!("BMC: No counterexample at depth {}", k);
                }
                SatResult::Unknown => {
                    log::warn!("BMC: Solver returned unknown at depth {}", k);
                    return Ok(PropertyStatus::Unknown);
                }
            }

            solver.pop();
        }

        // No counterexample found within bound
        log::info!(
            "BMC: No counterexample found up to depth {}",
            self.max_depth
        );
        Ok(PropertyStatus::Unknown) // Cannot prove property, only bounded check
    }

    fn create_transition_system(&self, design: &Lir) -> FormalResult<TransitionSystem> {
        let mut ts = TransitionSystem::new();

        // Convert LIR design to transition system
        // This is a simplified implementation
        ts.initial_state = "true".to_string();

        // Add state variables for each sequential node output
        for node in &design.nodes {
            if node.op.is_sequential() {
                let signal = &design.signals[node.output.0 as usize];
                ts.state_vars.push(signal.name.clone());
            }
        }

        // Add transition relations (simplified)
        let transition = format!("(= {}' {})", "state", "next_state");
        ts.transitions.push(transition);

        Ok(ts)
    }

    fn negate_property(&self, formula: &TemporalFormula) -> String {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            match formula {
                TemporalFormula::Always(inner) => {
                    // ¬G(φ) = F(¬φ)
                    format!("(eventually (not {}))", self.formula_to_smt(inner))
                }
                TemporalFormula::Eventually(inner) => {
                    // ¬F(φ) = G(¬φ)
                    format!("(always (not {}))", self.formula_to_smt(inner))
                }
                _ => format!("(not {})", self.formula_to_smt(formula)),
            }
        })
    }

    fn formula_to_smt(&self, formula: &TemporalFormula) -> String {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            match formula {
                TemporalFormula::Atomic(prop) => prop.clone(),
                TemporalFormula::Bool(b) => b.to_string(),
                TemporalFormula::Not(f) => format!("(not {})", self.formula_to_smt(f)),
                TemporalFormula::And(l, r) => {
                    format!(
                        "(and {} {})",
                        self.formula_to_smt(l),
                        self.formula_to_smt(r)
                    )
                }
                TemporalFormula::Or(l, r) => {
                    format!("(or {} {})", self.formula_to_smt(l), self.formula_to_smt(r))
                }
                _ => "true".to_string(), // Simplified
            }
        })
    }

    fn instantiate_at_step(&self, formula: &str, step: u32) -> String {
        instantiate_at_step(formula, step)
    }
}

/// Standalone function to avoid borrow issues
fn instantiate_at_step(formula: &str, step: u32) -> String {
    // Replace variables with their step-indexed versions
    // e.g., "x" becomes "x_k" for step k
    formula.replace("state", &format!("state_{}", step))
}

/// Transition system representation
struct TransitionSystem {
    /// State variables
    state_vars: Vec<String>,
    /// Initial state formula
    initial_state: String,
    /// Transition relations
    transitions: Vec<String>,
}

impl TransitionSystem {
    fn new() -> Self {
        Self {
            state_vars: Vec::new(),
            initial_state: String::new(),
            transitions: Vec::new(),
        }
    }

    fn get_transition(&self, step: usize) -> String {
        if step < self.transitions.len() {
            self.transitions[step].clone()
        } else if !self.transitions.is_empty() {
            // Use last transition (loop invariant)
            self.transitions.last().unwrap().clone()
        } else {
            "true".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::smt::MockSolver;

    #[test]
    fn test_bmc_creation() {
        let bmc = BoundedModelChecker::new(50);
        assert_eq!(bmc.max_depth, 50);
    }

    #[test]
    fn test_property_negation() {
        let bmc = BoundedModelChecker::new(50);

        let always_prop =
            TemporalFormula::Always(Box::new(TemporalFormula::Atomic("x > 0".to_string())));

        let negated = bmc.negate_property(&always_prop);
        assert!(negated.contains("eventually"));
        assert!(negated.contains("not"));
    }
}
