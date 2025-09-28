//! Bounded Model Checking implementation

use crate::smt::{SmtSolver, SatResult};
use crate::property::{Property, TemporalFormula};
use crate::{FormalResult, FormalError, PropertyStatus};
use skalp_lir::LirDesign;

/// Bounded Model Checker
pub struct BoundedModelChecker {
    /// Maximum unrolling depth
    max_depth: u32,
    /// SMT solver
    solver: Box<dyn SmtSolver>,
}

impl BoundedModelChecker {
    pub fn new(solver: Box<dyn SmtSolver>) -> Self {
        Self {
            max_depth: 50,
            solver,
        }
    }

    pub fn with_max_depth(mut self, depth: u32) -> Self {
        self.max_depth = depth;
        self
    }

    /// Check property using BMC
    pub fn check_property(
        &mut self,
        design: &LirDesign,
        property: &Property,
    ) -> FormalResult<PropertyStatus> {
        // Create transition system
        let ts = self.create_transition_system(design)?;

        // Convert property to negated formula for counterexample search
        let negated_property = self.negate_property(&property.formula);

        // Try different unrolling depths
        for k in 0..=self.max_depth {
            log::debug!("BMC: Checking depth {}", k);

            // Assert initial state
            self.solver.push();
            self.solver.assert(&ts.initial_state);

            // Unroll transition relation
            for i in 0..k {
                let transition = ts.get_transition(i as usize);
                self.solver.assert(&transition);
            }

            // Assert negated property at step k
            let property_at_k = self.instantiate_at_step(&negated_property, k);
            self.solver.assert(&property_at_k);

            // Check satisfiability
            match self.solver.check_sat() {
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

            self.solver.pop();
        }

        // No counterexample found within bound
        log::info!("BMC: No counterexample found up to depth {}", self.max_depth);
        Ok(PropertyStatus::Unknown) // Cannot prove property, only bounded check
    }

    fn create_transition_system(&self, design: &LirDesign) -> FormalResult<TransitionSystem> {
        let mut ts = TransitionSystem::new();

        // Convert LIR design to transition system
        // This is a simplified implementation
        ts.initial_state = "true".to_string();

        for module in &design.modules {
            // Add state variables for each register/memory
            for signal in &module.signals {
                if signal.is_register {
                    ts.state_vars.push(signal.name.clone());
                }
            }

            // Add transition relations
            let transition = format!("(= {}' {})", "state", "next_state");
            ts.transitions.push(transition);
        }

        Ok(ts)
    }

    fn negate_property(&self, formula: &TemporalFormula) -> String {
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
    }

    fn formula_to_smt(&self, formula: &TemporalFormula) -> String {
        match formula {
            TemporalFormula::Atomic(prop) => prop.clone(),
            TemporalFormula::Bool(b) => b.to_string(),
            TemporalFormula::Not(f) => format!("(not {})", self.formula_to_smt(f)),
            TemporalFormula::And(l, r) => {
                format!("(and {} {})", self.formula_to_smt(l), self.formula_to_smt(r))
            }
            TemporalFormula::Or(l, r) => {
                format!("(or {} {})", self.formula_to_smt(l), self.formula_to_smt(r))
            }
            _ => "true".to_string(), // Simplified
        }
    }

    fn instantiate_at_step(&self, formula: &str, step: u32) -> String {
        // Replace variables with their step-indexed versions
        // e.g., "x" becomes "x_k" for step k
        formula.replace("state", &format!("state_{}", step))
    }
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
        let solver = Box::new(MockSolver::new());
        let bmc = BoundedModelChecker::new(solver);

        assert_eq!(bmc.max_depth, 50);
    }

    #[test]
    fn test_property_negation() {
        let solver = Box::new(MockSolver::new());
        let bmc = BoundedModelChecker::new(solver);

        let always_prop = TemporalFormula::Always(
            Box::new(TemporalFormula::Atomic("x > 0".to_string()))
        );

        let negated = bmc.negate_property(&always_prop);
        assert!(negated.contains("eventually"));
        assert!(negated.contains("not"));
    }
}