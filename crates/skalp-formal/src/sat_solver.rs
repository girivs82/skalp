//! SAT Solver wrapper using CaDiCaL
//!
//! Provides a varisat-compatible API on top of CaDiCaL for minimal code changes.

use std::collections::HashSet;

/// A SAT variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(pub usize);

impl Var {
    pub fn from_index(idx: usize) -> Self {
        Var(idx)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// A SAT literal (variable with polarity)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lit {
    var: Var,
    negated: bool,
}

impl Lit {
    pub fn positive(var: Var) -> Self {
        Lit { var, negated: false }
    }

    pub fn negative(var: Var) -> Self {
        Lit { var, negated: true }
    }

    pub fn var(&self) -> Var {
        self.var
    }

    pub fn is_positive(&self) -> bool {
        !self.negated
    }

    pub fn is_negative(&self) -> bool {
        self.negated
    }

    /// Convert to CaDiCaL literal format (1-indexed, negative for negation)
    fn to_cadical(&self) -> i32 {
        let var_num = (self.var.0 + 1) as i32; // CaDiCaL is 1-indexed
        if self.negated {
            -var_num
        } else {
            var_num
        }
    }
}

impl std::ops::Not for Lit {
    type Output = Lit;

    fn not(self) -> Self::Output {
        Lit {
            var: self.var,
            negated: !self.negated,
        }
    }
}

/// CNF formula builder (varisat-compatible API)
pub struct CnfFormula {
    clauses: Vec<Vec<Lit>>,
    max_var: usize,
}

impl CnfFormula {
    pub fn new() -> Self {
        CnfFormula {
            clauses: Vec::new(),
            max_var: 0,
        }
    }

    pub fn add_clause(&mut self, clause: &[Lit]) {
        for lit in clause {
            self.max_var = self.max_var.max(lit.var.0);
        }
        self.clauses.push(clause.to_vec());
    }

    pub fn clauses(&self) -> &[Vec<Lit>] {
        &self.clauses
    }

    pub fn max_var(&self) -> usize {
        self.max_var
    }
}

/// Trait for extending CNF formulas (varisat compatibility)
pub trait ExtendFormula {
    fn add_clause(&mut self, clause: &[Lit]);
}

impl ExtendFormula for CnfFormula {
    fn add_clause(&mut self, clause: &[Lit]) {
        CnfFormula::add_clause(self, clause);
    }
}

/// SAT Solver using CaDiCaL
pub struct Solver {
    solver: cadical::Solver,
    model: Option<Vec<Lit>>,
    max_var: usize,
}

impl Solver {
    pub fn new() -> Self {
        Solver {
            solver: cadical::Solver::new(),
            model: None,
            max_var: 0,
        }
    }

    /// Add a formula to the solver
    pub fn add_formula(&mut self, formula: &CnfFormula) {
        self.max_var = self.max_var.max(formula.max_var());
        for clause in formula.clauses() {
            let cadical_clause: Vec<i32> = clause.iter().map(|lit| lit.to_cadical()).collect();
            self.solver.add_clause(cadical_clause);
        }
    }

    /// Add a single clause
    pub fn add_clause(&mut self, clause: &[Lit]) {
        for lit in clause {
            self.max_var = self.max_var.max(lit.var.0);
        }
        let cadical_clause: Vec<i32> = clause.iter().map(|lit| lit.to_cadical()).collect();
        self.solver.add_clause(cadical_clause);
    }

    /// Solve the formula
    /// Returns Ok(true) for SAT, Ok(false) for UNSAT
    pub fn solve(&mut self) -> Result<bool, SolverError> {
        match self.solver.solve() {
            Some(true) => {
                // SAT - extract model
                let mut model = Vec::new();
                for var_idx in 0..=self.max_var {
                    let var = Var(var_idx);
                    let cadical_var = (var_idx + 1) as i32;
                    match self.solver.value(cadical_var) {
                        Some(true) => model.push(Lit::positive(var)),
                        Some(false) => model.push(Lit::negative(var)),
                        None => model.push(Lit::positive(var)), // Default to positive if unknown
                    }
                }
                self.model = Some(model);
                Ok(true)
            }
            Some(false) => {
                // UNSAT
                self.model = None;
                Ok(false)
            }
            None => {
                // Unknown/interrupted
                Err(SolverError::Unknown)
            }
        }
    }

    /// Get the satisfying assignment (if SAT)
    pub fn model(&self) -> Option<&[Lit]> {
        self.model.as_deref()
    }
}

impl Default for Solver {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum SolverError {
    Unknown,
}

impl std::fmt::Display for SolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SolverError::Unknown => write!(f, "Solver returned unknown"),
        }
    }
}

impl std::error::Error for SolverError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_sat() {
        // (x1 OR x2) AND (NOT x1 OR x2) => x2 must be true
        let mut formula = CnfFormula::new();
        let x1 = Var::from_index(0);
        let x2 = Var::from_index(1);

        formula.add_clause(&[Lit::positive(x1), Lit::positive(x2)]);
        formula.add_clause(&[Lit::negative(x1), Lit::positive(x2)]);

        let mut solver = Solver::new();
        solver.add_formula(&formula);

        let result = solver.solve().unwrap();
        assert!(result, "Should be SAT");

        let model = solver.model().unwrap();
        // x2 should be positive in the model
        let x2_lit = model.iter().find(|lit| lit.var.0 == 1).unwrap();
        assert!(x2_lit.is_positive(), "x2 should be true");
    }

    #[test]
    fn test_simple_unsat() {
        // x1 AND NOT x1 => UNSAT
        let mut formula = CnfFormula::new();
        let x1 = Var::from_index(0);

        formula.add_clause(&[Lit::positive(x1)]);
        formula.add_clause(&[Lit::negative(x1)]);

        let mut solver = Solver::new();
        solver.add_formula(&formula);

        let result = solver.solve().unwrap();
        assert!(!result, "Should be UNSAT");
    }
}
