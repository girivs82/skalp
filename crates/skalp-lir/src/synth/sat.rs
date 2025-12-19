//! Embedded SAT Solver for FRAIG
//!
//! A simple but efficient DPLL-based SAT solver with 2-literal watching
//! for use in functional equivalence checking.

use std::collections::VecDeque;

/// A literal in the SAT solver (variable with polarity)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lit {
    /// Variable index (0-based)
    pub var: u32,
    /// Whether the literal is negated
    pub negated: bool,
}

impl Lit {
    /// Create a positive literal
    pub fn pos(var: u32) -> Self {
        Self {
            var,
            negated: false,
        }
    }

    /// Create a negative literal
    pub fn neg(var: u32) -> Self {
        Self { var, negated: true }
    }

    /// Negate the literal
    pub fn negate(self) -> Self {
        Self {
            var: self.var,
            negated: !self.negated,
        }
    }

    /// Get the literal index for array access (2*var + negated)
    fn index(self) -> usize {
        (self.var as usize) * 2 + (self.negated as usize)
    }
}

/// Result of SAT solving
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SatResult {
    /// Satisfiable with the given assignment
    Sat(Vec<bool>),
    /// Unsatisfiable
    Unsat,
    /// Unknown (resource limit reached)
    Unknown,
}

impl SatResult {
    pub fn is_sat(&self) -> bool {
        matches!(self, SatResult::Sat(_))
    }

    pub fn is_unsat(&self) -> bool {
        matches!(self, SatResult::Unsat)
    }
}

/// Variable assignment state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VarState {
    /// Unassigned
    Free,
    /// Assigned true
    True,
    /// Assigned false
    False,
}

impl VarState {
    fn from_bool(b: bool) -> Self {
        if b {
            VarState::True
        } else {
            VarState::False
        }
    }

    fn to_bool(self) -> Option<bool> {
        match self {
            VarState::True => Some(true),
            VarState::False => Some(false),
            VarState::Free => None,
        }
    }
}

/// A clause in CNF
#[derive(Debug, Clone)]
struct Clause {
    /// Literals in the clause
    lits: Vec<Lit>,
    /// First watched literal index
    watch1: usize,
    /// Second watched literal index
    watch2: usize,
}

impl Clause {
    fn new(lits: Vec<Lit>) -> Self {
        let watch1 = 0;
        let watch2 = if lits.len() > 1 { 1 } else { 0 };
        Self {
            lits,
            watch1,
            watch2,
        }
    }

    fn len(&self) -> usize {
        self.lits.len()
    }

    fn is_unit(&self) -> bool {
        self.lits.len() == 1
    }
}

/// Simple DPLL SAT solver with 2-literal watching
pub struct Solver {
    /// Number of variables
    num_vars: u32,
    /// Clauses
    clauses: Vec<Clause>,
    /// Variable assignments
    assignment: Vec<VarState>,
    /// Decision level for each variable
    level: Vec<u32>,
    /// Trail of assignments
    trail: Vec<Lit>,
    /// Trail limits for each decision level
    trail_lim: Vec<usize>,
    /// Watch lists: for each literal, list of clause indices watching it
    watches: Vec<Vec<usize>>,
    /// Propagation queue
    prop_queue: VecDeque<Lit>,
    /// Conflict limit
    conflict_limit: u64,
    /// Conflicts encountered
    conflicts: u64,
    /// Propagations done
    propagations: u64,
    /// Whether a conflict was detected at the root level (during clause addition)
    unsat_at_root: bool,
}

impl Solver {
    /// Create a new solver with the given number of variables
    pub fn new(num_vars: u32) -> Self {
        Self {
            num_vars,
            clauses: Vec::new(),
            assignment: vec![VarState::Free; num_vars as usize],
            level: vec![0; num_vars as usize],
            trail: Vec::new(),
            trail_lim: Vec::new(),
            watches: vec![Vec::new(); (num_vars as usize) * 2],
            prop_queue: VecDeque::new(),
            conflict_limit: 100_000,
            conflicts: 0,
            propagations: 0,
            unsat_at_root: false,
        }
    }

    /// Set conflict limit
    pub fn set_conflict_limit(&mut self, limit: u64) {
        self.conflict_limit = limit;
    }

    /// Add a clause to the solver
    pub fn add_clause(&mut self, lits: Vec<Lit>) -> bool {
        if lits.is_empty() {
            self.unsat_at_root = true;
            return false; // Empty clause = UNSAT
        }

        // Handle unit clause
        if lits.len() == 1 {
            let result = self.enqueue(lits[0], None);
            if !result {
                self.unsat_at_root = true;
            }
            return result;
        }

        let clause_idx = self.clauses.len();
        let clause = Clause::new(lits.clone());

        // Add watches
        self.watches[clause.lits[clause.watch1].negate().index()].push(clause_idx);
        if clause.len() > 1 {
            self.watches[clause.lits[clause.watch2].negate().index()].push(clause_idx);
        }

        self.clauses.push(clause);
        true
    }

    /// Get current decision level
    fn decision_level(&self) -> u32 {
        self.trail_lim.len() as u32
    }

    /// Enqueue a literal for assignment
    fn enqueue(&mut self, lit: Lit, _reason: Option<usize>) -> bool {
        let var = lit.var as usize;
        match self.assignment[var] {
            VarState::Free => {
                self.assignment[var] = VarState::from_bool(!lit.negated);
                self.level[var] = self.decision_level();
                self.trail.push(lit);
                self.prop_queue.push_back(lit);
                true
            }
            VarState::True => !lit.negated,
            VarState::False => lit.negated,
        }
    }

    /// Evaluate a literal under current assignment
    fn eval_lit(&self, lit: Lit) -> Option<bool> {
        self.assignment[lit.var as usize]
            .to_bool()
            .map(|v| v != lit.negated)
    }

    /// Propagate unit clauses
    fn propagate(&mut self) -> Option<usize> {
        while let Some(lit) = self.prop_queue.pop_front() {
            self.propagations += 1;

            // Get clauses watching the negation of this literal
            let watch_idx = lit.index();
            let mut i = 0;

            while i < self.watches[watch_idx].len() {
                let clause_idx = self.watches[watch_idx][i];
                let clause = &self.clauses[clause_idx];

                // Find which watch this is
                let (this_watch, other_watch) = if clause.lits[clause.watch1] == lit.negate() {
                    (clause.watch1, clause.watch2)
                } else {
                    (clause.watch2, clause.watch1)
                };

                // Check if the other watched literal is true
                if let Some(true) = self.eval_lit(clause.lits[other_watch]) {
                    i += 1;
                    continue;
                }

                // Try to find a new literal to watch
                let mut found_new = false;
                for j in 0..clause.lits.len() {
                    if j == this_watch || j == other_watch {
                        continue;
                    }
                    if self.eval_lit(clause.lits[j]) != Some(false) {
                        // Found a new literal to watch
                        let clause = &mut self.clauses[clause_idx];
                        if this_watch == clause.watch1 {
                            clause.watch1 = j;
                        } else {
                            clause.watch2 = j;
                        }

                        // Update watch lists
                        self.watches[watch_idx].swap_remove(i);
                        self.watches[clause.lits[j].negate().index()].push(clause_idx);
                        found_new = true;
                        break;
                    }
                }

                if found_new {
                    continue;
                }

                // No new watch found - clause is unit or conflicting
                let clause = &self.clauses[clause_idx];
                let unit_lit = clause.lits[other_watch];

                match self.eval_lit(unit_lit) {
                    Some(false) => {
                        // Conflict!
                        self.prop_queue.clear();
                        return Some(clause_idx);
                    }
                    None => {
                        // Unit propagation
                        if !self.enqueue(unit_lit, Some(clause_idx)) {
                            self.prop_queue.clear();
                            return Some(clause_idx);
                        }
                    }
                    Some(true) => {}
                }
                i += 1;
            }
        }
        None
    }

    /// Choose an unassigned variable
    fn pick_branch_var(&self) -> Option<u32> {
        for (i, &state) in self.assignment.iter().enumerate() {
            if state == VarState::Free {
                return Some(i as u32);
            }
        }
        None
    }

    /// Backtrack to the given level
    fn backtrack(&mut self, level: u32) {
        if self.decision_level() > level {
            while self.trail.len() > self.trail_lim[level as usize] {
                let lit = self.trail.pop().unwrap();
                self.assignment[lit.var as usize] = VarState::Free;
            }
            self.trail_lim.truncate(level as usize);
            self.prop_queue.clear();
        }
    }

    /// Solve the formula
    pub fn solve(&mut self) -> SatResult {
        // Check if conflict was detected during clause addition
        if self.unsat_at_root {
            return SatResult::Unsat;
        }

        // Initial propagation
        if self.propagate().is_some() {
            return SatResult::Unsat;
        }

        loop {
            // Check conflict limit
            if self.conflicts > self.conflict_limit {
                return SatResult::Unknown;
            }

            // Pick a variable to branch on
            match self.pick_branch_var() {
                None => {
                    // All variables assigned - SAT!
                    let model: Vec<bool> = self
                        .assignment
                        .iter()
                        .map(|s| s.to_bool().unwrap_or(false))
                        .collect();
                    return SatResult::Sat(model);
                }
                Some(var) => {
                    // Make a decision
                    self.trail_lim.push(self.trail.len());
                    let lit = Lit::pos(var);
                    self.enqueue(lit, None);

                    // Propagate
                    while let Some(_conflict) = self.propagate() {
                        self.conflicts += 1;

                        // Simple backtracking (no CDCL)
                        if self.decision_level() == 0 {
                            return SatResult::Unsat;
                        }

                        // Try the opposite polarity
                        let level = self.decision_level() - 1;
                        let last_decision = self.trail[self.trail_lim[level as usize]];
                        self.backtrack(level);

                        // Enqueue the negation
                        if !self.enqueue(last_decision.negate(), None) {
                            if level == 0 {
                                return SatResult::Unsat;
                            }
                            // Need to backtrack further
                            self.backtrack(level.saturating_sub(1));
                            continue;
                        }

                        // Propagate again
                        if self.propagate().is_some() {
                            if self.decision_level() == 0 {
                                return SatResult::Unsat;
                            }
                            self.backtrack(self.decision_level() - 1);
                        }
                        break;
                    }
                }
            }
        }
    }

    /// Solve with assumptions
    pub fn solve_with_assumptions(&mut self, assumptions: &[Lit]) -> SatResult {
        // Check if conflict was detected during clause addition
        if self.unsat_at_root {
            return SatResult::Unsat;
        }

        // Add assumptions as decisions at level 0
        for &lit in assumptions {
            if !self.enqueue(lit, None) {
                return SatResult::Unsat;
            }
        }

        if self.propagate().is_some() {
            return SatResult::Unsat;
        }

        self.solve()
    }

    /// Get statistics
    pub fn stats(&self) -> SolverStats {
        SolverStats {
            conflicts: self.conflicts,
            propagations: self.propagations,
            clauses: self.clauses.len(),
            variables: self.num_vars,
        }
    }
}

/// Solver statistics
#[derive(Debug, Clone, Default)]
pub struct SolverStats {
    pub conflicts: u64,
    pub propagations: u64,
    pub clauses: usize,
    pub variables: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lit_creation() {
        let pos = Lit::pos(5);
        assert_eq!(pos.var, 5);
        assert!(!pos.negated);

        let neg = Lit::neg(5);
        assert_eq!(neg.var, 5);
        assert!(neg.negated);

        assert_eq!(pos.negate(), neg);
        assert_eq!(neg.negate(), pos);
    }

    #[test]
    fn test_sat_simple() {
        // (a | b) & (a | !b) should be SAT with a=true
        let mut solver = Solver::new(2);
        solver.add_clause(vec![Lit::pos(0), Lit::pos(1)]); // a | b
        solver.add_clause(vec![Lit::pos(0), Lit::neg(1)]); // a | !b

        let result = solver.solve();
        assert!(result.is_sat());

        if let SatResult::Sat(model) = result {
            assert!(model[0]); // a must be true
        }
    }

    #[test]
    fn test_unsat_simple() {
        // (a) & (!a) should be UNSAT
        let mut solver = Solver::new(1);
        solver.add_clause(vec![Lit::pos(0)]); // a
        solver.add_clause(vec![Lit::neg(0)]); // !a

        let result = solver.solve();
        assert!(result.is_unsat());
    }

    #[test]
    fn test_sat_xor() {
        // XOR(a, b) = (a | b) & (!a | !b)
        let mut solver = Solver::new(2);
        solver.add_clause(vec![Lit::pos(0), Lit::pos(1)]); // a | b
        solver.add_clause(vec![Lit::neg(0), Lit::neg(1)]); // !a | !b

        let result = solver.solve();
        assert!(result.is_sat());

        if let SatResult::Sat(model) = result {
            // XOR should be true: a != b
            assert_ne!(model[0], model[1]);
        }
    }

    #[test]
    fn test_unsat_3vars() {
        // (a | b) & (a | c) & (!a) & (!b | !c) should be UNSAT
        let mut solver = Solver::new(3);
        solver.add_clause(vec![Lit::pos(0), Lit::pos(1)]); // a | b
        solver.add_clause(vec![Lit::pos(0), Lit::pos(2)]); // a | c
        solver.add_clause(vec![Lit::neg(0)]); // !a
        solver.add_clause(vec![Lit::neg(1), Lit::neg(2)]); // !b | !c

        // !a means we need b and c (from first two clauses)
        // but !b | !c means we can't have both
        let result = solver.solve();
        assert!(result.is_unsat());
    }

    #[test]
    fn test_unit_propagation() {
        // (a) & (a -> b) = (a) & (!a | b) should give a=true, b=true
        let mut solver = Solver::new(2);
        solver.add_clause(vec![Lit::pos(0)]); // a
        solver.add_clause(vec![Lit::neg(0), Lit::pos(1)]); // !a | b

        let result = solver.solve();
        assert!(result.is_sat());

        if let SatResult::Sat(model) = result {
            assert!(model[0]); // a
            assert!(model[1]); // b
        }
    }

    #[test]
    fn test_empty_formula() {
        let mut solver = Solver::new(3);
        let result = solver.solve();
        assert!(result.is_sat());
    }

    #[test]
    fn test_solve_with_assumptions() {
        // Formula: (a | b) & (!a | b) & (a | !b) = b
        let mut solver = Solver::new(2);
        solver.add_clause(vec![Lit::pos(0), Lit::pos(1)]); // a | b
        solver.add_clause(vec![Lit::neg(0), Lit::pos(1)]); // !a | b
        solver.add_clause(vec![Lit::pos(0), Lit::neg(1)]); // a | !b

        // With assumption b=true, should be SAT
        let result = solver.solve_with_assumptions(&[Lit::pos(1)]);
        assert!(result.is_sat());
    }
}
