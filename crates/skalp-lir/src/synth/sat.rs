//! Embedded SAT solver for equivalence checking (FRAIG, resubstitution proofs)
//!
//! A conflict-driven clause-learning (CDCL) solver in the style of MiniSat:
//! two watched literals with blocking literals, first-UIP conflict analysis
//! with recursive-free clause minimisation, VSIDS variable activity with
//! phase saving, Luby restarts and learnt-clause reduction.
//!
//! The solver is incremental: clauses can be added between calls, and each
//! call can take assumptions, which hold for that call only. Learnt clauses
//! follow from the clauses alone, so they stay valid across calls. Variables
//! are created on first use, so `Solver::new`'s count is only a size hint.

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
    /// Satisfiable with the given assignment (indexed by variable)
    Sat(Vec<bool>),
    /// Unsatisfiable (under the assumptions, if any)
    Unsat,
    /// Unknown (conflict limit reached)
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

/// Value of a variable or literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    True,
    False,
    Undef,
}

const NO_REASON: u32 = u32::MAX;

#[derive(Debug, Clone)]
struct Clause {
    lits: Vec<Lit>,
    learnt: bool,
    deleted: bool,
    activity: f64,
}

/// A clause watching a literal, with another literal of the clause that,
/// when true, lets propagation skip the clause without reading it.
#[derive(Debug, Clone, Copy)]
struct Watcher {
    clause: u32,
    blocker: Lit,
}

/// Max-heap of variables by activity.
#[derive(Debug, Default)]
struct VarHeap {
    heap: Vec<u32>,
    /// Position of each variable in `heap`, or `usize::MAX` when absent
    position: Vec<usize>,
}

impl VarHeap {
    fn grow(&mut self, num_vars: usize) {
        self.position.resize(num_vars, usize::MAX);
    }

    fn contains(&self, var: u32) -> bool {
        self.position[var as usize] != usize::MAX
    }

    fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    fn insert(&mut self, var: u32, activity: &[f64]) {
        if self.contains(var) {
            return;
        }
        self.position[var as usize] = self.heap.len();
        self.heap.push(var);
        self.sift_up(self.heap.len() - 1, activity);
    }

    fn increased(&mut self, var: u32, activity: &[f64]) {
        if let Some(&pos) = self.position.get(var as usize) {
            if pos != usize::MAX {
                self.sift_up(pos, activity);
            }
        }
    }

    fn pop(&mut self, activity: &[f64]) -> Option<u32> {
        let top = *self.heap.first()?;
        let last = self.heap.pop().expect("heap is not empty");
        self.position[top as usize] = usize::MAX;
        if !self.heap.is_empty() {
            self.heap[0] = last;
            self.position[last as usize] = 0;
            self.sift_down(0, activity);
        }
        Some(top)
    }

    fn sift_up(&mut self, mut pos: usize, activity: &[f64]) {
        let var = self.heap[pos];
        while pos > 0 {
            let parent = (pos - 1) / 2;
            let parent_var = self.heap[parent];
            if activity[parent_var as usize] >= activity[var as usize] {
                break;
            }
            self.heap[pos] = parent_var;
            self.position[parent_var as usize] = pos;
            pos = parent;
        }
        self.heap[pos] = var;
        self.position[var as usize] = pos;
    }

    fn sift_down(&mut self, mut pos: usize, activity: &[f64]) {
        let var = self.heap[pos];
        loop {
            let left = 2 * pos + 1;
            if left >= self.heap.len() {
                break;
            }
            let right = left + 1;
            let child = if right < self.heap.len()
                && activity[self.heap[right] as usize] > activity[self.heap[left] as usize]
            {
                right
            } else {
                left
            };
            let child_var = self.heap[child];
            if activity[child_var as usize] <= activity[var as usize] {
                break;
            }
            self.heap[pos] = child_var;
            self.position[child_var as usize] = pos;
            pos = child;
        }
        self.heap[pos] = var;
        self.position[var as usize] = pos;
    }
}

/// CDCL SAT solver
pub struct Solver {
    clauses: Vec<Clause>,
    /// `watches[l]`: clauses with `!l` among their two watched literals,
    /// visited when `l` becomes true
    watches: Vec<Vec<Watcher>>,
    assigns: Vec<Value>,
    level: Vec<u32>,
    reason: Vec<u32>,
    /// Saved polarity: the value a variable last had
    polarity: Vec<bool>,
    activity: Vec<f64>,
    var_inc: f64,
    clause_inc: f64,
    heap: VarHeap,
    trail: Vec<Lit>,
    trail_lim: Vec<usize>,
    qhead: usize,
    seen: Vec<bool>,
    /// False once the clauses are unsatisfiable without assumptions
    ok: bool,
    learnts: usize,
    deleted: usize,
    max_learnts: f64,
    conflict_limit: u64,
    conflicts: u64,
    propagations: u64,
    decisions: u64,
}

const VAR_DECAY: f64 = 0.95;
const CLAUSE_DECAY: f64 = 0.999;
const RESTART_BASE: u64 = 100;

impl Solver {
    /// Create a solver; `num_vars` is a size hint, variables grow on use.
    pub fn new(num_vars: u32) -> Self {
        let mut solver = Self {
            clauses: Vec::new(),
            watches: Vec::new(),
            assigns: Vec::new(),
            level: Vec::new(),
            reason: Vec::new(),
            polarity: Vec::new(),
            activity: Vec::new(),
            var_inc: 1.0,
            clause_inc: 1.0,
            heap: VarHeap::default(),
            trail: Vec::new(),
            trail_lim: Vec::new(),
            qhead: 0,
            seen: Vec::new(),
            ok: true,
            learnts: 0,
            deleted: 0,
            max_learnts: 0.0,
            conflict_limit: 100_000,
            conflicts: 0,
            propagations: 0,
            decisions: 0,
        };
        solver.ensure_vars(num_vars as usize);
        solver
    }

    /// Conflicts each `solve` call may spend before returning `Unknown`.
    pub fn set_conflict_limit(&mut self, limit: u64) {
        self.conflict_limit = limit;
    }

    /// Number of variables known to the solver.
    pub fn num_vars(&self) -> u32 {
        self.assigns.len() as u32
    }

    fn ensure_vars(&mut self, count: usize) {
        if count <= self.assigns.len() {
            return;
        }
        let first = self.assigns.len();
        self.assigns.resize(count, Value::Undef);
        self.level.resize(count, 0);
        self.reason.resize(count, NO_REASON);
        self.polarity.resize(count, false);
        self.activity.resize(count, 0.0);
        self.seen.resize(count, false);
        self.watches.resize(count * 2, Vec::new());
        self.heap.grow(count);
        for var in first..count {
            self.heap.insert(var as u32, &self.activity);
        }
    }

    fn value(&self, lit: Lit) -> Value {
        match self.assigns[lit.var as usize] {
            Value::Undef => Value::Undef,
            Value::True if lit.negated => Value::False,
            Value::False if lit.negated => Value::True,
            v => v,
        }
    }

    fn decision_level(&self) -> u32 {
        self.trail_lim.len() as u32
    }

    /// Add a clause. Returns false once the clauses are unsatisfiable.
    pub fn add_clause(&mut self, lits: Vec<Lit>) -> bool {
        if !self.ok {
            return false;
        }
        if let Some(max) = lits.iter().map(|l| l.var).max() {
            self.ensure_vars(max as usize + 1);
        }
        // Clauses are added between solves, at the root level.
        self.cancel_until(0);

        let mut lits = lits;
        lits.sort_by_key(|l| l.index());
        lits.dedup();
        let mut kept: Vec<Lit> = Vec::with_capacity(lits.len());
        for (i, &lit) in lits.iter().enumerate() {
            if i + 1 < lits.len() && lits[i + 1] == lit.negate() {
                return true; // tautology
            }
            match self.value(lit) {
                Value::True => return true, // satisfied at the root
                Value::False => {}          // false at the root: drop
                Value::Undef => kept.push(lit),
            }
        }
        match kept.len() {
            0 => {
                self.ok = false;
                false
            }
            1 => {
                self.assign(kept[0], NO_REASON);
                if self.propagate().is_some() {
                    self.ok = false;
                }
                self.ok
            }
            _ => {
                self.attach(Clause {
                    lits: kept,
                    learnt: false,
                    deleted: false,
                    activity: 0.0,
                });
                true
            }
        }
    }

    fn attach(&mut self, clause: Clause) -> u32 {
        let index = self.clauses.len() as u32;
        let (a, b) = (clause.lits[0], clause.lits[1]);
        self.watches[a.negate().index()].push(Watcher {
            clause: index,
            blocker: b,
        });
        self.watches[b.negate().index()].push(Watcher {
            clause: index,
            blocker: a,
        });
        if clause.learnt {
            self.learnts += 1;
        }
        self.clauses.push(clause);
        index
    }

    fn assign(&mut self, lit: Lit, reason: u32) {
        let var = lit.var as usize;
        debug_assert_eq!(self.assigns[var], Value::Undef);
        self.assigns[var] = if lit.negated {
            Value::False
        } else {
            Value::True
        };
        self.level[var] = self.decision_level();
        self.reason[var] = reason;
        self.trail.push(lit);
    }

    /// Propagate everything on the trail; returns a conflicting clause.
    fn propagate(&mut self) -> Option<u32> {
        while self.qhead < self.trail.len() {
            let lit = self.trail[self.qhead];
            self.qhead += 1;
            self.propagations += 1;
            let false_lit = lit.negate();
            let mut watchers = std::mem::take(&mut self.watches[lit.index()]);
            let mut i = 0;
            let mut j = 0;
            let mut conflict = None;
            while i < watchers.len() {
                let watcher = watchers[i];
                i += 1;
                if self.clauses[watcher.clause as usize].deleted {
                    continue; // drop the watch of a deleted clause
                }
                if self.value(watcher.blocker) == Value::True {
                    watchers[j] = watcher;
                    j += 1;
                    continue;
                }
                let index = watcher.clause as usize;
                // Make lits[1] the false literal.
                {
                    let lits = &mut self.clauses[index].lits;
                    if lits[0] == false_lit {
                        lits.swap(0, 1);
                    }
                }
                let first = self.clauses[index].lits[0];
                let updated = Watcher {
                    clause: watcher.clause,
                    blocker: first,
                };
                if first != watcher.blocker && self.value(first) == Value::True {
                    watchers[j] = updated;
                    j += 1;
                    continue;
                }
                // Look for a new literal to watch.
                let mut moved = false;
                let len = self.clauses[index].lits.len();
                for k in 2..len {
                    let candidate = self.clauses[index].lits[k];
                    if self.value(candidate) != Value::False {
                        self.clauses[index].lits.swap(1, k);
                        self.watches[candidate.negate().index()].push(updated);
                        moved = true;
                        break;
                    }
                }
                if moved {
                    continue;
                }
                watchers[j] = updated;
                j += 1;
                match self.value(first) {
                    Value::False => {
                        conflict = Some(watcher.clause);
                        while i < watchers.len() {
                            watchers[j] = watchers[i];
                            j += 1;
                            i += 1;
                        }
                    }
                    Value::Undef => self.assign(first, watcher.clause),
                    Value::True => {}
                }
            }
            watchers.truncate(j);
            // New watches of `lit` may have been pushed meanwhile (a clause
            // whose other literal is `!lit`); keep them as well.
            let added = std::mem::take(&mut self.watches[lit.index()]);
            watchers.extend(added);
            self.watches[lit.index()] = watchers;
            if conflict.is_some() {
                self.qhead = self.trail.len();
                return conflict;
            }
        }
        None
    }

    fn cancel_until(&mut self, level: u32) {
        if self.decision_level() <= level {
            return;
        }
        let keep = self.trail_lim[level as usize];
        for index in (keep..self.trail.len()).rev() {
            let lit = self.trail[index];
            let var = lit.var as usize;
            self.assigns[var] = Value::Undef;
            self.reason[var] = NO_REASON;
            self.polarity[var] = !lit.negated;
            self.heap.insert(lit.var, &self.activity);
        }
        self.trail.truncate(keep);
        self.trail_lim.truncate(level as usize);
        self.qhead = keep;
    }

    fn bump_var(&mut self, var: u32) {
        let v = var as usize;
        self.activity[v] += self.var_inc;
        if self.activity[v] > 1e100 {
            for a in &mut self.activity {
                *a *= 1e-100;
            }
            self.var_inc *= 1e-100;
        }
        self.heap.increased(var, &self.activity);
    }

    fn bump_clause(&mut self, index: u32) {
        let clause = &mut self.clauses[index as usize];
        if !clause.learnt {
            return;
        }
        clause.activity += self.clause_inc;
        if clause.activity > 1e20 {
            for c in self.clauses.iter_mut().filter(|c| c.learnt) {
                c.activity *= 1e-20;
            }
            self.clause_inc *= 1e-20;
        }
    }

    /// First-UIP conflict analysis: the learnt clause (asserting literal
    /// first) and the level to go back to.
    fn analyze(&mut self, conflict: u32) -> (Vec<Lit>, u32) {
        let mut learnt: Vec<Lit> = vec![Lit::pos(0)]; // slot for the UIP
        let mut pending = 0usize;
        let mut index = self.trail.len();
        let mut clause = conflict;
        let mut uip: Option<Lit> = None;
        loop {
            self.bump_clause(clause);
            let lits = self.clauses[clause as usize].lits.clone();
            // The asserted literal of a reason clause is its first literal.
            let start = if uip.is_some() { 1 } else { 0 };
            for &lit in &lits[start..] {
                let var = lit.var as usize;
                if self.seen[var] || self.level[var] == 0 {
                    continue;
                }
                self.seen[var] = true;
                self.bump_var(lit.var);
                if self.level[var] == self.decision_level() {
                    pending += 1;
                } else {
                    learnt.push(lit);
                }
            }
            // Next literal of the current level on the trail.
            loop {
                index -= 1;
                if self.seen[self.trail[index].var as usize] {
                    break;
                }
            }
            let lit = self.trail[index];
            self.seen[lit.var as usize] = false;
            pending -= 1;
            uip = Some(lit);
            if pending == 0 {
                break;
            }
            clause = self.reason[lit.var as usize];
            debug_assert_ne!(clause, NO_REASON);
        }
        learnt[0] = uip.expect("conflict has a UIP").negate();

        // Minimisation: drop a literal whose reason's other literals are all
        // in the clause (or at the root).
        let marked: Vec<u32> = learnt.iter().map(|l| l.var).collect();
        for &var in &marked {
            self.seen[var as usize] = true;
        }
        let mut kept = vec![learnt[0]];
        for &lit in &learnt[1..] {
            let reason = self.reason[lit.var as usize];
            let redundant = reason != NO_REASON
                && self.clauses[reason as usize].lits[1..]
                    .iter()
                    .all(|l| self.seen[l.var as usize] || self.level[l.var as usize] == 0);
            if !redundant {
                kept.push(lit);
            }
        }
        for &var in &marked {
            self.seen[var as usize] = false;
        }
        let mut learnt = kept;

        // Backjump level: the highest level among the other literals, which
        // becomes the second watched literal.
        let back_level = if learnt.len() == 1 {
            0
        } else {
            let mut max_index = 1;
            for i in 2..learnt.len() {
                if self.level[learnt[i].var as usize] > self.level[learnt[max_index].var as usize] {
                    max_index = i;
                }
            }
            learnt.swap(1, max_index);
            self.level[learnt[1].var as usize]
        };
        (learnt, back_level)
    }

    /// Delete about half of the learnt clauses, least active first; clauses
    /// that are the reason of a current assignment stay.
    fn reduce_learnts(&mut self) {
        let mut candidates: Vec<u32> = (0..self.clauses.len() as u32)
            .filter(|&i| {
                let c = &self.clauses[i as usize];
                c.learnt && !c.deleted && c.lits.len() > 2
            })
            .collect();
        candidates.sort_by(|&a, &b| {
            self.clauses[a as usize]
                .activity
                .total_cmp(&self.clauses[b as usize].activity)
        });
        let target = candidates.len() / 2;
        let mut removed = 0;
        for index in candidates {
            if removed >= target {
                break;
            }
            let first = self.clauses[index as usize].lits[0];
            let locked =
                self.value(first) == Value::True && self.reason[first.var as usize] == index;
            if locked {
                continue;
            }
            let clause = &mut self.clauses[index as usize];
            clause.deleted = true;
            clause.lits = Vec::new();
            self.learnts -= 1;
            self.deleted += 1;
            removed += 1;
        }
    }

    /// Drop deleted clauses for good (at the root, between solves).
    fn compact(&mut self) {
        if self.deleted * 2 < self.clauses.len() || self.decision_level() != 0 {
            return;
        }
        let old = std::mem::take(&mut self.clauses);
        let mut remap = vec![NO_REASON; old.len()];
        for (index, clause) in old.into_iter().enumerate() {
            if !clause.deleted {
                remap[index] = self.clauses.len() as u32;
                self.clauses.push(clause);
            }
        }
        for watchers in &mut self.watches {
            watchers.retain_mut(|w| {
                let new = remap[w.clause as usize];
                w.clause = new;
                new != NO_REASON
            });
        }
        for reason in &mut self.reason {
            if *reason != NO_REASON {
                *reason = remap[*reason as usize];
            }
        }
        self.deleted = 0;
    }

    fn pick_branch(&mut self) -> Option<Lit> {
        while let Some(var) = self.heap.pop(&self.activity) {
            if self.assigns[var as usize] == Value::Undef {
                return Some(if self.polarity[var as usize] {
                    Lit::pos(var)
                } else {
                    Lit::neg(var)
                });
            }
        }
        None
    }

    /// Solve the clauses
    pub fn solve(&mut self) -> SatResult {
        self.solve_with_assumptions(&[])
    }

    /// Solve with assumptions, which hold for this call only.
    pub fn solve_with_assumptions(&mut self, assumptions: &[Lit]) -> SatResult {
        if let Some(max) = assumptions.iter().map(|l| l.var).max() {
            self.ensure_vars(max as usize + 1);
        }
        self.cancel_until(0);
        self.compact();
        if !self.ok {
            return SatResult::Unsat;
        }
        if self.propagate().is_some() {
            self.ok = false;
            return SatResult::Unsat;
        }
        if self.max_learnts == 0.0 {
            self.max_learnts = (self.clauses.len() as f64 / 3.0).max(1000.0);
        }

        let budget_end = self.conflicts.saturating_add(self.conflict_limit);
        let mut restart = 0u32;
        let result = loop {
            let allowed = luby(restart) * RESTART_BASE;
            restart += 1;
            match self.search(assumptions, allowed, budget_end) {
                Some(result) => break result,
                None if self.conflicts >= budget_end => break SatResult::Unknown,
                None => {}
            }
        };
        self.cancel_until(0);
        result
    }

    /// Search until a result, a restart (after `allowed` conflicts) or the
    /// end of the budget; `None` means restart or out of budget.
    fn search(&mut self, assumptions: &[Lit], allowed: u64, budget_end: u64) -> Option<SatResult> {
        let mut conflicts_here = 0u64;
        loop {
            if let Some(conflict) = self.propagate() {
                self.conflicts += 1;
                conflicts_here += 1;
                if self.decision_level() == 0 {
                    self.ok = false;
                    return Some(SatResult::Unsat);
                }
                let (learnt, back_level) = self.analyze(conflict);
                self.cancel_until(back_level);
                if learnt.len() == 1 {
                    self.assign(learnt[0], NO_REASON);
                } else {
                    let asserting = learnt[0];
                    let index = self.attach(Clause {
                        lits: learnt,
                        learnt: true,
                        deleted: false,
                        activity: 0.0,
                    });
                    self.bump_clause(index);
                    self.assign(asserting, index);
                }
                self.var_inc /= VAR_DECAY;
                self.clause_inc /= CLAUSE_DECAY;
                continue;
            }

            if conflicts_here >= allowed || self.conflicts >= budget_end {
                self.cancel_until(0);
                return None;
            }
            if self.learnts as f64 >= self.max_learnts + self.trail.len() as f64 {
                self.reduce_learnts();
                self.max_learnts *= 1.1;
            }

            // Assumptions first, one decision level each.
            let mut next = None;
            while (self.decision_level() as usize) < assumptions.len() {
                let assumption = assumptions[self.decision_level() as usize];
                match self.value(assumption) {
                    Value::True => self.trail_lim.push(self.trail.len()),
                    Value::False => return Some(SatResult::Unsat),
                    Value::Undef => {
                        next = Some(assumption);
                        break;
                    }
                }
            }
            let decision = match next {
                Some(lit) => lit,
                None => match self.pick_branch() {
                    Some(lit) => lit,
                    None => {
                        let model = self.assigns.iter().map(|&v| v == Value::True).collect();
                        return Some(SatResult::Sat(model));
                    }
                },
            };
            self.decisions += 1;
            self.trail_lim.push(self.trail.len());
            self.assign(decision, NO_REASON);
        }
    }

    /// Get statistics
    pub fn stats(&self) -> SolverStats {
        SolverStats {
            conflicts: self.conflicts,
            propagations: self.propagations,
            clauses: self.clauses.iter().filter(|c| !c.deleted).count(),
            variables: self.num_vars(),
        }
    }
}

/// The Luby sequence 1, 1, 2, 1, 1, 2, 4, ... (index from 0).
fn luby(index: u32) -> u64 {
    let mut size = 1u64;
    let mut seq = 0u32;
    let x = index as u64;
    while size < x + 1 {
        seq += 1;
        size = 2 * size + 1;
    }
    let mut x = x;
    while size - 1 != x {
        size = (size - 1) >> 1;
        seq -= 1;
        x %= size;
    }
    1u64 << seq
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

    /// A small deterministic generator for the random tests.
    struct Rng(u64);
    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }
        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }
    }

    fn random_clauses(rng: &mut Rng, vars: u32, count: usize) -> Vec<Vec<Lit>> {
        (0..count)
            .map(|_| {
                let len = 1 + rng.below(4) as usize;
                (0..len)
                    .map(|_| {
                        let var = rng.below(vars as u64) as u32;
                        if rng.next() & 1 == 0 {
                            Lit::pos(var)
                        } else {
                            Lit::neg(var)
                        }
                    })
                    .collect()
            })
            .collect()
    }

    fn satisfies(assignment: &[bool], clauses: &[Vec<Lit>], assumptions: &[Lit]) -> bool {
        let holds = |l: &Lit| assignment[l.var as usize] != l.negated;
        clauses.iter().all(|c| c.iter().any(holds)) && assumptions.iter().all(holds)
    }

    fn brute_force(vars: u32, clauses: &[Vec<Lit>], assumptions: &[Lit]) -> bool {
        (0..1u32 << vars).any(|bits| {
            let assignment: Vec<bool> = (0..vars).map(|v| bits >> v & 1 == 1).collect();
            satisfies(&assignment, clauses, assumptions)
        })
    }

    #[test]
    fn test_random_formulas_match_brute_force() {
        let mut rng = Rng(0x9E3779B97F4A7C15);
        for round in 0..3000 {
            let vars = 1 + rng.below(12) as u32;
            let count = rng.below(5 * vars as u64 + 4) as usize;
            let clauses = random_clauses(&mut rng, vars, count);
            let mut solver = Solver::new(vars);
            for clause in &clauses {
                solver.add_clause(clause.clone());
            }
            let expected = brute_force(vars, &clauses, &[]);
            match solver.solve() {
                SatResult::Sat(model) => {
                    assert!(expected, "round {round}: SAT on an unsatisfiable formula");
                    assert!(satisfies(&model, &clauses, &[]), "round {round}: bad model");
                }
                SatResult::Unsat => {
                    assert!(!expected, "round {round}: UNSAT on a satisfiable formula")
                }
                SatResult::Unknown => panic!("round {round}: unknown on a tiny formula"),
            }
        }
    }

    #[test]
    fn test_incremental_assumptions_match_brute_force() {
        // One solver, many calls with different assumptions and clauses
        // added in between: each answer is for the clauses so far plus
        // that call's assumptions only.
        let mut rng = Rng(0xD1B54A32D192ED03);
        for round in 0..300 {
            let vars = 2 + rng.below(11) as u32;
            let mut solver = Solver::new(vars);
            let mut clauses: Vec<Vec<Lit>> = Vec::new();
            for step in 0..20 {
                let count = rng.below(4) as usize;
                for clause in random_clauses(&mut rng, vars, count) {
                    solver.add_clause(clause.clone());
                    clauses.push(clause);
                }
                let assumptions: Vec<Lit> = random_clauses(&mut rng, vars, 1)
                    .into_iter()
                    .flatten()
                    .collect();
                let expected = brute_force(vars, &clauses, &assumptions);
                match solver.solve_with_assumptions(&assumptions) {
                    SatResult::Sat(model) => {
                        assert!(expected, "round {round} step {step}: wrong SAT");
                        assert!(satisfies(&model, &clauses, &assumptions), "bad model");
                    }
                    SatResult::Unsat => {
                        assert!(!expected, "round {round} step {step}: wrong UNSAT")
                    }
                    SatResult::Unknown => panic!("unknown on a tiny formula"),
                }
            }
        }
    }

    #[test]
    fn test_pigeonhole_is_unsat_and_needs_learning() {
        // 7 pigeons in 6 holes.
        let (pigeons, holes) = (7u32, 6u32);
        let var = |p: u32, h: u32| p * holes + h;
        let mut solver = Solver::new(pigeons * holes);
        for p in 0..pigeons {
            solver.add_clause((0..holes).map(|h| Lit::pos(var(p, h))).collect());
        }
        for h in 0..holes {
            for a in 0..pigeons {
                for b in a + 1..pigeons {
                    solver.add_clause(vec![Lit::neg(var(a, h)), Lit::neg(var(b, h))]);
                }
            }
        }
        solver.set_conflict_limit(1_000_000);
        assert_eq!(solver.solve(), SatResult::Unsat);
    }

    #[test]
    fn test_conflict_limit_gives_unknown() {
        let (pigeons, holes) = (9u32, 8u32);
        let var = |p: u32, h: u32| p * holes + h;
        let mut solver = Solver::new(0);
        for p in 0..pigeons {
            solver.add_clause((0..holes).map(|h| Lit::pos(var(p, h))).collect());
        }
        for h in 0..holes {
            for a in 0..pigeons {
                for b in a + 1..pigeons {
                    solver.add_clause(vec![Lit::neg(var(a, h)), Lit::neg(var(b, h))]);
                }
            }
        }
        solver.set_conflict_limit(10);
        assert_eq!(solver.solve(), SatResult::Unknown);
    }

    #[test]
    fn test_luby_sequence() {
        let seq: Vec<u64> = (0..15).map(luby).collect();
        assert_eq!(seq, vec![1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8]);
    }
}
