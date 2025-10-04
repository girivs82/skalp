//! Coverage collection and reporting for SKALP verification

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Coverage collector
pub struct Coverage {
    /// Statement coverage
    pub statement: StatementCoverage,

    /// Branch coverage
    pub branch: BranchCoverage,

    /// Condition coverage
    pub condition: ConditionCoverage,

    /// FSM coverage
    pub fsm: FSMCoverage,

    /// Toggle coverage
    pub toggle: ToggleCoverage,

    /// Cross coverage
    pub cross: CrossCoverage,

    /// Configuration
    config: CoverageConfig,
}

/// Coverage configuration
#[derive(Debug, Clone)]
pub struct CoverageConfig {
    /// Enable statement coverage
    pub statement: bool,

    /// Enable branch coverage
    pub branch: bool,

    /// Enable condition coverage
    pub condition: bool,

    /// Enable FSM coverage
    pub fsm: bool,

    /// Enable toggle coverage
    pub toggle: bool,

    /// Enable cross coverage
    pub cross: bool,

    /// Coverage goal percentage
    pub goal: f64,
}

impl Default for CoverageConfig {
    fn default() -> Self {
        Self {
            statement: true,
            branch: true,
            condition: true,
            fsm: true,
            toggle: true,
            cross: false,
            goal: 90.0,
        }
    }
}

/// Statement coverage
#[derive(Debug, Clone)]
pub struct StatementCoverage {
    /// Total statements
    pub total: usize,

    /// Covered statements
    covered: HashSet<StatementId>,

    /// Statement execution counts
    counts: HashMap<StatementId, usize>,
}

/// Branch coverage
#[derive(Debug, Clone)]
pub struct BranchCoverage {
    /// Branch points
    branches: Vec<BranchPoint>,

    /// Covered branches
    covered: HashSet<BranchId>,
}

/// Condition coverage
#[derive(Debug, Clone)]
pub struct ConditionCoverage {
    /// Conditions
    conditions: Vec<Condition>,

    /// Covered condition values
    covered: HashSet<ConditionValue>,
}

/// FSM state coverage
#[derive(Debug, Clone)]
pub struct FSMCoverage {
    /// FSMs in the design
    pub fsms: Vec<FSM>,

    /// Covered states
    covered_states: HashSet<(FsmId, StateId)>,

    /// Covered transitions
    covered_transitions: HashSet<(FsmId, TransitionId)>,
}

/// Toggle coverage
#[derive(Debug, Clone)]
pub struct ToggleCoverage {
    /// Signals to track
    pub signals: Vec<Signal>,

    /// Toggle counts (0->1 and 1->0)
    toggles: HashMap<SignalId, ToggleCount>,
}

/// Cross coverage
#[derive(Debug, Clone)]
pub struct CrossCoverage {
    /// Cross coverage points
    pub cross_points: Vec<CrossPoint>,

    /// Covered combinations
    covered: HashSet<CrossValue>,
}

/// Statement identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StatementId(pub usize);

/// Branch identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BranchId(pub usize);

/// FSM identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FsmId(pub usize);

/// State identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StateId(pub usize);

/// Transition identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TransitionId(pub usize);

/// Signal identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SignalId(pub usize);

/// Branch point
#[derive(Debug, Clone)]
pub struct BranchPoint {
    /// Branch ID
    pub id: BranchId,

    /// Location in source
    pub location: String,

    /// True branch taken
    pub true_taken: bool,

    /// False branch taken
    pub false_taken: bool,
}

/// Condition
#[derive(Debug, Clone)]
pub struct Condition {
    /// Condition expression
    pub expr: String,

    /// Location
    pub location: String,

    /// Sub-conditions for MCDC
    pub sub_conditions: Vec<String>,
}

/// Condition value combination
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionValue {
    /// Condition index
    pub condition: usize,

    /// Values of sub-conditions
    pub values: Vec<bool>,
}

/// FSM definition
#[derive(Debug, Clone)]
pub struct FSM {
    /// FSM ID
    pub id: FsmId,

    /// FSM name
    pub name: String,

    /// States
    pub states: Vec<State>,

    /// Transitions
    pub transitions: Vec<Transition>,
}

/// FSM state
#[derive(Debug, Clone)]
pub struct State {
    /// State ID
    pub id: StateId,

    /// State name
    pub name: String,
}

/// FSM transition
#[derive(Debug, Clone)]
pub struct Transition {
    /// Transition ID
    pub id: TransitionId,

    /// Source state
    pub from: StateId,

    /// Target state
    pub to: StateId,

    /// Condition
    pub condition: String,
}

/// Signal for toggle coverage
#[derive(Debug, Clone)]
pub struct Signal {
    /// Signal ID
    pub id: SignalId,

    /// Signal name
    pub name: String,

    /// Width in bits
    pub width: usize,
}

/// Toggle count
#[derive(Debug, Clone)]
pub struct ToggleCount {
    /// 0 to 1 transitions
    pub rising: usize,

    /// 1 to 0 transitions
    pub falling: usize,
}

/// Cross coverage point
#[derive(Debug, Clone)]
pub struct CrossPoint {
    /// Cross point name
    pub name: String,

    /// Variables to cross
    pub variables: Vec<String>,
}

/// Cross coverage value
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrossValue {
    /// Cross point index
    pub point: usize,

    /// Values of variables
    pub values: Vec<u64>,
}

impl Coverage {
    /// Create a new coverage collector
    pub fn new(config: CoverageConfig) -> Self {
        Self {
            statement: StatementCoverage::new(),
            branch: BranchCoverage::new(),
            condition: ConditionCoverage::new(),
            fsm: FSMCoverage::new(),
            toggle: ToggleCoverage::new(),
            cross: CrossCoverage::new(),
            config,
        }
    }

    /// Get coverage configuration
    pub fn config(&self) -> &CoverageConfig {
        &self.config
    }

    /// Record statement execution
    pub fn record_statement(&mut self, id: StatementId) {
        self.statement.record(id);
    }

    /// Record branch taken
    pub fn record_branch(&mut self, id: BranchId, taken: bool) {
        self.branch.record(id, taken);
    }

    /// Record condition evaluation
    pub fn record_condition(&mut self, condition: usize, values: Vec<bool>) {
        self.condition.record(ConditionValue { condition, values });
    }

    /// Record FSM state
    pub fn record_fsm_state(&mut self, fsm: FsmId, state: StateId) {
        self.fsm.record_state(fsm, state);
    }

    /// Record FSM transition
    pub fn record_fsm_transition(&mut self, fsm: FsmId, transition: TransitionId) {
        self.fsm.record_transition(fsm, transition);
    }

    /// Record signal toggle
    pub fn record_toggle(&mut self, signal: SignalId, rising: bool) {
        self.toggle.record(signal, rising);
    }

    /// Get coverage metrics
    pub fn get_metrics(&self) -> CoverageMetrics {
        CoverageMetrics {
            statement: self.statement.get_percentage(),
            branch: self.branch.get_percentage(),
            condition: self.condition.get_percentage(),
            fsm_state: self.fsm.get_state_percentage(),
            fsm_transition: self.fsm.get_transition_percentage(),
            toggle: self.toggle.get_percentage(),
            cross: self.cross.get_percentage(),
            overall: self.get_overall_percentage(),
        }
    }

    /// Get overall coverage percentage
    fn get_overall_percentage(&self) -> f64 {
        let mut sum = 0.0;
        let mut count = 0;

        if self.config.statement {
            sum += self.statement.get_percentage();
            count += 1;
        }
        if self.config.branch {
            sum += self.branch.get_percentage();
            count += 1;
        }
        if self.config.condition {
            sum += self.condition.get_percentage();
            count += 1;
        }
        if self.config.fsm {
            sum += self.fsm.get_state_percentage();
            sum += self.fsm.get_transition_percentage();
            count += 2;
        }
        if self.config.toggle {
            sum += self.toggle.get_percentage();
            count += 1;
        }
        if self.config.cross {
            sum += self.cross.get_percentage();
            count += 1;
        }

        if count > 0 {
            sum / count as f64
        } else {
            0.0
        }
    }

    /// Generate coverage report
    pub fn generate_report(&self) -> CoverageReport {
        let metrics = self.get_metrics();
        let goal_met = metrics.overall >= self.config.goal;

        CoverageReport {
            metrics,
            goal: self.config.goal,
            goal_met,
            uncovered_statements: self.statement.get_uncovered(),
            uncovered_branches: self.branch.get_uncovered(),
            uncovered_conditions: self.condition.get_uncovered(),
        }
    }
}

// Implementation for individual coverage types

impl StatementCoverage {
    fn new() -> Self {
        Self {
            total: 0,
            covered: HashSet::new(),
            counts: HashMap::new(),
        }
    }

    fn record(&mut self, id: StatementId) {
        self.covered.insert(id);
        *self.counts.entry(id).or_insert(0) += 1;
    }

    fn get_percentage(&self) -> f64 {
        if self.total > 0 {
            100.0 * self.covered.len() as f64 / self.total as f64
        } else {
            0.0
        }
    }

    fn get_uncovered(&self) -> Vec<StatementId> {
        (0..self.total)
            .map(StatementId)
            .filter(|id| !self.covered.contains(id))
            .collect()
    }
}

impl BranchCoverage {
    fn new() -> Self {
        Self {
            branches: Vec::new(),
            covered: HashSet::new(),
        }
    }

    fn record(&mut self, id: BranchId, taken: bool) {
        if let Some(branch) = self.branches.iter_mut().find(|b| b.id == id) {
            if taken {
                branch.true_taken = true;
            } else {
                branch.false_taken = true;
            }
            self.covered.insert(id);
        }
    }

    fn get_percentage(&self) -> f64 {
        let total = self.branches.len() * 2; // Each branch has 2 outcomes
        let covered = self
            .branches
            .iter()
            .map(|b| (b.true_taken as usize) + (b.false_taken as usize))
            .sum::<usize>();

        if total > 0 {
            100.0 * covered as f64 / total as f64
        } else {
            0.0
        }
    }

    fn get_uncovered(&self) -> Vec<String> {
        let mut uncovered = Vec::new();
        for branch in &self.branches {
            if !branch.true_taken {
                uncovered.push(format!("{} (true branch)", branch.location));
            }
            if !branch.false_taken {
                uncovered.push(format!("{} (false branch)", branch.location));
            }
        }
        uncovered
    }
}

impl ConditionCoverage {
    fn new() -> Self {
        Self {
            conditions: Vec::new(),
            covered: HashSet::new(),
        }
    }

    fn record(&mut self, value: ConditionValue) {
        self.covered.insert(value);
    }

    fn get_percentage(&self) -> f64 {
        if self.conditions.is_empty() {
            return 0.0;
        }

        let total_combinations: usize = self
            .conditions
            .iter()
            .map(|c| 1 << c.sub_conditions.len())
            .sum();

        if total_combinations > 0 {
            100.0 * self.covered.len() as f64 / total_combinations as f64
        } else {
            0.0
        }
    }

    fn get_uncovered(&self) -> Vec<String> {
        Vec::new() // Simplified
    }
}

impl FSMCoverage {
    fn new() -> Self {
        Self {
            fsms: Vec::new(),
            covered_states: HashSet::new(),
            covered_transitions: HashSet::new(),
        }
    }

    fn record_state(&mut self, fsm: FsmId, state: StateId) {
        self.covered_states.insert((fsm, state));
    }

    fn record_transition(&mut self, fsm: FsmId, transition: TransitionId) {
        self.covered_transitions.insert((fsm, transition));
    }

    fn get_state_percentage(&self) -> f64 {
        let total: usize = self.fsms.iter().map(|f| f.states.len()).sum();
        if total > 0 {
            100.0 * self.covered_states.len() as f64 / total as f64
        } else {
            0.0
        }
    }

    fn get_transition_percentage(&self) -> f64 {
        let total: usize = self.fsms.iter().map(|f| f.transitions.len()).sum();
        if total > 0 {
            100.0 * self.covered_transitions.len() as f64 / total as f64
        } else {
            0.0
        }
    }
}

impl ToggleCoverage {
    fn new() -> Self {
        Self {
            signals: Vec::new(),
            toggles: HashMap::new(),
        }
    }

    fn record(&mut self, signal: SignalId, rising: bool) {
        let toggle = self.toggles.entry(signal).or_insert(ToggleCount {
            rising: 0,
            falling: 0,
        });

        if rising {
            toggle.rising += 1;
        } else {
            toggle.falling += 1;
        }
    }

    fn get_percentage(&self) -> f64 {
        if self.signals.is_empty() {
            return 0.0;
        }

        let total = self.signals.len() * 2; // Each signal needs both toggles
        let covered = self
            .toggles
            .values()
            .map(|t| ((t.rising > 0) as usize) + ((t.falling > 0) as usize))
            .sum::<usize>();

        100.0 * covered as f64 / total as f64
    }
}

impl CrossCoverage {
    fn new() -> Self {
        Self {
            cross_points: Vec::new(),
            covered: HashSet::new(),
        }
    }

    fn get_percentage(&self) -> f64 {
        if self.cross_points.is_empty() {
            return 0.0;
        }

        // Simplified - would calculate based on variable ranges
        100.0 * self.covered.len() as f64 / 100.0
    }
}

/// Coverage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageMetrics {
    /// Statement coverage percentage
    pub statement: f64,

    /// Branch coverage percentage
    pub branch: f64,

    /// Condition coverage percentage
    pub condition: f64,

    /// FSM state coverage percentage
    pub fsm_state: f64,

    /// FSM transition coverage percentage
    pub fsm_transition: f64,

    /// Toggle coverage percentage
    pub toggle: f64,

    /// Cross coverage percentage
    pub cross: f64,

    /// Overall coverage percentage
    pub overall: f64,
}

/// Coverage report
#[derive(Debug)]
pub struct CoverageReport {
    /// Coverage metrics
    pub metrics: CoverageMetrics,

    /// Coverage goal
    pub goal: f64,

    /// Goal met?
    pub goal_met: bool,

    /// Uncovered statements
    pub uncovered_statements: Vec<StatementId>,

    /// Uncovered branches
    pub uncovered_branches: Vec<String>,

    /// Uncovered conditions
    pub uncovered_conditions: Vec<String>,
}

impl fmt::Display for CoverageReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Coverage Report ===")?;
        writeln!(f, "Statement:    {:.1}%", self.metrics.statement)?;
        writeln!(f, "Branch:       {:.1}%", self.metrics.branch)?;
        writeln!(f, "Condition:    {:.1}%", self.metrics.condition)?;
        writeln!(f, "FSM State:    {:.1}%", self.metrics.fsm_state)?;
        writeln!(f, "FSM Trans:    {:.1}%", self.metrics.fsm_transition)?;
        writeln!(f, "Toggle:       {:.1}%", self.metrics.toggle)?;
        writeln!(f, "Cross:        {:.1}%", self.metrics.cross)?;
        writeln!(f, "====================")?;
        writeln!(f, "Overall:      {:.1}%", self.metrics.overall)?;
        writeln!(f, "Goal:         {:.1}%", self.goal)?;
        writeln!(
            f,
            "Status:       {}",
            if self.goal_met {
                "✓ PASSED"
            } else {
                "✗ FAILED"
            }
        )?;

        if !self.uncovered_statements.is_empty() {
            writeln!(
                f,
                "\nUncovered statements: {}",
                self.uncovered_statements.len()
            )?;
        }
        if !self.uncovered_branches.is_empty() {
            writeln!(f, "Uncovered branches: {}", self.uncovered_branches.len())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_statement_coverage_new() {
        let coverage = StatementCoverage::new();
        assert_eq!(coverage.total, 0);
        assert!(coverage.covered.is_empty());
        assert!(coverage.counts.is_empty());
    }

    #[test]
    fn test_statement_coverage_record() {
        let mut coverage = StatementCoverage::new();
        coverage.total = 5;

        coverage.record(StatementId(0));
        coverage.record(StatementId(1));
        coverage.record(StatementId(0)); // Record again

        assert_eq!(coverage.covered.len(), 2);
        assert_eq!(*coverage.counts.get(&StatementId(0)).unwrap(), 2);
        assert_eq!(*coverage.counts.get(&StatementId(1)).unwrap(), 1);
    }

    #[test]
    fn test_statement_coverage_percentage() {
        let mut coverage = StatementCoverage::new();
        coverage.total = 4;

        // 0% coverage
        assert_eq!(coverage.get_percentage(), 0.0);

        // 50% coverage
        coverage.record(StatementId(0));
        coverage.record(StatementId(1));
        assert_eq!(coverage.get_percentage(), 50.0);

        // 100% coverage
        coverage.record(StatementId(2));
        coverage.record(StatementId(3));
        assert_eq!(coverage.get_percentage(), 100.0);
    }

    #[test]
    fn test_branch_coverage_new() {
        let coverage = BranchCoverage::new();
        assert!(coverage.branches.is_empty());
        assert!(coverage.covered.is_empty());
    }

    #[test]
    fn test_coverage_config_default() {
        let config = CoverageConfig::default();
        assert!(config.statement);
        assert!(config.branch);
        assert!(config.condition);
        assert!(config.fsm);
        assert!(config.toggle);
        assert!(!config.cross); // Default is false
        assert_eq!(config.goal, 90.0);
    }

    #[test]
    fn test_coverage_new() {
        let config = CoverageConfig::default();
        let coverage = Coverage::new(config);

        // All coverage types should be initialized
        assert_eq!(coverage.statement.total, 0);
        assert!(coverage.branch.branches.is_empty());
        assert!(coverage.condition.conditions.is_empty());
        assert!(coverage.fsm.fsms.is_empty());
        assert!(coverage.toggle.signals.is_empty());
        assert!(coverage.cross.cross_points.is_empty());
    }

    #[test]
    fn test_coverage_overall_calculation() {
        let config = CoverageConfig {
            statement: true,
            branch: true,
            condition: false,
            fsm: false,
            toggle: false,
            cross: false,
            goal: 80.0,
        };

        let mut coverage = Coverage::new(config);
        coverage.statement.total = 2;
        coverage.statement.record(StatementId(0)); // 50% statement coverage

        let overall = coverage.get_overall_percentage();
        // Only statement and branch are enabled, but no branches exist
        // Statement: 50%, Branch: 0% -> Overall: 25%
        assert_eq!(overall, 25.0);
    }

    #[test]
    fn test_fsm_coverage() {
        let mut fsm_coverage = FSMCoverage::new();

        // Add an FSM
        fsm_coverage.fsms.push(FSM {
            id: FsmId(0),
            name: "test_fsm".to_string(),
            states: vec![
                State {
                    id: StateId(0),
                    name: "IDLE".to_string(),
                },
                State {
                    id: StateId(1),
                    name: "ACTIVE".to_string(),
                },
            ],
            transitions: vec![Transition {
                id: TransitionId(0),
                from: StateId(0),
                to: StateId(1),
                condition: "start".to_string(),
            }],
        });

        // Test state coverage
        assert_eq!(fsm_coverage.get_state_percentage(), 0.0);
        fsm_coverage.record_state(FsmId(0), StateId(0));
        assert_eq!(fsm_coverage.get_state_percentage(), 50.0);
        fsm_coverage.record_state(FsmId(0), StateId(1));
        assert_eq!(fsm_coverage.get_state_percentage(), 100.0);

        // Test transition coverage
        assert_eq!(fsm_coverage.get_transition_percentage(), 0.0);
        fsm_coverage.record_transition(FsmId(0), TransitionId(0));
        assert_eq!(fsm_coverage.get_transition_percentage(), 100.0);
    }

    #[test]
    fn test_toggle_coverage() {
        let mut toggle_coverage = ToggleCoverage::new();

        // Add a signal
        toggle_coverage.signals.push(Signal {
            id: SignalId(0),
            name: "clk".to_string(),
            width: 1,
        });

        // Test initial coverage
        assert_eq!(toggle_coverage.get_percentage(), 0.0);

        // Record rising edge
        toggle_coverage.record(SignalId(0), true);
        assert_eq!(toggle_coverage.get_percentage(), 50.0);

        // Record falling edge
        toggle_coverage.record(SignalId(0), false);
        assert_eq!(toggle_coverage.get_percentage(), 100.0);
    }
}
