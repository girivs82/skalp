//! Manual CTS Control and Buffer Placement
//!
//! Interactive clock tree synthesis with manual buffer insertion

use crate::cts::{BufferStrategy as CtsBufferStrategy, ClockSpecification as CtsClockSpec};
use crate::cts::{
    ClockSource, ClockSpec, ClockTopology, ClockTree,
    ClockTreeSynthesizer,
};
use crate::placement::Placement;
use crate::routing::{CongestionMap, RoutingResult};
use crate::AsicError;
use std::collections::HashMap;

/// Manual CTS controller
pub struct ManualCTS {
    /// Clock specifications with constraints
    pub clock_specs: Vec<ClockSpecification>,
    /// Manual buffer placements
    pub manual_buffers: HashMap<String, BufferPlacement>,
    /// Clock tree constraints
    pub constraints: ClockTreeConstraints,
    /// Insertion delay targets
    pub insertion_delays: HashMap<String, f64>,
    /// Skew groups
    pub skew_groups: Vec<SkewGroup>,
    /// Buffer restrictions
    pub buffer_restrictions: BufferRestrictions,
    /// Clock tree templates
    pub templates: HashMap<String, ClockTreeTemplate>,
}

/// Enhanced clock specification
#[derive(Debug, Clone)]
pub struct ClockSpecification {
    /// Clock name
    pub name: String,
    /// Target frequency
    pub frequency: f64,
    /// Source location
    pub source: (f64, f64),
    /// Topology preference
    pub topology: TopologyPreference,
    /// Maximum skew target
    pub max_skew: f64,
    /// Maximum insertion delay
    pub max_insertion_delay: f64,
    /// Power budget
    pub power_budget: f64,
    /// Sink groups
    pub sink_groups: Vec<SinkGroup>,
}

/// Topology preference
#[derive(Debug, Clone)]
pub enum TopologyPreference {
    /// H-tree (symmetric)
    HTree { max_levels: usize },
    /// Balanced tree
    BalancedTree { target_fanout: usize },
    /// Mesh (for high frequency)
    Mesh { density: f64 },
    /// Hybrid approach
    Hybrid,
    /// Custom template
    Template { name: String },
}

/// Manual buffer placement
#[derive(Debug, Clone)]
pub struct BufferPlacement {
    /// Buffer type
    pub buffer_type: String,
    /// Position
    pub position: (f64, f64),
    /// Fixed placement
    pub fixed: bool,
    /// Drive strength override
    pub drive_strength: Option<f64>,
    /// Parent buffer
    pub parent: Option<String>,
    /// Child buffers/sinks
    pub children: Vec<String>,
}

/// Clock tree constraints
#[derive(Debug, Clone)]
pub struct ClockTreeConstraints {
    /// Global constraints
    pub global: GlobalConstraints,
    /// Per-clock constraints
    pub per_clock: HashMap<String, LocalConstraints>,
    /// Timing constraints
    pub timing: TimingConstraints,
    /// Physical constraints
    pub physical: PhysicalConstraints,
}

/// Global CTS constraints
#[derive(Debug, Clone)]
pub struct GlobalConstraints {
    /// Maximum buffer levels
    pub max_levels: usize,
    /// Maximum fanout per buffer
    pub max_fanout: usize,
    /// Minimum buffer distance
    pub min_buffer_distance: f64,
    /// Buffer library restrictions
    pub allowed_buffers: Vec<String>,
    /// Routing layer restrictions
    pub clock_layers: Vec<usize>,
}

/// Local constraints for specific clock
#[derive(Debug, Clone)]
pub struct LocalConstraints {
    /// Override global max levels
    pub max_levels: Option<usize>,
    /// Override global max fanout
    pub max_fanout: Option<usize>,
    /// Specific buffer types
    pub buffer_types: Vec<String>,
    /// Exclusion zones
    pub exclusion_zones: Vec<ExclusionZone>,
}

/// Timing constraints
#[derive(Debug, Clone)]
pub struct TimingConstraints {
    /// Setup time requirement
    pub setup_time: f64,
    /// Hold time requirement
    pub hold_time: f64,
    /// Maximum transition time
    pub max_transition: f64,
    /// Clock uncertainty
    pub uncertainty: f64,
    /// Jitter tolerance
    pub jitter: f64,
}

/// Physical constraints
#[derive(Debug, Clone)]
pub struct PhysicalConstraints {
    /// NDR (Non-Default Rules) for clock nets
    pub ndr_rules: NDRRules,
    /// Shielding requirements
    pub shielding: ShieldingSpec,
    /// Via restrictions
    pub via_rules: ViaRules,
    /// Electromigration limits
    pub em_limits: EMRules,
}

/// Non-default routing rules
#[derive(Debug, Clone)]
pub struct NDRRules {
    /// Width multiplier
    pub width_multiplier: f64,
    /// Spacing multiplier
    pub spacing_multiplier: f64,
    /// Via count
    pub via_count: usize,
}

/// Shielding specification
#[derive(Debug, Clone)]
pub struct ShieldingSpec {
    /// Enable shielding
    pub enabled: bool,
    /// Shield net (usually VSS)
    pub shield_net: String,
    /// Shield width
    pub shield_width: f64,
}

/// Via rules for clock nets
#[derive(Debug, Clone)]
pub struct ViaRules {
    /// Preferred via types
    pub preferred_vias: Vec<String>,
    /// Via array size
    pub array_size: usize,
}

/// Electromigration rules
#[derive(Debug, Clone)]
pub struct EMRules {
    /// Current density limit
    pub max_current_density: f64,
    /// Temperature derating
    pub temp_derating: f64,
}

/// Exclusion zone for buffer placement
#[derive(Debug, Clone)]
pub struct ExclusionZone {
    pub x1: f64,
    pub y1: f64,
    pub x2: f64,
    pub y2: f64,
    pub reason: String,
}

/// Sink grouping for balanced distribution
#[derive(Debug, Clone)]
pub struct SinkGroup {
    /// Group name
    pub name: String,
    /// Sink patterns
    pub patterns: Vec<String>,
    /// Target insertion delay
    pub target_delay: Option<f64>,
    /// Group weight for balancing
    pub weight: f64,
}

/// Skew group definition
#[derive(Debug, Clone)]
pub struct SkewGroup {
    /// Group name
    pub name: String,
    /// Member sinks
    pub members: Vec<String>,
    /// Maximum skew within group
    pub max_skew: f64,
    /// Priority
    pub priority: usize,
}

/// Buffer usage restrictions
#[derive(Debug, Clone)]
pub struct BufferRestrictions {
    /// Maximum buffer count
    pub max_buffers: usize,
    /// Buffer type limits
    pub type_limits: HashMap<String, usize>,
    /// Region restrictions
    pub region_limits: Vec<RegionLimit>,
}

/// Region-specific buffer limit
#[derive(Debug, Clone)]
pub struct RegionLimit {
    pub region: ExclusionZone,
    pub max_buffers: usize,
}

/// Clock tree template
#[derive(Debug, Clone)]
pub struct ClockTreeTemplate {
    /// Template name
    pub name: String,
    /// Predefined topology
    pub topology: ClockTopology,
    /// Buffer placement strategy
    pub buffer_strategy: BufferStrategy,
    /// Routing strategy
    pub routing_strategy: RoutingStrategy,
}

/// Buffer placement strategy
#[derive(Debug, Clone)]
pub enum BufferStrategy {
    /// Equal spacing
    EqualSpacing,
    /// Load-based
    LoadBalanced,
    /// Critical path aware
    TimingDriven,
    /// Custom function
    Custom,
}

/// Clock routing strategy
#[derive(Debug, Clone)]
pub enum RoutingStrategy {
    /// Minimize wire length
    MinWireLength,
    /// Minimize skew
    MinSkew,
    /// Minimize power
    MinPower,
    /// Balanced approach
    Balanced,
}

/// CTS analysis result
#[derive(Debug, Clone)]
pub struct CTSAnalysis {
    /// Skew analysis
    pub skew: SkewAnalysis,
    /// Power analysis
    pub power: PowerAnalysis,
    /// Buffer analysis
    pub buffers: BufferAnalysis,
    /// Violation report
    pub violations: Vec<CTSViolation>,
    /// Improvement suggestions
    pub suggestions: Vec<CTSSuggestion>,
}

/// Skew analysis details
#[derive(Debug, Clone)]
pub struct SkewAnalysis {
    /// Global skew
    pub global_skew: f64,
    /// Local skew per group
    pub local_skew: HashMap<String, f64>,
    /// Worst paths
    pub worst_paths: Vec<SkewPath>,
    /// Histogram data
    pub distribution: Vec<(f64, usize)>,
}

/// Skew path information
#[derive(Debug, Clone)]
pub struct SkewPath {
    pub source: String,
    pub sink: String,
    pub delay: f64,
    pub buffers: Vec<String>,
}

/// Power analysis
#[derive(Debug, Clone)]
pub struct PowerAnalysis {
    /// Total clock power
    pub total_power: f64,
    /// Buffer power
    pub buffer_power: f64,
    /// Wire power
    pub wire_power: f64,
    /// Power by level
    pub level_power: Vec<f64>,
}

/// Buffer utilization analysis
#[derive(Debug, Clone)]
pub struct BufferAnalysis {
    /// Total buffer count
    pub total_count: usize,
    /// Count by type
    pub type_count: HashMap<String, usize>,
    /// Average fanout
    pub avg_fanout: f64,
    /// Max fanout
    pub max_fanout: usize,
    /// Utilization map
    pub utilization: HashMap<String, f64>,
}

/// CTS violation
#[derive(Debug, Clone)]
pub struct CTSViolation {
    pub violation_type: ViolationType,
    pub severity: Severity,
    pub location: String,
    pub value: f64,
    pub limit: f64,
}

/// Violation types
#[derive(Debug, Clone)]
pub enum ViolationType {
    SkewViolation,
    TransitionViolation,
    FanoutViolation,
    InsertionDelayViolation,
    PowerViolation,
}

/// Severity levels
#[derive(Debug, Clone)]
pub enum Severity {
    Warning,
    Error,
    Critical,
}

/// CTS improvement suggestion
#[derive(Debug, Clone)]
pub struct CTSSuggestion {
    pub suggestion_type: SuggestionType,
    pub priority: usize,
    pub estimated_improvement: f64,
    pub description: String,
}

/// Suggestion types
#[derive(Debug, Clone)]
pub enum SuggestionType {
    AddBuffer,
    RemoveBuffer,
    MoveBuffer,
    ChangeBufferType,
    AdjustTopology,
    CreateSkewGroup,
}

impl Default for ManualCTS {
    fn default() -> Self {
        Self::new()
    }
}

impl ManualCTS {
    /// Create new manual CTS controller
    pub fn new() -> Self {
        Self {
            clock_specs: Vec::new(),
            manual_buffers: HashMap::new(),
            constraints: ClockTreeConstraints::default(),
            insertion_delays: HashMap::new(),
            skew_groups: Vec::new(),
            buffer_restrictions: BufferRestrictions::default(),
            templates: HashMap::new(),
        }
    }

    /// Add clock specification
    pub fn add_clock(&mut self, spec: ClockSpecification) {
        self.clock_specs.push(spec);
    }

    /// Place buffer manually
    pub fn place_buffer(&mut self, name: String, placement: BufferPlacement) {
        self.manual_buffers.insert(name, placement);
    }

    /// Add skew group
    pub fn add_skew_group(&mut self, group: SkewGroup) {
        self.skew_groups.push(group);
    }

    /// Set insertion delay target
    pub fn set_insertion_delay(&mut self, sink: String, delay: f64) {
        self.insertion_delays.insert(sink, delay);
    }

    /// Build clock tree with manual guidance
    pub fn build_guided_tree(&self, placement: &Placement) -> Result<ClockTree, AsicError> {
        let mut synthesizer =
            ClockTreeSynthesizer::new(crate::Technology::Sky130, crate::DesignRules::default());

        // Apply manual buffers
        for (name, buffer_placement) in &self.manual_buffers {
            self.apply_manual_buffer(&mut synthesizer, name, buffer_placement)?;
        }

        // Build tree with constraints
        let clock_spec = self.create_clock_spec()?;

        // Need routing result for synthesis - create a default one for now
        let routing = RoutingResult {
            routed_nets: Vec::new(),
            congestion: CongestionMap {
                grid_size: (10, 10),
                values: vec![vec![0.0; 10]; 10],
            },
            total_wirelength: 0.0,
            num_vias: 0,
        };

        // Convert ClockSpec to CTS ClockSpecification
        let spec = CtsClockSpec {
            source: ClockSource {
                name: clock_spec.name.clone(),
                position: clock_spec.source,
                drive_strength: 1.0,
                frequency: clock_spec.frequency,
            },
            max_skew: clock_spec.max_skew,
            target_delay: 0.1,   // Default
            max_transition: 0.2, // Default
            buffer_strategy: CtsBufferStrategy::MinSkew,
        };

        synthesizer.synthesize(placement, &routing, &spec)
    }

    /// Apply manual buffer placement
    fn apply_manual_buffer(
        &self,
        synthesizer: &mut ClockTreeSynthesizer,
        name: &str,
        placement: &BufferPlacement,
    ) -> Result<(), AsicError> {
        // TODO: Integrate with synthesizer
        Ok(())
    }

    /// Create clock specification from constraints
    fn create_clock_spec(&self) -> Result<ClockSpec, AsicError> {
        if self.clock_specs.is_empty() {
            return Err(AsicError::CtsError("No clock specifications".to_string()));
        }

        let primary = &self.clock_specs[0];
        Ok(ClockSpec {
            name: primary.name.clone(),
            frequency: primary.frequency,
            source: primary.source,
            max_skew: primary.max_skew,
            max_fanout: self.constraints.global.max_fanout,
            buffer_types: self.constraints.global.allowed_buffers.clone(),
        })
    }

    /// Analyze clock tree
    pub fn analyze(&self, clock_tree: &ClockTree) -> CTSAnalysis {
        let skew = self.analyze_skew(clock_tree);
        let power = self.analyze_power(clock_tree);
        let buffers = self.analyze_buffers(clock_tree);
        let violations = self.check_violations(clock_tree);
        let suggestions = self.generate_suggestions(clock_tree, &violations);

        CTSAnalysis {
            skew,
            power,
            buffers,
            violations,
            suggestions,
        }
    }

    /// Analyze skew
    fn analyze_skew(&self, clock_tree: &ClockTree) -> SkewAnalysis {
        let worst_paths = Vec::new();
        let mut delays: Vec<f64> = Vec::new();

        // Use sink_slack to estimate delays
        for slack in clock_tree.timing.sink_slack.values() {
            // Convert slack to effective delay (assuming period - slack)
            delays.push(clock_tree.timing.period - slack);
        }

        delays.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let global_skew = if !delays.is_empty() {
            delays[delays.len() - 1] - delays[0]
        } else {
            0.0
        };

        // Create delay histogram
        let mut distribution = Vec::new();
        if !delays.is_empty() {
            let min = delays[0];
            let max = delays[delays.len() - 1];
            let bins = 10;
            let bin_width = (max - min) / bins as f64;

            for i in 0..bins {
                let bin_start = min + i as f64 * bin_width;
                let bin_end = bin_start + bin_width;
                let count = delays
                    .iter()
                    .filter(|&&d| d >= bin_start && d < bin_end)
                    .count();
                distribution.push((bin_start, count));
            }
        }

        SkewAnalysis {
            global_skew,
            local_skew: HashMap::new(),
            worst_paths,
            distribution,
        }
    }

    /// Analyze power
    fn analyze_power(&self, clock_tree: &ClockTree) -> PowerAnalysis {
        PowerAnalysis {
            total_power: clock_tree.timing.power,
            buffer_power: clock_tree.timing.power * 0.7, // Estimate
            wire_power: clock_tree.timing.power * 0.3,
            level_power: Vec::new(),
        }
    }

    /// Analyze buffer utilization
    fn analyze_buffers(&self, clock_tree: &ClockTree) -> BufferAnalysis {
        let mut type_count = HashMap::new();
        for buffer in &clock_tree.buffers {
            *type_count.entry(buffer.cell_type.clone()).or_insert(0) += 1;
        }

        BufferAnalysis {
            total_count: clock_tree.buffers.len(),
            type_count,
            avg_fanout: 0.0, // TODO: Calculate
            max_fanout: 0,
            utilization: HashMap::new(),
        }
    }

    /// Check for violations
    fn check_violations(&self, clock_tree: &ClockTree) -> Vec<CTSViolation> {
        let mut violations = Vec::new();

        // Check skew violations
        if clock_tree.timing.max_skew > self.clock_specs[0].max_skew {
            violations.push(CTSViolation {
                violation_type: ViolationType::SkewViolation,
                severity: Severity::Error,
                location: "Global".to_string(),
                value: clock_tree.timing.max_skew,
                limit: self.clock_specs[0].max_skew,
            });
        }

        violations
    }

    /// Generate improvement suggestions
    fn generate_suggestions(
        &self,
        _clock_tree: &ClockTree,
        violations: &[CTSViolation],
    ) -> Vec<CTSSuggestion> {
        let mut suggestions = Vec::new();

        for violation in violations {
            if let ViolationType::SkewViolation = violation.violation_type {
                suggestions.push(CTSSuggestion {
                    suggestion_type: SuggestionType::AddBuffer,
                    priority: 1,
                    estimated_improvement: 0.1,
                    description: "Add buffer to balance delays".to_string(),
                });
            }
        }

        suggestions
    }

    /// Export CTS script
    pub fn export_script(&self, clock_tree: &ClockTree) -> String {
        let mut script = String::new();

        script.push_str("# Clock Tree Synthesis Script\n");
        script.push_str("# Generated by SKALP\n\n");

        // Clock definition
        for spec in &self.clock_specs {
            script.push_str(&format!(
                "create_clock -name {} -period {:.3}\n",
                spec.name,
                1.0 / spec.frequency
            ));
        }

        // Constraints
        script.push_str("\n# CTS Constraints\n");
        script.push_str(&format!(
            "set_clock_tree_options -max_skew {:.3}\n",
            self.clock_specs[0].max_skew
        ));

        // Manual buffers
        if !self.manual_buffers.is_empty() {
            script.push_str("\n# Manual Buffer Placements\n");
            for (name, placement) in &self.manual_buffers {
                script.push_str(&format!(
                    "place_buffer {} -location ({:.2}, {:.2})\n",
                    name, placement.position.0, placement.position.1
                ));
            }
        }

        script
    }
}

impl Default for ClockTreeConstraints {
    fn default() -> Self {
        Self {
            global: GlobalConstraints {
                max_levels: 10,
                max_fanout: 20,
                min_buffer_distance: 50.0,
                allowed_buffers: vec!["BUF_X1".to_string(), "BUF_X2".to_string()],
                clock_layers: vec![3, 4, 5],
            },
            per_clock: HashMap::new(),
            timing: TimingConstraints {
                setup_time: 0.1,
                hold_time: 0.05,
                max_transition: 0.5,
                uncertainty: 0.05,
                jitter: 0.02,
            },
            physical: PhysicalConstraints {
                ndr_rules: NDRRules {
                    width_multiplier: 2.0,
                    spacing_multiplier: 2.0,
                    via_count: 2,
                },
                shielding: ShieldingSpec {
                    enabled: false,
                    shield_net: "VSS".to_string(),
                    shield_width: 0.2,
                },
                via_rules: ViaRules {
                    preferred_vias: Vec::new(),
                    array_size: 2,
                },
                em_limits: EMRules {
                    max_current_density: 1.0,
                    temp_derating: 0.9,
                },
            },
        }
    }
}

impl Default for BufferRestrictions {
    fn default() -> Self {
        Self {
            max_buffers: 1000,
            type_limits: HashMap::new(),
            region_limits: Vec::new(),
        }
    }
}
