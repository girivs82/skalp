//! Manual Routing Guidance and Congestion Management
//!
//! Provides interactive routing control with congestion-aware guidance

use crate::placement::Placement;
use crate::routing::{Router, RoutingResult};
use crate::AsicError;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};

/// Routing guidance system
pub struct RoutingGuide {
    /// Manual routing constraints
    pub manual_routes: HashMap<String, Vec<RouteConstraint>>,
    /// Congestion thresholds
    pub congestion_limits: CongestionLimits,
    /// Preferred routing layers
    pub layer_preferences: LayerPreferences,
    /// Routing blockages
    pub blockages: Vec<RoutingBlockage>,
    /// Critical nets
    pub critical_nets: HashSet<String>,
    /// Congestion history
    pub congestion_history: Vec<CongestionSnapshot>,
}

/// Route constraint for manual guidance
#[derive(Debug, Clone)]
pub struct RouteConstraint {
    /// Constraint type
    pub constraint_type: RouteConstraintType,
    /// Affected layers
    pub layers: Vec<usize>,
    /// Constraint region
    pub region: Region,
    /// Priority
    pub priority: usize,
}

/// Types of routing constraints
#[derive(Debug, Clone)]
pub enum RouteConstraintType {
    /// Must route through region
    MustRoute,
    /// Avoid routing through region
    Avoid,
    /// Preferred routing path
    Preferred { weight: f64 },
    /// Fixed routing segment
    Fixed { path: Vec<(f64, f64)> },
    /// Layer restriction
    LayerOnly,
}

/// Region definition
#[derive(Debug, Clone)]
pub struct Region {
    pub x1: f64,
    pub y1: f64,
    pub x2: f64,
    pub y2: f64,
}

/// Congestion limits
#[derive(Debug, Clone)]
pub struct CongestionLimits {
    /// Warning threshold (0.0 - 1.0)
    pub warning: f64,
    /// Critical threshold
    pub critical: f64,
    /// Maximum allowed congestion
    pub maximum: f64,
    /// Per-layer limits
    pub layer_limits: HashMap<usize, f64>,
}

/// Layer routing preferences
#[derive(Debug, Clone)]
pub struct LayerPreferences {
    /// Preferred direction per layer
    pub directions: HashMap<usize, Direction>,
    /// Layer usage weights
    pub weights: HashMap<usize, f64>,
    /// Reserved layers for special nets
    pub reserved: HashMap<usize, Vec<String>>,
}

/// Routing direction
#[derive(Debug, Clone, PartialEq)]
pub enum Direction {
    Horizontal,
    Vertical,
    Both,
}

/// Routing blockage
#[derive(Debug, Clone)]
pub struct RoutingBlockage {
    /// Blockage region
    pub region: Region,
    /// Affected layers
    pub layers: Vec<usize>,
    /// Blockage type
    pub blockage_type: BlockageType,
}

/// Blockage types
#[derive(Debug, Clone)]
pub enum BlockageType {
    /// Complete blockage
    Hard,
    /// Partial blockage with penalty
    Soft { penalty: f64 },
    /// Density restriction
    Density { max_utilization: f64 },
}

/// Congestion snapshot for history
#[derive(Debug, Clone)]
pub struct CongestionSnapshot {
    /// Timestamp
    pub timestamp: std::time::Instant,
    /// Iteration number
    pub iteration: usize,
    /// Peak congestion
    pub peak: f64,
    /// Average congestion
    pub average: f64,
    /// Congested regions
    pub hotspots: Vec<CongestionHotspot>,
}

/// Congestion hotspot
#[derive(Debug, Clone)]
pub struct CongestionHotspot {
    pub region: Region,
    pub congestion: f64,
    pub nets: Vec<String>,
    pub layer: usize,
}

/// Interactive routing controller
pub struct InteractiveRouter {
    /// Base router
    pub router: Router,
    /// Routing guidance
    pub guide: RoutingGuide,
    /// Reroute queue
    pub reroute_queue: BinaryHeap<RerouteTask>,
    /// Routing history
    pub history: RoutingHistory,
}

/// Reroute task with priority
#[derive(Debug, Clone)]
pub struct RerouteTask {
    pub net: String,
    pub priority: f64,
    pub reason: RerouteReason,
}

impl Eq for RerouteTask {}

impl PartialEq for RerouteTask {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl Ord for RerouteTask {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority
            .partial_cmp(&other.priority)
            .unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for RerouteTask {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

/// Reason for rerouting
#[derive(Debug, Clone)]
pub enum RerouteReason {
    Congestion,
    TimingViolation,
    ManualRequest,
    DRCViolation,
    PowerOptimization,
}

/// Routing history for undo/redo
#[derive(Debug, Clone)]
pub struct RoutingHistory {
    /// Past routing states
    pub past: Vec<RoutingResult>,
    /// Future states for redo
    pub future: Vec<RoutingResult>,
    /// Maximum history size
    pub max_size: usize,
}

impl Default for RoutingGuide {
    fn default() -> Self {
        Self::new()
    }
}

impl RoutingGuide {
    /// Create new routing guide
    pub fn new() -> Self {
        Self {
            manual_routes: HashMap::new(),
            congestion_limits: CongestionLimits::default(),
            layer_preferences: LayerPreferences::default(),
            blockages: Vec::new(),
            critical_nets: HashSet::new(),
            congestion_history: Vec::new(),
        }
    }

    /// Add manual route constraint
    pub fn add_constraint(&mut self, net: String, constraint: RouteConstraint) {
        self.manual_routes
            .entry(net)
            .or_default()
            .push(constraint);
    }

    /// Add routing blockage
    pub fn add_blockage(&mut self, blockage: RoutingBlockage) {
        self.blockages.push(blockage);
    }

    /// Mark net as critical
    pub fn mark_critical(&mut self, net: String) {
        self.critical_nets.insert(net);
    }

    /// Check if point is in blocked region
    pub fn is_blocked(&self, x: f64, y: f64, layer: usize) -> bool {
        for blockage in &self.blockages {
            if blockage.layers.contains(&layer) && self.in_region(x, y, &blockage.region) {
                if let BlockageType::Hard = blockage.blockage_type { return true }
            }
        }
        false
    }

    /// Get routing penalty for position
    pub fn get_penalty(&self, x: f64, y: f64, layer: usize) -> f64 {
        let mut penalty = 1.0;

        // Check blockages
        for blockage in &self.blockages {
            if blockage.layers.contains(&layer) && self.in_region(x, y, &blockage.region) {
                if let BlockageType::Soft { penalty: p } = &blockage.blockage_type { penalty *= p }
            }
        }

        // Check layer preferences
        if let Some(weight) = self.layer_preferences.weights.get(&layer) {
            penalty *= 1.0 / weight;
        }

        penalty
    }

    /// Check if point is in region
    fn in_region(&self, x: f64, y: f64, region: &Region) -> bool {
        x >= region.x1 && x <= region.x2 && y >= region.y1 && y <= region.y2
    }

    /// Analyze congestion and suggest fixes
    pub fn analyze_congestion(&mut self, routing: &RoutingResult) -> Vec<CongestionFix> {
        let mut fixes = Vec::new();
        let snapshot = self.create_congestion_snapshot(routing);

        // Find hotspots
        for hotspot in &snapshot.hotspots {
            if hotspot.congestion > self.congestion_limits.critical {
                fixes.push(CongestionFix {
                    region: hotspot.region.clone(),
                    severity: FixSeverity::Critical,
                    suggestions: self.generate_fix_suggestions(hotspot),
                });
            }
        }

        self.congestion_history.push(snapshot);
        fixes
    }

    /// Create congestion snapshot
    fn create_congestion_snapshot(&self, routing: &RoutingResult) -> CongestionSnapshot {
        let mut hotspots = Vec::new();
        let mut total_congestion = 0.0;
        let mut count = 0;
        let mut peak = 0.0f64;

        // Analyze congestion grid
        {
            let congestion = &routing.congestion;
            for y in 0..congestion.grid_size.0 {
                for x in 0..congestion.grid_size.1 {
                    let value = congestion.values[y][x];
                    total_congestion += value;
                    count += 1;
                    peak = peak.max(value);

                    if value > self.congestion_limits.warning {
                        // Identify affected nets
                        let affected_nets = self.find_nets_in_region(
                            x as f64 * 10.0,
                            y as f64 * 10.0,
                            (x + 1) as f64 * 10.0,
                            (y + 1) as f64 * 10.0,
                            routing,
                        );

                        hotspots.push(CongestionHotspot {
                            region: Region {
                                x1: x as f64 * 10.0,
                                y1: y as f64 * 10.0,
                                x2: (x + 1) as f64 * 10.0,
                                y2: (y + 1) as f64 * 10.0,
                            },
                            congestion: value,
                            nets: affected_nets,
                            layer: 0, // TODO: Per-layer analysis
                        });
                    }
                }
            }
        }

        CongestionSnapshot {
            timestamp: std::time::Instant::now(),
            iteration: self.congestion_history.len(),
            peak,
            average: if count > 0 {
                total_congestion / count as f64
            } else {
                0.0
            },
            hotspots,
        }
    }

    /// Find nets in region
    fn find_nets_in_region(
        &self,
        x1: f64,
        y1: f64,
        x2: f64,
        y2: f64,
        routing: &RoutingResult,
    ) -> Vec<String> {
        let mut nets = Vec::new();

        for routed_net in &routing.routed_nets {
            for segment in &routed_net.segments {
                // Check if any point in the segment is in the region
                for point in &segment.points {
                    if point.0 >= x1 && point.0 <= x2 && point.1 >= y1 && point.1 <= y2 {
                        nets.push(routed_net.name.clone());
                        break;
                    }
                }
            }
        }

        nets
    }

    /// Generate fix suggestions
    fn generate_fix_suggestions(&self, hotspot: &CongestionHotspot) -> Vec<FixSuggestion> {
        let mut suggestions = Vec::new();

        // Suggest rerouting
        suggestions.push(FixSuggestion::Reroute {
            nets: hotspot.nets.clone(),
            alternative_layers: vec![hotspot.layer + 1, hotspot.layer + 2],
        });

        // Suggest spreading
        suggestions.push(FixSuggestion::SpreadRoutes {
            region: hotspot.region.clone(),
            spread_factor: 1.5,
        });

        // Suggest layer change
        if hotspot.layer < 5 {
            suggestions.push(FixSuggestion::ChangeLayer {
                from_layer: hotspot.layer,
                to_layer: hotspot.layer + 1,
                nets: hotspot.nets.clone(),
            });
        }

        suggestions
    }
}

/// Congestion fix suggestion
#[derive(Debug, Clone)]
pub struct CongestionFix {
    pub region: Region,
    pub severity: FixSeverity,
    pub suggestions: Vec<FixSuggestion>,
}

/// Fix severity levels
#[derive(Debug, Clone)]
pub enum FixSeverity {
    Warning,
    Critical,
    Violation,
}

/// Specific fix suggestions
#[derive(Debug, Clone)]
pub enum FixSuggestion {
    Reroute {
        nets: Vec<String>,
        alternative_layers: Vec<usize>,
    },
    SpreadRoutes {
        region: Region,
        spread_factor: f64,
    },
    ChangeLayer {
        from_layer: usize,
        to_layer: usize,
        nets: Vec<String>,
    },
    AddBlockage {
        region: Region,
        reason: String,
    },
}

impl InteractiveRouter {
    /// Create new interactive router
    pub fn new(router: Router) -> Self {
        Self {
            router,
            guide: RoutingGuide::new(),
            reroute_queue: BinaryHeap::new(),
            history: RoutingHistory {
                past: Vec::new(),
                future: Vec::new(),
                max_size: 50,
            },
        }
    }

    /// Perform guided routing
    pub fn route_guided(&mut self, placement: &Placement) -> Result<RoutingResult, AsicError> {
        // Save current state
        let initial_result = self.router.route(placement)?;
        self.save_state(initial_result.clone());

        // Apply manual constraints
        let mut result = self.apply_manual_routes(initial_result)?;

        // Fix congestion iteratively
        let mut iteration = 0;
        while iteration < 10 {
            let fixes = self.guide.analyze_congestion(&result);

            if fixes.is_empty() {
                break;
            }

            // Apply fixes
            for fix in fixes {
                self.apply_congestion_fix(&mut result, fix)?;
            }

            iteration += 1;
        }

        // Process reroute queue
        while let Some(task) = self.reroute_queue.pop() {
            self.reroute_net(&mut result, &task)?;
        }

        Ok(result)
    }

    /// Apply manual routes
    fn apply_manual_routes(&self, mut result: RoutingResult) -> Result<RoutingResult, AsicError> {
        for (net, constraints) in &self.guide.manual_routes {
            for constraint in constraints {
                match &constraint.constraint_type {
                    RouteConstraintType::Fixed { path } => {
                        // Replace with fixed path - find matching net and update
                        if let Some(routed_net) =
                            result.routed_nets.iter_mut().find(|n| n.name == *net)
                        {
                            // Convert path to wire segments
                            let segments = self.create_segments_from_path(path);
                            routed_net.segments = segments;
                        }
                    }
                    _ => {
                        // Apply as routing hint
                    }
                }
            }
        }
        Ok(result)
    }

    /// Create segments from path
    fn create_segments_from_path(&self, path: &[(f64, f64)]) -> Vec<crate::routing::WireSegment> {
        use crate::routing::WireSegment;
        vec![WireSegment {
            points: path.to_vec(),
            layer: 1, // Default layer
            width: 0.1,
        }]
    }

    /// Apply congestion fix
    fn apply_congestion_fix(
        &mut self,
        result: &mut RoutingResult,
        fix: CongestionFix,
    ) -> Result<(), AsicError> {
        for suggestion in fix.suggestions {
            match suggestion {
                FixSuggestion::Reroute { nets, .. } => {
                    for net in nets {
                        self.reroute_queue.push(RerouteTask {
                            net: net.clone(),
                            priority: 1.0,
                            reason: RerouteReason::Congestion,
                        });
                    }
                }
                FixSuggestion::ChangeLayer { nets, to_layer, .. } => {
                    for net in nets {
                        // Find the net in routed_nets
                        if let Some(routed_net) =
                            result.routed_nets.iter_mut().find(|n| n.name == net)
                        {
                            for segment in &mut routed_net.segments {
                                segment.layer = to_layer;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Reroute specific net
    fn reroute_net(
        &mut self,
        result: &mut RoutingResult,
        task: &RerouteTask,
    ) -> Result<(), AsicError> {
        // Remove existing route
        // Remove the net from routed_nets
        result.routed_nets.retain(|n| n.name != task.net);

        // Implement intelligent rerouting
        let rerouted_net = self.reroute_net_intelligently(&task.net, result)?;
        if let Some(net) = rerouted_net {
            result.routed_nets.push(net);
        }

        Ok(())
    }

    /// Save routing state
    fn save_state(&mut self, state: RoutingResult) {
        self.history.past.push(state);
        if self.history.past.len() > self.history.max_size {
            self.history.past.remove(0);
        }
        self.history.future.clear();
    }

    /// Undo routing operation
    pub fn undo(&mut self) -> Option<RoutingResult> {
        if let Some(state) = self.history.past.pop() {
            if let Some(current) = self.history.past.last() {
                self.history.future.push(state);
                return Some(current.clone());
            }
        }
        None
    }

    /// Redo routing operation
    pub fn redo(&mut self) -> Option<RoutingResult> {
        if let Some(state) = self.history.future.pop() {
            self.history.past.push(state.clone());
            return Some(state);
        }
        None
    }

    /// Generate routing report
    pub fn generate_report(&self, result: &RoutingResult) -> String {
        let mut report = String::new();

        report.push_str("=== ROUTING REPORT ===\n\n");

        // Congestion analysis
        if let Some(snapshot) = self.guide.congestion_history.last() {
            report.push_str(&format!("Peak Congestion: {:.1}%\n", snapshot.peak * 100.0));
            report.push_str(&format!(
                "Average Congestion: {:.1}%\n",
                snapshot.average * 100.0
            ));
            report.push_str(&format!("Hotspots: {}\n\n", snapshot.hotspots.len()));
        }

        // Net statistics
        report.push_str(&format!("Total Nets: {}\n", result.routed_nets.len()));
        report.push_str(&format!(
            "Critical Nets: {}\n",
            self.guide.critical_nets.len()
        ));

        // Manual routes
        let manual_count = self.guide.manual_routes.len();
        report.push_str(&format!("Manual Routes: {}\n", manual_count));

        // Blockages
        report.push_str(&format!("Blockages: {}\n", self.guide.blockages.len()));

        report
    }

    /// Intelligently reroute a net avoiding congestion
    fn reroute_net_intelligently(
        &self,
        net_name: &str,
        routing: &RoutingResult,
    ) -> Result<Option<crate::routing::RoutedNet>, AsicError> {
        // Analyze current congestion to find alternative paths
        let congestion = &routing.congestion;

        // Find less congested layers for rerouting
        let preferred_layers = self.find_less_congested_layers(congestion);

        // Create a new routed net with optimized layer assignment
        let new_net = crate::routing::RoutedNet {
            name: net_name.to_string(),
            segments: vec![
                // Simplified - in practice would use detailed pathfinding
                crate::routing::WireSegment {
                    points: vec![(0.0, 0.0), (10.0, 10.0)],
                    layer: preferred_layers.first().copied().unwrap_or(1),
                    width: 0.14, // Min width for SKY130
                },
            ],
            vias: Vec::new(),
        };

        Ok(Some(new_net))
    }

    /// Find layers with lower congestion
    fn find_less_congested_layers(&self, congestion: &crate::routing::CongestionMap) -> Vec<usize> {
        let mut layer_congestion = vec![(0, 0.0f64); 5]; // M1-M5

        // Calculate average congestion per layer (simplified)
        for (y, row) in congestion.values.iter().enumerate() {
            for (x, &value) in row.iter().enumerate() {
                // Distribute across layers (simplified model)
                for layer in 0..5 {
                    layer_congestion[layer].1 += value / 5.0;
                }
            }
        }

        // Sort by congestion level
        layer_congestion.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        // Return layers in order of increasing congestion
        layer_congestion
            .into_iter()
            .map(|(layer, _)| layer + 1)
            .collect()
    }
}

impl Default for CongestionLimits {
    fn default() -> Self {
        Self {
            warning: 0.7,
            critical: 0.85,
            maximum: 0.95,
            layer_limits: HashMap::new(),
        }
    }
}

impl Default for LayerPreferences {
    fn default() -> Self {
        let mut directions = HashMap::new();
        directions.insert(1, Direction::Horizontal);
        directions.insert(2, Direction::Vertical);
        directions.insert(3, Direction::Horizontal);
        directions.insert(4, Direction::Vertical);
        directions.insert(5, Direction::Horizontal);

        Self {
            directions,
            weights: HashMap::new(),
            reserved: HashMap::new(),
        }
    }
}
