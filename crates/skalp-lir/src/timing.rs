//! Timing analysis for LIR
//!
//! Static timing analysis (STA) for gate-level netlists

use crate::lir::{GateType, Lir};
use std::collections::{HashMap, HashSet, VecDeque};

/// Timing node in the timing graph
#[derive(Debug, Clone)]
pub struct TimingNode {
    /// Node identifier (net name)
    pub id: String,
    /// Arrival time (picoseconds)
    pub arrival_time: f64,
    /// Required time (picoseconds)
    pub required_time: f64,
    /// Slack (required - arrival)
    pub slack: f64,
    /// Is this a register output?
    pub is_register: bool,
    /// Is this a primary input?
    pub is_input: bool,
    /// Is this a primary output?
    pub is_output: bool,
}

/// Timing arc between nodes
#[derive(Debug, Clone)]
pub struct TimingArc {
    /// Source node
    pub from: String,
    /// Destination node
    pub to: String,
    /// Propagation delay (picoseconds)
    pub delay: f64,
    /// Gate that creates this arc
    pub gate: String,
}

/// Timing graph for analysis
#[derive(Debug)]
pub struct TimingGraph {
    /// Nodes in the graph
    pub nodes: HashMap<String, TimingNode>,
    /// Forward arcs (from input to output)
    pub forward_arcs: HashMap<String, Vec<TimingArc>>,
    /// Backward arcs (from output to input)
    pub backward_arcs: HashMap<String, Vec<TimingArc>>,
}

/// Timing analysis engine
pub struct TimingAnalyzer {
    /// Timing graph
    graph: TimingGraph,
    /// Clock period (picoseconds)
    clock_period: f64,
    /// Setup time for registers (picoseconds)
    setup_time: f64,
    /// Hold time for registers (picoseconds)
    hold_time: f64,
    /// Gate delay library
    delay_library: HashMap<GateType, f64>,
}

impl TimingAnalyzer {
    /// Create a new timing analyzer
    pub fn new(clock_period: f64) -> Self {
        let mut delay_library = HashMap::new();

        // Default gate delays in picoseconds
        delay_library.insert(GateType::And, 50.0);
        delay_library.insert(GateType::Or, 50.0);
        delay_library.insert(GateType::Not, 30.0);
        delay_library.insert(GateType::Nand, 40.0);
        delay_library.insert(GateType::Nor, 40.0);
        delay_library.insert(GateType::Xor, 60.0);
        delay_library.insert(GateType::Xnor, 60.0);
        delay_library.insert(GateType::Buffer, 20.0);
        delay_library.insert(GateType::DFF, 100.0);
        delay_library.insert(GateType::Latch, 80.0);

        Self {
            graph: TimingGraph {
                nodes: HashMap::new(),
                forward_arcs: HashMap::new(),
                backward_arcs: HashMap::new(),
            },
            clock_period,
            setup_time: 50.0, // Default setup time
            hold_time: 30.0,  // Default hold time
            delay_library,
        }
    }

    /// Build timing graph from LIR
    pub fn build_graph(&mut self, lir: &Lir) {
        // Create nodes for all nets
        for net in &lir.nets {
            let node = TimingNode {
                id: net.id.clone(),
                arrival_time: 0.0,
                required_time: self.clock_period,
                slack: self.clock_period,
                is_register: false,
                is_input: net.id.starts_with("in_") || net.id.starts_with("input"),
                is_output: net.id.starts_with("out_") || net.id.starts_with("output"),
            };
            self.graph.nodes.insert(net.id.clone(), node);
        }

        // Create timing arcs for gates
        for gate in &lir.gates {
            let delay = self.get_gate_delay(&gate.gate_type);

            // Mark register outputs
            if gate.gate_type == GateType::DFF {
                for output in &gate.outputs {
                    if let Some(node) = self.graph.nodes.get_mut(output) {
                        node.is_register = true;
                    }
                }
            }

            // Create arcs from each input to each output
            for input in &gate.inputs {
                for output in &gate.outputs {
                    let arc = TimingArc {
                        from: input.clone(),
                        to: output.clone(),
                        delay,
                        gate: gate.id.clone(),
                    };

                    // Add to forward arcs
                    self.graph
                        .forward_arcs
                        .entry(input.clone())
                        .or_insert_with(Vec::new)
                        .push(arc.clone());

                    // Add to backward arcs
                    self.graph
                        .backward_arcs
                        .entry(output.clone())
                        .or_insert_with(Vec::new)
                        .push(arc);
                }
            }
        }
    }

    /// Perform static timing analysis
    pub fn analyze(&mut self) -> TimingReport {
        // Forward pass - compute arrival times
        self.compute_arrival_times();

        // Backward pass - compute required times
        self.compute_required_times();

        // Compute slacks
        self.compute_slacks();

        // Find critical path
        let critical_path = self.find_critical_path();

        // Generate report
        TimingReport {
            clock_period: self.clock_period,
            critical_path_delay: critical_path.total_delay,
            critical_path: critical_path.path,
            setup_violations: self.find_setup_violations(),
            hold_violations: self.find_hold_violations(),
            worst_slack: self.find_worst_slack(),
            histogram: self.generate_slack_histogram(),
        }
    }

    /// Compute arrival times (forward pass)
    fn compute_arrival_times(&mut self) {
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        // Start from primary inputs and register outputs
        for (id, node) in &self.graph.nodes {
            if node.is_input || node.is_register {
                queue.push_back(id.clone());
                visited.insert(id.clone());
            }
        }

        // Propagate arrival times
        while let Some(node_id) = queue.pop_front() {
            let arrival = self.graph.nodes[&node_id].arrival_time;

            if let Some(arcs) = self.graph.forward_arcs.get(&node_id) {
                for arc in arcs {
                    let new_arrival = arrival + arc.delay;

                    if let Some(dest_node) = self.graph.nodes.get_mut(&arc.to) {
                        // Update arrival time if this path is longer
                        if new_arrival > dest_node.arrival_time {
                            dest_node.arrival_time = new_arrival;
                        }

                        // Add to queue if not visited
                        if !visited.contains(&arc.to) {
                            queue.push_back(arc.to.clone());
                            visited.insert(arc.to.clone());
                        }
                    }
                }
            }
        }
    }

    /// Compute required times (backward pass)
    fn compute_required_times(&mut self) {
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        // Start from primary outputs and register inputs
        let output_nodes: Vec<_> = self
            .graph
            .nodes
            .iter()
            .filter(|(_, node)| node.is_output)
            .map(|(id, _)| id.clone())
            .collect();

        for id in output_nodes {
            queue.push_back(id.clone());
            visited.insert(id.clone());
            // Set required time to clock period for outputs
            if let Some(n) = self.graph.nodes.get_mut(&id) {
                n.required_time = self.clock_period - self.setup_time;
            }
        }

        // Find register inputs and set their required times
        let register_nodes: Vec<_> = self
            .graph
            .backward_arcs
            .keys()
            .filter(|id| self.graph.nodes.get(*id).map_or(false, |n| n.is_register))
            .cloned()
            .collect();

        for id in register_nodes {
            queue.push_back(id.clone());
            visited.insert(id.clone());
            if let Some(n) = self.graph.nodes.get_mut(&id) {
                n.required_time = self.clock_period - self.setup_time;
            }
        }

        // Propagate required times backward
        while let Some(node_id) = queue.pop_front() {
            let required = self.graph.nodes[&node_id].required_time;

            if let Some(arcs) = self.graph.backward_arcs.get(&node_id) {
                for arc in arcs {
                    let new_required = required - arc.delay;

                    if let Some(source_node) = self.graph.nodes.get_mut(&arc.from) {
                        // Update required time if this requirement is tighter
                        if new_required < source_node.required_time {
                            source_node.required_time = new_required;
                        }

                        // Add to queue if not visited
                        if !visited.contains(&arc.from) {
                            queue.push_back(arc.from.clone());
                            visited.insert(arc.from.clone());
                        }
                    }
                }
            }
        }
    }

    /// Compute slacks
    fn compute_slacks(&mut self) {
        for node in self.graph.nodes.values_mut() {
            node.slack = node.required_time - node.arrival_time;
        }
    }

    /// Find the critical path
    fn find_critical_path(&self) -> CriticalPath {
        let mut path = Vec::new();
        let mut total_delay = 0.0;

        // Find the node with worst slack
        let mut current = self
            .graph
            .nodes
            .values()
            .filter(|n| n.is_output || n.is_register)
            .min_by(|a, b| a.slack.partial_cmp(&b.slack).unwrap())
            .map(|n| n.id.clone());

        // Trace back the critical path
        while let Some(node_id) = current {
            path.push(node_id.clone());

            // Find the predecessor with worst slack
            current = self.graph.backward_arcs.get(&node_id).and_then(|arcs| {
                arcs.iter()
                    .map(|arc| &arc.from)
                    .min_by_key(|id| {
                        self.graph
                            .nodes
                            .get(*id)
                            .map(|n| (n.slack * 1000.0) as i64)
                            .unwrap_or(i64::MAX)
                    })
                    .cloned()
            });

            let node = &self.graph.nodes[&node_id];
            if node.is_input || node.is_register {
                break;
            }
        }

        path.reverse();

        // Calculate total delay
        if path.len() > 1 {
            for i in 0..path.len() - 1 {
                if let Some(arcs) = self.graph.forward_arcs.get(&path[i]) {
                    for arc in arcs {
                        if arc.to == path[i + 1] {
                            total_delay += arc.delay;
                            break;
                        }
                    }
                }
            }
        }

        CriticalPath { path, total_delay }
    }

    /// Find setup violations
    fn find_setup_violations(&self) -> Vec<TimingViolation> {
        let mut violations = Vec::new();

        for node in self.graph.nodes.values() {
            if node.is_register && node.slack < 0.0 {
                violations.push(TimingViolation {
                    net: node.id.clone(),
                    violation_type: ViolationType::Setup,
                    slack: node.slack,
                    required: node.required_time,
                    actual: node.arrival_time,
                });
            }
        }

        violations
    }

    /// Find hold violations
    fn find_hold_violations(&self) -> Vec<TimingViolation> {
        // Simplified hold check - would need more sophisticated analysis in practice
        Vec::new()
    }

    /// Find worst slack
    fn find_worst_slack(&self) -> f64 {
        self.graph
            .nodes
            .values()
            .map(|n| n.slack)
            .min_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap_or(0.0)
    }

    /// Generate slack histogram
    fn generate_slack_histogram(&self) -> SlackHistogram {
        let mut histogram = SlackHistogram::new();

        for node in self.graph.nodes.values() {
            histogram.add_slack(node.slack);
        }

        histogram
    }

    /// Get gate delay from library
    fn get_gate_delay(&self, gate_type: &GateType) -> f64 {
        *self.delay_library.get(gate_type).unwrap_or(&50.0)
    }
}

/// Critical path information
#[derive(Debug)]
pub struct CriticalPath {
    /// Nodes on the critical path
    pub path: Vec<String>,
    /// Total delay of the critical path
    pub total_delay: f64,
}

/// Timing violation
#[derive(Debug)]
pub struct TimingViolation {
    /// Net with violation
    pub net: String,
    /// Type of violation
    pub violation_type: ViolationType,
    /// Slack (negative for violations)
    pub slack: f64,
    /// Required time
    pub required: f64,
    /// Actual time
    pub actual: f64,
}

/// Type of timing violation
#[derive(Debug)]
pub enum ViolationType {
    Setup,
    Hold,
}

/// Slack histogram for timing analysis
#[derive(Debug)]
pub struct SlackHistogram {
    /// Buckets for slack values
    pub buckets: Vec<(f64, f64, usize)>, // (min, max, count)
}

impl SlackHistogram {
    /// Create a new histogram
    pub fn new() -> Self {
        Self {
            buckets: vec![
                (f64::NEG_INFINITY, -100.0, 0),
                (-100.0, -50.0, 0),
                (-50.0, -20.0, 0),
                (-20.0, -10.0, 0),
                (-10.0, 0.0, 0),
                (0.0, 10.0, 0),
                (10.0, 20.0, 0),
                (20.0, 50.0, 0),
                (50.0, 100.0, 0),
                (100.0, f64::INFINITY, 0),
            ],
        }
    }

    /// Add a slack value to the histogram
    pub fn add_slack(&mut self, slack: f64) {
        for bucket in &mut self.buckets {
            if slack >= bucket.0 && slack < bucket.1 {
                bucket.2 += 1;
                break;
            }
        }
    }
}

/// Timing analysis report
#[derive(Debug)]
pub struct TimingReport {
    /// Clock period
    pub clock_period: f64,
    /// Critical path delay
    pub critical_path_delay: f64,
    /// Critical path nodes
    pub critical_path: Vec<String>,
    /// Setup violations
    pub setup_violations: Vec<TimingViolation>,
    /// Hold violations
    pub hold_violations: Vec<TimingViolation>,
    /// Worst slack
    pub worst_slack: f64,
    /// Slack histogram
    pub histogram: SlackHistogram,
}

impl TimingReport {
    /// Print the timing report
    pub fn print(&self) {
        println!("=== Timing Analysis Report ===");
        println!("Clock Period: {:.2} ps", self.clock_period);
        println!("Critical Path Delay: {:.2} ps", self.critical_path_delay);
        println!("Worst Slack: {:.2} ps", self.worst_slack);

        if self.worst_slack < 0.0 {
            println!("WARNING: Timing violations detected!");
        }

        println!("\nCritical Path:");
        for (i, node) in self.critical_path.iter().enumerate() {
            println!("  {}: {}", i, node);
        }

        if !self.setup_violations.is_empty() {
            println!("\nSetup Violations:");
            for violation in &self.setup_violations {
                println!("  {} - Slack: {:.2} ps", violation.net, violation.slack);
            }
        }

        println!("\nSlack Distribution:");
        for bucket in &self.histogram.buckets {
            if bucket.2 > 0 {
                println!("  [{:.0}, {:.0}): {} nets", bucket.0, bucket.1, bucket.2);
            }
        }
    }
}
