//! Timing analysis and closure for FPGA designs

use crate::device::{Device, LogicTile, WireSegment};
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use skalp_lir::{Gate, GateType, LirDesign};
use std::collections::{HashMap, HashSet, VecDeque};

/// Static timing analyzer for FPGA designs
pub struct TimingAnalyzer {
    /// Target device for timing models
    device: Device,
    /// Timing configuration
    config: TimingConfig,
    /// Timing graph
    timing_graph: TimingGraph,
    /// Path delay cache
    path_cache: HashMap<String, f64>,
}

/// Timing analysis configuration
#[derive(Debug, Clone)]
pub struct TimingConfig {
    /// Target clock frequency (MHz)
    pub target_frequency: f64,
    /// Setup time margin (ns)
    pub setup_margin: f64,
    /// Hold time margin (ns)
    pub hold_margin: f64,
    /// Clock uncertainty (ns)
    pub clock_uncertainty: f64,
    /// Enable multicycle path analysis
    pub multicycle_analysis: bool,
    /// Maximum number of critical paths to report
    pub max_critical_paths: usize,
}

impl Default for TimingConfig {
    fn default() -> Self {
        Self {
            target_frequency: 100.0, // 100 MHz
            setup_margin: 0.5,       // 500 ps
            hold_margin: 0.1,        // 100 ps
            clock_uncertainty: 0.2,  // 200 ps
            multicycle_analysis: false,
            max_critical_paths: 10,
        }
    }
}

/// Timing graph representing signal flow
#[derive(Debug, Clone)]
pub struct TimingGraph {
    /// Timing nodes (inputs, outputs, registers)
    pub nodes: Vec<TimingNode>,
    /// Node lookup by signal name
    pub node_map: HashMap<String, usize>,
    /// Timing edges (signal paths)
    pub edges: Vec<TimingEdge>,
    /// Clock domains
    pub clock_domains: Vec<ClockDomain>,
}

/// A node in the timing graph
#[derive(Debug, Clone)]
pub struct TimingNode {
    /// Node ID
    pub id: usize,
    /// Signal name
    pub signal: String,
    /// Node type
    pub node_type: TimingNodeType,
    /// Position on device
    pub position: Option<(usize, usize)>,
    /// Required arrival time (ns)
    pub required_time: f64,
    /// Actual arrival time (ns)
    pub arrival_time: f64,
    /// Slack (required - arrival)
    pub slack: f64,
    /// Clock domain
    pub clock_domain: usize,
}

/// Types of timing nodes
#[derive(Debug, Clone)]
pub enum TimingNodeType {
    /// Primary input
    PrimaryInput,
    /// Primary output
    PrimaryOutput,
    /// Register output (Q)
    RegisterOutput,
    /// Register input (D)
    RegisterInput,
    /// Combinational logic
    Combinational,
    /// Clock pin
    Clock,
}

/// Timing edge representing signal path
#[derive(Debug, Clone)]
pub struct TimingEdge {
    /// Source node ID
    pub from: usize,
    /// Destination node ID
    pub to: usize,
    /// Edge delay (ns)
    pub delay: f64,
    /// Edge type
    pub edge_type: TimingEdgeType,
}

/// Types of timing edges
#[derive(Debug, Clone)]
pub enum TimingEdgeType {
    /// Combinational delay
    Combinational,
    /// Setup constraint
    Setup,
    /// Hold constraint
    Hold,
    /// Clock path
    Clock,
    /// Interconnect delay
    Interconnect,
}

/// Clock domain definition
#[derive(Debug, Clone)]
pub struct ClockDomain {
    /// Domain ID
    pub id: usize,
    /// Clock signal name
    pub clock_signal: String,
    /// Clock period (ns)
    pub period: f64,
    /// Clock skew (ns)
    pub skew: f64,
    /// Domain coverage
    pub registers: HashSet<String>,
}

/// Critical path analysis result
#[derive(Debug)]
pub struct CriticalPath {
    /// Path endpoints
    pub start_point: String,
    pub end_point: String,
    /// Total path delay (ns)
    pub delay: f64,
    /// Required time (ns)
    pub required: f64,
    /// Slack (ns) - negative means timing violation
    pub slack: f64,
    /// Path segments
    pub segments: Vec<PathSegment>,
    /// Clock domain
    pub clock_domain: usize,
}

/// A segment of a timing path
#[derive(Debug)]
pub struct PathSegment {
    /// Segment start signal
    pub from: String,
    /// Segment end signal
    pub to: String,
    /// Segment delay (ns)
    pub delay: f64,
    /// Segment type
    pub segment_type: String,
}

/// Comprehensive timing analysis result
#[derive(Debug)]
pub struct TimingReport {
    /// Overall timing status
    pub meets_timing: bool,
    /// Worst negative slack (WNS)
    pub worst_negative_slack: f64,
    /// Total negative slack (TNS)
    pub total_negative_slack: f64,
    /// Number of failing paths
    pub failing_paths: usize,
    /// Critical paths
    pub critical_paths: Vec<CriticalPath>,
    /// Clock domain summaries
    pub clock_summaries: Vec<ClockDomainSummary>,
    /// Design frequency (MHz)
    pub design_frequency: f64,
}

/// Clock domain timing summary
#[derive(Debug)]
pub struct ClockDomainSummary {
    /// Clock domain ID
    pub domain_id: usize,
    /// Clock name
    pub clock_name: String,
    /// Domain frequency (MHz)
    pub frequency: f64,
    /// Setup slack summary
    pub setup_slack: SlackSummary,
    /// Hold slack summary
    pub hold_slack: SlackSummary,
    /// Number of registers
    pub register_count: usize,
}

/// Slack distribution summary
#[derive(Debug)]
pub struct SlackSummary {
    /// Worst slack (ns)
    pub worst: f64,
    /// Total negative slack (ns)
    pub total_negative: f64,
    /// Number of failing endpoints
    pub failing_endpoints: usize,
    /// Number of total endpoints
    pub total_endpoints: usize,
}

impl TimingAnalyzer {
    /// Create a new timing analyzer
    pub fn new(config: TimingConfig, device: Device) -> Self {
        Self {
            device,
            config,
            timing_graph: TimingGraph {
                nodes: Vec::new(),
                node_map: HashMap::new(),
                edges: Vec::new(),
                clock_domains: Vec::new(),
            },
            path_cache: HashMap::new(),
        }
    }

    /// Perform complete timing analysis
    pub fn analyze_timing(
        &mut self,
        design: &LirDesign,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<TimingReport, TimingError> {
        println!("⏰ Running Static Timing Analysis");

        // Step 1: Build timing graph
        self.build_timing_graph(design, placement, routing)?;
        println!(
            "   Built timing graph: {} nodes, {} edges",
            self.timing_graph.nodes.len(),
            self.timing_graph.edges.len()
        );

        // Step 2: Extract clock domains
        self.extract_clock_domains(design)?;
        println!(
            "   Identified {} clock domains",
            self.timing_graph.clock_domains.len()
        );

        // Step 3: Calculate delays
        self.calculate_delays(placement, routing)?;
        println!("   Calculated path delays");

        // Step 4: Perform timing analysis
        let mut critical_paths = self.find_critical_paths()?;
        let clock_summaries = self.analyze_clock_domains()?;

        // Step 5: Generate timing report
        let (wns, tns, failing_paths) = self.calculate_slack_metrics(&critical_paths);

        // Sort critical paths by slack (worst first)
        critical_paths.sort_by(|a, b| {
            a.slack
                .partial_cmp(&b.slack)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        critical_paths.truncate(self.config.max_critical_paths);

        let design_frequency = if wns < 0.0 {
            1000.0 / (self.target_period() - wns) // Achievable frequency
        } else {
            self.config.target_frequency
        };

        let report = TimingReport {
            meets_timing: wns >= 0.0,
            worst_negative_slack: wns,
            total_negative_slack: tns,
            failing_paths,
            critical_paths,
            clock_summaries,
            design_frequency,
        };

        self.print_timing_summary(&report);
        Ok(report)
    }

    /// Build timing graph from design
    fn build_timing_graph(
        &mut self,
        design: &LirDesign,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<(), TimingError> {
        let mut node_id = 0;

        // Create nodes for all signals
        for module in &design.modules {
            // Input/output signals (derived from LirSignal)
            for signal in &module.signals {
                if signal.is_input {
                    let node = TimingNode {
                        id: node_id,
                        signal: signal.name.clone(),
                        node_type: TimingNodeType::PrimaryInput,
                        position: None,
                        required_time: 0.0,
                        arrival_time: 0.0,
                        slack: 0.0,
                        clock_domain: 0,
                    };
                    self.timing_graph
                        .node_map
                        .insert(signal.name.clone(), node_id);
                    self.timing_graph.nodes.push(node);
                    node_id += 1;
                }

                if signal.is_output {
                    let node = TimingNode {
                        id: node_id,
                        signal: signal.name.clone(),
                        node_type: TimingNodeType::PrimaryOutput,
                        position: None,
                        required_time: self.target_period(),
                        arrival_time: 0.0,
                        slack: 0.0,
                        clock_domain: 0,
                    };
                    self.timing_graph
                        .node_map
                        .insert(signal.name.clone(), node_id);
                    self.timing_graph.nodes.push(node);
                    node_id += 1;
                }
            }

            // Gate nodes
            for gate in &module.gates {
                let position = placement.placements.get(&gate.id).copied();

                // Output node
                let output_node = TimingNode {
                    id: node_id,
                    signal: gate.id.clone(),
                    node_type: if matches!(gate.gate_type, GateType::DFF | GateType::Latch) {
                        TimingNodeType::RegisterOutput
                    } else {
                        TimingNodeType::Combinational
                    },
                    position,
                    required_time: 0.0,
                    arrival_time: 0.0,
                    slack: 0.0,
                    clock_domain: 0,
                };
                self.timing_graph.node_map.insert(gate.id.clone(), node_id);
                self.timing_graph.nodes.push(output_node);
                node_id += 1;

                // For registers, create separate D input node
                if matches!(gate.gate_type, GateType::DFF | GateType::Latch) {
                    let input_signal = format!("{}_D", gate.id);
                    let input_node = TimingNode {
                        id: node_id,
                        signal: input_signal.clone(),
                        node_type: TimingNodeType::RegisterInput,
                        position,
                        required_time: 0.0,
                        arrival_time: 0.0,
                        slack: 0.0,
                        clock_domain: 0,
                    };
                    self.timing_graph.node_map.insert(input_signal, node_id);
                    self.timing_graph.nodes.push(input_node);
                    node_id += 1;
                }
            }
        }

        // Create timing edges
        self.build_timing_edges(design, routing)?;

        Ok(())
    }

    /// Build timing edges from design connectivity
    fn build_timing_edges(
        &mut self,
        design: &LirDesign,
        routing: &RoutingResult,
    ) -> Result<(), TimingError> {
        for module in &design.modules {
            for gate in &module.gates {
                if let Some(&from_id) = self.timing_graph.node_map.get(&gate.id) {
                    // Create edges to all gates that use this output
                    for other_gate in &module.gates {
                        if other_gate.inputs.contains(&gate.id) {
                            if let Some(&to_id) = self.timing_graph.node_map.get(&other_gate.id) {
                                let edge = TimingEdge {
                                    from: from_id,
                                    to: to_id,
                                    delay: 0.0, // Will be calculated later
                                    edge_type: TimingEdgeType::Combinational,
                                };
                                self.timing_graph.edges.push(edge);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Extract clock domains from design
    fn extract_clock_domains(&mut self, design: &LirDesign) -> Result<(), TimingError> {
        let mut domain_id = 0;

        // Find clock signals (simplified - look for signals named 'clk' or 'clock')
        for module in &design.modules {
            for signal in &module.signals {
                if signal.is_input
                    && (signal.name.to_lowercase().contains("clk")
                        || signal.name.to_lowercase().contains("clock"))
                {
                    let domain = ClockDomain {
                        id: domain_id,
                        clock_signal: signal.name.clone(),
                        period: self.target_period(),
                        skew: 0.1, // 100 ps default skew
                        registers: HashSet::new(),
                    };
                    self.timing_graph.clock_domains.push(domain);
                    domain_id += 1;
                }
            }
        }

        // Default domain if no clocks found
        if self.timing_graph.clock_domains.is_empty() {
            let domain = ClockDomain {
                id: 0,
                clock_signal: "default_clk".to_string(),
                period: self.target_period(),
                skew: 0.0,
                registers: HashSet::new(),
            };
            self.timing_graph.clock_domains.push(domain);
        }

        Ok(())
    }

    /// Calculate path delays based on placement and routing
    fn calculate_delays(
        &mut self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<(), TimingError> {
        // Calculate delays for all edges
        let num_edges = self.timing_graph.edges.len();
        for i in 0..num_edges {
            let from_node_id = self.timing_graph.edges[i].from;
            let to_node_id = self.timing_graph.edges[i].to;
            let edge_type = self.timing_graph.edges[i].edge_type.clone();

            let from_node = &self.timing_graph.nodes[from_node_id];
            let to_node = &self.timing_graph.nodes[to_node_id];

            let delay = match edge_type {
                TimingEdgeType::Combinational => {
                    // Logic delay + interconnect delay
                    let logic_delay =
                        self.calculate_logic_delay(&from_node.signal, &to_node.signal);
                    let interconnect_delay = self.calculate_interconnect_delay(
                        from_node.position,
                        to_node.position,
                        routing,
                    );
                    logic_delay + interconnect_delay
                }
                TimingEdgeType::Setup => self.get_setup_time(&to_node.signal),
                TimingEdgeType::Hold => self.get_hold_time(&to_node.signal),
                TimingEdgeType::Clock => {
                    self.calculate_clock_delay(from_node.position, to_node.position)
                }
                TimingEdgeType::Interconnect => {
                    self.calculate_interconnect_delay(from_node.position, to_node.position, routing)
                }
            };

            self.timing_graph.edges[i].delay = delay;
        }

        Ok(())
    }

    /// Calculate logic delay for a gate
    fn calculate_logic_delay(&self, _from: &str, to: &str) -> f64 {
        // Simplified delay models based on gate name/type patterns
        let to_lower = to.to_lowercase();
        if to_lower.contains("and") || to_lower.contains("or") || to_lower.contains("xor") {
            0.2 // 200 ps for basic logic
        } else if to_lower.contains("mux") {
            0.3 // 300 ps for mux
        } else if to_lower.contains("add") {
            0.5 // 500 ps for adder
        } else if to_lower.contains("ff") || to_lower.contains("dff") {
            0.1 // 100 ps clk-to-q delay
        } else {
            0.15 // 150 ps default
        }
    }

    /// Calculate interconnect delay based on routing
    fn calculate_interconnect_delay(
        &self,
        from_pos: Option<(usize, usize)>,
        to_pos: Option<(usize, usize)>,
        routing: &RoutingResult,
    ) -> f64 {
        match (from_pos, to_pos) {
            (Some((fx, fy)), Some((tx, ty))) => {
                let manhattan_dist =
                    ((fx as i32 - tx as i32).abs() + (fy as i32 - ty as i32).abs()) as f64;

                // Base delay of 50 ps per tile + wire segments
                let base_delay = manhattan_dist * 0.05;

                // Add wire segment delays
                let mut wire_delay = 0.0;
                for wire_segment in &self.device.routing.wire_segments {
                    wire_delay += wire_segment.resistance * wire_segment.capacitance * 0.001;
                }

                base_delay + wire_delay.min(1.0) // Cap at 1ns
            }
            _ => 0.1, // Default 100 ps if position unknown
        }
    }

    /// Get setup time for a register
    fn get_setup_time(&self, signal: &str) -> f64 {
        let signal_lower = signal.to_lowercase();
        if signal_lower.contains("ff") || signal_lower.contains("dff") {
            0.1 // 100 ps setup time
        } else {
            0.0
        }
    }

    /// Get hold time for a register
    fn get_hold_time(&self, signal: &str) -> f64 {
        let signal_lower = signal.to_lowercase();
        if signal_lower.contains("ff") || signal_lower.contains("dff") {
            0.05 // 50 ps hold time
        } else {
            0.0
        }
    }

    /// Calculate clock path delay
    fn calculate_clock_delay(
        &self,
        from_pos: Option<(usize, usize)>,
        to_pos: Option<(usize, usize)>,
    ) -> f64 {
        // Clock tree delay (simplified)
        match (from_pos, to_pos) {
            (Some(_), Some(_)) => 0.2, // 200 ps clock tree delay
            _ => 0.1,                  // 100 ps default
        }
    }

    /// Find critical paths using graph traversal
    fn find_critical_paths(&mut self) -> Result<Vec<CriticalPath>, TimingError> {
        let mut critical_paths = Vec::new();

        // Perform arrival time analysis (forward pass)
        self.calculate_arrival_times()?;

        // Perform required time analysis (backward pass)
        self.calculate_required_times()?;

        // Calculate slack for all nodes
        for node in &mut self.timing_graph.nodes {
            node.slack = node.required_time - node.arrival_time;
        }

        // Find paths with negative slack
        for node in &self.timing_graph.nodes {
            if node.slack < 0.0
                && matches!(
                    node.node_type,
                    TimingNodeType::RegisterInput | TimingNodeType::PrimaryOutput
                )
            {
                if let Ok(path) = self.trace_critical_path(node.id) {
                    critical_paths.push(path);
                }
            }
        }

        Ok(critical_paths)
    }

    /// Calculate arrival times (forward timing analysis)
    fn calculate_arrival_times(&mut self) -> Result<(), TimingError> {
        // Topological sort for timing analysis
        let sorted_nodes = self.topological_sort()?;

        for &node_id in &sorted_nodes {
            let mut max_arrival: f64 = 0.0;

            // Find maximum arrival time from all predecessors
            for edge in &self.timing_graph.edges {
                if edge.to == node_id {
                    let predecessor_arrival = self.timing_graph.nodes[edge.from].arrival_time;
                    max_arrival = max_arrival.max(predecessor_arrival + edge.delay);
                }
            }

            self.timing_graph.nodes[node_id].arrival_time = max_arrival;
        }

        Ok(())
    }

    /// Calculate required times (backward timing analysis)
    fn calculate_required_times(&mut self) -> Result<(), TimingError> {
        let sorted_nodes = self.topological_sort()?;
        let target_period = self.target_period();
        let setup_margin = self.config.setup_margin;

        // Initialize required times for outputs
        for node in &mut self.timing_graph.nodes {
            match node.node_type {
                TimingNodeType::PrimaryOutput => {
                    node.required_time = target_period;
                }
                TimingNodeType::RegisterInput => {
                    node.required_time = target_period - setup_margin;
                }
                _ => {
                    node.required_time = f64::INFINITY;
                }
            }
        }

        // Backward pass
        for &node_id in sorted_nodes.iter().rev() {
            let mut min_required = self.timing_graph.nodes[node_id].required_time;

            // Find minimum required time from all successors
            for edge in &self.timing_graph.edges {
                if edge.from == node_id {
                    let successor_required = self.timing_graph.nodes[edge.to].required_time;
                    min_required = min_required.min(successor_required - edge.delay);
                }
            }

            if min_required != f64::INFINITY {
                self.timing_graph.nodes[node_id].required_time = min_required;
            }
        }

        Ok(())
    }

    /// Topological sort of timing graph
    fn topological_sort(&self) -> Result<Vec<usize>, TimingError> {
        let mut in_degree = vec![0; self.timing_graph.nodes.len()];
        let mut queue = VecDeque::new();
        let mut sorted = Vec::new();

        // Calculate in-degrees
        for edge in &self.timing_graph.edges {
            in_degree[edge.to] += 1;
        }

        // Find nodes with no incoming edges
        for (i, &degree) in in_degree.iter().enumerate() {
            if degree == 0 {
                queue.push_back(i);
            }
        }

        // Process nodes in topological order
        while let Some(node_id) = queue.pop_front() {
            sorted.push(node_id);

            // Update successors
            for edge in &self.timing_graph.edges {
                if edge.from == node_id {
                    in_degree[edge.to] -= 1;
                    if in_degree[edge.to] == 0 {
                        queue.push_back(edge.to);
                    }
                }
            }
        }

        if sorted.len() != self.timing_graph.nodes.len() {
            return Err(TimingError::CyclicGraph(
                "Timing graph contains cycles".to_string(),
            ));
        }

        Ok(sorted)
    }

    /// Trace critical path from endpoint
    fn trace_critical_path(&self, end_node_id: usize) -> Result<CriticalPath, TimingError> {
        let mut segments = Vec::new();
        let mut current_node = end_node_id;
        let mut total_delay = 0.0;

        // Trace path backwards
        while let Some(critical_edge) = self.find_critical_incoming_edge(current_node) {
            let from_node = &self.timing_graph.nodes[critical_edge.from];
            let to_node = &self.timing_graph.nodes[critical_edge.to];

            segments.push(PathSegment {
                from: from_node.signal.clone(),
                to: to_node.signal.clone(),
                delay: critical_edge.delay,
                segment_type: format!("{:?}", critical_edge.edge_type),
            });

            total_delay += critical_edge.delay;
            current_node = critical_edge.from;

            // Stop at primary inputs or register outputs
            if matches!(
                from_node.node_type,
                TimingNodeType::PrimaryInput | TimingNodeType::RegisterOutput
            ) {
                break;
            }
        }

        segments.reverse();

        let end_node = &self.timing_graph.nodes[end_node_id];
        let start_node = if let Some(first_segment) = segments.first() {
            first_segment.from.clone()
        } else {
            end_node.signal.clone()
        };

        Ok(CriticalPath {
            start_point: start_node,
            end_point: end_node.signal.clone(),
            delay: total_delay,
            required: end_node.required_time,
            slack: end_node.slack,
            segments,
            clock_domain: end_node.clock_domain,
        })
    }

    /// Find the critical incoming edge (edge on critical path)
    fn find_critical_incoming_edge(&self, node_id: usize) -> Option<&TimingEdge> {
        let node = &self.timing_graph.nodes[node_id];
        let target_arrival = node.arrival_time;

        for edge in &self.timing_graph.edges {
            if edge.to == node_id {
                let from_node = &self.timing_graph.nodes[edge.from];
                if (from_node.arrival_time + edge.delay - target_arrival).abs() < 0.001 {
                    return Some(edge);
                }
            }
        }

        None
    }

    /// Analyze timing for each clock domain
    fn analyze_clock_domains(&self) -> Result<Vec<ClockDomainSummary>, TimingError> {
        let mut summaries = Vec::new();

        for domain in &self.timing_graph.clock_domains {
            let mut setup_slacks = Vec::new();
            let mut hold_slacks = Vec::new();
            let mut register_count = 0;

            // Collect slack values for this domain
            for node in &self.timing_graph.nodes {
                if node.clock_domain == domain.id {
                    match node.node_type {
                        TimingNodeType::RegisterInput => {
                            setup_slacks.push(node.slack);
                            register_count += 1;
                        }
                        TimingNodeType::RegisterOutput => {
                            hold_slacks.push(node.slack);
                        }
                        _ => {}
                    }
                }
            }

            let setup_summary = self.calculate_slack_summary(&setup_slacks);
            let hold_summary = self.calculate_slack_summary(&hold_slacks);

            summaries.push(ClockDomainSummary {
                domain_id: domain.id,
                clock_name: domain.clock_signal.clone(),
                frequency: 1000.0 / domain.period, // Convert to MHz
                setup_slack: setup_summary,
                hold_slack: hold_summary,
                register_count,
            });
        }

        Ok(summaries)
    }

    /// Calculate slack distribution summary
    fn calculate_slack_summary(&self, slacks: &[f64]) -> SlackSummary {
        if slacks.is_empty() {
            return SlackSummary {
                worst: 0.0,
                total_negative: 0.0,
                failing_endpoints: 0,
                total_endpoints: 0,
            };
        }

        let worst = slacks.iter().copied().fold(f64::INFINITY, f64::min);
        let total_negative = slacks.iter().filter(|&&s| s < 0.0).sum();
        let failing_endpoints = slacks.iter().filter(|&&s| s < 0.0).count();

        SlackSummary {
            worst,
            total_negative,
            failing_endpoints,
            total_endpoints: slacks.len(),
        }
    }

    /// Calculate overall slack metrics
    fn calculate_slack_metrics(&self, critical_paths: &[CriticalPath]) -> (f64, f64, usize) {
        let wns = critical_paths
            .iter()
            .map(|p| p.slack)
            .fold(f64::INFINITY, f64::min);

        let tns = critical_paths
            .iter()
            .filter(|p| p.slack < 0.0)
            .map(|p| p.slack)
            .sum();

        let failing_paths = critical_paths.iter().filter(|p| p.slack < 0.0).count();

        (wns, tns, failing_paths)
    }

    /// Get target clock period
    fn target_period(&self) -> f64 {
        1000.0 / self.config.target_frequency // Convert MHz to ns
    }

    /// Print timing analysis summary
    fn print_timing_summary(&self, report: &TimingReport) {
        println!("\n📊 Timing Analysis Results:");
        println!(
            "   Target Frequency: {:.1} MHz",
            self.config.target_frequency
        );
        println!("   Design Frequency: {:.1} MHz", report.design_frequency);
        println!(
            "   Timing Status: {}",
            if report.meets_timing {
                "✅ PASS"
            } else {
                "❌ FAIL"
            }
        );

        if !report.meets_timing {
            println!(
                "   Worst Negative Slack: {:.3} ns",
                report.worst_negative_slack
            );
            println!(
                "   Total Negative Slack: {:.3} ns",
                report.total_negative_slack
            );
            println!("   Failing Paths: {}", report.failing_paths);
        }

        println!("\n   Clock Domain Summary:");
        for summary in &report.clock_summaries {
            println!(
                "     {}: {:.1} MHz, {} registers",
                summary.clock_name, summary.frequency, summary.register_count
            );
            if summary.setup_slack.failing_endpoints > 0 {
                println!(
                    "       Setup violations: {} (worst: {:.3} ns)",
                    summary.setup_slack.failing_endpoints, summary.setup_slack.worst
                );
            }
        }

        if !report.critical_paths.is_empty() {
            println!("\n   Critical Paths:");
            for (i, path) in report.critical_paths.iter().take(3).enumerate() {
                println!(
                    "     #{}: {} → {} ({:.3} ns slack)",
                    i + 1,
                    path.start_point,
                    path.end_point,
                    path.slack
                );
            }
        }
    }
}

/// Timing analysis errors
#[derive(Debug, thiserror::Error)]
pub enum TimingError {
    #[error("Timing analysis failed: {0}")]
    Failed(String),
    #[error("Cyclic timing graph: {0}")]
    CyclicGraph(String),
    #[error("Invalid timing constraint: {0}")]
    InvalidConstraint(String),
}

/// Timing-driven placement optimizer
pub struct TimingDrivenPlacer {
    /// Base placer configuration
    base_config: crate::placer::PlacerConfig,
    /// Timing analyzer
    timing_analyzer: TimingAnalyzer,
    /// Timing weight in cost function
    timing_weight: f64,
}

impl TimingDrivenPlacer {
    /// Create timing-driven placer
    pub fn new(
        base_config: crate::placer::PlacerConfig,
        timing_config: TimingConfig,
        device: Device,
    ) -> Self {
        Self {
            base_config,
            timing_analyzer: TimingAnalyzer::new(timing_config, device),
            timing_weight: 0.7,
        }
    }

    /// Perform timing-driven placement
    pub fn place_with_timing(
        &mut self,
        design: &LirDesign,
    ) -> Result<(PlacementResult, TimingReport), TimingError> {
        println!("🎯 Running Timing-Driven Placement");

        // Start with regular placement
        let mut placer = crate::placer::Placer::new(
            self.base_config.clone(),
            self.timing_analyzer.device.clone(),
        );
        let mut placement = placer
            .place(design)
            .map_err(|e| TimingError::Failed(format!("Placement failed: {}", e)))?;

        println!("   Initial placement cost: {:.2}", placement.cost);

        // Iterative timing-driven optimization
        for iteration in 0..5 {
            // Create dummy routing for timing analysis
            let dummy_routing = crate::router::RoutingResult {
                routes: std::collections::HashMap::new(),
                congestion: 0.0,
                wirelength: 0,
            };

            // Analyze timing
            let timing_report =
                self.timing_analyzer
                    .analyze_timing(design, &placement, &dummy_routing)?;

            if timing_report.meets_timing {
                println!(
                    "   Timing closure achieved in {} iterations!",
                    iteration + 1
                );
                return Ok((placement, timing_report));
            }

            println!(
                "   Iteration {}: WNS = {:.3} ns",
                iteration + 1,
                timing_report.worst_negative_slack
            );

            // Optimize critical paths
            placement = self.optimize_critical_paths(design, placement, &timing_report)?;
        }

        // Final timing analysis
        let dummy_routing = crate::router::RoutingResult {
            routes: std::collections::HashMap::new(),
            congestion: 0.0,
            wirelength: 0,
        };
        let final_timing =
            self.timing_analyzer
                .analyze_timing(design, &placement, &dummy_routing)?;

        Ok((placement, final_timing))
    }

    /// Optimize placement for critical paths
    fn optimize_critical_paths(
        &self,
        _design: &LirDesign,
        placement: PlacementResult,
        timing_report: &TimingReport,
    ) -> Result<PlacementResult, TimingError> {
        // Simple optimization: try to place critical path gates closer together
        for path in &timing_report.critical_paths {
            if path.slack < -0.1 {
                // Focus on paths with >100ps violation
                // This is a simplified optimization - a real implementation would
                // use more sophisticated algorithms like force-directed placement
                println!(
                    "     Optimizing critical path: {} → {} ({:.3} ns slack)",
                    path.start_point, path.end_point, path.slack
                );
            }
        }

        // For now, return original placement (real optimization would modify positions)
        Ok(placement)
    }
}
