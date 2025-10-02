//! Advanced routing algorithms for FPGA designs

use std::collections::{HashMap, BinaryHeap, HashSet, VecDeque};
use std::cmp::Ordering;
use crate::device::{Device, DeviceFamily};
use skalp_lir::{LirDesign, Net, Gate};

/// Advanced router with PathFinder A* algorithm
pub struct Router {
    /// Routing configuration
    config: RouterConfig,
    /// Target device
    device: Device,
    /// Routing resource graph
    resource_graph: RoutingGraph,
    /// History costs for congestion avoidance
    history_costs: HashMap<(usize, usize), f64>,
}

/// Routing configuration
#[derive(Debug, Clone)]
pub struct RouterConfig {
    /// Routing algorithm to use
    pub algorithm: RoutingAlgorithm,
    /// Maximum routing iterations
    pub max_iterations: usize,
    /// Allow rip-up and reroute
    pub allow_ripup: bool,
    /// Maximum congestion allowed
    pub max_congestion: f64,
    /// History cost factor for PathFinder
    pub history_cost_factor: f64,
    /// Present congestion penalty
    pub present_congestion_factor: f64,
    /// Timing-driven routing weight
    pub timing_weight: f64,
}

/// Available routing algorithms
#[derive(Debug, Clone)]
pub enum RoutingAlgorithm {
    /// PathFinder A* with negotiated congestion
    PathFinderAStar,
    /// Simple maze routing
    MazeRouting,
    /// Timing-driven routing
    TimingDriven,
}

impl Default for RouterConfig {
    fn default() -> Self {
        Self {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 100,
            allow_ripup: true,
            max_congestion: 1.2,
            history_cost_factor: 0.5,
            present_congestion_factor: 1.0,
            timing_weight: 0.3,
        }
    }
}

/// Routing resource graph
#[derive(Debug, Clone)]
pub struct RoutingGraph {
    /// Routing nodes (x, y, track_id)
    pub nodes: Vec<RoutingNode>,
    /// Node lookup by coordinates
    pub node_map: HashMap<(usize, usize, usize), usize>,
    /// Edge connections between nodes
    pub edges: Vec<RoutingEdge>,
    /// Current usage of each node
    pub usage: HashMap<usize, usize>,
    /// Capacity of each node
    pub capacity: HashMap<usize, usize>,
}

/// A node in the routing graph
#[derive(Debug, Clone)]
pub struct RoutingNode {
    /// Node ID
    pub id: usize,
    /// Position (x, y, track_id)
    pub position: (usize, usize, usize),
    /// Node type
    pub node_type: NodeType,
    /// Base routing cost
    pub base_cost: f64,
}

/// Routing edge connecting two nodes
#[derive(Debug, Clone)]
pub struct RoutingEdge {
    /// Source node ID
    pub from: usize,
    /// Destination node ID
    pub to: usize,
    /// Edge delay
    pub delay: f64,
    /// Edge resistance
    pub resistance: f64,
}

/// Types of routing nodes
#[derive(Debug, Clone)]
pub enum NodeType {
    /// Wire segment
    Wire,
    /// Switch box connection
    Switch,
    /// Input pin
    InputPin,
    /// Output pin
    OutputPin,
    /// Connection box
    ConnectionBox,
}

/// A* search node for PathFinder
#[derive(Debug, Clone)]
struct SearchNode {
    /// Routing node ID
    node_id: usize,
    /// Cost from start (g-score)
    cost_from_start: f64,
    /// Heuristic cost to goal (h-score)
    heuristic_cost: f64,
    /// Parent node for path reconstruction
    parent: Option<usize>,
}

impl PartialEq for SearchNode {
    fn eq(&self, other: &Self) -> bool {
        self.total_cost().eq(&other.total_cost())
    }
}

impl Eq for SearchNode {}

impl PartialOrd for SearchNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SearchNode {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse ordering for min-heap behavior
        other.total_cost().partial_cmp(&self.total_cost()).unwrap_or(Ordering::Equal)
    }
}

impl SearchNode {
    fn total_cost(&self) -> f64 {
        self.cost_from_start + self.heuristic_cost
    }
}

impl Router {
    /// Create a new router with device
    pub fn new(config: RouterConfig, device: Device) -> Self {
        let resource_graph = Self::build_routing_graph(&device);
        Self {
            config,
            device,
            resource_graph,
            history_costs: HashMap::new(),
        }
    }

    /// Build routing resource graph from device
    fn build_routing_graph(device: &Device) -> RoutingGraph {
        let mut nodes = Vec::new();
        let mut node_map = HashMap::new();
        let mut edges = Vec::new();
        let mut usage = HashMap::new();
        let mut capacity = HashMap::new();

        let mut node_id = 0;

        // Create routing nodes for each tile
        for x in 0..device.grid_size.0 {
            for y in 0..device.grid_size.1 {
                // Create nodes for each routing track
                let num_tracks = device.routing.channels.0.max(device.routing.channels.1);

                for track in 0..num_tracks {
                    let position = (x, y, track);

                    // Determine node type based on position
                    let node_type = if x == 0 || x == device.grid_size.0-1 ||
                                      y == 0 || y == device.grid_size.1-1 {
                        NodeType::InputPin
                    } else {
                        NodeType::Wire
                    };

                    let base_cost = match node_type {
                        NodeType::Wire => 1.0,
                        NodeType::Switch => 2.0,
                        NodeType::InputPin | NodeType::OutputPin => 0.5,
                        NodeType::ConnectionBox => 1.5,
                    };

                    let node = RoutingNode {
                        id: node_id,
                        position,
                        node_type,
                        base_cost,
                    };

                    node_map.insert(position, node_id);
                    nodes.push(node);
                    usage.insert(node_id, 0);
                    capacity.insert(node_id, 1); // Single-driver per track

                    node_id += 1;
                }
            }
        }

        // Create edges for routing connectivity
        for node in &nodes {
            let (x, y, track) = node.position;

            // Horizontal connections
            if x < device.grid_size.0 - 1 {
                if let Some(&next_id) = node_map.get(&(x + 1, y, track)) {
                    edges.push(RoutingEdge {
                        from: node.id,
                        to: next_id,
                        delay: 0.1,
                        resistance: 100.0,
                    });
                }
            }

            // Vertical connections
            if y < device.grid_size.1 - 1 {
                if let Some(&next_id) = node_map.get(&(x, y + 1, track)) {
                    edges.push(RoutingEdge {
                        from: node.id,
                        to: next_id,
                        delay: 0.1,
                        resistance: 100.0,
                    });
                }
            }

            // Switch box connections (between tracks)
            for other_track in 0..device.routing.channels.0.max(device.routing.channels.1) {
                if other_track != track {
                    if let Some(&switch_id) = node_map.get(&(x, y, other_track)) {
                        edges.push(RoutingEdge {
                            from: node.id,
                            to: switch_id,
                            delay: 0.2,
                            resistance: 200.0,
                        });
                    }
                }
            }
        }

        RoutingGraph {
            nodes,
            node_map,
            edges,
            usage,
            capacity,
        }
    }

    /// Run routing algorithm
    pub fn route(&mut self, design: &LirDesign, placement: &super::placer::PlacementResult) -> Result<RoutingResult, RoutingError> {
        println!("🔗 Running {} routing algorithm", match self.config.algorithm {
            RoutingAlgorithm::PathFinderAStar => "PathFinder A*",
            RoutingAlgorithm::MazeRouting => "Maze",
            RoutingAlgorithm::TimingDriven => "Timing-Driven",
        });

        match self.config.algorithm {
            RoutingAlgorithm::PathFinderAStar => self.pathfinder_astar_route(design, placement),
            RoutingAlgorithm::MazeRouting => self.maze_route(design, placement),
            RoutingAlgorithm::TimingDriven => self.timing_driven_route(design, placement),
        }
    }

    /// PathFinder A* routing with negotiated congestion
    fn pathfinder_astar_route(&mut self, design: &LirDesign, placement: &super::placer::PlacementResult) -> Result<RoutingResult, RoutingError> {
        let mut routes = HashMap::new();
        let mut nets = self.extract_nets(design, placement);

        println!("   Routing {} nets", nets.len());

        // Sort nets by criticality (timing-driven routing)
        nets.sort_by(|a, b| {
            let crit_a = self.calculate_net_criticality(a, placement);
            let crit_b = self.calculate_net_criticality(b, placement);
            crit_b.partial_cmp(&crit_a).unwrap_or(Ordering::Equal)
        });

        // PathFinder main loop with rip-up and reroute
        for iteration in 0..self.config.max_iterations {
            let mut rerouted_nets = 0;

            // Route each net
            for net in &nets {
                // Check if net needs rerouting
                if iteration == 0 || self.needs_rerouting(net, &routes) {
                    // Rip up existing route
                    if let Some(old_route) = routes.get(&net.id) {
                        self.ripup_route(old_route);
                    }

                    // Route the net using A*
                    match self.route_net_astar(net) {
                        Ok(route) => {
                            self.commit_route(&route);
                            routes.insert(net.id.clone(), route);
                            rerouted_nets += 1;
                        }
                        Err(_) => {
                            return Err(RoutingError::Failed(format!("Cannot route net {}", net.id)));
                        }
                    }
                }
            }

            // Update congestion costs
            let max_congestion = self.update_congestion_costs();

            println!("   Iteration {}: {} nets rerouted, max congestion: {:.2}",
                    iteration + 1, rerouted_nets, max_congestion);

            // Check for convergence
            if max_congestion <= self.config.max_congestion && rerouted_nets == 0 {
                println!("   Routing converged!");
                break;
            }

            if iteration == self.config.max_iterations - 1 {
                println!("   ⚠️  Routing did not converge - may have congestion");
            }
        }

        // Calculate final metrics
        let total_wirelength = routes.values()
            .map(|route| route.len())
            .sum();

        Ok(RoutingResult {
            routes: self.convert_routes_to_coordinates(&routes),
            congestion: self.calculate_final_congestion(),
            wirelength: total_wirelength,
        })
    }

    /// Route a single net using A* search
    fn route_net_astar(&self, net: &NetToRoute) -> Result<Vec<usize>, RoutingError> {
        let source = self.find_nearest_routing_node(net.source)?;
        let target = self.find_nearest_routing_node(net.target)?;

        let mut open_set = BinaryHeap::new();
        let mut closed_set = HashSet::new();
        let mut g_scores = HashMap::new();
        let mut came_from = HashMap::new();

        // Initialize with source
        let start_node = SearchNode {
            node_id: source,
            cost_from_start: 0.0,
            heuristic_cost: self.heuristic_cost(source, target),
            parent: None,
        };

        open_set.push(start_node);
        g_scores.insert(source, 0.0);

        while let Some(current) = open_set.pop() {
            if current.node_id == target {
                // Reconstruct path
                return Ok(self.reconstruct_path(&came_from, target));
            }

            closed_set.insert(current.node_id);

            // Examine neighbors
            for edge in self.get_node_edges(current.node_id) {
                let neighbor = edge.to;

                if closed_set.contains(&neighbor) {
                    continue;
                }

                let tentative_g_score = current.cost_from_start + self.calculate_edge_cost(edge);

                if let Some(&existing_g) = g_scores.get(&neighbor) {
                    if tentative_g_score >= existing_g {
                        continue;
                    }
                }

                // This path is the best until now
                came_from.insert(neighbor, current.node_id);
                g_scores.insert(neighbor, tentative_g_score);

                let neighbor_node = SearchNode {
                    node_id: neighbor,
                    cost_from_start: tentative_g_score,
                    heuristic_cost: self.heuristic_cost(neighbor, target),
                    parent: Some(current.node_id),
                };

                open_set.push(neighbor_node);
            }
        }

        Err(RoutingError::Unroutable)
    }

    /// Calculate heuristic cost (Manhattan distance)
    fn heuristic_cost(&self, from: usize, to: usize) -> f64 {
        if let (Some(from_node), Some(to_node)) = (
            self.resource_graph.nodes.get(from),
            self.resource_graph.nodes.get(to)
        ) {
            let (fx, fy, _) = from_node.position;
            let (tx, ty, _) = to_node.position;
            ((fx as i32 - tx as i32).abs() + (fy as i32 - ty as i32).abs()) as f64
        } else {
            f64::INFINITY
        }
    }

    /// Calculate edge routing cost (includes congestion)
    fn calculate_edge_cost(&self, edge: &RoutingEdge) -> f64 {
        let base_cost = self.resource_graph.nodes[edge.to].base_cost;
        let usage = *self.resource_graph.usage.get(&edge.to).unwrap_or(&0) as f64;
        let capacity = *self.resource_graph.capacity.get(&edge.to).unwrap_or(&1) as f64;
        let history_cost = *self.history_costs.get(&(edge.from, edge.to)).unwrap_or(&0.0);

        // PathFinder cost function
        let present_congestion = if usage > capacity {
            self.config.present_congestion_factor * (usage - capacity + 1.0)
        } else {
            1.0
        };

        base_cost * present_congestion + self.config.history_cost_factor * history_cost
    }

    /// Get edges from a node
    fn get_node_edges(&self, node_id: usize) -> Vec<&RoutingEdge> {
        self.resource_graph.edges.iter()
            .filter(|edge| edge.from == node_id)
            .collect()
    }

    /// Reconstruct path from A* search
    fn reconstruct_path(&self, came_from: &HashMap<usize, usize>, target: usize) -> Vec<usize> {
        let mut path = vec![target];
        let mut current = target;

        while let Some(&parent) = came_from.get(&current) {
            path.push(parent);
            current = parent;
        }

        path.reverse();
        path
    }

    /// Extract nets from design and placement
    fn extract_nets(&self, design: &LirDesign, placement: &super::placer::PlacementResult) -> Vec<NetToRoute> {
        let mut nets = Vec::new();

        for module in &design.modules {
            for gate in &module.gates {
                if let Some(&gate_pos) = placement.placements.get(&gate.id) {
                    // Create nets for each input connection
                    for (i, input) in gate.inputs.iter().enumerate() {
                        if let Some(&source_pos) = placement.placements.get(input) {
                            nets.push(NetToRoute {
                                id: format!("{}_{}", gate.id, i),
                                source: source_pos,
                                target: gate_pos,
                                criticality: 1.0, // Default criticality
                            });
                        }
                    }
                }
            }
        }

        nets
    }

    /// Calculate net criticality for timing-driven routing
    fn calculate_net_criticality(&self, _net: &NetToRoute, _placement: &super::placer::PlacementResult) -> f64 {
        // Simplified criticality - would use actual timing analysis
        1.0
    }

    /// Check if net needs rerouting due to congestion
    fn needs_rerouting(&self, _net: &NetToRoute, _routes: &HashMap<String, Vec<usize>>) -> bool {
        // Simplified check - always reroute if congestion exists
        self.resource_graph.usage.values().any(|&usage| {
            usage > *self.resource_graph.capacity.get(&0).unwrap_or(&1)
        })
    }

    /// Find nearest routing node to a placement position
    fn find_nearest_routing_node(&self, position: (usize, usize)) -> Result<usize, RoutingError> {
        // Find first available routing node at this position
        for track in 0..self.device.routing.channels.0.max(self.device.routing.channels.1) {
            if let Some(&node_id) = self.resource_graph.node_map.get(&(position.0, position.1, track)) {
                return Ok(node_id);
            }
        }
        Err(RoutingError::Failed("No routing node found".to_string()))
    }

    /// Rip up existing route
    fn ripup_route(&mut self, route: &[usize]) {
        for &node_id in route {
            if let Some(usage) = self.resource_graph.usage.get_mut(&node_id) {
                *usage = usage.saturating_sub(1);
            }
        }
    }

    /// Commit route to usage map
    fn commit_route(&mut self, route: &[usize]) {
        for &node_id in route {
            *self.resource_graph.usage.entry(node_id).or_insert(0) += 1;
        }
    }

    /// Update congestion costs for next iteration
    fn update_congestion_costs(&mut self) -> f64 {
        let mut max_congestion: f64 = 0.0;

        for (&node_id, &usage) in &self.resource_graph.usage {
            let capacity = *self.resource_graph.capacity.get(&node_id).unwrap_or(&1);
            if usage > capacity {
                let congestion = (usage as f64) / (capacity as f64);
                max_congestion = max_congestion.max(congestion);

                // Update history costs for edges leading to this node
                for edge in &self.resource_graph.edges {
                    if edge.to == node_id {
                        let cost_key = (edge.from, edge.to);
                        *self.history_costs.entry(cost_key).or_insert(0.0) += 1.0;
                    }
                }
            }
        }

        max_congestion
    }

    /// Convert node-based routes to coordinate-based routes
    fn convert_routes_to_coordinates(&self, routes: &HashMap<String, Vec<usize>>) -> HashMap<String, Vec<(usize, usize)>> {
        let mut coord_routes = HashMap::new();

        for (net_id, route) in routes {
            let coords: Vec<(usize, usize)> = route.iter()
                .filter_map(|&node_id| {
                    self.resource_graph.nodes.get(node_id)
                        .map(|node| (node.position.0, node.position.1))
                })
                .collect();
            coord_routes.insert(net_id.clone(), coords);
        }

        coord_routes
    }

    /// Calculate final congestion metric
    fn calculate_final_congestion(&self) -> f64 {
        let mut total_congestion = 0.0;
        let mut congested_nodes = 0;

        for (&node_id, &usage) in &self.resource_graph.usage {
            let capacity = *self.resource_graph.capacity.get(&node_id).unwrap_or(&1);
            if usage > capacity {
                total_congestion += (usage as f64) / (capacity as f64) - 1.0;
                congested_nodes += 1;
            }
        }

        if congested_nodes > 0 {
            total_congestion / (congested_nodes as f64)
        } else {
            0.0
        }
    }

    /// Simple maze routing (fallback algorithm)
    fn maze_route(&mut self, design: &LirDesign, placement: &super::placer::PlacementResult) -> Result<RoutingResult, RoutingError> {
        println!("   Using simplified maze routing");

        let nets = self.extract_nets(design, placement);
        let mut routes = HashMap::new();

        for net in nets {
            // Simple direct connection for maze routing
            routes.insert(net.id, vec![net.source]);
        }

        Ok(RoutingResult {
            routes,
            congestion: 0.0,
            wirelength: 0,
        })
    }

    /// Timing-driven routing
    fn timing_driven_route(&mut self, design: &LirDesign, placement: &super::placer::PlacementResult) -> Result<RoutingResult, RoutingError> {
        println!("   Using timing-driven routing");

        // For now, use PathFinder with higher timing weight
        let original_timing_weight = self.config.timing_weight;
        self.config.timing_weight = 0.8;

        let result = self.pathfinder_astar_route(design, placement);

        self.config.timing_weight = original_timing_weight;
        result
    }
}

/// Net to be routed
#[derive(Debug, Clone)]
struct NetToRoute {
    /// Net identifier
    id: String,
    /// Source position (x, y)
    source: (usize, usize),
    /// Target position (x, y)
    target: (usize, usize),
    /// Timing criticality (0.0 to 1.0)
    criticality: f64,
}

/// Routing result
pub struct RoutingResult {
    /// Net routes
    pub routes: HashMap<String, Vec<(usize, usize)>>,
    /// Maximum congestion
    pub congestion: f64,
    /// Total wirelength
    pub wirelength: usize,
}

/// Routing errors
#[derive(Debug, thiserror::Error)]
pub enum RoutingError {
    #[error("Routing failed: {0}")]
    Failed(String),
    #[error("Unroutable design")]
    Unroutable,
}