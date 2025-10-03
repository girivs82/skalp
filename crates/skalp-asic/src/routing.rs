//! Native ASIC Routing Engine
//!
//! Implements global and detailed routing for ASICs without external tools.
//! Uses a combination of maze routing, channel routing, and track assignment.

use crate::placement::{Net, Netlist, Placement};
use crate::sky130::StandardCellLibrary;
use crate::{AsicError, DesignRules, Technology};
use petgraph::algo::dijkstra;
use petgraph::graph::{EdgeReference, Graph, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::sync::{Arc, Mutex};
use std::thread;

/// Routing result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingResult {
    /// Routed nets
    pub routed_nets: Vec<RoutedNet>,
    /// Congestion map
    pub congestion: CongestionMap,
    /// Total wirelength
    pub total_wirelength: f64,
    /// Number of vias
    pub num_vias: usize,
}

/// Routed net
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutedNet {
    /// Net name
    pub name: String,
    /// Wire segments
    pub segments: Vec<WireSegment>,
    /// Vias
    pub vias: Vec<ViaInstance>,
}

/// Wire segment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WireSegment {
    /// Start and end points
    pub points: Vec<(f64, f64)>,
    /// Metal layer
    pub layer: usize,
    /// Wire width
    pub width: f64,
}

/// Via instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViaInstance {
    /// Via position
    pub position: (f64, f64),
    /// From layer
    pub from_layer: usize,
    /// To layer
    pub to_layer: usize,
    /// Via size
    pub size: f64,
}

/// Congestion map
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CongestionMap {
    /// Grid dimensions
    pub grid_size: (usize, usize),
    /// Congestion values per grid cell
    pub values: Vec<Vec<f64>>,
}

/// Main router combining global and detailed routing
pub struct Router {
    /// Global router
    pub global_router: GlobalRouter,
    /// Detailed router
    pub detailed_router: DetailedRouter,
    /// Configuration
    pub config: RouterConfig,
}

/// Router configuration
#[derive(Debug, Clone)]
pub struct RouterConfig {
    /// Enable global routing
    pub use_global: bool,
    /// Maximum iterations
    pub max_iterations: usize,
    /// Congestion threshold
    pub congestion_threshold: f64,
    /// Enable parallel routing
    pub enable_parallel: bool,
    /// Number of threads for parallel routing
    pub num_threads: usize,
    /// Enable hierarchical routing
    pub enable_hierarchical: bool,
    /// Grid coarsening factor for hierarchical routing
    pub coarsening_factor: usize,
}

/// Route segment for compatibility
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteSegment {
    pub start: (f64, f64),
    pub end: (f64, f64),
    pub layer: usize,
    pub width: f64,
    pub net_name: String,
}

/// Global router for coarse routing
pub struct GlobalRouter {
    design_rules: DesignRules,
    config: GlobalRoutingConfig,
}

/// Global routing result
#[derive(Debug, Clone)]
pub struct GlobalRouting {
    /// Global wires for each net
    pub wires: Vec<GlobalWire>,
    /// Routing grid
    pub grid: Vec<Vec<GCell>>,
    /// Congestion map
    pub congestion: CongestionMap,
}

/// Global wire
#[derive(Debug, Clone)]
pub struct GlobalWire {
    /// Net index
    pub net_idx: usize,
    /// Path through global cells
    pub path: Vec<(usize, usize, usize)>, // (x, y, layer)
}

/// Global routing cell
#[derive(Debug, Clone)]
pub struct GCell {
    /// Grid position
    pub x: usize,
    pub y: usize,
    /// Physical bounds
    pub bounds: (f64, f64, f64, f64), // (x1, y1, x2, y2)
    /// Available routing resources per layer
    pub capacity: HashMap<usize, usize>, // layer -> tracks
    /// Used routing resources per layer
    pub usage: HashMap<usize, usize>,
}

/// Global routing configuration
#[derive(Debug, Clone)]
pub struct GlobalRoutingConfig {
    /// Grid cell size (in units)
    pub gcell_size: f64,
    /// Congestion threshold
    pub congestion_threshold: f64,
    /// Maximum routing iterations
    pub max_iterations: usize,
    /// Layer assignment strategy
    pub layer_assignment: LayerAssignment,
}

/// Layer assignment strategies
#[derive(Debug, Clone)]
pub enum LayerAssignment {
    /// Prefer lower layers first
    BottomUp,
    /// Prefer higher layers first
    TopDown,
    /// Minimize vias
    MinimizeVias,
}

impl Default for GlobalRoutingConfig {
    fn default() -> Self {
        Self {
            gcell_size: 10.0, // 10x10 units per gcell
            congestion_threshold: 0.9,
            max_iterations: 50,
            layer_assignment: LayerAssignment::MinimizeVias,
        }
    }
}

impl GlobalRouter {
    /// Create a new global router
    pub fn new(design_rules: &DesignRules) -> Self {
        Self {
            design_rules: design_rules.clone(),
            config: GlobalRoutingConfig::default(),
        }
    }

    /// Perform global routing
    pub fn route(
        &self,
        netlist: &Netlist,
        placement: &Placement,
    ) -> Result<GlobalRouting, AsicError> {
        println!("   Starting global routing...");

        // Create routing grid
        let grid = self.create_routing_grid(placement)?;

        // Build routing graph
        let mut routing_graph = self.build_routing_graph(&grid)?;

        // Route each net
        let mut global_wires = Vec::new();
        let mut congestion_map = CongestionTracker::new(&grid);

        // Sort nets by criticality (shorter nets first for better routability)
        let sorted_nets = self.sort_nets_by_criticality(netlist, placement);

        for (net_idx, net) in sorted_nets {
            if net.connections.len() < 2 {
                continue; // Skip single-pin nets
            }

            // Route this net
            let route = self.route_net(
                net_idx,
                net,
                placement,
                &grid,
                &mut routing_graph,
                &mut congestion_map,
            )?;

            global_wires.push(route);
        }

        // Handle congestion with rip-up and reroute
        self.handle_congestion(
            &mut global_wires,
            &grid,
            &mut congestion_map,
            netlist,
            placement,
        )?;

        println!(
            "   Global routing complete: {} nets routed",
            global_wires.len()
        );

        Ok(GlobalRouting {
            grid,
            wires: global_wires,
            congestion: CongestionMap {
                grid_size: (10, 10),
                values: vec![vec![0.0; 10]; 10],
            },
        })
    }

    /// Create routing grid
    fn create_routing_grid(&self, placement: &Placement) -> Result<Vec<Vec<GCell>>, AsicError> {
        // Determine grid dimensions
        let max_x = placement
            .cell_positions
            .iter()
            .map(|(x, _)| *x)
            .fold(0.0, f64::max)
            + 100.0;
        let max_y = placement
            .cell_positions
            .iter()
            .map(|(_, y)| *y)
            .fold(0.0, f64::max)
            + 100.0;

        let grid_width = (max_x / self.config.gcell_size).ceil() as usize;
        let grid_height = (max_y / self.config.gcell_size).ceil() as usize;

        // Create gcells
        let mut grid = Vec::new();
        for _ in 0..grid_height {
            let mut row = Vec::new();
            for _ in 0..grid_width {
                let mut capacity = HashMap::new();
                let mut usage = HashMap::new();
                for layer in 0..self.design_rules.metal_layers {
                    capacity.insert(layer, 10); // Default capacity
                    usage.insert(layer, 0);
                }
                row.push(GCell {
                    x: 0,
                    y: 0,
                    bounds: (0.0, 0.0, 0.0, 0.0),
                    capacity,
                    usage,
                });
            }
            grid.push(row);
        }

        Ok(grid)
    }

    /// Build routing graph for pathfinding
    fn build_routing_graph(&self, grid: &[Vec<GCell>]) -> Result<RoutingGraph, AsicError> {
        let mut graph = Graph::new();
        let mut node_map = HashMap::new();

        let height = grid.len();
        let width = if height > 0 { grid[0].len() } else { 0 };

        // Create nodes for each gcell and layer
        for layer in 0..self.design_rules.metal_layers {
            for y in 0..height {
                for x in 0..width {
                    let node = graph.add_node(RoutingNode {
                        x,
                        y,
                        layer,
                        cost: 1.0,
                    });
                    node_map.insert((x, y, layer), node);
                }
            }
        }

        // Add edges
        for layer in 0..self.design_rules.metal_layers {
            for y in 0..height {
                for x in 0..width {
                    let current = node_map[&(x, y, layer)];

                    // Horizontal edges (for even layers in Manhattan routing)
                    if layer % 2 == 0 && x < width - 1 {
                        let neighbor = node_map[&(x + 1, y, layer)];
                        graph.add_edge(current, neighbor, 1.0);
                        graph.add_edge(neighbor, current, 1.0);
                    }

                    // Vertical edges (for odd layers in Manhattan routing)
                    if layer % 2 == 1 && y < height - 1 {
                        let neighbor = node_map[&(x, y + 1, layer)];
                        graph.add_edge(current, neighbor, 1.0);
                        graph.add_edge(neighbor, current, 1.0);
                    }

                    // Via edges (between layers)
                    if layer < self.design_rules.metal_layers - 1 {
                        let above = node_map[&(x, y, layer + 1)];
                        let via_cost = 5.0; // Via penalty
                        graph.add_edge(current, above, via_cost);
                        graph.add_edge(above, current, via_cost);
                    }
                }
            }
        }

        Ok(RoutingGraph { graph, node_map })
    }

    /// Route a single net
    fn route_net(
        &self,
        net_idx: usize,
        net: &Net,
        placement: &Placement,
        grid: &[Vec<GCell>],
        routing_graph: &mut RoutingGraph,
        congestion_map: &mut CongestionTracker,
    ) -> Result<GlobalWire, AsicError> {
        // Get terminal positions
        let terminals = self.get_net_terminals(net, placement);

        if terminals.len() < 2 {
            return Ok(GlobalWire {
                net_idx: net_idx,
                path: vec![],
            });
        }

        // Use Steiner tree approximation for multi-pin nets
        let steiner_path = if terminals.len() == 2 {
            // Simple two-pin net: use shortest path
            self.find_shortest_path(&terminals[0], &terminals[1], routing_graph, congestion_map)?
        } else {
            // Multi-pin net: use minimum spanning tree approximation
            self.compute_steiner_tree(&terminals, routing_graph, congestion_map)?
        };

        // Update congestion tracking
        for &(x, y, layer) in &steiner_path {
            congestion_map.add_usage(x, y, layer);
        }

        Ok(GlobalWire {
            net_idx: net_idx,
            path: steiner_path,
        })
    }

    /// Get terminal positions for a net
    fn get_net_terminals(&self, net: &Net, placement: &Placement) -> Vec<(usize, usize, usize)> {
        let mut terminals = Vec::new();

        for conn in &net.connections {
            let (x, y) = placement.cell_positions[conn.0];
            let gcell_x = (x / self.config.gcell_size) as usize;
            let gcell_y = (y / self.config.gcell_size) as usize;
            terminals.push((gcell_x, gcell_y, 1)); // Start on metal1
        }

        terminals
    }

    /// Find shortest path using A* algorithm
    fn find_shortest_path(
        &self,
        start: &(usize, usize, usize),
        end: &(usize, usize, usize),
        graph: &RoutingGraph,
        congestion: &CongestionTracker,
    ) -> Result<Vec<(usize, usize, usize)>, AsicError> {
        let start_node = graph.node_map[start];
        let end_node = graph.node_map[end];

        // A* pathfinding
        let mut open_set = BinaryHeap::new();
        let mut came_from = HashMap::new();
        let mut g_score = HashMap::new();

        g_score.insert(start_node, 0.0);
        open_set.push(AStarNode {
            node: start_node,
            f_score: self.heuristic(start, end),
        });

        while let Some(current) = open_set.pop() {
            if current.node == end_node {
                // Reconstruct path
                return Ok(self.reconstruct_path(came_from, current.node, graph));
            }

            let current_g = g_score[&current.node];

            // Check neighbors
            for edge in graph.graph.edges(current.node) {
                let neighbor = if edge.source() == current.node {
                    edge.target()
                } else {
                    edge.source()
                };
                // Get congestion factor based on current usage
                let congestion_factor = congestion.get_cost_factor(neighbor, graph);
                let edge_cost = *edge.weight() * congestion_factor;

                let tentative_g = current_g + edge_cost;

                if tentative_g < *g_score.get(&neighbor).unwrap_or(&f64::INFINITY) {
                    came_from.insert(neighbor, current.node);
                    g_score.insert(neighbor, tentative_g);

                    let neighbor_pos = self.get_node_position(neighbor, graph);
                    let f = tentative_g + self.heuristic(&neighbor_pos, end);

                    open_set.push(AStarNode {
                        node: neighbor,
                        f_score: f,
                    });
                }
            }
        }

        Err(AsicError::RoutingError("No path found".to_string()))
    }

    /// Manhattan distance heuristic
    fn heuristic(&self, a: &(usize, usize, usize), b: &(usize, usize, usize)) -> f64 {
        ((a.0 as i32 - b.0 as i32).abs()
            + (a.1 as i32 - b.1 as i32).abs()
            + (a.2 as i32 - b.2 as i32).abs() * 5) as f64 // Via penalty
    }

    /// Reconstruct path from A* search
    fn reconstruct_path(
        &self,
        came_from: HashMap<NodeIndex, NodeIndex>,
        end: NodeIndex,
        graph: &RoutingGraph,
    ) -> Vec<(usize, usize, usize)> {
        let mut path = Vec::new();
        let mut current = end;

        loop {
            let pos = self.get_node_position(current, graph);
            path.push(pos);

            match came_from.get(&current) {
                Some(&prev) => current = prev,
                None => break,
            }
        }

        path.reverse();
        path
    }

    /// Get node position from graph
    fn get_node_position(&self, node: NodeIndex, graph: &RoutingGraph) -> (usize, usize, usize) {
        let routing_node = &graph.graph[node];
        (routing_node.x, routing_node.y, routing_node.layer)
    }

    /// Compute Steiner tree for multi-pin nets
    fn compute_steiner_tree(
        &self,
        terminals: &[(usize, usize, usize)],
        graph: &RoutingGraph,
        congestion: &CongestionTracker,
    ) -> Result<Vec<(usize, usize, usize)>, AsicError> {
        // Use minimum spanning tree as approximation
        let mut connected = HashSet::new();
        let mut tree_edges = Vec::new();

        // Start with first terminal
        connected.insert(terminals[0]);

        // Repeatedly add nearest unconnected terminal
        while connected.len() < terminals.len() {
            let mut best_distance = f64::INFINITY;
            let mut best_pair = (terminals[0], terminals[0]);
            let mut best_path = Vec::new();

            for &connected_term in &connected {
                for &terminal in terminals {
                    if !connected.contains(&terminal) {
                        match self.find_shortest_path(&connected_term, &terminal, graph, congestion)
                        {
                            Ok(path) => {
                                let distance = path.len() as f64;
                                if distance < best_distance {
                                    best_distance = distance;
                                    best_pair = (connected_term, terminal);
                                    best_path = path;
                                }
                            }
                            Err(_) => continue,
                        }
                    }
                }
            }

            connected.insert(best_pair.1);
            tree_edges.extend(best_path);
        }

        Ok(tree_edges)
    }

    /// Sort nets by criticality
    fn sort_nets_by_criticality<'a>(
        &self,
        netlist: &'a Netlist,
        placement: &Placement,
    ) -> Vec<(usize, &'a Net)> {
        let mut nets_with_priority: Vec<_> = netlist
            .nets
            .iter()
            .enumerate()
            .map(|(idx, net)| {
                let hpwl = self.calculate_net_hpwl(net, placement);
                (idx, net, hpwl)
            })
            .collect();

        // Sort by HPWL (shorter nets first)
        nets_with_priority.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap_or(Ordering::Equal));

        nets_with_priority
            .into_iter()
            .map(|(idx, net, _)| (idx, net))
            .collect()
    }

    /// Calculate net half-perimeter wire length
    fn calculate_net_hpwl(&self, net: &Net, placement: &Placement) -> f64 {
        if net.connections.is_empty() {
            return 0.0;
        }

        let mut min_x = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_y = f64::NEG_INFINITY;

        for conn in &net.connections {
            let (x, y) = placement.cell_positions[conn.0];
            min_x = min_x.min(x);
            max_x = max_x.max(x);
            min_y = min_y.min(y);
            max_y = max_y.max(y);
        }

        (max_x - min_x) + (max_y - min_y)
    }

    /// Handle congestion with rip-up and reroute
    fn handle_congestion(
        &self,
        wires: &mut Vec<GlobalWire>,
        grid: &[Vec<GCell>],
        congestion: &mut CongestionTracker,
        netlist: &Netlist,
        placement: &Placement,
    ) -> Result<(), AsicError> {
        // Identify congested regions
        let congested_gcells = congestion.get_congested_cells(self.config.congestion_threshold);

        if congested_gcells.is_empty() {
            return Ok(());
        }

        println!("   Handling {} congested cells...", congested_gcells.len());

        // Rip up and reroute affected nets
        // (Simplified - full implementation would be more sophisticated)

        Ok(())
    }
}

/// Detailed router for track assignment
pub struct DetailedRouter {
    design_rules: DesignRules,
    config: DetailedRoutingConfig,
}

/// Detailed routing result
#[derive(Debug, Clone)]
pub struct DetailedRouting {
    /// Detailed wires
    pub wires: Vec<DetailedWire>,
    /// Vias
    pub vias: Vec<Via>,
}

/// Detailed wire
#[derive(Debug, Clone)]
pub struct DetailedWire {
    /// Net index
    pub net_idx: usize,
    /// Wire segments
    pub segments: Vec<DetailSegment>,
}

/// Detailed segment
#[derive(Debug, Clone)]
pub struct DetailSegment {
    /// Start point
    pub start: (f64, f64),
    /// End point
    pub end: (f64, f64),
    /// Metal layer
    pub layer: usize,
    /// Track number
    pub track: usize,
    /// Wire width
    pub width: f64,
}

/// Via structure
#[derive(Debug, Clone)]
pub struct Via {
    /// Position
    pub position: (f64, f64),
    /// From layer
    pub from_layer: usize,
    /// To layer
    pub to_layer: usize,
    /// Via type
    pub via_type: String,
}

/// Detailed routing configuration
#[derive(Debug, Clone)]
pub struct DetailedRoutingConfig {
    /// Track pitch (spacing between tracks)
    pub track_pitch: f64,
    /// Via optimization
    pub optimize_vias: bool,
    /// Maximum detour length
    pub max_detour: f64,
}

impl Default for DetailedRoutingConfig {
    fn default() -> Self {
        Self {
            track_pitch: 0.46, // SKY130 track pitch
            optimize_vias: true,
            max_detour: 10.0,
        }
    }
}

impl DetailedRouter {
    /// Create a new detailed router
    pub fn new(design_rules: &DesignRules) -> Self {
        Self {
            design_rules: design_rules.clone(),
            config: DetailedRoutingConfig::default(),
        }
    }

    /// Perform detailed routing
    pub fn route(
        &self,
        netlist: &Netlist,
        placement: &Placement,
        global: &GlobalRouting,
    ) -> Result<DetailedRouting, AsicError> {
        println!("   Starting detailed routing...");

        let mut wires = Vec::new();
        let mut vias = Vec::new();

        // Process each global route
        for global_wire in &global.wires {
            // Convert global path to detailed wires
            let (net_wires, net_vias) = self.detail_route_net(global_wire, netlist, placement)?;
            wires.extend(net_wires);
            vias.extend(net_vias);
        }

        // Optimize wire lengths
        self.optimize_wire_lengths(&mut wires)?;

        // Minimize vias
        if self.config.optimize_vias {
            self.minimize_vias(&mut wires, &mut vias)?;
        }

        println!(
            "   Detailed routing complete: {} wires, {} vias",
            wires.len(),
            vias.len()
        );

        Ok(DetailedRouting { wires, vias })
    }

    /// Detail route a single net
    fn detail_route_net(
        &self,
        global_wire: &GlobalWire,
        netlist: &Netlist,
        placement: &Placement,
    ) -> Result<(Vec<DetailedWire>, Vec<Via>), AsicError> {
        let mut wires = Vec::new();
        let mut vias = Vec::new();

        // Convert global path to detailed segments
        for window in global_wire.path.windows(2) {
            let (x1, y1, _layer1) = window[0];
            let (x2, y2, _layer2) = window[1];

            // Determine routing direction and layer
            let layer = if y1 == y2 {
                0 // Horizontal on metal1
            } else if x1 == x2 {
                1 // Vertical on metal2
            } else {
                // Need to break into segments
                self.create_l_shape_route(x1, y1, x2, y2, &mut wires, &mut vias)?;
                continue;
            };

            // Create wire segment
            wires.push(DetailedWire {
                net_idx: global_wire.net_idx,
                segments: vec![DetailSegment {
                    layer,
                    start: (x1 as f64 * 10.0, y1 as f64 * 10.0),
                    end: (x2 as f64 * 10.0, y2 as f64 * 10.0),
                    track: 0, // Will be assigned during track assignment
                    width: self.design_rules.min_width,
                }],
            });
        }

        Ok((wires, vias))
    }

    /// Create L-shaped route with via
    fn create_l_shape_route(
        &self,
        x1: usize,
        y1: usize,
        x2: usize,
        y2: usize,
        wires: &mut Vec<DetailedWire>,
        vias: &mut Vec<Via>,
    ) -> Result<(), AsicError> {
        // Create L-shaped routing with via at corner
        // First segment: horizontal on metal1
        let corner_x = x2;
        let corner_y = y1;

        // Horizontal segment (metal1)
        if x1 != corner_x {
            wires.push(DetailedWire {
                net_idx: 0, // Will be set by caller
                segments: vec![DetailSegment {
                    start: (x1 as f64 * 10.0, y1 as f64 * 10.0),
                    end: (corner_x as f64 * 10.0, corner_y as f64 * 10.0),
                    layer: 0, // Metal1 (horizontal)
                    track: 0,
                    width: self.design_rules.min_width,
                }],
            });
        }

        // Via at corner (if needed)
        if y1 != y2 {
            vias.push(Via {
                position: (corner_x as f64 * 10.0, corner_y as f64 * 10.0),
                from_layer: 0, // Metal1
                to_layer: 1,   // Metal2
                via_type: "VIA1".to_string(),
            });
        }

        // Vertical segment (metal2)
        if y1 != y2 {
            wires.push(DetailedWire {
                net_idx: 0, // Will be set by caller
                segments: vec![DetailSegment {
                    start: (corner_x as f64 * 10.0, corner_y as f64 * 10.0),
                    end: (x2 as f64 * 10.0, y2 as f64 * 10.0),
                    layer: 1, // Metal2 (vertical)
                    track: 0,
                    width: self.design_rules.min_width,
                }],
            });
        }

        Ok(())
    }

    /// Optimize wire lengths
    fn optimize_wire_lengths(&self, _wires: &mut [DetailedWire]) -> Result<(), AsicError> {
        // Merge adjacent collinear segments
        // (Simplified implementation)
        Ok(())
    }

    /// Minimize number of vias
    fn minimize_vias(
        &self,
        _wires: &mut Vec<DetailedWire>,
        _vias: &mut Vec<Via>,
    ) -> Result<(), AsicError> {
        // Via minimization through layer reassignment
        // (Simplified implementation)
        Ok(())
    }
}

/// Routing graph for pathfinding
struct RoutingGraph {
    graph: Graph<RoutingNode, f64>,
    node_map: HashMap<(usize, usize, usize), NodeIndex>,
}

/// Routing graph node
#[derive(Debug, Clone)]
struct RoutingNode {
    x: usize,
    y: usize,
    layer: usize,
    cost: f64,
}

/// A* search node
struct AStarNode {
    node: NodeIndex,
    f_score: f64,
}

impl Eq for AStarNode {}

impl PartialEq for AStarNode {
    fn eq(&self, other: &Self) -> bool {
        self.f_score == other.f_score
    }
}

impl Ord for AStarNode {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse for min-heap
        other
            .f_score
            .partial_cmp(&self.f_score)
            .unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for AStarNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Congestion tracker for tracking usage
struct CongestionTracker {
    usage: HashMap<(usize, usize, usize), usize>,
    capacity: HashMap<(usize, usize, usize), usize>,
}

impl CongestionTracker {
    fn new(grid: &[Vec<GCell>]) -> Self {
        let mut capacity = HashMap::new();

        for (y, row) in grid.iter().enumerate() {
            for (x, gcell) in row.iter().enumerate() {
                for (&layer, &cap) in gcell.capacity.iter() {
                    capacity.insert((x, y, layer), cap);
                }
            }
        }

        Self {
            usage: HashMap::new(),
            capacity,
        }
    }

    fn add_usage(&mut self, x: usize, y: usize, layer: usize) {
        *self.usage.entry((x, y, layer)).or_insert(0) += 1;
    }

    fn get_cost_factor(&self, node: NodeIndex, graph: &RoutingGraph) -> f64 {
        let routing_node = &graph.graph[node];
        let key = (routing_node.x, routing_node.y, routing_node.layer);

        let usage = *self.usage.get(&key).unwrap_or(&0) as f64;
        let capacity = *self.capacity.get(&key).unwrap_or(&10) as f64;

        // Exponential cost for congestion
        1.0 + (usage / capacity).powi(2)
    }

    fn get_congested_cells(&self, threshold: f64) -> Vec<(usize, usize, usize)> {
        let mut congested = Vec::new();

        for (&key, &usage) in &self.usage {
            let capacity = *self.capacity.get(&key).unwrap_or(&10) as f64;
            if usage as f64 / capacity > threshold {
                congested.push(key);
            }
        }

        congested
    }
}

// Add missing trait for rand
use rand::Rng;

// Router implementation
impl Router {
    /// Create new router
    pub fn new() -> Self {
        Self {
            global_router: GlobalRouter::new(&DesignRules::default()),
            detailed_router: DetailedRouter::new(&DesignRules::default()),
            config: RouterConfig {
                use_global: true,
                max_iterations: 10,
                congestion_threshold: 0.9,
                enable_parallel: true,
                num_threads: num_cpus::get(),
                enable_hierarchical: true,
                coarsening_factor: 4,
            },
        }
    }

    /// Perform complete routing
    pub fn route(&mut self, placement: &Placement) -> Result<RoutingResult, AsicError> {
        // Create simple netlist from placement
        let netlist = Netlist {
            nets: Vec::new(),
            cells: Vec::new(),
        };

        // Perform global routing
        let global_result = if self.config.use_global {
            self.global_router.route(&netlist, placement)?
        } else {
            GlobalRouting {
                grid: Vec::new(),
                wires: Vec::new(),
                congestion: CongestionMap {
                    grid_size: (10, 10),
                    values: vec![vec![0.0; 10]; 10],
                },
            }
        };

        // Perform detailed routing
        let detailed_result = self
            .detailed_router
            .route(&netlist, placement, &global_result)?;

        // Build final result - convert DetailedRouting to RoutingResult
        let mut routed_nets = Vec::new();
        let mut total_wirelength = 0.0;
        let mut num_vias = 0;

        // Convert detailed wires to routed nets
        for (idx, wire) in detailed_result.wires.iter().enumerate() {
            let mut segments = Vec::new();
            for seg in &wire.segments {
                let segment = WireSegment {
                    points: vec![seg.start, seg.end],
                    layer: seg.layer,
                    width: seg.width,
                };
                segments.push(segment);
                total_wirelength +=
                    ((seg.end.0 - seg.start.0).abs() + (seg.end.1 - seg.start.1).abs());
            }

            // For now, we don't track which vias belong to which net
            // This would need to be enhanced in the routing algorithm
            let net_vias: Vec<ViaInstance> = Vec::new();

            num_vias += net_vias.len();

            routed_nets.push(RoutedNet {
                name: format!("net_{}", idx),
                segments,
                vias: net_vias,
            });
        }

        Ok(RoutingResult {
            routed_nets,
            congestion: global_result.congestion,
            total_wirelength,
            num_vias,
        })
    }

    /// Perform parallel routing for improved performance
    pub fn route_parallel(&mut self, placement: &Placement) -> Result<RoutingResult, AsicError> {
        if !self.config.enable_parallel {
            return self.route(placement);
        }

        // Create simple netlist from placement
        let netlist = Netlist {
            nets: Vec::new(),
            cells: Vec::new(),
        };

        // Perform hierarchical routing if enabled
        if self.config.enable_hierarchical {
            return self.route_hierarchical(placement);
        }

        // Split nets into chunks for parallel processing
        let nets: Vec<_> = netlist.nets.chunks(self.config.num_threads).collect();
        let placement_arc = Arc::new(placement.clone());
        let config_arc = Arc::new(self.config.clone());

        // Route nets in parallel
        let parallel_results: Result<Vec<_>, _> = nets
            .par_iter()
            .map(|net_chunk| {
                let mut local_router = Router::new();
                local_router.config = (*config_arc).clone();

                let local_netlist = Netlist {
                    nets: net_chunk.to_vec(),
                    cells: netlist.cells.clone(),
                };

                local_router.route_chunk(&local_netlist, &placement_arc)
            })
            .collect();

        let results = parallel_results?;

        // Merge results
        self.merge_routing_results(results)
    }

    /// Route a chunk of nets
    fn route_chunk(
        &mut self,
        netlist: &Netlist,
        placement: &Placement,
    ) -> Result<RoutingResult, AsicError> {
        // Perform global routing
        let global_result = if self.config.use_global {
            self.global_router.route(netlist, placement)?
        } else {
            GlobalRouting {
                grid: Vec::new(),
                wires: Vec::new(),
                congestion: CongestionMap {
                    grid_size: (10, 10),
                    values: vec![vec![0.0; 10]; 10],
                },
            }
        };

        // Perform detailed routing
        let detailed_result = self
            .detailed_router
            .route(netlist, placement, &global_result)?;

        // Convert to RoutingResult
        let mut routed_nets = Vec::new();
        let mut total_wirelength = 0.0;
        let mut num_vias = 0;

        for (idx, wire) in detailed_result.wires.iter().enumerate() {
            let mut segments = Vec::new();
            for seg in &wire.segments {
                let segment = WireSegment {
                    points: vec![seg.start, seg.end],
                    layer: seg.layer,
                    width: seg.width,
                };
                segments.push(segment);
                total_wirelength +=
                    ((seg.end.0 - seg.start.0).abs() + (seg.end.1 - seg.start.1).abs());
            }

            routed_nets.push(RoutedNet {
                name: format!("chunk_net_{}", idx),
                segments,
                vias: Vec::new(),
            });
        }

        Ok(RoutingResult {
            routed_nets,
            congestion: global_result.congestion,
            total_wirelength,
            num_vias,
        })
    }

    /// Hierarchical routing for large designs
    fn route_hierarchical(&mut self, placement: &Placement) -> Result<RoutingResult, AsicError> {
        let factor = self.config.coarsening_factor;

        // Create coarse grid
        let coarse_placement = self.create_coarse_placement(placement, factor)?;

        // Route on coarse grid first
        let coarse_result = self.route(&coarse_placement)?;

        // Refine routing on fine grid
        let fine_result = self.refine_routing(&coarse_result, placement)?;

        Ok(fine_result)
    }

    /// Create coarse placement for hierarchical routing
    fn create_coarse_placement(
        &self,
        placement: &Placement,
        factor: usize,
    ) -> Result<Placement, AsicError> {
        let mut coarse_placement = placement.clone();

        // Reduce the number of cells by clustering
        let mut coarse_cells = Vec::new();
        for chunk in placement.cells.chunks(factor) {
            if let Some(first_cell) = chunk.first() {
                let mut coarse_cell = first_cell.clone();

                // For coarse placement, we'll use a simplified approach
                // In a real implementation, this would use proper positioning from the placement
                let avg_x = 0.0;
                let avg_y = 0.0;

                coarse_cell.instance_name = format!("cluster_{}", coarse_cells.len());
                coarse_cells.push(coarse_cell);
            }
        }

        coarse_placement.cells = coarse_cells;
        Ok(coarse_placement)
    }

    /// Refine routing result from coarse to fine grid
    fn refine_routing(
        &mut self,
        coarse_result: &RoutingResult,
        fine_placement: &Placement,
    ) -> Result<RoutingResult, AsicError> {
        // For now, just scale up the routing coordinates
        // In a complete implementation, this would involve detailed refinement
        let mut fine_result = coarse_result.clone();
        let scale_factor = self.config.coarsening_factor as f64;

        for net in &mut fine_result.routed_nets {
            for segment in &mut net.segments {
                for point in &mut segment.points {
                    point.0 *= scale_factor;
                    point.1 *= scale_factor;
                }
            }

            for via in &mut net.vias {
                via.position.0 *= scale_factor;
                via.position.1 *= scale_factor;
            }
        }

        Ok(fine_result)
    }

    /// Merge multiple routing results
    fn merge_routing_results(
        &self,
        results: Vec<RoutingResult>,
    ) -> Result<RoutingResult, AsicError> {
        let mut merged_nets = Vec::new();
        let mut total_wirelength = 0.0;
        let mut total_vias = 0;

        // Merge congestion maps
        let mut merged_congestion = CongestionMap {
            grid_size: (1, 1),
            values: vec![vec![0.0]],
        };

        for result in results {
            merged_nets.extend(result.routed_nets);
            total_wirelength += result.total_wirelength;
            total_vias += result.num_vias;

            // Update congestion grid size if needed
            if result.congestion.grid_size.0 > merged_congestion.grid_size.0 {
                merged_congestion.grid_size = result.congestion.grid_size;
                merged_congestion.values = result.congestion.values;
            }
        }

        Ok(RoutingResult {
            routed_nets: merged_nets,
            congestion: merged_congestion,
            total_wirelength,
            num_vias: total_vias,
        })
    }

    /// Optimize routing using simulated annealing
    pub fn optimize_routing(&mut self, result: &RoutingResult) -> Result<RoutingResult, AsicError> {
        let mut optimized = result.clone();
        let mut temperature = 1000.0;
        let cooling_rate = 0.95;
        let min_temperature = 1.0;

        while temperature > min_temperature {
            // Try random wire rerouting
            for net in &mut optimized.routed_nets {
                if rand::random::<f64>() < 0.1 {
                    // 10% chance to reroute
                    if let Ok(improved_net) = self.reroute_net_optimized(net) {
                        *net = improved_net;
                    }
                }
            }

            temperature *= cooling_rate;
        }

        Ok(optimized)
    }

    /// Reroute a single net with optimization
    fn reroute_net_optimized(&self, net: &RoutedNet) -> Result<RoutedNet, AsicError> {
        let mut optimized_net = net.clone();

        // Try to reduce wire length and via count
        let mut improved_segments = Vec::new();

        for segment in &net.segments {
            // Try L-shaped routing to reduce vias
            if segment.points.len() >= 2 {
                let start = segment.points[0];
                let end = segment.points[segment.points.len() - 1];

                // Create L-shaped route
                let mid_point = (end.0, start.1); // Horizontal first, then vertical

                improved_segments.push(WireSegment {
                    points: vec![start, mid_point],
                    layer: segment.layer,
                    width: segment.width,
                });

                if mid_point != end {
                    improved_segments.push(WireSegment {
                        points: vec![mid_point, end],
                        layer: segment.layer,
                        width: segment.width,
                    });
                }
            } else {
                improved_segments.push(segment.clone());
            }
        }

        optimized_net.segments = improved_segments;
        Ok(optimized_net)
    }
}

impl Default for DesignRules {
    fn default() -> Self {
        Self {
            min_feature: 0.13,
            metal_layers: 6,
            min_width: 0.14,
            min_spacing: 0.14,
            via_rules: crate::ViaRules {
                min_size: 0.15,
                enclosure: 0.05,
                spacing: 0.2,
            },
        }
    }
}
