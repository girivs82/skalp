//! Web-based Interactive UI for ASIC Design
//!
//! Provides real-time visualization and manual adjustment capabilities

use crate::cts::ClockTree;
use crate::interactive::{
    BufferInsertion, DesignCheckpoint, DesignConstraints, InteractiveDesign, RoutingPoint,
};
use crate::placement::{Netlist, Placement};
use crate::routing::RoutingResult;
use crate::visualization::Visualizer;
use crate::AsicError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};

/// Web UI server for interactive ASIC design
pub struct WebUIServer {
    /// Current design state
    pub designer: Arc<Mutex<InteractiveDesign>>,
    /// Visualization generator
    pub visualizer: Visualizer,
    /// Active sessions
    pub sessions: HashMap<String, SessionState>,
    /// WebSocket connections
    pub connections: Vec<WebSocketClient>,
}

/// Client connection for real-time updates
pub struct WebSocketClient {
    /// Client ID
    pub id: String,
    /// Active view
    pub view: ViewType,
    /// Selected elements
    pub selection: Vec<String>,
}

/// Current view type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ViewType {
    Placement,
    Routing,
    ClockTree,
    Congestion,
    Timing,
    Power,
}

/// Session state for each user
#[derive(Debug, Clone)]
pub struct SessionState {
    /// Session ID
    pub id: String,
    /// Current checkpoint
    pub checkpoint: Option<DesignCheckpoint>,
    /// Undo stack
    pub undo_stack: Vec<DesignCheckpoint>,
    /// Redo stack
    pub redo_stack: Vec<DesignCheckpoint>,
    /// Active constraints
    pub constraints: DesignConstraints,
}

/// UI Commands from client
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UICommand {
    /// Move cell
    MoveCell {
        instance: String,
        x: f64,
        y: f64,
    },
    /// Fix cell position
    LockCell {
        instance: String,
        locked: bool,
    },
    /// Add routing guide
    AddRoutingGuide {
        net: String,
        layer: usize,
        path: Vec<(f64, f64)>,
    },
    /// Insert buffer
    InsertBuffer {
        net: String,
        position: (f64, f64),
        buffer_type: String,
    },
    /// Update constraint
    UpdateConstraint {
        constraint_type: String,
        value: serde_json::Value,
    },
    /// Run optimization
    RunOptimization {
        target: OptimizationTarget,
        options: OptimizationOptions,
    },
    /// Checkpoint operations
    SaveCheckpoint {
        name: String,
    },
    RestoreCheckpoint {
        name: String,
    },
    /// Undo/Redo
    Undo,
    Redo,
    /// View control
    ChangeView {
        view: ViewType,
    },
    /// Selection
    Select {
        elements: Vec<String>,
    },
}

/// Optimization targets
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OptimizationTarget {
    Timing,
    Power,
    Area,
    Congestion,
    ClockSkew,
}

/// Optimization options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizationOptions {
    /// Maximum iterations
    pub max_iterations: usize,
    /// Target improvement
    pub target_improvement: f64,
    /// Allow cell resizing
    pub allow_sizing: bool,
    /// Allow buffer insertion
    pub allow_buffering: bool,
}

/// UI Response to client
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UIResponse {
    /// Design update
    DesignUpdate {
        placement: PlacementData,
        routing: Option<RoutingData>,
        metrics: DesignMetrics,
    },
    /// Constraint violation
    ConstraintViolation { violations: Vec<Violation> },
    /// Operation result
    OperationResult {
        success: bool,
        message: String,
        changes: Vec<DesignChange>,
    },
    /// Real-time metric update
    MetricUpdate { metric: String, value: f64 },
}

/// Simplified placement data for UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlacementData {
    pub cells: Vec<CellData>,
    pub nets: Vec<NetData>,
    pub rows: Vec<RowData>,
}

/// Cell information for UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellData {
    pub id: String,
    pub cell_type: String,
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
    pub orientation: String,
    pub locked: bool,
    pub critical: bool,
}

/// Net information for UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetData {
    pub id: String,
    pub connections: Vec<String>,
    pub criticality: f64,
    pub wire_length: f64,
    pub routed: bool,
}

/// Row information for UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RowData {
    pub id: usize,
    pub y: f64,
    pub height: f64,
    pub utilization: f64,
}

/// Routing data for UI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingData {
    pub routes: Vec<RouteData>,
    pub congestion: Vec<CongestionData>,
    pub violations: Vec<RoutingViolation>,
}

/// Individual route data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteData {
    pub net: String,
    pub segments: Vec<RouteSegment>,
    pub layer: usize,
    pub length: f64,
}

/// Route segment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteSegment {
    pub start: (f64, f64),
    pub end: (f64, f64),
    pub layer: usize,
    pub via: Option<ViaData>,
}

/// Wire information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WireData {
    pub net_name: String,
    pub layer: usize,
    pub start: (f64, f64),
    pub end: (f64, f64),
    pub width: f64,
}

/// Via information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViaData {
    pub net_name: String,
    pub position: (f64, f64),
    pub from_layer: usize,
    pub to_layer: usize,
    pub via_type: String,
}

/// Congestion information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CongestionData {
    pub x: usize,
    pub y: usize,
    pub congestion: f64,
    pub capacity: f64,
    pub layer: usize,
}

/// Routing violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingViolation {
    pub violation_type: String,
    pub location: (f64, f64),
    pub nets: Vec<String>,
    pub severity: String,
}

/// Design metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignMetrics {
    pub timing: TimingMetrics,
    pub power: PowerMetrics,
    pub area: AreaMetrics,
    pub quality: QualityMetrics,
}

/// Timing metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingMetrics {
    pub worst_slack: f64,
    pub total_negative_slack: f64,
    pub worst_hold: f64,
    pub clock_skew: f64,
    pub critical_paths: usize,
}

/// Power metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerMetrics {
    pub total_power: f64,
    pub dynamic_power: f64,
    pub leakage_power: f64,
    pub clock_power: f64,
}

/// Area metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AreaMetrics {
    pub total_area: f64,
    pub cell_area: f64,
    pub utilization: f64,
}

/// Quality metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    pub wirelength: f64,
    pub congestion: f64,
    pub drc_violations: usize,
}

/// Constraint violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Violation {
    pub constraint_type: String,
    pub element: String,
    pub value: f64,
    pub limit: f64,
    pub severity: String,
}

/// Design change for undo/redo
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignChange {
    pub change_type: String,
    pub element: String,
    pub old_value: serde_json::Value,
    pub new_value: serde_json::Value,
}

impl WebUIServer {
    /// Create new web UI server
    pub fn new(designer: InteractiveDesign) -> Self {
        Self {
            designer: Arc::new(Mutex::new(designer)),
            visualizer: Visualizer::new(),
            sessions: HashMap::new(),
            connections: Vec::new(),
        }
    }

    /// Handle UI command from client
    pub fn handle_command(
        &mut self,
        session_id: &str,
        command: UICommand,
    ) -> Result<UIResponse, AsicError> {
        match command {
            UICommand::MoveCell { instance, x, y } => {
                self.save_checkpoint(session_id)?;
                let mut designer = self.designer.lock().unwrap();
                designer
                    .overrides
                    .cell_positions
                    .insert(instance.clone(), (x, y));
                self.update_placement(&mut designer)?;
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::LockCell {
                instance: _,
                locked: _,
            } => {
                // Lock functionality would need separate tracking
                // For now, just acknowledge the command
                let designer = self.designer.lock().unwrap();
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::AddRoutingGuide { net, layer, path } => {
                let mut designer = self.designer.lock().unwrap();
                let routing_points: Vec<RoutingPoint> = path
                    .into_iter()
                    .map(|pos| RoutingPoint {
                        position: pos,
                        layer,
                        width: None,
                    })
                    .collect();
                designer.overrides.routing_paths.insert(net, routing_points);
                self.update_routing(&mut designer)?;
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::InsertBuffer {
                net,
                position,
                buffer_type,
            } => {
                let mut designer = self.designer.lock().unwrap();
                designer.overrides.buffer_insertions.push(BufferInsertion {
                    net,
                    position,
                    buffer_type,
                    drive_strength: 1.0, // Default drive strength
                });
                self.update_routing(&mut designer)?;
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::UpdateConstraint {
                constraint_type,
                value,
            } => {
                let mut designer = self.designer.lock().unwrap();
                self.update_constraint(&mut designer, &constraint_type, value)?;
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::RunOptimization { target, options } => {
                let mut designer = self.designer.lock().unwrap();
                let changes = self.run_optimization(&mut designer, target, options)?;
                Ok(UIResponse::OperationResult {
                    success: true,
                    message: "Optimization completed".to_string(),
                    changes,
                })
            }

            UICommand::SaveCheckpoint { name } => {
                let mut designer = self.designer.lock().unwrap();
                // Create checkpoint from current state
                let checkpoint = DesignCheckpoint {
                    name: name.clone(),
                    placement: designer.placement.clone(),
                    routing: designer.routing.clone(),
                    clock_tree: designer.clock_tree.clone(),
                    timestamp: std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs(),
                    metrics: crate::interactive::DesignMetrics::default(),
                };
                designer.checkpoints.push(checkpoint);
                Ok(UIResponse::OperationResult {
                    success: true,
                    message: format!("Checkpoint '{}' saved", name),
                    changes: Vec::new(),
                })
            }

            UICommand::RestoreCheckpoint { name } => {
                let mut designer = self.designer.lock().unwrap();
                // Find and restore checkpoint
                if let Some(checkpoint) = designer.checkpoints.iter().find(|c| c.name == name) {
                    let checkpoint = checkpoint.clone(); // Clone to avoid borrow issues
                    designer.placement = checkpoint.placement;
                    designer.routing = checkpoint.routing;
                    designer.clock_tree = checkpoint.clock_tree;
                    Ok(self.create_design_update(&designer)?)
                } else {
                    Ok(UIResponse::OperationResult {
                        success: false,
                        message: format!("Checkpoint '{}' not found", name),
                        changes: Vec::new(),
                    })
                }
            }

            UICommand::Undo => {
                self.undo(session_id)?;
                let designer = self.designer.lock().unwrap();
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::Redo => {
                self.redo(session_id)?;
                let designer = self.designer.lock().unwrap();
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::ChangeView { view } => {
                self.set_view(session_id, view);
                let designer = self.designer.lock().unwrap();
                Ok(self.create_design_update(&designer)?)
            }

            UICommand::Select { elements } => {
                self.set_selection(session_id, elements);
                let designer = self.designer.lock().unwrap();
                Ok(self.create_design_update(&designer)?)
            }
        }
    }

    /// Create design update response
    fn create_design_update(&self, designer: &InteractiveDesign) -> Result<UIResponse, AsicError> {
        Ok(UIResponse::DesignUpdate {
            placement: self.extract_placement_data(designer)?,
            routing: self.extract_routing_data(designer)?,
            metrics: self.calculate_metrics(designer)?,
        })
    }

    /// Extract placement data for UI
    fn extract_placement_data(
        &self,
        designer: &InteractiveDesign,
    ) -> Result<PlacementData, AsicError> {
        // Extract placement information
        let cells: Vec<CellData> = if let Some(placement) = &designer.placement {
            placement
                .cells
                .iter()
                .enumerate()
                .map(|(i, cell)| {
                    let position = if i < placement.cell_positions.len() {
                        placement.cell_positions[i]
                    } else {
                        (0.0, 0.0)
                    };

                    CellData {
                        id: cell.instance_name.clone(),
                        cell_type: cell.cell_type.clone(),
                        x: position.0,
                        y: position.1,
                        width: 1.0,  // Default width
                        height: 1.0, // Default height
                        orientation: if i < placement.orientation.len() {
                            match placement.orientation[i] {
                                crate::placement::Orientation::R0 => "R0".to_string(),
                                crate::placement::Orientation::R90 => "R90".to_string(),
                                crate::placement::Orientation::R180 => "R180".to_string(),
                                crate::placement::Orientation::R270 => "R270".to_string(),
                                _ => "R0".to_string(),
                            }
                        } else {
                            "R0".to_string()
                        },
                        locked: false,   // Default unlocked
                        critical: false, // Default not critical
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        let nets: Vec<NetData> = Vec::new(); // Would need netlist access

        let rows: Vec<RowData> = if let Some(placement) = &designer.placement {
            placement
                .rows
                .iter()
                .enumerate()
                .map(|(i, row)| {
                    RowData {
                        id: i,
                        y: row.y,
                        height: row.height,
                        utilization: 0.5, // Default utilization
                    }
                })
                .collect()
        } else {
            Vec::new()
        };

        Ok(PlacementData { cells, nets, rows })
    }

    /// Extract routing data for UI
    fn extract_routing_data(
        &self,
        designer: &InteractiveDesign,
    ) -> Result<Option<RoutingData>, AsicError> {
        // Extract routing information if available
        if let Some(routing) = &designer.routing {
            let wires: Vec<WireData> = routing
                .routed_nets
                .iter()
                .flat_map(|net| {
                    net.segments.iter().map(|seg| WireData {
                        net_name: net.name.clone(),
                        layer: seg.layer,
                        start: if seg.points.len() >= 1 {
                            seg.points[0]
                        } else {
                            (0.0, 0.0)
                        },
                        end: if seg.points.len() >= 2 {
                            seg.points[seg.points.len() - 1]
                        } else {
                            (0.0, 0.0)
                        },
                        width: seg.width,
                    })
                })
                .collect();

            let vias: Vec<ViaData> = routing
                .routed_nets
                .iter()
                .flat_map(|net| {
                    net.vias.iter().map(|via| ViaData {
                        net_name: net.name.clone(),
                        position: via.position,
                        from_layer: via.from_layer,
                        to_layer: via.to_layer,
                        via_type: format!("Via{}_{}", via.from_layer, via.to_layer),
                    })
                })
                .collect();

            let congestion_data: Vec<CongestionData> = routing
                .congestion
                .values
                .iter()
                .enumerate()
                .flat_map(|(y, row)| {
                    row.iter().enumerate().map(move |(x, &value)| {
                        CongestionData {
                            x,
                            y,
                            congestion: value,
                            capacity: 10.0, // Default capacity
                            layer: 1,       // Default layer
                        }
                    })
                })
                .collect();

            // Convert to expected format
            let routes: Vec<RouteData> = routing
                .routed_nets
                .iter()
                .map(|net| {
                    let segments: Vec<RouteSegment> = net
                        .segments
                        .iter()
                        .map(|seg| {
                            RouteSegment {
                                start: if seg.points.len() >= 1 {
                                    seg.points[0]
                                } else {
                                    (0.0, 0.0)
                                },
                                end: if seg.points.len() >= 2 {
                                    seg.points[seg.points.len() - 1]
                                } else {
                                    (0.0, 0.0)
                                },
                                layer: seg.layer,
                                via: None, // Simplified for now
                            }
                        })
                        .collect();

                    RouteData {
                        net: net.name.clone(),
                        segments,
                        layer: net.segments.get(0).map(|s| s.layer).unwrap_or(1),
                        length: net
                            .segments
                            .iter()
                            .map(|s| {
                                if s.points.len() >= 2 {
                                    let start = s.points[0];
                                    let end = s.points[s.points.len() - 1];
                                    ((end.0 - start.0).powi(2) + (end.1 - start.1).powi(2)).sqrt()
                                } else {
                                    0.0
                                }
                            })
                            .sum(),
                    }
                })
                .collect();

            Ok(Some(RoutingData {
                routes,
                congestion: congestion_data,
                violations: Vec::new(), // Empty for now
            }))
        } else {
            Ok(None)
        }
    }

    /// Calculate design metrics
    fn calculate_metrics(&self, _designer: &InteractiveDesign) -> Result<DesignMetrics, AsicError> {
        Ok(DesignMetrics {
            timing: TimingMetrics {
                worst_slack: 0.0,
                total_negative_slack: 0.0,
                worst_hold: 0.0,
                clock_skew: 0.0,
                critical_paths: 0,
            },
            power: PowerMetrics {
                total_power: 0.0,
                dynamic_power: 0.0,
                leakage_power: 0.0,
                clock_power: 0.0,
            },
            area: AreaMetrics {
                total_area: 0.0,
                cell_area: 0.0,
                utilization: 0.0,
            },
            quality: QualityMetrics {
                wirelength: 0.0,
                congestion: 0.0,
                drc_violations: 0,
            },
        })
    }

    /// Update placement
    fn update_placement(&self, designer: &mut InteractiveDesign) -> Result<(), AsicError> {
        // Apply manual position overrides to placement
        if let Some(placement) = &mut designer.placement {
            for (instance_name, &(x, y)) in &designer.overrides.cell_positions {
                // Find the cell in the placement and update its position
                if let Some(cell_idx) = placement
                    .cells
                    .iter()
                    .position(|cell| cell.instance_name == *instance_name)
                {
                    // Update cell position
                    if cell_idx < placement.cell_positions.len() {
                        placement.cell_positions[cell_idx] = (x, y);
                    }

                    // Update position mapping
                    placement.positions.insert(instance_name.clone(), (x, y));
                }
            }
        }

        // Trigger incremental legalization if needed
        // This would call incremental placement algorithms

        Ok(())
    }

    /// Update routing
    fn update_routing(&self, designer: &mut InteractiveDesign) -> Result<(), AsicError> {
        // Apply routing guides and buffer insertions
        if let Some(routing) = &mut designer.routing {
            // Apply manual routing paths
            for (net_name, routing_points) in &designer.overrides.routing_paths {
                // Convert routing points to wire segments
                let segments: Vec<crate::routing::WireSegment> = routing_points
                    .windows(2)
                    .map(|window| {
                        crate::routing::WireSegment {
                            points: vec![window[0].position, window[1].position],
                            layer: window[0].layer,
                            width: window[0].width.unwrap_or(0.14), // Default width
                        }
                    })
                    .collect();

                // Update or add net to routing
                if let Some(net) = routing.routed_nets.iter_mut().find(|n| n.name == *net_name) {
                    net.segments = segments;
                } else {
                    routing.routed_nets.push(crate::routing::RoutedNet {
                        name: net_name.clone(),
                        segments,
                        vias: Vec::new(),
                    });
                }
            }

            // Apply buffer insertions
            for buffer in &designer.overrides.buffer_insertions {
                // Insert buffer as a via or split existing segment
                // This is a simplified implementation
                if let Some(net) = routing
                    .routed_nets
                    .iter_mut()
                    .find(|n| n.name == buffer.net)
                {
                    // Add via at buffer position
                    net.vias.push(crate::routing::ViaInstance {
                        position: buffer.position,
                        from_layer: 0,
                        to_layer: 1,
                        size: 0.1, // Default via size
                    });
                }
            }
        }

        Ok(())
    }

    /// Update constraint
    fn update_constraint(
        &self,
        designer: &mut InteractiveDesign,
        constraint_type: &str,
        value: serde_json::Value,
    ) -> Result<(), AsicError> {
        // TODO: Parse and apply constraint update
        match constraint_type {
            "timing" => {
                // Update timing constraints
            }
            "placement" => {
                // Update placement constraints
            }
            "routing" => {
                // Update routing constraints
            }
            _ => {}
        }
        Ok(())
    }

    /// Run optimization
    fn run_optimization(
        &self,
        _designer: &mut InteractiveDesign,
        _target: OptimizationTarget,
        _options: OptimizationOptions,
    ) -> Result<Vec<DesignChange>, AsicError> {
        // TODO: Run targeted optimization
        Ok(Vec::new())
    }

    /// Save checkpoint for session
    fn save_checkpoint(&mut self, session_id: &str) -> Result<(), AsicError> {
        // TODO: Implement checkpoint save
        Ok(())
    }

    /// Undo last operation
    fn undo(&mut self, session_id: &str) -> Result<(), AsicError> {
        if let Some(session) = self.sessions.get_mut(session_id) {
            if let Some(checkpoint) = session.undo_stack.pop() {
                if let Some(current) = &session.checkpoint {
                    session.redo_stack.push(current.clone());
                }
                session.checkpoint = Some(checkpoint);
                // TODO: Restore state
            }
        }
        Ok(())
    }

    /// Redo operation
    fn redo(&mut self, session_id: &str) -> Result<(), AsicError> {
        if let Some(session) = self.sessions.get_mut(session_id) {
            if let Some(checkpoint) = session.redo_stack.pop() {
                if let Some(current) = &session.checkpoint {
                    session.undo_stack.push(current.clone());
                }
                session.checkpoint = Some(checkpoint);
                // TODO: Restore state
            }
        }
        Ok(())
    }

    /// Set view for session
    fn set_view(&mut self, _session_id: &str, _view: ViewType) {
        // TODO: Update session view
    }

    /// Set selection for session
    fn set_selection(&mut self, _session_id: &str, _elements: Vec<String>) {
        // TODO: Update session selection
    }

    /// Generate HTML interface
    pub fn generate_html(&self) -> String {
        r#"<!DOCTYPE html>
<html>
<head>
    <title>SKALP ASIC Interactive Designer</title>
    <style>
        body {
            margin: 0;
            font-family: 'Segoe UI', sans-serif;
            background: #1e1e1e;
            color: #e0e0e0;
        }
        #main-container {
            display: flex;
            height: 100vh;
        }
        #canvas-container {
            flex: 1;
            position: relative;
            overflow: hidden;
        }
        #design-canvas {
            width: 100%;
            height: 100%;
            background: #2d2d30;
        }
        #sidebar {
            width: 350px;
            background: #252526;
            border-left: 1px solid #3c3c3c;
            overflow-y: auto;
        }
        .panel {
            padding: 15px;
            border-bottom: 1px solid #3c3c3c;
        }
        .panel h3 {
            margin: 0 0 10px 0;
            color: #4ec9b0;
            font-size: 14px;
            text-transform: uppercase;
        }
        .metric {
            display: flex;
            justify-content: space-between;
            padding: 5px 0;
            font-size: 13px;
        }
        .metric-value {
            font-weight: bold;
            color: #4fc1ff;
        }
        .critical { color: #f48771; }
        .warning { color: #ffcc00; }
        .good { color: #6bb644; }
        .toolbar {
            background: #2d2d30;
            padding: 10px;
            border-bottom: 1px solid #3c3c3c;
            display: flex;
            gap: 10px;
        }
        button {
            background: #0e639c;
            color: white;
            border: none;
            padding: 6px 12px;
            border-radius: 3px;
            cursor: pointer;
            font-size: 12px;
        }
        button:hover { background: #1177bb; }
        button:active { background: #005a9e; }
        .view-selector {
            display: flex;
            gap: 5px;
            margin-left: auto;
        }
        .view-btn {
            padding: 6px 10px;
            background: #3c3c3c;
        }
        .view-btn.active { background: #0e639c; }
        #status-bar {
            position: absolute;
            bottom: 0;
            left: 0;
            right: 0;
            background: #007acc;
            color: white;
            padding: 5px 10px;
            font-size: 12px;
        }
        .constraint-input {
            width: 100%;
            background: #3c3c3c;
            border: 1px solid #555;
            color: white;
            padding: 5px;
            margin: 5px 0;
            border-radius: 3px;
        }
        .cell-info {
            background: #2d2d30;
            padding: 10px;
            border-radius: 5px;
            margin: 10px 0;
        }
    </style>
</head>
<body>
    <div id="main-container">
        <div id="canvas-container">
            <div class="toolbar">
                <button onclick="undo()">↶ Undo</button>
                <button onclick="redo()">↷ Redo</button>
                <button onclick="saveCheckpoint()">💾 Save</button>
                <button onclick="optimize()">⚡ Optimize</button>
                <button onclick="runDRC()">✓ Check DRC</button>
                <div class="view-selector">
                    <button class="view-btn active" onclick="setView('placement')">Placement</button>
                    <button class="view-btn" onclick="setView('routing')">Routing</button>
                    <button class="view-btn" onclick="setView('clock')">Clock</button>
                    <button class="view-btn" onclick="setView('congestion')">Congestion</button>
                    <button class="view-btn" onclick="setView('timing')">Timing</button>
                </div>
            </div>
            <canvas id="design-canvas"></canvas>
            <div id="status-bar">Ready | Cells: 0 | Nets: 0 | WNS: 0.00ns</div>
        </div>

        <div id="sidebar">
            <div class="panel">
                <h3>Design Metrics</h3>
                <div class="metric">
                    <span>Worst Negative Slack</span>
                    <span class="metric-value critical">-0.15 ns</span>
                </div>
                <div class="metric">
                    <span>Total Negative Slack</span>
                    <span class="metric-value critical">-2.34 ns</span>
                </div>
                <div class="metric">
                    <span>Clock Skew</span>
                    <span class="metric-value warning">45 ps</span>
                </div>
                <div class="metric">
                    <span>Total Power</span>
                    <span class="metric-value">1.23 mW</span>
                </div>
                <div class="metric">
                    <span>Area Utilization</span>
                    <span class="metric-value good">68%</span>
                </div>
                <div class="metric">
                    <span>Total Wire Length</span>
                    <span class="metric-value">1234 µm</span>
                </div>
                <div class="metric">
                    <span>DRC Violations</span>
                    <span class="metric-value good">0</span>
                </div>
            </div>

            <div class="panel">
                <h3>Selected Cell</h3>
                <div class="cell-info" id="cell-info">
                    <div>No cell selected</div>
                </div>
            </div>

            <div class="panel">
                <h3>Constraints</h3>
                <label>Clock Period (ns):</label>
                <input type="number" class="constraint-input" value="5.0" step="0.1">
                <label>Max Fanout:</label>
                <input type="number" class="constraint-input" value="20" step="1">
                <label>Max Transition (ns):</label>
                <input type="number" class="constraint-input" value="0.5" step="0.1">
                <button onclick="applyConstraints()">Apply</button>
            </div>

            <div class="panel">
                <h3>Optimization</h3>
                <label>Target:</label>
                <select class="constraint-input">
                    <option>Timing</option>
                    <option>Power</option>
                    <option>Area</option>
                    <option>Congestion</option>
                </select>
                <button onclick="runOptimization()">Run</button>
            </div>
        </div>
    </div>

    <script>
        const canvas = document.getElementById('design-canvas');
        const ctx = canvas.getContext('2d');
        let ws = null;
        let currentView = 'placement';
        let selectedCells = [];
        let draggedCell = null;

        // Initialize WebSocket connection
        function initWebSocket() {
            ws = new WebSocket('ws://localhost:8080/ws');

            ws.onmessage = (event) => {
                const response = JSON.parse(event.data);
                handleResponse(response);
            };

            ws.onerror = (error) => {
                console.error('WebSocket error:', error);
            };
        }

        // Send command to server
        function sendCommand(command) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(command));
            }
        }

        // Handle server response
        function handleResponse(response) {
            if (response.DesignUpdate) {
                updateDesign(response.DesignUpdate);
            } else if (response.MetricUpdate) {
                updateMetric(response.MetricUpdate);
            }
        }

        // Update design visualization
        function updateDesign(update) {
            // Clear canvas
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            // Draw based on current view
            if (currentView === 'placement') {
                drawPlacement(update.placement);
            } else if (currentView === 'routing' && update.routing) {
                drawRouting(update.routing);
            }

            // Update metrics panel
            updateMetricsPanel(update.metrics);
        }

        // Draw placement
        function drawPlacement(placement) {
            // Draw rows
            ctx.strokeStyle = '#444';
            placement.rows.forEach(row => {
                ctx.strokeRect(0, row.y, canvas.width, row.height);
            });

            // Draw cells
            placement.cells.forEach(cell => {
                ctx.fillStyle = cell.critical ? '#f48771' : '#4ec9b0';
                ctx.fillRect(cell.x, cell.y, cell.width, cell.height);

                if (cell.locked) {
                    ctx.strokeStyle = '#ffcc00';
                    ctx.lineWidth = 2;
                    ctx.strokeRect(cell.x, cell.y, cell.width, cell.height);
                }
            });

            // Draw nets as flylines
            ctx.strokeStyle = '#4fc1ff33';
            ctx.lineWidth = 0.5;
            placement.nets.forEach(net => {
                // Draw simplified net connections
            });
        }

        // Mouse interaction handlers
        canvas.addEventListener('mousedown', (e) => {
            const rect = canvas.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;

            // Check if clicking on a cell
            // If so, start dragging
        });

        canvas.addEventListener('mousemove', (e) => {
            if (draggedCell) {
                const rect = canvas.getBoundingClientRect();
                const x = e.clientX - rect.left;
                const y = e.clientY - rect.top;

                sendCommand({
                    MoveCell: {
                        instance: draggedCell,
                        x: x,
                        y: y
                    }
                });
            }
        });

        canvas.addEventListener('mouseup', (e) => {
            draggedCell = null;
        });

        // UI functions
        function undo() {
            sendCommand({ Undo: {} });
        }

        function redo() {
            sendCommand({ Redo: {} });
        }

        function saveCheckpoint() {
            const name = prompt('Checkpoint name:');
            if (name) {
                sendCommand({ SaveCheckpoint: { name: name } });
            }
        }

        function setView(view) {
            currentView = view;
            document.querySelectorAll('.view-btn').forEach(btn => {
                btn.classList.remove('active');
            });
            event.target.classList.add('active');
            sendCommand({ ChangeView: { view: view } });
        }

        function optimize() {
            sendCommand({
                RunOptimization: {
                    target: 'Timing',
                    options: {
                        max_iterations: 100,
                        target_improvement: 0.1,
                        allow_sizing: true,
                        allow_buffering: true
                    }
                }
            });
        }

        // Initialize
        canvas.width = canvas.offsetWidth;
        canvas.height = canvas.offsetHeight;
        initWebSocket();
    </script>
</body>
</html>"#.to_string()
    }
}
