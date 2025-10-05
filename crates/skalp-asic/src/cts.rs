//! Native Clock Tree Synthesis (CTS) for ASIC Implementation
//!
//! Implements H-tree and balanced tree clock distribution with:
//! - Skew minimization
//! - Buffer insertion
//! - Load balancing
//! - Power optimization

use crate::placement::Placement;
use crate::routing::RoutingResult;
use crate::sky130::StandardCellLibrary;
use crate::{AsicError, DesignRules, Technology};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Clock specification for synthesis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockSpec {
    /// Clock name
    pub name: String,
    /// Frequency in MHz
    pub frequency: f64,
    /// Clock source location
    pub source: (f64, f64),
    /// Maximum skew
    pub max_skew: f64,
    /// Maximum fanout
    pub max_fanout: usize,
    /// Buffer types
    pub buffer_types: Vec<String>,
}

/// Clock tree synthesis engine
pub struct ClockTreeSynthesizer {
    /// Technology node
    pub technology: Technology,
    /// Design rules
    pub design_rules: DesignRules,
    /// Standard cell library
    pub std_cells: StandardCellLibrary,
}

/// Clock tree result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockTree {
    /// Clock source point
    pub source: ClockSource,
    /// Clock sinks (flip-flops)
    pub sinks: Vec<ClockSink>,
    /// Clock buffers inserted
    pub buffers: Vec<ClockBuffer>,
    /// Clock nets
    pub nets: Vec<ClockNet>,
    /// Clock tree topology
    pub topology: ClockTopology,
    /// Timing analysis
    pub timing: ClockTiming,
}

/// Clock source definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockSource {
    /// Source name
    pub name: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Drive strength
    pub drive_strength: f64,
    /// Target frequency (MHz)
    pub frequency: f64,
}

/// Clock sink (flip-flop)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockSink {
    /// Instance name
    pub instance: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Input capacitance
    pub capacitance: f64,
    /// Setup time requirement
    pub setup_time: f64,
    /// Hold time requirement
    pub hold_time: f64,
}

/// Clock buffer for driving
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockBuffer {
    /// Buffer instance name
    pub name: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Buffer type (from std cell lib)
    pub cell_type: String,
    /// Input capacitance
    pub input_cap: f64,
    /// Output drive strength
    pub drive_strength: f64,
    /// Propagation delay
    pub delay: f64,
}

/// Clock net segment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockNet {
    /// Net name
    pub name: String,
    /// Source (buffer or clock source)
    pub source: String,
    /// Sinks (buffers or flip-flops)
    pub sinks: Vec<String>,
    /// Wire segments
    pub segments: Vec<WireSegment>,
    /// Total wire length
    pub length: f64,
    /// Wire capacitance
    pub capacitance: f64,
    /// Wire resistance
    pub resistance: f64,
}

/// Wire segment in clock net
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WireSegment {
    /// Start point
    pub start: (f64, f64),
    /// End point
    pub end: (f64, f64),
    /// Metal layer
    pub layer: usize,
    /// Wire width
    pub width: f64,
}

/// Clock tree topology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClockTopology {
    /// H-tree balanced distribution
    HTree {
        levels: usize,
        branches: Vec<HTreeBranch>,
    },
    /// Balanced tree (binary or n-ary)
    BalancedTree {
        fanout: usize,
        levels: usize,
        nodes: Vec<TreeNode>,
    },
    /// Mesh clock distribution
    Mesh {
        rows: usize,
        cols: usize,
        grid_points: Vec<(f64, f64)>,
    },
    /// Hybrid approach
    Hybrid {
        global_topology: Box<ClockTopology>,
        local_topology: Box<ClockTopology>,
    },
}

/// H-tree branch
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HTreeBranch {
    /// Branch level
    pub level: usize,
    /// Center point
    pub center: (f64, f64),
    /// Branch length
    pub length: f64,
    /// Children branches
    pub children: Vec<usize>,
}

/// Tree node for balanced tree
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TreeNode {
    /// Node index
    pub index: usize,
    /// Position
    pub position: (f64, f64),
    /// Parent node
    pub parent: Option<usize>,
    /// Children nodes
    pub children: Vec<usize>,
    /// Buffer at this node
    pub buffer: Option<String>,
}

/// Clock timing analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockTiming {
    /// Maximum clock skew
    pub max_skew: f64,
    /// Minimum clock delay
    pub min_delay: f64,
    /// Maximum clock delay
    pub max_delay: f64,
    /// Clock period
    pub period: f64,
    /// Duty cycle
    pub duty_cycle: f64,
    /// Power consumption
    pub power: f64,
    /// Timing slack per sink
    pub sink_slack: HashMap<String, f64>,
}

/// Alias for compatibility
pub type ClockTreeTiming = ClockTiming;

impl ClockTreeSynthesizer {
    /// Create new clock tree synthesizer
    pub fn new(technology: Technology, design_rules: DesignRules) -> Self {
        Self {
            technology: technology.clone(),
            design_rules,
            std_cells: StandardCellLibrary::new(),
        }
    }

    /// Synthesize clock tree
    pub fn synthesize(
        &self,
        placement: &Placement,
        _routing: &RoutingResult,
        clock_spec: &ClockSpecification,
    ) -> Result<ClockTree, AsicError> {
        // Extract clock sinks from placement
        let sinks = self.extract_clock_sinks(placement)?;

        // Determine optimal topology
        let topology = self.select_topology(&sinks, clock_spec)?;

        // Build clock tree structure
        let (buffers, nets) = match &topology {
            ClockTopology::HTree { .. } => self.build_htree(&sinks, clock_spec)?,
            ClockTopology::BalancedTree { .. } => self.build_balanced_tree(&sinks, clock_spec)?,
            ClockTopology::Mesh { .. } => self.build_mesh(&sinks, clock_spec)?,
            ClockTopology::Hybrid { .. } => self.build_hybrid(&sinks, clock_spec)?,
        };

        // Perform buffer insertion
        let buffers = self.insert_buffers(buffers, &nets, clock_spec)?;

        // Balance clock tree for minimal skew
        let (buffers, nets) = self.balance_tree(buffers, nets, &sinks, clock_spec)?;

        // Analyze timing
        let timing = self.analyze_timing(&buffers, &nets, &sinks, clock_spec)?;

        Ok(ClockTree {
            source: clock_spec.source.clone(),
            sinks,
            buffers,
            nets,
            topology,
            timing,
        })
    }

    /// Extract clock sinks from placement
    fn extract_clock_sinks(&self, placement: &Placement) -> Result<Vec<ClockSink>, AsicError> {
        let mut sinks = Vec::new();

        for (instance, position) in &placement.positions {
            // Check if instance is a flip-flop or latch
            if let Some(cell) = placement
                .cells
                .iter()
                .find(|c| &c.instance_name == instance)
            {
                if cell.cell_type.contains("DFF") || cell.cell_type.contains("LATCH") {
                    sinks.push(ClockSink {
                        instance: instance.clone(),
                        position: *position,
                        capacitance: 0.01, // fF from cell library
                        setup_time: 0.1,   // ns
                        hold_time: 0.05,   // ns
                    });
                }
            }
        }

        if sinks.is_empty() {
            return Err(AsicError::PlacementError(
                "No clock sinks found".to_string(),
            ));
        }

        Ok(sinks)
    }

    /// Select optimal clock tree topology
    fn select_topology(
        &self,
        sinks: &[ClockSink],
        _spec: &ClockSpecification,
    ) -> Result<ClockTopology, AsicError> {
        let num_sinks = sinks.len();

        // For small designs, use balanced tree
        if num_sinks < 100 {
            let fanout = 4; // Optimal fanout for most technologies
            let levels = (num_sinks as f64).log(fanout as f64).ceil() as usize;

            Ok(ClockTopology::BalancedTree {
                fanout,
                levels,
                nodes: Vec::new(),
            })
        }
        // For medium designs, use H-tree
        else if num_sinks < 10000 {
            let levels = ((num_sinks as f64).log2() / 2.0).ceil() as usize;

            Ok(ClockTopology::HTree {
                levels,
                branches: Vec::new(),
            })
        }
        // For large designs, use hybrid approach
        else {
            let global = Box::new(ClockTopology::HTree {
                levels: 3,
                branches: Vec::new(),
            });

            let local = Box::new(ClockTopology::BalancedTree {
                fanout: 4,
                levels: 3,
                nodes: Vec::new(),
            });

            Ok(ClockTopology::Hybrid {
                global_topology: global,
                local_topology: local,
            })
        }
    }

    /// Build H-tree structure
    fn build_htree(
        &self,
        sinks: &[ClockSink],
        _spec: &ClockSpecification,
    ) -> Result<(Vec<ClockBuffer>, Vec<ClockNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        // Find bounding box of all sinks
        let (min_x, min_y, max_x, max_y) = self.find_bounding_box(sinks);
        let center_x = (min_x + max_x) / 2.0;
        let center_y = (min_y + max_y) / 2.0;

        // Build H-tree recursively
        self.build_htree_recursive(
            (center_x, center_y),
            (max_x - min_x) / 2.0,
            (max_y - min_y) / 2.0,
            0,
            3, // levels
            &mut buffers,
            &mut nets,
            sinks,
        )?;

        Ok((buffers, nets))
    }

    /// Recursive H-tree construction
    #[allow(clippy::too_many_arguments)]
    fn build_htree_recursive(
        &self,
        center: (f64, f64),
        half_width: f64,
        half_height: f64,
        level: usize,
        max_level: usize,
        buffers: &mut Vec<ClockBuffer>,
        nets: &mut Vec<ClockNet>,
        sinks: &[ClockSink],
    ) -> Result<(), AsicError> {
        if level >= max_level {
            // Connect to local sinks
            let local_sinks =
                self.find_local_sinks(center, half_width * 2.0, half_height * 2.0, sinks);
            if !local_sinks.is_empty() {
                // Add buffer at this point
                let buffer_name = format!("clk_buf_{}_{}", level, buffers.len());
                buffers.push(ClockBuffer {
                    name: buffer_name.clone(),
                    position: center,
                    cell_type: "sky130_fd_sc_hd__buf_4".to_string(),
                    input_cap: 0.02,
                    drive_strength: 4.0,
                    delay: 0.05,
                });

                // Create net connecting buffer to local sinks
                let net = ClockNet {
                    name: format!("clk_net_{}", nets.len()),
                    source: buffer_name,
                    sinks: local_sinks.iter().map(|s| s.instance.clone()).collect(),
                    segments: Vec::new(),
                    length: 0.0,
                    capacitance: 0.0,
                    resistance: 0.0,
                };
                nets.push(net);
            }
            return Ok(());
        }

        // Add buffer at center
        let buffer_name = format!("clk_buf_{}_{}", level, buffers.len());
        buffers.push(ClockBuffer {
            name: buffer_name.clone(),
            position: center,
            cell_type: "sky130_fd_sc_hd__buf_8".to_string(),
            input_cap: 0.04,
            drive_strength: 8.0,
            delay: 0.04,
        });

        // Create four branches
        let branches = [
            (center.0 - half_width / 2.0, center.1 - half_height / 2.0),
            (center.0 + half_width / 2.0, center.1 - half_height / 2.0),
            (center.0 - half_width / 2.0, center.1 + half_height / 2.0),
            (center.0 + half_width / 2.0, center.1 + half_height / 2.0),
        ];

        for branch_center in &branches {
            self.build_htree_recursive(
                *branch_center,
                half_width / 2.0,
                half_height / 2.0,
                level + 1,
                max_level,
                buffers,
                nets,
                sinks,
            )?;
        }

        Ok(())
    }

    /// Build balanced tree structure
    fn build_balanced_tree(
        &self,
        sinks: &[ClockSink],
        _spec: &ClockSpecification,
    ) -> Result<(Vec<ClockBuffer>, Vec<ClockNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();
        let fanout = 4;

        // Cluster sinks using k-means
        let clusters = self.cluster_sinks(sinks, fanout)?;

        // Build tree bottom-up
        let mut current_level = clusters;
        let mut level = 0;

        while current_level.len() > 1 {
            let mut next_level = Vec::new();

            for chunk in current_level.chunks(fanout) {
                // Find center of this cluster
                let center = self.find_cluster_center(chunk);

                // Add buffer at center
                let buffer_name = format!("clk_buf_l{}_n{}", level, buffers.len());
                buffers.push(ClockBuffer {
                    name: buffer_name.clone(),
                    position: center,
                    cell_type: self.select_buffer_size(chunk.len()),
                    input_cap: 0.02 * chunk.len() as f64,
                    drive_strength: 2.0 * chunk.len() as f64,
                    delay: 0.05,
                });

                // Create net connecting to children
                let net = ClockNet {
                    name: format!("clk_net_l{}_n{}", level, nets.len()),
                    source: buffer_name.clone(),
                    sinks: chunk.iter().map(|c| c.0.clone()).collect(),
                    segments: Vec::new(),
                    length: 0.0,
                    capacitance: 0.0,
                    resistance: 0.0,
                };
                nets.push(net);

                next_level.push((buffer_name, center));
            }

            current_level = next_level;
            level += 1;
        }

        Ok((buffers, nets))
    }

    /// Build mesh clock distribution
    fn build_mesh(
        &self,
        sinks: &[ClockSink],
        _spec: &ClockSpecification,
    ) -> Result<(Vec<ClockBuffer>, Vec<ClockNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        // Determine mesh dimensions
        let num_sinks = sinks.len();
        let rows = (num_sinks as f64).sqrt().ceil() as usize;
        let cols = rows;

        // Find bounding box
        let (min_x, min_y, max_x, max_y) = self.find_bounding_box(sinks);
        let step_x = (max_x - min_x) / cols as f64;
        let step_y = (max_y - min_y) / rows as f64;

        // Create mesh grid points with buffers
        for row in 0..rows {
            for col in 0..cols {
                let x = min_x + (col as f64 + 0.5) * step_x;
                let y = min_y + (row as f64 + 0.5) * step_y;

                let buffer_name = format!("clk_mesh_buf_r{}_c{}", row, col);
                buffers.push(ClockBuffer {
                    name: buffer_name.clone(),
                    position: (x, y),
                    cell_type: "sky130_fd_sc_hd__buf_4".to_string(),
                    input_cap: 0.02,
                    drive_strength: 4.0,
                    delay: 0.05,
                });

                // Connect to adjacent mesh points
                let mut adjacent = Vec::new();
                if col > 0 {
                    adjacent.push(format!("clk_mesh_buf_r{}_c{}", row, col - 1));
                }
                if row > 0 {
                    adjacent.push(format!("clk_mesh_buf_r{}_c{}", row - 1, col));
                }

                if !adjacent.is_empty() {
                    nets.push(ClockNet {
                        name: format!("clk_mesh_net_r{}_c{}", row, col),
                        source: buffer_name,
                        sinks: adjacent,
                        segments: Vec::new(),
                        length: step_x.min(step_y),
                        capacitance: 0.01,
                        resistance: 0.1,
                    });
                }
            }
        }

        // Connect sinks to nearest mesh points
        for sink in sinks {
            let nearest_buffer = self.find_nearest_buffer(&buffers, sink.position)?;
            nets.push(ClockNet {
                name: format!("clk_sink_net_{}", sink.instance),
                source: nearest_buffer,
                sinks: vec![sink.instance.clone()],
                segments: Vec::new(),
                length: 0.0,
                capacitance: 0.0,
                resistance: 0.0,
            });
        }

        Ok((buffers, nets))
    }

    /// Build hybrid clock tree
    fn build_hybrid(
        &self,
        sinks: &[ClockSink],
        spec: &ClockSpecification,
    ) -> Result<(Vec<ClockBuffer>, Vec<ClockNet>), AsicError> {
        // Global distribution with H-tree
        let (mut global_buffers, mut global_nets) = self.build_htree(sinks, spec)?;

        // Local distribution with balanced trees
        let local_regions = self.partition_sinks(sinks, 16)?; // 16 regions

        for (region_id, region_sinks) in local_regions.iter().enumerate() {
            let (local_buffers, local_nets) = self.build_balanced_tree(region_sinks, spec)?;

            // Offset buffer names to avoid conflicts
            for mut buffer in local_buffers {
                buffer.name = format!("region_{}__{}", region_id, buffer.name);
                global_buffers.push(buffer);
            }

            for mut net in local_nets {
                net.name = format!("region_{}__{}", region_id, net.name);
                global_nets.push(net);
            }
        }

        Ok((global_buffers, global_nets))
    }

    /// Insert buffers for timing and load requirements
    fn insert_buffers(
        &self,
        mut buffers: Vec<ClockBuffer>,
        nets: &[ClockNet],
        _spec: &ClockSpecification,
    ) -> Result<Vec<ClockBuffer>, AsicError> {
        let mut additional_buffers = Vec::new();

        // Calculate load capacitance for each net
        for net in nets {
            let load_cap = self.calculate_load_capacitance(net)?;

            // Check if buffer driving this net can handle the load
            if let Some(buffer_idx) = buffers.iter().position(|b| b.name == net.source) {
                let buffer = &buffers[buffer_idx];
                let max_load = buffer.drive_strength * 10.0; // fF

                if load_cap > max_load {
                    // Need to insert additional buffers
                    let num_buffers = (load_cap / max_load).ceil() as usize;

                    for i in 0..num_buffers - 1 {
                        let new_buffer = ClockBuffer {
                            name: format!("{}_extra_{}", buffer.name, i),
                            position: buffer.position,
                            cell_type: buffer.cell_type.clone(),
                            input_cap: buffer.input_cap,
                            drive_strength: buffer.drive_strength,
                            delay: buffer.delay,
                        };
                        additional_buffers.push(new_buffer);
                    }
                }
            }
        }

        buffers.extend(additional_buffers);
        Ok(buffers)
    }

    /// Balance clock tree for minimal skew
    fn balance_tree(
        &self,
        buffers: Vec<ClockBuffer>,
        nets: Vec<ClockNet>,
        sinks: &[ClockSink],
        spec: &ClockSpecification,
    ) -> Result<(Vec<ClockBuffer>, Vec<ClockNet>), AsicError> {
        let mut balanced_buffers = buffers.clone();
        let balanced_nets = nets.clone();

        // Calculate path delay to each sink
        let mut sink_delays = HashMap::new();
        for sink in sinks {
            let delay =
                self.calculate_path_delay(&sink.instance, &balanced_buffers, &balanced_nets)?;
            sink_delays.insert(sink.instance.clone(), delay);
        }

        // Find max and min delays
        let max_delay = sink_delays.values().cloned().fold(0.0, f64::max);
        let min_delay = sink_delays.values().cloned().fold(f64::INFINITY, f64::min);
        let skew = max_delay - min_delay;

        // Add delay buffers to balance paths
        if skew > spec.max_skew {
            for (sink_name, delay) in &sink_delays {
                let delay_needed = max_delay - delay;
                if delay_needed > 0.01 {
                    // 10ps threshold
                    let num_delay_bufs = (delay_needed / 0.05).ceil() as usize; // 50ps per buffer

                    for i in 0..num_delay_bufs {
                        let buffer = ClockBuffer {
                            name: format!("delay_buf_{}_{}", sink_name, i),
                            position: (0.0, 0.0), // Will be placed later
                            cell_type: "sky130_fd_sc_hd__buf_1".to_string(),
                            input_cap: 0.01,
                            drive_strength: 1.0,
                            delay: 0.05,
                        };
                        balanced_buffers.push(buffer);
                    }
                }
            }
        }

        Ok((balanced_buffers, balanced_nets))
    }

    /// Analyze clock tree timing
    fn analyze_timing(
        &self,
        buffers: &[ClockBuffer],
        nets: &[ClockNet],
        sinks: &[ClockSink],
        spec: &ClockSpecification,
    ) -> Result<ClockTiming, AsicError> {
        let mut sink_slack = HashMap::new();
        let mut delays = Vec::new();

        // Calculate delay to each sink
        for sink in sinks {
            let delay = self.calculate_path_delay(&sink.instance, buffers, nets)?;
            delays.push(delay);

            // Calculate slack (positive is good)
            let period = 1000.0 / spec.source.frequency; // ns
            let slack = period - delay - sink.setup_time;
            sink_slack.insert(sink.instance.clone(), slack);
        }

        let max_delay = delays.iter().cloned().fold(0.0, f64::max);
        let min_delay = delays.iter().cloned().fold(f64::INFINITY, f64::min);
        let max_skew = max_delay - min_delay;

        // Calculate power
        let power = self.calculate_clock_power(buffers, nets, spec.source.frequency)?;

        Ok(ClockTiming {
            max_skew,
            min_delay,
            max_delay,
            period: 1000.0 / spec.source.frequency,
            duty_cycle: 50.0,
            power,
            sink_slack,
        })
    }

    /// Helper: Find bounding box of sinks
    fn find_bounding_box(&self, sinks: &[ClockSink]) -> (f64, f64, f64, f64) {
        let min_x = sinks
            .iter()
            .map(|s| s.position.0)
            .fold(f64::INFINITY, f64::min);
        let min_y = sinks
            .iter()
            .map(|s| s.position.1)
            .fold(f64::INFINITY, f64::min);
        let max_x = sinks.iter().map(|s| s.position.0).fold(0.0, f64::max);
        let max_y = sinks.iter().map(|s| s.position.1).fold(0.0, f64::max);
        (min_x, min_y, max_x, max_y)
    }

    /// Helper: Find sinks in local region
    fn find_local_sinks<'a>(
        &self,
        center: (f64, f64),
        width: f64,
        height: f64,
        sinks: &'a [ClockSink],
    ) -> Vec<&'a ClockSink> {
        sinks
            .iter()
            .filter(|s| {
                (s.position.0 - center.0).abs() <= width / 2.0
                    && (s.position.1 - center.1).abs() <= height / 2.0
            })
            .collect()
    }

    /// Helper: Cluster sinks using k-means
    #[allow(clippy::type_complexity)]
    fn cluster_sinks(
        &self,
        sinks: &[ClockSink],
        num_clusters: usize,
    ) -> Result<Vec<(String, (f64, f64))>, AsicError> {
        // Simple clustering - divide into equal groups
        let mut clusters = Vec::new();
        let chunk_size = sinks.len().div_ceil(num_clusters);

        for (i, chunk) in sinks.chunks(chunk_size).enumerate() {
            let center = self.find_cluster_center_sinks(chunk);
            clusters.push((format!("cluster_{}", i), center));
        }

        Ok(clusters)
    }

    /// Helper: Find center of cluster
    fn find_cluster_center(&self, cluster: &[(String, (f64, f64))]) -> (f64, f64) {
        let sum_x: f64 = cluster.iter().map(|c| c.1 .0).sum();
        let sum_y: f64 = cluster.iter().map(|c| c.1 .1).sum();
        let count = cluster.len() as f64;
        (sum_x / count, sum_y / count)
    }

    /// Helper: Find center of sink cluster
    fn find_cluster_center_sinks(&self, sinks: &[ClockSink]) -> (f64, f64) {
        let sum_x: f64 = sinks.iter().map(|s| s.position.0).sum();
        let sum_y: f64 = sinks.iter().map(|s| s.position.1).sum();
        let count = sinks.len() as f64;
        (sum_x / count, sum_y / count)
    }

    /// Helper: Select buffer size based on fanout
    fn select_buffer_size(&self, fanout: usize) -> String {
        match fanout {
            1 => "sky130_fd_sc_hd__buf_1",
            2..=4 => "sky130_fd_sc_hd__buf_4",
            5..=8 => "sky130_fd_sc_hd__buf_8",
            _ => "sky130_fd_sc_hd__buf_16",
        }
        .to_string()
    }

    /// Helper: Find nearest buffer to position
    fn find_nearest_buffer(
        &self,
        buffers: &[ClockBuffer],
        position: (f64, f64),
    ) -> Result<String, AsicError> {
        buffers
            .iter()
            .min_by(|a, b| {
                let dist_a = ((a.position.0 - position.0).powi(2)
                    + (a.position.1 - position.1).powi(2))
                .sqrt();
                let dist_b = ((b.position.0 - position.0).powi(2)
                    + (b.position.1 - position.1).powi(2))
                .sqrt();
                dist_a.partial_cmp(&dist_b).unwrap()
            })
            .map(|b| b.name.clone())
            .ok_or_else(|| AsicError::RoutingError("No buffers found".to_string()))
    }

    /// Helper: Partition sinks into regions
    fn partition_sinks(
        &self,
        sinks: &[ClockSink],
        num_regions: usize,
    ) -> Result<Vec<Vec<ClockSink>>, AsicError> {
        let mut regions = vec![Vec::new(); num_regions];
        let region_size = sinks.len().div_ceil(num_regions);

        for (i, sink) in sinks.iter().enumerate() {
            let region_id = i / region_size;
            regions[region_id].push(sink.clone());
        }

        Ok(regions)
    }

    /// Helper: Calculate load capacitance
    fn calculate_load_capacitance(&self, net: &ClockNet) -> Result<f64, AsicError> {
        // Wire capacitance + sink capacitances
        let wire_cap = net.length * 0.2; // fF/um for typical metal
        let sink_cap = net.sinks.len() as f64 * 0.01; // fF per sink
        Ok(wire_cap + sink_cap)
    }

    /// Helper: Calculate path delay
    fn calculate_path_delay(
        &self,
        sink: &str,
        buffers: &[ClockBuffer],
        nets: &[ClockNet],
    ) -> Result<f64, AsicError> {
        // Simple RC delay model
        let mut total_delay = 0.0;
        let mut current = sink.to_string();

        // Trace back to source
        while let Some(net) = nets.iter().find(|n| n.sinks.contains(&current)) {
            // Wire delay
            let wire_delay = net.resistance * net.capacitance;
            total_delay += wire_delay;

            // Buffer delay
            if let Some(buffer) = buffers.iter().find(|b| b.name == net.source) {
                total_delay += buffer.delay;
            }

            current = net.source.clone();
        }

        Ok(total_delay)
    }

    /// Helper: Calculate clock power
    fn calculate_clock_power(
        &self,
        buffers: &[ClockBuffer],
        nets: &[ClockNet],
        frequency: f64,
    ) -> Result<f64, AsicError> {
        // P = C * V^2 * f
        let vdd = 1.8; // V for SKY130
        let mut total_cap = 0.0;

        // Buffer capacitances
        for buffer in buffers {
            total_cap += buffer.input_cap;
        }

        // Wire capacitances
        for net in nets {
            total_cap += net.capacitance;
        }

        let power = total_cap * vdd * vdd * frequency * 1e-3; // mW
        Ok(power)
    }
}

/// Clock specification
#[derive(Debug, Clone)]
pub struct ClockSpecification {
    /// Clock source
    pub source: ClockSource,
    /// Maximum allowed skew
    pub max_skew: f64,
    /// Target insertion delay
    pub target_delay: f64,
    /// Maximum transition time
    pub max_transition: f64,
    /// Buffer insertion strategy
    pub buffer_strategy: BufferStrategy,
}

/// Buffer insertion strategy
#[derive(Debug, Clone)]
pub enum BufferStrategy {
    /// Minimize delay
    MinDelay,
    /// Minimize skew
    MinSkew,
    /// Minimize power
    MinPower,
    /// Balanced optimization
    Balanced,
}

/// Error type for CTS
#[derive(Debug, thiserror::Error)]
pub enum CtsError {
    #[error("Clock tree synthesis error: {0}")]
    SynthesisError(String),

    #[error("Timing constraint violation: {0}")]
    TimingViolation(String),

    #[error("Buffer insertion failed: {0}")]
    BufferError(String),
}
