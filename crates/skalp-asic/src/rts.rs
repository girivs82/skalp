//! Reset Tree Synthesis (RTS) for ASIC Implementation
//!
//! Implements balanced reset distribution with:
//! - Deassertion skew minimization (all FFs exit reset together)
//! - Fast assertion propagation
//! - Buffer insertion for fanout management
//! - Support for sync and async reset topologies
//!
//! # Reset Tree vs Clock Tree
//!
//! While similar to CTS, reset trees have different requirements:
//! - No periodic timing constraints (one-shot signal)
//! - Focus on deassertion skew (synchronous release)
//! - Fast assertion is critical for safety
//! - Less strict skew requirements than clock
//!
//! # Reset Types
//!
//! - **Asynchronous reset**: Immediate effect, needs synchronizer for release
//! - **Synchronous reset**: Clock-synchronized, simpler tree design

use crate::placement::Placement;
use crate::sky130::StandardCellLibrary;
use crate::{AsicError, DesignRules, Technology};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Reset specification for synthesis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSpec {
    /// Reset signal name
    pub name: String,
    /// Reset polarity (true = active high, false = active low)
    pub active_high: bool,
    /// Reset type
    pub reset_type: ResetType,
    /// Reset source location
    pub source: (f64, f64),
    /// Maximum deassertion skew (ps)
    pub max_deassertion_skew: f64,
    /// Maximum assertion delay (ps) - for safety-critical designs
    pub max_assertion_delay: f64,
    /// Maximum fanout per buffer
    pub max_fanout: usize,
    /// Buffer types to use
    pub buffer_types: Vec<String>,
}

impl Default for ResetSpec {
    fn default() -> Self {
        Self {
            name: "rst".to_string(),
            active_high: true,
            reset_type: ResetType::Asynchronous,
            source: (0.0, 0.0),
            max_deassertion_skew: 100.0, // 100ps
            max_assertion_delay: 500.0,  // 500ps
            max_fanout: 16,
            buffer_types: vec![
                "CLKBUF_X1".to_string(),
                "CLKBUF_X2".to_string(),
                "CLKBUF_X4".to_string(),
            ],
        }
    }
}

/// Reset type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ResetType {
    /// Asynchronous reset - immediate assertion, synchronized deassertion
    Asynchronous,
    /// Synchronous reset - clock-synchronized assertion and deassertion
    Synchronous,
    /// Power-on reset - special handling for POR sequences
    PowerOn,
}

/// Reset tree synthesis engine
pub struct ResetTreeSynthesizer {
    /// Technology node
    pub technology: Technology,
    /// Design rules
    pub design_rules: DesignRules,
    /// Standard cell library
    pub std_cells: StandardCellLibrary,
}

/// Reset tree result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetTree {
    /// Reset source
    pub source: ResetSource,
    /// Reset sinks (flip-flops with reset)
    pub sinks: Vec<ResetSink>,
    /// Reset buffers inserted
    pub buffers: Vec<ResetBuffer>,
    /// Reset nets
    pub nets: Vec<ResetNet>,
    /// Reset tree topology
    pub topology: ResetTopology,
    /// Timing analysis results
    pub timing: ResetTiming,
    /// Reset synchronizers (for async reset)
    pub synchronizers: Vec<ResetSynchronizer>,
}

/// Reset source definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSource {
    /// Source name
    pub name: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Drive strength
    pub drive_strength: f64,
    /// Active high polarity
    pub active_high: bool,
    /// Reset type
    pub reset_type: ResetType,
}

/// Reset sink (flip-flop with reset input)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSink {
    /// Instance name
    pub instance: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Input capacitance (fF)
    pub capacitance: f64,
    /// Reset to Q delay (ps)
    pub reset_to_q: f64,
    /// Recovery time (ps) - min time before clock after reset deassertion
    pub recovery_time: f64,
    /// Removal time (ps) - min time reset must be stable after clock
    pub removal_time: f64,
    /// Associated clock domain
    pub clock_domain: Option<String>,
}

/// Reset buffer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetBuffer {
    /// Buffer instance name
    pub name: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Buffer cell type
    pub cell_type: String,
    /// Input capacitance (fF)
    pub input_cap: f64,
    /// Output drive strength
    pub drive_strength: f64,
    /// Propagation delay (ps)
    pub delay: f64,
    /// Level in reset tree (0 = root)
    pub level: usize,
}

/// Reset net connecting buffers and sinks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetNet {
    /// Net name
    pub name: String,
    /// Driver (buffer or source)
    pub driver: String,
    /// Sink instances
    pub sinks: Vec<String>,
    /// Wire length (um)
    pub length: f64,
    /// Wire capacitance (fF)
    pub capacitance: f64,
    /// Wire resistance (ohms)
    pub resistance: f64,
}

/// Reset tree topology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResetTopology {
    /// Simple fanout with single level of buffers
    SingleLevel { buffers: usize },
    /// Balanced tree (binary or n-ary)
    BalancedTree { fanout: usize, levels: usize },
    /// H-tree distribution (for large designs)
    HTree { levels: usize },
    /// Spine-based distribution (common for reset)
    Spine {
        spine_buffers: usize,
        branch_fanout: usize,
    },
}

/// Reset synchronizer for async reset deassertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSynchronizer {
    /// Synchronizer instance name
    pub name: String,
    /// Position (x, y)
    pub position: (f64, f64),
    /// Number of synchronization stages (typically 2-3)
    pub stages: usize,
    /// Associated clock domain
    pub clock_domain: String,
    /// Output reset signal name
    pub output_reset: String,
}

/// Reset timing analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetTiming {
    /// Maximum deassertion skew across all sinks (ps)
    pub max_deassertion_skew: f64,
    /// Minimum assertion delay from source to any sink (ps)
    pub min_assertion_delay: f64,
    /// Maximum assertion delay from source to any sink (ps)
    pub max_assertion_delay: f64,
    /// Total reset tree power (uW)
    pub power: f64,
    /// Per-sink timing
    pub sink_timing: HashMap<String, ResetSinkTiming>,
    /// Recovery/removal slack per sink
    pub timing_slack: HashMap<String, f64>,
}

/// Per-sink reset timing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSinkTiming {
    /// Delay from source to this sink (ps)
    pub delay: f64,
    /// Slew at reset input (ps)
    pub slew: f64,
    /// Recovery slack (ps)
    pub recovery_slack: f64,
    /// Removal slack (ps)
    pub removal_slack: f64,
}

impl ResetTreeSynthesizer {
    /// Create new reset tree synthesizer
    pub fn new(technology: Technology, design_rules: DesignRules) -> Self {
        Self {
            technology: technology.clone(),
            design_rules,
            std_cells: StandardCellLibrary::new(),
        }
    }

    /// Synthesize reset tree
    pub fn synthesize(
        &self,
        placement: &Placement,
        reset_spec: &ResetSpec,
    ) -> Result<ResetTree, AsicError> {
        // Extract reset sinks from placement
        let sinks = self.extract_reset_sinks(placement)?;

        // Determine optimal topology based on sink count and distribution
        let topology = self.select_topology(&sinks, reset_spec)?;

        // Build reset tree structure
        let (buffers, nets) = match &topology {
            ResetTopology::SingleLevel { .. } => self.build_single_level(&sinks, reset_spec)?,
            ResetTopology::BalancedTree { .. } => self.build_balanced_tree(&sinks, reset_spec)?,
            ResetTopology::HTree { .. } => self.build_htree(&sinks, reset_spec)?,
            ResetTopology::Spine { .. } => self.build_spine(&sinks, reset_spec)?,
        };

        // Insert reset synchronizers for async reset
        let synchronizers = if reset_spec.reset_type == ResetType::Asynchronous {
            self.insert_synchronizers(&sinks, reset_spec)?
        } else {
            Vec::new()
        };

        // Balance tree for minimal deassertion skew
        let (buffers, nets) = self.balance_tree(buffers, nets, &sinks, reset_spec)?;

        // Analyze timing
        let timing = self.analyze_timing(&buffers, &nets, &sinks, reset_spec)?;

        // Create reset source
        let source = ResetSource {
            name: reset_spec.name.clone(),
            position: reset_spec.source,
            drive_strength: 1.0,
            active_high: reset_spec.active_high,
            reset_type: reset_spec.reset_type,
        };

        Ok(ResetTree {
            source,
            sinks,
            buffers,
            nets,
            topology,
            timing,
            synchronizers,
        })
    }

    /// Extract reset sinks from placement
    fn extract_reset_sinks(&self, placement: &Placement) -> Result<Vec<ResetSink>, AsicError> {
        let mut sinks = Vec::new();

        for (instance, position) in &placement.positions {
            // Check if instance is a flip-flop with reset
            if let Some(cell) = placement
                .cells
                .iter()
                .find(|c| &c.instance_name == instance)
            {
                // Look for cells with reset capability (DFFR, DFFS, etc.)
                if cell.cell_type.contains("DFFR")
                    || cell.cell_type.contains("DFFS")
                    || cell.cell_type.contains("SDFF")
                    || cell.cell_type.contains("_R")
                {
                    sinks.push(ResetSink {
                        instance: instance.clone(),
                        position: *position,
                        capacitance: 0.01,   // fF from cell library
                        reset_to_q: 50.0,    // ps
                        recovery_time: 30.0, // ps
                        removal_time: 20.0,  // ps
                        clock_domain: None,
                    });
                }
            }
        }

        if sinks.is_empty() {
            return Err(AsicError::PlacementError(
                "No reset sinks found".to_string(),
            ));
        }

        Ok(sinks)
    }

    /// Select optimal reset tree topology
    fn select_topology(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
    ) -> Result<ResetTopology, AsicError> {
        let num_sinks = sinks.len();

        // For very small designs, single level of buffers
        if num_sinks <= spec.max_fanout {
            Ok(ResetTopology::SingleLevel { buffers: 1 })
        }
        // For small-medium designs, use spine topology (common for reset)
        else if num_sinks < 500 {
            let spine_buffers = (num_sinks as f64 / spec.max_fanout as f64).ceil() as usize;
            Ok(ResetTopology::Spine {
                spine_buffers,
                branch_fanout: spec.max_fanout,
            })
        }
        // For medium designs, use balanced tree
        else if num_sinks < 5000 {
            let fanout = spec.max_fanout.min(8);
            let levels = (num_sinks as f64).log(fanout as f64).ceil() as usize;
            Ok(ResetTopology::BalancedTree { fanout, levels })
        }
        // For large designs, use H-tree
        else {
            let levels = ((num_sinks as f64).log2() / 2.0).ceil() as usize;
            Ok(ResetTopology::HTree { levels })
        }
    }

    /// Build single-level buffer structure
    fn build_single_level(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
    ) -> Result<(Vec<ResetBuffer>, Vec<ResetNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        // Find center of all sinks
        let center = self.find_center(sinks);

        // Single buffer at center
        let buffer = ResetBuffer {
            name: "rst_buf_0".to_string(),
            position: center,
            cell_type: self.select_buffer_type(sinks.len(), spec),
            input_cap: 0.02,
            drive_strength: 4.0,
            delay: 30.0,
            level: 0,
        };
        buffers.push(buffer);

        // Net from buffer to all sinks
        let net = ResetNet {
            name: "rst_net_0".to_string(),
            driver: "rst_buf_0".to_string(),
            sinks: sinks.iter().map(|s| s.instance.clone()).collect(),
            length: self.estimate_wire_length(&center, sinks),
            capacitance: 0.0,
            resistance: 0.0,
        };
        nets.push(net);

        Ok((buffers, nets))
    }

    /// Build spine-based reset tree
    fn build_spine(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
    ) -> Result<(Vec<ResetBuffer>, Vec<ResetNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        // Find bounding box
        let (min_x, min_y, max_x, max_y) = self.find_bounding_box(sinks);

        // Create spine along the longer dimension
        let horizontal_spine = (max_x - min_x) >= (max_y - min_y);

        // Number of spine buffers
        let num_spine = (sinks.len() as f64 / spec.max_fanout as f64).ceil() as usize;

        // Root buffer
        let root_pos = ((min_x + max_x) / 2.0, (min_y + max_y) / 2.0);
        buffers.push(ResetBuffer {
            name: "rst_root".to_string(),
            position: root_pos,
            cell_type: self.select_buffer_type(num_spine, spec),
            input_cap: 0.02,
            drive_strength: 8.0,
            delay: 25.0,
            level: 0,
        });

        // Create spine buffers
        for i in 0..num_spine {
            let pos = if horizontal_spine {
                let x = min_x + (max_x - min_x) * (i as f64 + 0.5) / num_spine as f64;
                (x, (min_y + max_y) / 2.0)
            } else {
                let y = min_y + (max_y - min_y) * (i as f64 + 0.5) / num_spine as f64;
                ((min_x + max_x) / 2.0, y)
            };

            buffers.push(ResetBuffer {
                name: format!("rst_spine_{}", i),
                position: pos,
                cell_type: self.select_buffer_type(spec.max_fanout, spec),
                input_cap: 0.02,
                drive_strength: 4.0,
                delay: 30.0,
                level: 1,
            });
        }

        // Net from root to spine buffers
        let spine_net = ResetNet {
            name: "rst_spine_net".to_string(),
            driver: "rst_root".to_string(),
            sinks: (0..num_spine).map(|i| format!("rst_spine_{}", i)).collect(),
            length: if horizontal_spine {
                max_x - min_x
            } else {
                max_y - min_y
            },
            capacitance: 0.0,
            resistance: 0.0,
        };
        nets.push(spine_net);

        // Assign sinks to nearest spine buffer
        let mut sink_assignments: Vec<Vec<String>> = vec![Vec::new(); num_spine];
        for sink in sinks {
            let nearest = if horizontal_spine {
                let normalized = (sink.position.0 - min_x) / (max_x - min_x);
                ((normalized * num_spine as f64) as usize).min(num_spine - 1)
            } else {
                let normalized = (sink.position.1 - min_y) / (max_y - min_y);
                ((normalized * num_spine as f64) as usize).min(num_spine - 1)
            };
            sink_assignments[nearest].push(sink.instance.clone());
        }

        // Create nets from spine buffers to sinks
        for (i, assigned) in sink_assignments.into_iter().enumerate() {
            if !assigned.is_empty() {
                nets.push(ResetNet {
                    name: format!("rst_branch_{}", i),
                    driver: format!("rst_spine_{}", i),
                    sinks: assigned,
                    length: 0.0,
                    capacitance: 0.0,
                    resistance: 0.0,
                });
            }
        }

        Ok((buffers, nets))
    }

    /// Build balanced tree structure
    fn build_balanced_tree(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
    ) -> Result<(Vec<ResetBuffer>, Vec<ResetNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        let fanout = spec.max_fanout.min(8);
        let num_sinks = sinks.len();
        let levels = (num_sinks as f64).log(fanout as f64).ceil() as usize;

        // Build tree level by level from root
        self.build_tree_level(
            sinks,
            spec,
            0,
            levels,
            fanout,
            &mut buffers,
            &mut nets,
            self.find_center(sinks),
            "rst",
        )?;

        Ok((buffers, nets))
    }

    /// Recursive tree level builder
    #[allow(clippy::too_many_arguments)]
    fn build_tree_level(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
        level: usize,
        max_level: usize,
        fanout: usize,
        buffers: &mut Vec<ResetBuffer>,
        nets: &mut Vec<ResetNet>,
        position: (f64, f64),
        prefix: &str,
    ) -> Result<String, AsicError> {
        let buffer_name = format!("{}_{}", prefix, buffers.len());

        buffers.push(ResetBuffer {
            name: buffer_name.clone(),
            position,
            cell_type: self.select_buffer_type(
                if level == max_level - 1 {
                    sinks.len().min(fanout)
                } else {
                    fanout
                },
                spec,
            ),
            input_cap: 0.02,
            drive_strength: if level == 0 { 8.0 } else { 4.0 },
            delay: 30.0,
            level,
        });

        if level >= max_level - 1 || sinks.len() <= fanout {
            // Leaf level - connect to sinks
            nets.push(ResetNet {
                name: format!("{}_net", buffer_name),
                driver: buffer_name.clone(),
                sinks: sinks.iter().map(|s| s.instance.clone()).collect(),
                length: self.estimate_wire_length(&position, sinks),
                capacitance: 0.0,
                resistance: 0.0,
            });
        } else {
            // Internal level - create children
            let chunk_size = sinks.len().div_ceil(fanout);
            let mut child_names = Vec::new();

            for (i, chunk) in sinks.chunks(chunk_size).enumerate() {
                let child_center = self.find_center(chunk);
                let child_prefix = format!("{}_{}", prefix, i);
                let child_name = self.build_tree_level(
                    chunk,
                    spec,
                    level + 1,
                    max_level,
                    fanout,
                    buffers,
                    nets,
                    child_center,
                    &child_prefix,
                )?;
                child_names.push(child_name);
            }

            nets.push(ResetNet {
                name: format!("{}_net", buffer_name),
                driver: buffer_name.clone(),
                sinks: child_names,
                length: 0.0,
                capacitance: 0.0,
                resistance: 0.0,
            });
        }

        Ok(buffer_name)
    }

    /// Build H-tree structure (similar to CTS)
    fn build_htree(
        &self,
        sinks: &[ResetSink],
        spec: &ResetSpec,
    ) -> Result<(Vec<ResetBuffer>, Vec<ResetNet>), AsicError> {
        let mut buffers = Vec::new();
        let mut nets = Vec::new();

        let (min_x, min_y, max_x, max_y) = self.find_bounding_box(sinks);
        let center = ((min_x + max_x) / 2.0, (min_y + max_y) / 2.0);

        self.build_htree_recursive(
            center,
            (max_x - min_x) / 2.0,
            (max_y - min_y) / 2.0,
            0,
            3,
            spec,
            sinks,
            &mut buffers,
            &mut nets,
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
        spec: &ResetSpec,
        sinks: &[ResetSink],
        buffers: &mut Vec<ResetBuffer>,
        nets: &mut Vec<ResetNet>,
    ) -> Result<(), AsicError> {
        let buffer_name = format!("rst_h_{}_{}", level, buffers.len());

        // Find local sinks in this quadrant
        let local_sinks: Vec<_> = sinks
            .iter()
            .filter(|s| {
                (s.position.0 - center.0).abs() <= half_width
                    && (s.position.1 - center.1).abs() <= half_height
            })
            .collect();

        if local_sinks.is_empty() {
            return Ok(());
        }

        buffers.push(ResetBuffer {
            name: buffer_name.clone(),
            position: center,
            cell_type: self.select_buffer_type(
                if level == max_level {
                    local_sinks.len()
                } else {
                    4
                },
                spec,
            ),
            input_cap: 0.02,
            drive_strength: if level == 0 { 8.0 } else { 4.0 },
            delay: 30.0,
            level,
        });

        if level >= max_level {
            // Leaf level
            nets.push(ResetNet {
                name: format!("{}_net", buffer_name),
                driver: buffer_name,
                sinks: local_sinks.iter().map(|s| s.instance.clone()).collect(),
                length: 0.0,
                capacitance: 0.0,
                resistance: 0.0,
            });
        } else {
            // Create four quadrants
            let quadrants = [
                (
                    center.0 - half_width / 2.0,
                    center.1 - half_height / 2.0,
                    half_width / 2.0,
                    half_height / 2.0,
                ),
                (
                    center.0 + half_width / 2.0,
                    center.1 - half_height / 2.0,
                    half_width / 2.0,
                    half_height / 2.0,
                ),
                (
                    center.0 - half_width / 2.0,
                    center.1 + half_height / 2.0,
                    half_width / 2.0,
                    half_height / 2.0,
                ),
                (
                    center.0 + half_width / 2.0,
                    center.1 + half_height / 2.0,
                    half_width / 2.0,
                    half_height / 2.0,
                ),
            ];

            for (qx, qy, qw, qh) in quadrants {
                self.build_htree_recursive(
                    (qx, qy),
                    qw,
                    qh,
                    level + 1,
                    max_level,
                    spec,
                    sinks,
                    buffers,
                    nets,
                )?;
            }
        }

        Ok(())
    }

    /// Insert reset synchronizers for async reset
    fn insert_synchronizers(
        &self,
        sinks: &[ResetSink],
        _spec: &ResetSpec,
    ) -> Result<Vec<ResetSynchronizer>, AsicError> {
        let mut synchronizers = Vec::new();

        // Group sinks by clock domain
        let mut domains: HashMap<String, Vec<&ResetSink>> = HashMap::new();
        for sink in sinks {
            let domain = sink
                .clock_domain
                .clone()
                .unwrap_or_else(|| "default".to_string());
            domains.entry(domain).or_default().push(sink);
        }

        // Create one synchronizer per clock domain
        for (domain, domain_sinks) in domains {
            let center = (
                domain_sinks.iter().map(|s| s.position.0).sum::<f64>() / domain_sinks.len() as f64,
                domain_sinks.iter().map(|s| s.position.1).sum::<f64>() / domain_sinks.len() as f64,
            );

            synchronizers.push(ResetSynchronizer {
                name: format!("rst_sync_{}", domain),
                position: center,
                stages: 2, // 2-stage synchronizer is standard
                clock_domain: domain.clone(),
                output_reset: format!("rst_{}_sync", domain),
            });
        }

        Ok(synchronizers)
    }

    /// Balance reset tree for minimal skew
    fn balance_tree(
        &self,
        mut buffers: Vec<ResetBuffer>,
        nets: Vec<ResetNet>,
        _sinks: &[ResetSink],
        _spec: &ResetSpec,
    ) -> Result<(Vec<ResetBuffer>, Vec<ResetNet>), AsicError> {
        // Calculate delays and adjust buffer sizes
        // For now, just ensure consistent buffer sizing per level
        let max_level = buffers.iter().map(|b| b.level).max().unwrap_or(0);

        for buffer in &mut buffers {
            // Use stronger buffers at higher levels (closer to root)
            buffer.drive_strength = match max_level - buffer.level {
                0 => 2.0,
                1 => 4.0,
                _ => 8.0,
            };
        }

        Ok((buffers, nets))
    }

    /// Analyze reset timing
    fn analyze_timing(
        &self,
        buffers: &[ResetBuffer],
        nets: &[ResetNet],
        sinks: &[ResetSink],
        _spec: &ResetSpec,
    ) -> Result<ResetTiming, AsicError> {
        let mut sink_timing = HashMap::new();
        let mut timing_slack = HashMap::new();

        // Calculate delay to each sink
        let mut min_delay = f64::MAX;
        let mut max_delay = 0.0f64;

        for sink in sinks {
            // Simple delay model: sum of buffer delays along path
            let path_delay = self.calculate_path_delay(buffers, nets, &sink.instance);

            min_delay = min_delay.min(path_delay);
            max_delay = max_delay.max(path_delay);

            let slew = 50.0; // Estimated slew
            let recovery_slack = sink.recovery_time - slew;
            let removal_slack = sink.removal_time - slew;

            sink_timing.insert(
                sink.instance.clone(),
                ResetSinkTiming {
                    delay: path_delay,
                    slew,
                    recovery_slack,
                    removal_slack,
                },
            );

            timing_slack.insert(sink.instance.clone(), recovery_slack.min(removal_slack));
        }

        let max_deassertion_skew = max_delay - min_delay;

        // Estimate power
        let power = buffers.len() as f64 * 0.5; // 0.5 uW per buffer (rough estimate)

        Ok(ResetTiming {
            max_deassertion_skew,
            min_assertion_delay: min_delay,
            max_assertion_delay: max_delay,
            power,
            sink_timing,
            timing_slack,
        })
    }

    /// Calculate path delay from source to a sink
    fn calculate_path_delay(
        &self,
        buffers: &[ResetBuffer],
        nets: &[ResetNet],
        sink_instance: &str,
    ) -> f64 {
        Self::calculate_path_delay_recursive(buffers, nets, sink_instance)
    }

    /// Recursive helper for path delay calculation
    fn calculate_path_delay_recursive(
        buffers: &[ResetBuffer],
        nets: &[ResetNet],
        sink_instance: &str,
    ) -> f64 {
        // Find the net driving this sink
        let mut total_delay = 0.0;

        for net in nets {
            if net.sinks.contains(&sink_instance.to_string()) {
                // Find the driver buffer
                if let Some(buffer) = buffers.iter().find(|b| b.name == net.driver) {
                    total_delay += buffer.delay;
                    // Recursively find path to this buffer
                    total_delay +=
                        Self::calculate_path_delay_recursive(buffers, nets, &buffer.name);
                }
                break;
            }
        }

        total_delay
    }

    // Helper functions

    fn find_center(&self, sinks: &[ResetSink]) -> (f64, f64) {
        if sinks.is_empty() {
            return (0.0, 0.0);
        }
        let sum_x: f64 = sinks.iter().map(|s| s.position.0).sum();
        let sum_y: f64 = sinks.iter().map(|s| s.position.1).sum();
        (sum_x / sinks.len() as f64, sum_y / sinks.len() as f64)
    }

    fn find_bounding_box(&self, sinks: &[ResetSink]) -> (f64, f64, f64, f64) {
        if sinks.is_empty() {
            return (0.0, 0.0, 0.0, 0.0);
        }
        let min_x = sinks.iter().map(|s| s.position.0).fold(f64::MAX, f64::min);
        let min_y = sinks.iter().map(|s| s.position.1).fold(f64::MAX, f64::min);
        let max_x = sinks.iter().map(|s| s.position.0).fold(f64::MIN, f64::max);
        let max_y = sinks.iter().map(|s| s.position.1).fold(f64::MIN, f64::max);
        (min_x, min_y, max_x, max_y)
    }

    fn select_buffer_type(&self, fanout: usize, _spec: &ResetSpec) -> String {
        match fanout {
            0..=4 => "CLKBUF_X1".to_string(),
            5..=8 => "CLKBUF_X2".to_string(),
            9..=16 => "CLKBUF_X4".to_string(),
            _ => "CLKBUF_X8".to_string(),
        }
    }

    fn estimate_wire_length(&self, center: &(f64, f64), sinks: &[ResetSink]) -> f64 {
        sinks
            .iter()
            .map(|s| {
                let dx = s.position.0 - center.0;
                let dy = s.position.1 - center.1;
                (dx * dx + dy * dy).sqrt()
            })
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ViaRules;

    #[test]
    fn test_reset_spec_default() {
        let spec = ResetSpec::default();
        assert!(spec.active_high);
        assert_eq!(spec.reset_type, ResetType::Asynchronous);
        assert_eq!(spec.max_fanout, 16);
    }

    #[test]
    fn test_topology_selection() {
        let rts = ResetTreeSynthesizer {
            technology: Technology::Sky130,
            design_rules: DesignRules {
                min_feature: 130.0,
                metal_layers: 5,
                min_width: 0.14,
                min_spacing: 0.14,
                via_rules: ViaRules {
                    min_size: 0.17,
                    enclosure: 0.06,
                    spacing: 0.17,
                },
            },
            std_cells: StandardCellLibrary::new(),
        };

        let spec = ResetSpec::default();

        // Small design -> SingleLevel
        let small_sinks: Vec<ResetSink> = (0..10)
            .map(|i| ResetSink {
                instance: format!("ff_{}", i),
                position: (i as f64 * 10.0, 0.0),
                capacitance: 0.01,
                reset_to_q: 50.0,
                recovery_time: 30.0,
                removal_time: 20.0,
                clock_domain: None,
            })
            .collect();

        let topo = rts.select_topology(&small_sinks, &spec).unwrap();
        assert!(matches!(topo, ResetTopology::SingleLevel { .. }));

        // Medium design -> Spine
        let medium_sinks: Vec<ResetSink> = (0..100)
            .map(|i| ResetSink {
                instance: format!("ff_{}", i),
                position: (i as f64 * 10.0, (i % 10) as f64 * 10.0),
                capacitance: 0.01,
                reset_to_q: 50.0,
                recovery_time: 30.0,
                removal_time: 20.0,
                clock_domain: None,
            })
            .collect();

        let topo = rts.select_topology(&medium_sinks, &spec).unwrap();
        assert!(matches!(topo, ResetTopology::Spine { .. }));
    }
}
