//! Power Infrastructure Synthesis
//!
//! Detects cross-domain signal crossings and automatically inserts:
//! - Level shifters at voltage domain boundaries
//! - Isolation cells at power-gated domain boundaries
//! - Always-on buffers for control signals
//!
//! This module processes LIR netlists and adds necessary power infrastructure
//! cells to ensure correct multi-voltage and power-gated operation.

use skalp_lir::lir::{Lir, LirNet, NetId, Primitive, PrimitiveId, PrimitiveType};
use std::collections::{HashMap, HashSet};

/// Result of power infrastructure analysis
#[derive(Debug, Clone, Default)]
pub struct PowerInfrastructureAnalysis {
    /// Cross-domain signal crossings detected
    pub crossings: Vec<DomainCrossing>,
    /// Level shifters needed
    pub level_shifters_needed: Vec<LevelShifterRequirement>,
    /// Isolation cells needed
    pub isolation_cells_needed: Vec<IsolationRequirement>,
    /// Violations found (missing infrastructure)
    pub violations: Vec<PowerViolation>,
    /// Statistics
    pub stats: PowerInfrastructureStats,
}

/// A signal crossing between power domains
#[derive(Debug, Clone)]
pub struct DomainCrossing {
    /// Net ID of the crossing signal
    pub net_id: NetId,
    /// Net name
    pub net_name: String,
    /// Source domain
    pub from_domain: PowerDomainInfo,
    /// Destination domain
    pub to_domain: PowerDomainInfo,
    /// Driver primitive path
    pub driver_path: String,
    /// Load primitive paths
    pub load_paths: Vec<String>,
}

/// Power domain information
#[derive(Debug, Clone)]
pub struct PowerDomainInfo {
    /// Domain name
    pub name: String,
    /// Voltage in millivolts (e.g., 1000 for 1.0V)
    pub voltage_mv: u16,
    /// True if domain can be power-gated
    pub is_power_gated: bool,
    /// True if always-on domain
    pub is_always_on: bool,
}

impl Default for PowerDomainInfo {
    fn default() -> Self {
        Self {
            name: "vdd_default".to_string(),
            voltage_mv: 1000,
            is_power_gated: false,
            is_always_on: false,
        }
    }
}

/// Level shifter requirement
#[derive(Debug, Clone)]
pub struct LevelShifterRequirement {
    /// Net to insert level shifter on
    pub net_id: NetId,
    /// From voltage (mV)
    pub from_voltage_mv: u16,
    /// To voltage (mV)
    pub to_voltage_mv: u16,
    /// Insertion point (after this primitive's output)
    pub after_primitive: PrimitiveId,
    /// Suggested level shifter type
    pub shifter_type: LevelShifterType,
}

/// Level shifter type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LevelShifterType {
    /// Low to high voltage translation
    LowToHigh,
    /// High to low voltage translation
    HighToLow,
}

/// Isolation cell requirement
#[derive(Debug, Clone)]
pub struct IsolationRequirement {
    /// Net to insert isolation cell on
    pub net_id: NetId,
    /// From domain (power-gated)
    pub from_domain: String,
    /// To domain
    pub to_domain: String,
    /// Clamp value (0=low, 1=high, 2=hold)
    pub clamp_value: u8,
    /// Isolation enable signal name
    pub iso_enable_signal: String,
}

/// Power infrastructure violation
#[derive(Debug, Clone)]
pub struct PowerViolation {
    /// Violation type
    pub violation_type: ViolationType,
    /// Net ID involved
    pub net_id: Option<NetId>,
    /// Description
    pub description: String,
    /// Severity (error, warning)
    pub severity: ViolationSeverity,
}

/// Type of power violation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationType {
    /// Missing level shifter at voltage boundary
    MissingLevelShifter,
    /// Missing isolation cell at power-gated boundary
    MissingIsolation,
    /// Signal from always-on to switchable without isolation
    AlwaysOnToSwitchable,
    /// Feedthrough signal (crosses multiple domains)
    CrossDomainFeedthrough,
}

/// Violation severity
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationSeverity {
    Error,
    Warning,
}

/// Statistics for power infrastructure
#[derive(Debug, Clone, Default)]
pub struct PowerInfrastructureStats {
    /// Number of power domains found
    pub domain_count: usize,
    /// Number of cross-domain crossings
    pub crossing_count: usize,
    /// Number of level shifters needed
    pub level_shifters_needed: usize,
    /// Number of isolation cells needed
    pub isolation_cells_needed: usize,
    /// Number of violations
    pub violation_count: usize,
}

/// Analyze LIR netlist for power infrastructure requirements
pub fn analyze_power_infrastructure(lir: &Lir) -> PowerInfrastructureAnalysis {
    let mut analysis = PowerInfrastructureAnalysis::default();

    // Build domain map from primitive power_domain annotations
    let domain_map = build_domain_map(lir);
    // Count unique domain names
    let unique_domains: HashSet<_> = domain_map.values().map(|d| &d.name).collect();
    analysis.stats.domain_count = unique_domains.len();

    // Find all cross-domain signal crossings
    for net in &lir.nets {
        if let Some(crossing) = detect_crossing(lir, net, &domain_map) {
            // Check if level shifter needed (voltage difference)
            if crossing.from_domain.voltage_mv != crossing.to_domain.voltage_mv {
                let shifter_type =
                    if crossing.from_domain.voltage_mv < crossing.to_domain.voltage_mv {
                        LevelShifterType::LowToHigh
                    } else {
                        LevelShifterType::HighToLow
                    };

                // Check if level shifter already present
                if !has_level_shifter(lir, net.id) {
                    let driver_id = net.driver.map(|(id, _)| id).unwrap_or(PrimitiveId(0));
                    analysis
                        .level_shifters_needed
                        .push(LevelShifterRequirement {
                            net_id: net.id,
                            from_voltage_mv: crossing.from_domain.voltage_mv,
                            to_voltage_mv: crossing.to_domain.voltage_mv,
                            after_primitive: driver_id,
                            shifter_type,
                        });

                    analysis.violations.push(PowerViolation {
                        violation_type: ViolationType::MissingLevelShifter,
                        net_id: Some(net.id),
                        description: format!(
                            "Signal '{}' crosses from {}mV to {}mV without level shifter",
                            net.name,
                            crossing.from_domain.voltage_mv,
                            crossing.to_domain.voltage_mv
                        ),
                        severity: ViolationSeverity::Error,
                    });
                }
            }

            // Check if isolation cell needed (power-gated to active)
            if crossing.from_domain.is_power_gated
                && !crossing.to_domain.is_power_gated
                && !has_isolation_cell(lir, net.id)
            {
                analysis.isolation_cells_needed.push(IsolationRequirement {
                    net_id: net.id,
                    from_domain: crossing.from_domain.name.clone(),
                    to_domain: crossing.to_domain.name.clone(),
                    clamp_value: 0, // Default: clamp to 0
                    iso_enable_signal: format!("iso_en_{}", crossing.from_domain.name),
                });

                analysis.violations.push(PowerViolation {
                    violation_type: ViolationType::MissingIsolation,
                    net_id: Some(net.id),
                    description: format!(
                        "Signal '{}' from power-gated domain '{}' needs isolation cell",
                        net.name, crossing.from_domain.name
                    ),
                    severity: ViolationSeverity::Error,
                });
            }

            analysis.crossings.push(crossing);
        }
    }

    // Update statistics
    analysis.stats.crossing_count = analysis.crossings.len();
    analysis.stats.level_shifters_needed = analysis.level_shifters_needed.len();
    analysis.stats.isolation_cells_needed = analysis.isolation_cells_needed.len();
    analysis.stats.violation_count = analysis.violations.len();

    analysis
}

/// Build map of primitive ID to power domain
fn build_domain_map(lir: &Lir) -> HashMap<PrimitiveId, PowerDomainInfo> {
    let mut map = HashMap::new();

    for prim in &lir.primitives {
        let domain = if let Some(domain_name) = &prim.power_domain {
            // Parse domain info from name
            let voltage_mv = infer_voltage_from_domain_name(domain_name);
            let is_always_on = domain_name.contains("always_on") || domain_name.contains("aon");
            let is_power_gated = domain_name.contains("_pg") || domain_name.contains("switchable");

            PowerDomainInfo {
                name: domain_name.clone(),
                voltage_mv,
                is_power_gated,
                is_always_on,
            }
        } else {
            PowerDomainInfo::default()
        };

        map.insert(prim.id, domain);
    }

    map
}

/// Infer voltage from domain name
fn infer_voltage_from_domain_name(name: &str) -> u16 {
    let name_lower = name.to_lowercase();

    // Check for explicit voltage patterns
    if name_lower.contains("3v3") || name_lower.contains("3.3v") || name_lower.contains("_io") {
        return 3300;
    }
    if name_lower.contains("1v8") || name_lower.contains("1.8v") {
        return 1800;
    }
    if name_lower.contains("1v2") || name_lower.contains("1.2v") {
        return 1200;
    }
    if name_lower.contains("1v0") || name_lower.contains("1.0v") || name_lower.contains("_core") {
        return 1000;
    }
    if name_lower.contains("0v9") || name_lower.contains("0.9v") {
        return 900;
    }
    if name_lower.contains("5v") || name_lower.contains("_analog") {
        return 5000;
    }

    // Default to 1.0V (core voltage)
    1000
}

/// Detect if a net crosses power domains
fn detect_crossing(
    lir: &Lir,
    net: &LirNet,
    domain_map: &HashMap<PrimitiveId, PowerDomainInfo>,
) -> Option<DomainCrossing> {
    // Get driver domain
    let (driver_id, _) = net.driver?;
    let driver_domain = domain_map.get(&driver_id)?;
    let driver_prim = lir.get_primitive(driver_id)?;

    // Check each load
    for (load_id, _) in &net.loads {
        if let Some(load_domain) = domain_map.get(load_id) {
            // Different domain detected
            if driver_domain.name != load_domain.name {
                let load_paths: Vec<String> = net
                    .loads
                    .iter()
                    .filter_map(|(id, _)| lir.get_primitive(*id).map(|p| p.path.clone()))
                    .collect();

                return Some(DomainCrossing {
                    net_id: net.id,
                    net_name: net.name.clone(),
                    from_domain: driver_domain.clone(),
                    to_domain: load_domain.clone(),
                    driver_path: driver_prim.path.clone(),
                    load_paths,
                });
            }
        }
    }

    None
}

/// Check if net already has a level shifter
fn has_level_shifter(lir: &Lir, net_id: NetId) -> bool {
    // Check if any primitive driving/loading this net is a level shifter
    if let Some(net) = lir.get_net(net_id) {
        if let Some((driver_id, _)) = net.driver {
            if let Some(prim) = lir.get_primitive(driver_id) {
                if matches!(prim.ptype, PrimitiveType::LevelShifter { .. }) {
                    return true;
                }
            }
        }
    }
    false
}

/// Check if net already has an isolation cell
fn has_isolation_cell(lir: &Lir, net_id: NetId) -> bool {
    if let Some(net) = lir.get_net(net_id) {
        if let Some((driver_id, _)) = net.driver {
            if let Some(prim) = lir.get_primitive(driver_id) {
                if matches!(prim.ptype, PrimitiveType::IsolationCell { .. }) {
                    return true;
                }
            }
        }
    }
    false
}

// ============================================================================
// Drive Strength and Buffer Tree Configuration
// ============================================================================

/// Configuration for drive strength selection
#[derive(Debug, Clone)]
pub struct DriveStrengthConfig {
    /// Base fanout per X1 cell
    pub base_fanout_per_x1: u32,
    /// Input capacitance per load (fF)
    pub load_capacitance_ff: u32,
    /// Available drive strengths
    pub available_strengths: Vec<u8>,
    /// Maximum buffer tree depth
    pub max_tree_depth: u32,
}

impl Default for DriveStrengthConfig {
    fn default() -> Self {
        Self {
            base_fanout_per_x1: 4,
            load_capacitance_ff: 10,
            available_strengths: vec![1, 2, 4, 8],
            max_tree_depth: 3,
        }
    }
}

/// Buffer tree node for high-fanout signals
#[derive(Debug, Clone)]
pub struct BufferTreeNode {
    /// Primitive ID of this buffer
    pub buffer_id: PrimitiveId,
    /// Output net ID
    pub output_net_id: NetId,
    /// Drive strength of this buffer
    pub drive_strength: u8,
    /// Child nodes (if any)
    pub children: Vec<BufferTreeNode>,
    /// Load primitive IDs driven by this buffer (leaf loads)
    pub loads: Vec<PrimitiveId>,
}

/// Plan for inserting power infrastructure with proper drive strength
#[derive(Debug, Clone)]
pub struct PowerInsertionPlan {
    /// Level shifter insertions with drive strength
    pub level_shifters: Vec<LevelShifterInsertion>,
    /// Isolation cell insertions with drive strength
    pub isolation_cells: Vec<IsolationInsertion>,
    /// Buffer trees for high fanout
    pub buffer_trees: Vec<BufferTreePlan>,
}

/// Level shifter insertion plan
#[derive(Debug, Clone)]
pub struct LevelShifterInsertion {
    /// Original net to tap
    pub source_net: NetId,
    /// From voltage (mV)
    pub from_voltage_mv: u16,
    /// To voltage (mV)
    pub to_voltage_mv: u16,
    /// Selected drive strength
    pub drive_strength: u8,
    /// Fanout in target domain
    pub target_fanout: u32,
    /// Needs buffer tree after level shifter
    pub needs_buffer_tree: bool,
    /// Load primitives to reconnect
    pub loads_to_reconnect: Vec<(PrimitiveId, u8)>, // (prim_id, input_pin)
}

/// Isolation cell insertion plan
#[derive(Debug, Clone)]
pub struct IsolationInsertion {
    /// Original net to tap
    pub source_net: NetId,
    /// From domain
    pub from_domain: String,
    /// To domain
    pub to_domain: String,
    /// Clamp value
    pub clamp_value: u8,
    /// Selected drive strength
    pub drive_strength: u8,
    /// Needs buffer tree
    pub needs_buffer_tree: bool,
    /// Loads to reconnect
    pub loads_to_reconnect: Vec<(PrimitiveId, u8)>,
}

/// Buffer tree plan for high fanout
#[derive(Debug, Clone)]
pub struct BufferTreePlan {
    /// Root input net
    pub input_net: NetId,
    /// Tree depth
    pub depth: u32,
    /// Total buffers needed
    pub buffer_count: u32,
    /// Drive strengths at each level (root to leaf)
    pub level_strengths: Vec<u8>,
}

/// Analyze fanout and create insertion plan
pub fn plan_power_infrastructure(
    lir: &Lir,
    analysis: &PowerInfrastructureAnalysis,
    config: &DriveStrengthConfig,
) -> PowerInsertionPlan {
    let mut plan = PowerInsertionPlan {
        level_shifters: Vec::new(),
        isolation_cells: Vec::new(),
        buffer_trees: Vec::new(),
    };

    // Plan level shifter insertions
    for req in &analysis.level_shifters_needed {
        if let Some(net) = lir.get_net(req.net_id) {
            let fanout = net.loads.len() as u32;

            // Calculate required drive strength
            let (drive_strength, needs_buffer_tree) = select_drive_strength(fanout, config);

            // Collect loads to reconnect
            let loads_to_reconnect: Vec<(PrimitiveId, u8)> =
                net.loads.iter().map(|(pid, pin)| (*pid, *pin)).collect();

            plan.level_shifters.push(LevelShifterInsertion {
                source_net: req.net_id,
                from_voltage_mv: req.from_voltage_mv,
                to_voltage_mv: req.to_voltage_mv,
                drive_strength,
                target_fanout: fanout,
                needs_buffer_tree,
                loads_to_reconnect,
            });
        }
    }

    // Plan isolation cell insertions
    for req in &analysis.isolation_cells_needed {
        if let Some(net) = lir.get_net(req.net_id) {
            let fanout = net.loads.len() as u32;
            let (drive_strength, needs_buffer_tree) = select_drive_strength(fanout, config);

            let loads_to_reconnect: Vec<(PrimitiveId, u8)> =
                net.loads.iter().map(|(pid, pin)| (*pid, *pin)).collect();

            plan.isolation_cells.push(IsolationInsertion {
                source_net: req.net_id,
                from_domain: req.from_domain.clone(),
                to_domain: req.to_domain.clone(),
                clamp_value: req.clamp_value,
                drive_strength,
                needs_buffer_tree,
                loads_to_reconnect,
            });
        }
    }

    plan
}

/// Select appropriate drive strength based on fanout
fn select_drive_strength(fanout: u32, config: &DriveStrengthConfig) -> (u8, bool) {
    // Find smallest drive strength that can handle the fanout
    for &strength in &config.available_strengths {
        let max_fanout = config.base_fanout_per_x1 * strength as u32;
        if fanout <= max_fanout {
            return (strength, false);
        }
    }

    // Fanout too high for single cell - need buffer tree
    let max_strength = *config.available_strengths.last().unwrap_or(&4);
    (max_strength, true)
}

/// Calculate buffer tree parameters for high fanout
fn calculate_buffer_tree(total_fanout: u32, config: &DriveStrengthConfig) -> BufferTreePlan {
    let max_strength = *config.available_strengths.last().unwrap_or(&4);
    let max_fanout_per_buf = config.base_fanout_per_x1 * max_strength as u32;

    // Calculate tree depth: ceil(log_base(fanout) / log_base(max_fanout))
    let mut depth = 1u32;
    let mut capacity = max_fanout_per_buf;
    while capacity < total_fanout && depth < config.max_tree_depth {
        depth += 1;
        capacity *= max_fanout_per_buf;
    }

    // Calculate buffers at each level
    let mut buffer_count = 0u32;
    let mut level_strengths = Vec::new();
    let mut remaining_fanout = total_fanout;

    for level in 0..depth {
        let bufs_at_level = if level == depth - 1 {
            // Leaf level
            remaining_fanout.div_ceil(max_fanout_per_buf)
        } else {
            // Intermediate level
            let next_level_bufs = remaining_fanout.div_ceil(max_fanout_per_buf);
            next_level_bufs.div_ceil(max_fanout_per_buf)
        };

        buffer_count += bufs_at_level;
        level_strengths.push(max_strength);
        remaining_fanout = bufs_at_level;
    }

    BufferTreePlan {
        input_net: NetId(0), // Will be set during insertion
        depth,
        buffer_count,
        level_strengths,
    }
}

/// Insert power infrastructure with proper net rewiring
pub fn insert_power_infrastructure(
    lir: &mut Lir,
    analysis: &PowerInfrastructureAnalysis,
) -> PowerInsertionResult {
    let config = DriveStrengthConfig::default();
    let plan = plan_power_infrastructure(lir, analysis, &config);

    insert_power_infrastructure_with_plan(lir, &plan, &config)
}

/// Insert power infrastructure using a pre-calculated plan
pub fn insert_power_infrastructure_with_plan(
    lir: &mut Lir,
    plan: &PowerInsertionPlan,
    config: &DriveStrengthConfig,
) -> PowerInsertionResult {
    let mut result = PowerInsertionResult::default();
    let mut next_prim_id = lir.primitives.len() as u32;
    let mut next_net_id = lir.nets.len() as u32;

    // Insert level shifters with proper rewiring
    for insertion in &plan.level_shifters {
        let ls_prim_id = PrimitiveId(next_prim_id);
        next_prim_id += 1;

        let ls_out_net_id = NetId(next_net_id);
        next_net_id += 1;

        // Create level shifter primitive
        let level_shifter = Primitive::new_level_shifter(
            ls_prim_id,
            format!(
                "power_infra/lvlshift_{}_X{}",
                insertion.source_net.0, insertion.drive_strength
            ),
            insertion.from_voltage_mv,
            insertion.to_voltage_mv,
            insertion.source_net,
            ls_out_net_id,
        );

        // Create output net
        let mut ls_out_net = LirNet::new(
            ls_out_net_id,
            format!("lvlshift_out_{}", insertion.source_net.0),
        );

        // Set driver
        ls_out_net.driver = Some((ls_prim_id, 0));

        lir.add_primitive(level_shifter);

        // Handle high fanout with buffer tree
        if insertion.needs_buffer_tree {
            let tree_plan = calculate_buffer_tree(insertion.target_fanout, config);
            let (bufs_inserted, final_nets) = insert_buffer_tree(
                lir,
                ls_out_net_id,
                &tree_plan,
                &mut next_prim_id,
                &mut next_net_id,
            );
            result.buffers_inserted += bufs_inserted;

            // Reconnect loads to buffer tree outputs
            reconnect_loads_to_tree(lir, &insertion.loads_to_reconnect, &final_nets);
        } else {
            // Direct connection - reconnect all loads to level shifter output
            for (load_prim_id, input_pin) in &insertion.loads_to_reconnect {
                ls_out_net.loads.push((*load_prim_id, *input_pin));

                // Update primitive's input
                if let Some(prim) = lir.get_primitive_mut(*load_prim_id) {
                    if (*input_pin as usize) < prim.inputs.len() {
                        prim.inputs[*input_pin as usize] = ls_out_net_id;
                    }
                }
            }
        }

        // Remove loads from original net (they now connect through level shifter)
        if let Some(orig_net) = lir.get_net_mut(insertion.source_net) {
            // Keep only loads in same domain
            orig_net.loads.retain(|(pid, _)| {
                !insertion
                    .loads_to_reconnect
                    .iter()
                    .any(|(lpid, _)| lpid == pid)
            });
            // Add level shifter as load
            orig_net.loads.push((ls_prim_id, 0));
        }

        lir.add_net(ls_out_net);
        result.level_shifters_inserted += 1;
    }

    // Insert isolation cells with proper rewiring
    for insertion in &plan.isolation_cells {
        let iso_prim_id = PrimitiveId(next_prim_id);
        next_prim_id += 1;

        let iso_out_net_id = NetId(next_net_id);
        next_net_id += 1;

        // Create isolation enable net (would need proper connection)
        let iso_en_net_id = NetId(next_net_id);
        next_net_id += 1;

        let iso_en_net = LirNet::new(iso_en_net_id, format!("iso_en_{}", insertion.from_domain));
        lir.add_net(iso_en_net);

        // Create isolation cell primitive
        let isolation_cell = Primitive::new_isolation_cell(
            iso_prim_id,
            format!(
                "power_infra/iso_{}_X{}",
                insertion.source_net.0, insertion.drive_strength
            ),
            insertion.clamp_value,
            true,
            insertion.source_net,
            iso_en_net_id,
            iso_out_net_id,
        );

        let mut iso_out_net = LirNet::new(
            iso_out_net_id,
            format!("iso_out_{}", insertion.source_net.0),
        );
        iso_out_net.driver = Some((iso_prim_id, 0));

        lir.add_primitive(isolation_cell);

        // Handle high fanout
        if insertion.needs_buffer_tree {
            let tree_plan =
                calculate_buffer_tree(insertion.loads_to_reconnect.len() as u32, config);
            let (bufs_inserted, final_nets) = insert_buffer_tree(
                lir,
                iso_out_net_id,
                &tree_plan,
                &mut next_prim_id,
                &mut next_net_id,
            );
            result.buffers_inserted += bufs_inserted;
            reconnect_loads_to_tree(lir, &insertion.loads_to_reconnect, &final_nets);
        } else {
            // Direct connection
            for (load_prim_id, input_pin) in &insertion.loads_to_reconnect {
                iso_out_net.loads.push((*load_prim_id, *input_pin));
                if let Some(prim) = lir.get_primitive_mut(*load_prim_id) {
                    if (*input_pin as usize) < prim.inputs.len() {
                        prim.inputs[*input_pin as usize] = iso_out_net_id;
                    }
                }
            }
        }

        // Update original net
        if let Some(orig_net) = lir.get_net_mut(insertion.source_net) {
            orig_net.loads.retain(|(pid, _)| {
                !insertion
                    .loads_to_reconnect
                    .iter()
                    .any(|(lpid, _)| lpid == pid)
            });
            orig_net.loads.push((iso_prim_id, 0));
        }

        lir.add_net(iso_out_net);
        result.isolation_cells_inserted += 1;
    }

    lir.update_stats();
    result
}

/// Insert a buffer tree and return the final output nets
fn insert_buffer_tree(
    lir: &mut Lir,
    root_input: NetId,
    plan: &BufferTreePlan,
    next_prim_id: &mut u32,
    next_net_id: &mut u32,
) -> (usize, Vec<NetId>) {
    let mut bufs_inserted = 0usize;
    let mut current_level_nets = vec![root_input];
    let mut final_nets = Vec::new();

    for (level, &strength) in plan.level_strengths.iter().enumerate() {
        let is_final_level = level == plan.level_strengths.len() - 1;
        let mut next_level_nets = Vec::new();

        for input_net in &current_level_nets {
            // Create buffer
            let buf_id = PrimitiveId(*next_prim_id);
            *next_prim_id += 1;

            let buf_out_id = NetId(*next_net_id);
            *next_net_id += 1;

            let buf = Primitive::new_always_on_buf(
                buf_id,
                format!("power_infra/buf_L{}_{}_X{}", level, buf_out_id.0, strength),
                *input_net,
                buf_out_id,
            );

            let mut buf_out_net =
                LirNet::new(buf_out_id, format!("buf_out_L{}_{}", level, buf_out_id.0));
            buf_out_net.driver = Some((buf_id, 0));

            // Connect buffer input to source net
            if let Some(src_net) = lir.get_net_mut(*input_net) {
                src_net.loads.push((buf_id, 0));
            }

            lir.add_primitive(buf);
            lir.add_net(buf_out_net);
            bufs_inserted += 1;

            if is_final_level {
                final_nets.push(buf_out_id);
            } else {
                next_level_nets.push(buf_out_id);
            }
        }

        current_level_nets = next_level_nets;
    }

    (bufs_inserted, final_nets)
}

/// Reconnect loads to buffer tree outputs
fn reconnect_loads_to_tree(lir: &mut Lir, loads: &[(PrimitiveId, u8)], tree_outputs: &[NetId]) {
    if tree_outputs.is_empty() {
        return;
    }

    // Distribute loads evenly across tree outputs
    let loads_per_output = loads.len().div_ceil(tree_outputs.len());

    for (i, (load_prim_id, input_pin)) in loads.iter().enumerate() {
        let output_idx = i / loads_per_output.max(1);
        let output_net = tree_outputs[output_idx.min(tree_outputs.len() - 1)];

        // Add load to output net
        if let Some(net) = lir.get_net_mut(output_net) {
            net.loads.push((*load_prim_id, *input_pin));
        }

        // Update primitive input
        if let Some(prim) = lir.get_primitive_mut(*load_prim_id) {
            if (*input_pin as usize) < prim.inputs.len() {
                prim.inputs[*input_pin as usize] = output_net;
            }
        }
    }
}

/// Result of power infrastructure insertion
#[derive(Debug, Clone, Default)]
pub struct PowerInsertionResult {
    /// Number of level shifters inserted
    pub level_shifters_inserted: usize,
    /// Number of isolation cells inserted
    pub isolation_cells_inserted: usize,
    /// Number of always-on buffers inserted
    pub always_on_bufs_inserted: usize,
    /// Number of buffer tree buffers inserted
    pub buffers_inserted: usize,
}

/// Format power infrastructure analysis as a report
pub fn format_power_infrastructure_report(analysis: &PowerInfrastructureAnalysis) -> String {
    let mut report = String::new();

    report.push_str("## Power Infrastructure Analysis\n\n");

    // Summary
    report.push_str("### Summary\n\n");
    report.push_str("| Metric | Count |\n|--------|-------|\n");
    report.push_str(&format!(
        "| Power domains | {} |\n",
        analysis.stats.domain_count
    ));
    report.push_str(&format!(
        "| Cross-domain crossings | {} |\n",
        analysis.stats.crossing_count
    ));
    report.push_str(&format!(
        "| Level shifters needed | {} |\n",
        analysis.stats.level_shifters_needed
    ));
    report.push_str(&format!(
        "| Isolation cells needed | {} |\n",
        analysis.stats.isolation_cells_needed
    ));
    report.push_str(&format!(
        "| Violations | {} |\n",
        analysis.stats.violation_count
    ));
    report.push('\n');

    // Violations
    if !analysis.violations.is_empty() {
        report.push_str("### Violations\n\n");
        for violation in &analysis.violations {
            let severity = match violation.severity {
                ViolationSeverity::Error => "ERROR",
                ViolationSeverity::Warning => "WARNING",
            };
            report.push_str(&format!("- **{}**: {}\n", severity, violation.description));
        }
        report.push('\n');
    }

    // Crossings detail
    if !analysis.crossings.is_empty() {
        report.push_str("### Domain Crossings\n\n");
        report.push_str("| Signal | From Domain | To Domain | Voltage Change |\n");
        report.push_str("|--------|-------------|-----------|----------------|\n");
        for crossing in &analysis.crossings {
            let voltage_change = if crossing.from_domain.voltage_mv != crossing.to_domain.voltage_mv
            {
                format!(
                    "{}mV → {}mV",
                    crossing.from_domain.voltage_mv, crossing.to_domain.voltage_mv
                )
            } else {
                "Same".to_string()
            };
            report.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                crossing.net_name,
                crossing.from_domain.name,
                crossing.to_domain.name,
                voltage_change
            ));
        }
        report.push('\n');
    }

    report
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_voltage_inference() {
        assert_eq!(infer_voltage_from_domain_name("vdd_core"), 1000);
        assert_eq!(infer_voltage_from_domain_name("vdd_io"), 3300);
        assert_eq!(infer_voltage_from_domain_name("vdd_1v8"), 1800);
        assert_eq!(infer_voltage_from_domain_name("vdd_3v3"), 3300);
        assert_eq!(infer_voltage_from_domain_name("vdd_analog"), 5000);
    }

    #[test]
    fn test_default_domain() {
        let domain = PowerDomainInfo::default();
        assert_eq!(domain.name, "vdd_default");
        assert_eq!(domain.voltage_mv, 1000);
        assert!(!domain.is_power_gated);
        assert!(!domain.is_always_on);
    }

    #[test]
    fn test_empty_analysis() {
        let lir = Lir::new("test".to_string());
        let analysis = analyze_power_infrastructure(&lir);
        assert_eq!(analysis.crossings.len(), 0);
        assert_eq!(analysis.violations.len(), 0);
    }
}
