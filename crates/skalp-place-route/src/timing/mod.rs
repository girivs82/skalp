//! Timing Analysis Module
//!
//! Implements static timing analysis (STA) for FPGA designs.

mod delays;

pub use delays::DelayModel;

use crate::device::Device;
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};

/// Timing configuration
#[derive(Debug, Clone)]
pub struct TimingConfig {
    /// Target frequency in MHz
    pub target_frequency: f64,
    /// Setup margin in ns
    pub setup_margin: f64,
    /// Hold margin in ns
    pub hold_margin: f64,
    /// Clock uncertainty in ns
    pub clock_uncertainty: f64,
    /// Enable multicycle path analysis
    pub multicycle_analysis: bool,
    /// Maximum critical paths to report
    pub max_critical_paths: usize,
}

impl Default for TimingConfig {
    fn default() -> Self {
        Self {
            target_frequency: 100.0,
            setup_margin: 0.2,
            hold_margin: 0.1,
            clock_uncertainty: 0.1,
            multicycle_analysis: false,
            max_critical_paths: 10,
        }
    }
}

/// Clock domain summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockSummary {
    /// Clock name
    pub name: String,
    /// Achieved frequency in MHz
    pub frequency: f64,
    /// Number of registers in this domain
    pub register_count: usize,
    /// Worst slack in ns
    pub worst_slack: f64,
}

/// Critical path information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalPath {
    /// Path delay in ns
    pub delay: f64,
    /// Required time in ns
    pub required: f64,
    /// Slack in ns (required - delay)
    pub slack: f64,
    /// Source register
    pub source: String,
    /// Destination register
    pub destination: String,
    /// Path elements (cell names)
    pub path: Vec<String>,
}

/// Timing analysis report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingReport {
    /// Whether timing constraints are met
    pub meets_timing: bool,
    /// Achieved design frequency in MHz
    pub design_frequency: f64,
    /// Target frequency in MHz
    pub target_frequency: f64,
    /// Worst negative slack in ns
    pub worst_negative_slack: f64,
    /// Number of failing paths
    pub failing_paths: usize,
    /// Clock domain summaries
    pub clock_summaries: Vec<ClockSummary>,
    /// Critical paths
    pub critical_paths: Vec<CriticalPath>,
}

impl Default for TimingReport {
    fn default() -> Self {
        Self {
            meets_timing: true,
            design_frequency: 0.0,
            target_frequency: 0.0,
            worst_negative_slack: 0.0,
            failing_paths: 0,
            clock_summaries: Vec::new(),
            critical_paths: Vec::new(),
        }
    }
}

/// Static timing analyzer
#[allow(dead_code)]
pub struct TimingAnalyzer<D: Device> {
    /// Configuration
    config: TimingConfig,
    /// Target device
    device: D,
    /// Delay model
    delay_model: DelayModel,
}

impl<D: Device + Clone> TimingAnalyzer<D> {
    /// Create a new timing analyzer
    pub fn new(config: TimingConfig, device: D) -> Self {
        let delay_model = DelayModel::ice40_default();
        Self {
            config,
            device,
            delay_model,
        }
    }

    /// Analyze timing for a design
    pub fn analyze_timing(
        &mut self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<TimingReport> {
        let mut report = TimingReport {
            target_frequency: self.config.target_frequency,
            ..Default::default()
        };

        // Find clock domains
        let clock_domains = self.find_clock_domains(netlist);

        // Analyze each clock domain
        for (clock_net, registers) in &clock_domains {
            let summary =
                self.analyze_clock_domain(*clock_net, registers, netlist, placement, routing);
            report.clock_summaries.push(summary);
        }

        // Find critical paths
        report.critical_paths =
            self.find_critical_paths(netlist, placement, routing, &clock_domains);

        // Calculate overall metrics
        report.worst_negative_slack = report
            .critical_paths
            .iter()
            .map(|p| -p.slack)
            .fold(0.0, f64::max);

        report.failing_paths = report
            .critical_paths
            .iter()
            .filter(|p| p.slack < 0.0)
            .count();

        // Calculate achieved frequency
        let min_clock_period = report
            .clock_summaries
            .iter()
            .map(|s| 1000.0 / s.frequency) // Convert to ns
            .fold(0.0, f64::max);

        report.design_frequency = if min_clock_period > 0.0 {
            1000.0 / min_clock_period
        } else {
            self.config.target_frequency
        };

        report.meets_timing = report.failing_paths == 0;

        Ok(report)
    }

    /// Find clock domains in the design
    fn find_clock_domains(
        &self,
        netlist: &GateNetlist,
    ) -> HashMap<GateNetId, Vec<skalp_lir::gate_netlist::CellId>> {
        let mut domains: HashMap<GateNetId, Vec<skalp_lir::gate_netlist::CellId>> = HashMap::new();

        for cell in &netlist.cells {
            // Check if cell is sequential (has clock)
            if let Some(clock_net) = cell.clock {
                domains.entry(clock_net).or_default().push(cell.id);
            }
        }

        // If no clock domains found, create a synthetic one
        if domains.is_empty() {
            // Find nets marked as clock
            for net in &netlist.nets {
                if net.is_clock {
                    domains.insert(net.id, Vec::new());
                    break;
                }
            }
        }

        // If still no clock domains, create a default
        if domains.is_empty() {
            domains.insert(GateNetId(0), Vec::new());
        }

        domains
    }

    /// Analyze a single clock domain
    fn analyze_clock_domain(
        &self,
        clock_net: GateNetId,
        registers: &[skalp_lir::gate_netlist::CellId],
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> ClockSummary {
        let clock_name = netlist
            .nets
            .get(clock_net.0 as usize)
            .map(|n| n.name.clone())
            .unwrap_or_else(|| format!("clock_{}", clock_net.0));

        // Calculate worst path delay in this domain
        let mut worst_delay = 0.0f64;

        for &reg_id in registers {
            // Find paths to this register
            let paths = self.find_paths_to_register(reg_id, netlist, placement, routing);
            for path_delay in paths {
                worst_delay = worst_delay.max(path_delay);
            }
        }

        // Ensure minimum realistic path delay for iCE40
        // Even the simplest path has at least: clock-to-Q + routing + setup
        // Minimum realistic delay: ~2ns (for a reg->reg path with minimal logic)
        let min_realistic_delay = self.delay_model.dff_clk_to_q
            + self.delay_model.local_wire_delay
            + self.delay_model.dff_setup;
        worst_delay = worst_delay.max(min_realistic_delay);

        // Convert to frequency
        let period = worst_delay + self.config.setup_margin + self.config.clock_uncertainty;
        // Cap maximum frequency to realistic iCE40 limits (275 MHz max)
        let frequency = (1000.0 / period).min(275.0); // MHz

        let target_period = 1000.0 / self.config.target_frequency;
        let worst_slack = target_period - worst_delay - self.config.setup_margin;

        ClockSummary {
            name: clock_name,
            frequency,
            register_count: registers.len(),
            worst_slack,
        }
    }

    /// Find paths to a register
    fn find_paths_to_register(
        &self,
        reg_id: skalp_lir::gate_netlist::CellId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Vec<f64> {
        let mut delays = Vec::new();

        // Get the register's input net
        let cell = match netlist.get_cell(reg_id) {
            Some(c) => c,
            None => return delays,
        };

        for &input_net_id in &cell.inputs {
            // Trace back through combinational logic
            let delay = self.trace_path_delay(
                input_net_id,
                netlist,
                placement,
                routing,
                &mut HashSet::new(),
            );
            delays.push(delay);
        }

        delays
    }

    /// Trace path delay through combinational logic
    #[allow(clippy::only_used_in_recursion)]
    fn trace_path_delay(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
        visited: &mut HashSet<GateNetId>,
    ) -> f64 {
        // Prevent infinite loops
        if visited.contains(&net_id) {
            return 0.0;
        }
        visited.insert(net_id);

        let net = match netlist.nets.get(net_id.0 as usize) {
            Some(n) => n,
            None => return 0.0,
        };

        // Get routing delay for this net
        let routing_delay = routing
            .routes
            .get(&net_id)
            .map(|r| r.delay as f64 / 1000.0) // Convert ps to ns
            .unwrap_or(0.1);

        // If this is a primary input or clock, stop here
        if net.is_input || net.is_clock {
            return routing_delay;
        }

        // Get the driver cell
        let driver_id = match net.driver {
            Some(id) => id,
            None => return routing_delay,
        };

        let driver = match netlist.get_cell(driver_id) {
            Some(c) => c,
            None => return routing_delay,
        };

        // If driver is sequential, this is a register-to-register path start
        if driver.clock.is_some() {
            let reg_delay = self.delay_model.register_clock_to_q();
            return reg_delay + routing_delay;
        }

        // Driver is combinational - get its delay and trace inputs
        let cell_delay = self.delay_model.cell_delay(&driver.cell_type);

        // Find maximum input delay
        let mut max_input_delay = 0.0f64;
        for &input_net in &driver.inputs {
            let input_delay =
                self.trace_path_delay(input_net, netlist, placement, routing, visited);
            max_input_delay = max_input_delay.max(input_delay);
        }

        max_input_delay + cell_delay + routing_delay
    }

    /// Find critical paths
    fn find_critical_paths(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
        clock_domains: &HashMap<GateNetId, Vec<skalp_lir::gate_netlist::CellId>>,
    ) -> Vec<CriticalPath> {
        let mut paths = Vec::new();
        let target_period = 1000.0 / self.config.target_frequency;

        for registers in clock_domains.values() {
            for &reg_id in registers {
                let cell = match netlist.get_cell(reg_id) {
                    Some(c) => c,
                    None => continue,
                };

                for &input_net in &cell.inputs {
                    let mut visited = HashSet::new();
                    let delay =
                        self.trace_path_delay(input_net, netlist, placement, routing, &mut visited);

                    let required = target_period - self.config.setup_margin;
                    let slack = required - delay;

                    paths.push(CriticalPath {
                        delay,
                        required,
                        slack,
                        source: "start".to_string(),
                        destination: cell.path.clone(),
                        path: vec![cell.path.clone()],
                    });
                }
            }
        }

        // Sort by slack (worst first) and take top N
        paths.sort_by(|a, b| {
            a.slack
                .partial_cmp(&b.slack)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        paths.truncate(self.config.max_critical_paths);

        paths
    }
}

/// Timing-driven placer wrapper
pub struct TimingDrivenPlacer<D: Device> {
    /// Underlying placer config
    placer_config: crate::placer::PlacerConfig,
    /// Timing config
    timing_config: TimingConfig,
    /// Device
    device: D,
}

impl<D: Device + Clone> TimingDrivenPlacer<D> {
    /// Create a new timing-driven placer
    pub fn new(
        placer_config: crate::placer::PlacerConfig,
        timing_config: TimingConfig,
        device: D,
    ) -> Self {
        Self {
            placer_config,
            timing_config,
            device,
        }
    }

    /// Place with timing optimization
    pub fn place_with_timing(
        &mut self,
        netlist: &GateNetlist,
    ) -> Result<(PlacementResult, TimingReport)> {
        // Run placement
        let mut placer =
            crate::placer::Placer::new(self.placer_config.clone(), self.device.clone());
        let placement = placer.place(netlist)?;

        // Run basic routing for timing estimation
        let router_config = crate::router::RouterConfig::default();
        let mut router = crate::router::Router::new(router_config, self.device.clone());
        let routing = router.route(netlist, &placement)?;

        // Run timing analysis
        let mut analyzer = TimingAnalyzer::new(self.timing_config.clone(), self.device.clone());
        let report = analyzer.analyze_timing(netlist, &placement, &routing)?;

        Ok((placement, report))
    }
}
