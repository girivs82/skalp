//! I/O Pad Insertion Pass
//!
//! This module implements automatic and explicit I/O pad insertion for
//! gate-level netlists. Pads are physical interface cells at the chip
//! boundary that provide ESD protection, level shifting, and drive capability.
//!
//! # Insertion Modes
//!
//! - **Auto**: Insert pads for all primary I/O ports
//! - **Explicit**: Only insert pads where explicitly requested via attributes
//! - **None**: No pad insertion (for IP blocks or hierarchical designs)
//!
//! # Pad Types
//!
//! - Input pads: External → core signal path
//! - Output pads: Core → external signal path with output enable
//! - Bidirectional pads: Both directions with direction control
//! - Clock pads: Low-jitter clock input
//! - Power/Ground pads: Supply connections with ESD
//! - Analog pads: Analog pass-through
//! - LDO pads: Integrated voltage regulation

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use crate::tech_library::{CellFunction, TechLibrary};

/// Pad insertion mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PadInsertionMode {
    /// Insert pads for all primary I/O ports
    Auto,
    /// Only insert pads where explicitly requested
    Explicit,
    /// No pad insertion
    #[default]
    None,
}

/// Configuration for pad insertion
#[derive(Debug, Clone)]
pub struct PadInsertConfig {
    /// Insertion mode
    pub mode: PadInsertionMode,
    /// Default input pad cell name (if not using library lookup)
    pub default_input_pad: Option<String>,
    /// Default output pad cell name (if not using library lookup)
    pub default_output_pad: Option<String>,
    /// Default bidirectional pad cell name
    pub default_bidir_pad: Option<String>,
    /// Default clock pad cell name
    pub default_clock_pad: Option<String>,
    /// Whether to insert power/ground pads
    pub insert_power_pads: bool,
    /// Clock signal pattern (regex pattern to identify clock inputs)
    pub clock_pattern: Option<String>,
}

impl Default for PadInsertConfig {
    fn default() -> Self {
        Self {
            mode: PadInsertionMode::None,
            default_input_pad: None,
            default_output_pad: None,
            default_bidir_pad: None,
            default_clock_pad: None,
            insert_power_pads: false,
            clock_pattern: Some("clk|clock".to_string()),
        }
    }
}

impl PadInsertConfig {
    /// Create config for auto mode with all I/O padded
    pub fn auto() -> Self {
        Self {
            mode: PadInsertionMode::Auto,
            ..Default::default()
        }
    }

    /// Create config for explicit mode
    pub fn explicit() -> Self {
        Self {
            mode: PadInsertionMode::Explicit,
            ..Default::default()
        }
    }
}

/// Statistics from pad insertion
#[derive(Debug, Clone, Default)]
pub struct PadInsertStats {
    /// Number of input pads inserted
    pub input_pads: usize,
    /// Number of output pads inserted
    pub output_pads: usize,
    /// Number of bidirectional pads inserted
    pub bidir_pads: usize,
    /// Number of clock pads inserted
    pub clock_pads: usize,
    /// Number of power pads inserted
    pub power_pads: usize,
    /// Number of ground pads inserted
    pub ground_pads: usize,
    /// Number of analog pads inserted
    pub analog_pads: usize,
    /// Total area added (µm²)
    pub area_added: f64,
    /// Total FIT added
    pub fit_added: f64,
}

impl PadInsertStats {
    /// Get total number of pads inserted
    pub fn total_pads(&self) -> usize {
        self.input_pads
            + self.output_pads
            + self.bidir_pads
            + self.clock_pads
            + self.power_pads
            + self.ground_pads
            + self.analog_pads
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Inserted {} pads (in:{}, out:{}, bidir:{}, clk:{}, pwr:{}, gnd:{}, analog:{}), area: {:.1} µm², FIT: {:.3}",
            self.total_pads(),
            self.input_pads,
            self.output_pads,
            self.bidir_pads,
            self.clock_pads,
            self.power_pads,
            self.ground_pads,
            self.analog_pads,
            self.area_added,
            self.fit_added
        )
    }
}

/// Pad insertion pass
pub struct PadInsertion<'a> {
    config: PadInsertConfig,
    library: &'a TechLibrary,
    stats: PadInsertStats,
    next_cell_id: u32,
    next_net_id: u32,
}

impl<'a> PadInsertion<'a> {
    /// Create a new pad insertion pass
    pub fn new(library: &'a TechLibrary, config: PadInsertConfig) -> Self {
        Self {
            config,
            library,
            stats: PadInsertStats::default(),
            next_cell_id: 0,
            next_net_id: 0,
        }
    }

    /// Run pad insertion on a netlist
    pub fn run(&mut self, netlist: &mut GateNetlist) -> PadInsertStats {
        if self.config.mode == PadInsertionMode::None {
            return self.stats.clone();
        }

        // Initialize IDs from existing netlist
        self.next_cell_id = netlist.cells.len() as u32;
        self.next_net_id = netlist.nets.len() as u32;

        // Collect inputs and outputs to process
        let inputs: Vec<GateNetId> = netlist.inputs.clone();
        let outputs: Vec<GateNetId> = netlist.outputs.clone();

        // Process inputs
        for input_net in inputs {
            self.process_input(netlist, input_net);
        }

        // Process outputs
        for output_net in outputs {
            self.process_output(netlist, output_net);
        }

        self.stats.clone()
    }

    /// Process an input port
    fn process_input(&mut self, netlist: &mut GateNetlist, net_id: GateNetId) {
        let net_name = netlist.nets[net_id.0 as usize].name.clone();

        // Determine pad type based on signal name
        let is_clock = self.is_clock_signal(&net_name);

        // Find appropriate pad cell
        let (cell_func, pad_cell) = if is_clock {
            (
                CellFunction::ClockPad,
                self.find_pad_cell(&CellFunction::ClockPad),
            )
        } else {
            (
                CellFunction::InputPad,
                self.find_pad_cell(&CellFunction::InputPad),
            )
        };

        if let Some((cell_name, cell_fit, cell_area)) = pad_cell {
            // Create internal net (the original net becomes the pad's output)
            let pad_input_net = self.create_net(netlist, format!("{}_pad", net_name));

            // Create pad cell
            let pad_cell = Cell::new_comb(
                CellId(self.next_cell_id),
                cell_name,
                self.library.name.clone(),
                cell_fit,
                format!("{}_pad_inst", net_name),
                vec![pad_input_net], // External pad signal
                vec![net_id],        // Core signal (original input net)
            );
            self.next_cell_id += 1;
            netlist.add_cell(pad_cell);

            // Update input to be the pad input net
            if let Some(pos) = netlist.inputs.iter().position(|&n| n == net_id) {
                netlist.inputs[pos] = pad_input_net;
            }

            // Update stats
            self.stats.area_added += cell_area;
            self.stats.fit_added += cell_fit;
            match cell_func {
                CellFunction::ClockPad => self.stats.clock_pads += 1,
                CellFunction::InputPad => self.stats.input_pads += 1,
                _ => {}
            }
        }
    }

    /// Process an output port
    fn process_output(&mut self, netlist: &mut GateNetlist, net_id: GateNetId) {
        let net_name = netlist.nets[net_id.0 as usize].name.clone();

        // Find output pad cell
        if let Some((cell_name, cell_fit, cell_area)) = self.find_pad_cell(&CellFunction::OutputPad)
        {
            // Create external pad net
            let pad_output_net = self.create_net(netlist, format!("{}_pad", net_name));

            // Create a constant 1 for output enable (always enabled)
            // In a real implementation, this would come from explicit OE signals
            let oe_net = self.get_or_create_const_net(netlist, true);

            // Create pad cell
            let pad_cell = Cell::new_comb(
                CellId(self.next_cell_id),
                cell_name,
                self.library.name.clone(),
                cell_fit,
                format!("{}_pad_inst", net_name),
                vec![net_id, oe_net], // Core signal + OE
                vec![pad_output_net], // External pad signal
            );
            self.next_cell_id += 1;
            netlist.add_cell(pad_cell);

            // Update output to be the pad output net
            if let Some(pos) = netlist.outputs.iter().position(|&n| n == net_id) {
                netlist.outputs[pos] = pad_output_net;
            }

            // Update stats
            self.stats.area_added += cell_area;
            self.stats.fit_added += cell_fit;
            self.stats.output_pads += 1;
        }
    }

    /// Create a new net in the netlist
    fn create_net(&mut self, netlist: &mut GateNetlist, name: String) -> GateNetId {
        let net_id = GateNetId(self.next_net_id);
        self.next_net_id += 1;
        netlist.add_net(GateNet::new(net_id, name));
        net_id
    }

    /// Get or create a constant net (0 or 1)
    fn get_or_create_const_net(&mut self, netlist: &mut GateNetlist, value: bool) -> GateNetId {
        let const_name = if value { "const_1" } else { "const_0" };

        // Check if constant net already exists
        for net in &netlist.nets {
            if net.name == const_name {
                return net.id;
            }
        }

        // Create new constant net
        self.create_net(netlist, const_name.to_string())
    }

    /// Check if a signal name indicates a clock
    fn is_clock_signal(&self, name: &str) -> bool {
        if let Some(pattern) = &self.config.clock_pattern {
            let lower = name.to_lowercase();
            pattern
                .split('|')
                .any(|p| lower.contains(&p.to_lowercase()))
        } else {
            false
        }
    }

    /// Find a pad cell in the library
    fn find_pad_cell(&self, func: &CellFunction) -> Option<(String, f64, f64)> {
        // Check for explicit default in config
        let default_name = match func {
            CellFunction::InputPad => self.config.default_input_pad.as_ref(),
            CellFunction::OutputPad => self.config.default_output_pad.as_ref(),
            CellFunction::BidirPad => self.config.default_bidir_pad.as_ref(),
            CellFunction::ClockPad => self.config.default_clock_pad.as_ref(),
            _ => None,
        };

        if let Some(name) = default_name {
            // Look up the named cell in the library
            if let Some(cell) = self.library.get_cell(name) {
                return Some((cell.name.clone(), cell.fit, cell.area.unwrap_or(0.0)));
            }
        }

        // Find best cell by function
        self.library
            .find_best_cell(func)
            .map(|cell| (cell.name.clone(), cell.fit, cell.area.unwrap_or(0.0)))
    }
}

/// Run pad insertion on a netlist
pub fn run_pad_insertion(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    config: PadInsertConfig,
) -> PadInsertStats {
    let mut pass = PadInsertion::new(library, config);
    pass.run(netlist)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::get_stdlib_library;

    #[test]
    fn test_pad_config_default() {
        let config = PadInsertConfig::default();
        assert_eq!(config.mode, PadInsertionMode::None);
    }

    #[test]
    fn test_pad_config_auto() {
        let config = PadInsertConfig::auto();
        assert_eq!(config.mode, PadInsertionMode::Auto);
    }

    #[test]
    fn test_clock_pattern_matching() {
        let library = get_stdlib_library("generic_asic").unwrap();
        let pass = PadInsertion::new(&library, PadInsertConfig::auto());

        assert!(pass.is_clock_signal("clk"));
        assert!(pass.is_clock_signal("clock"));
        assert!(pass.is_clock_signal("sys_clk"));
        assert!(pass.is_clock_signal("CLOCK_IN"));
        assert!(!pass.is_clock_signal("data_in"));
        assert!(!pass.is_clock_signal("reset"));
    }

    #[test]
    fn test_no_insertion_when_disabled() {
        let library = get_stdlib_library("generic_asic").unwrap();
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        // Add a simple input/output
        let input = netlist.add_input("data_in".to_string());
        let output = netlist.add_output("data_out".to_string());

        // Run with None mode
        let config = PadInsertConfig::default();
        let stats = run_pad_insertion(&mut netlist, &library, config);

        assert_eq!(stats.total_pads(), 0);
        assert_eq!(netlist.inputs[0], input);
        assert_eq!(netlist.outputs[0], output);
    }
}
