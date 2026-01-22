//! Delay Models for iCE40 FPGAs
//!
//! Provides timing delay models for cells and routing.

use serde::{Deserialize, Serialize};

/// Delay model for timing analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DelayModel {
    /// LUT4 propagation delay (ns)
    pub lut4_delay: f64,
    /// DFF clock-to-Q delay (ns)
    pub dff_clk_to_q: f64,
    /// DFF setup time (ns)
    pub dff_setup: f64,
    /// DFF hold time (ns)
    pub dff_hold: f64,
    /// Carry chain delay per bit (ns)
    pub carry_delay: f64,
    /// I/O input delay (ns)
    pub io_input_delay: f64,
    /// I/O output delay (ns)
    pub io_output_delay: f64,
    /// Local wire delay (ns)
    pub local_wire_delay: f64,
    /// Span-4 wire delay (ns)
    pub span4_delay: f64,
    /// Span-12 wire delay (ns)
    pub span12_delay: f64,
    /// Global clock delay (ns)
    pub global_clock_delay: f64,
    /// PIP switch delay (ns)
    pub pip_delay: f64,
    /// RAM read delay (ns)
    pub ram_read_delay: f64,
    /// RAM write delay (ns)
    pub ram_write_delay: f64,
}

impl Default for DelayModel {
    fn default() -> Self {
        Self::ice40_default()
    }
}

impl DelayModel {
    /// Default delay model for iCE40 FPGAs
    pub fn ice40_default() -> Self {
        Self {
            // iCE40 typical delays (from datasheet)
            lut4_delay: 0.59,        // 590ps
            dff_clk_to_q: 0.85,      // 850ps
            dff_setup: 0.18,         // 180ps
            dff_hold: 0.0,           // 0ps
            carry_delay: 0.09,       // 90ps per bit
            io_input_delay: 1.2,     // 1.2ns
            io_output_delay: 2.5,    // 2.5ns
            local_wire_delay: 0.05,  // 50ps
            span4_delay: 0.2,        // 200ps
            span12_delay: 0.4,       // 400ps
            global_clock_delay: 0.1, // 100ps
            pip_delay: 0.1,          // 100ps per switch
            ram_read_delay: 3.5,     // 3.5ns
            ram_write_delay: 0.0,    // Write is synchronous
        }
    }

    /// Delay model for iCE40 HX series (higher performance)
    pub fn ice40_hx() -> Self {
        Self {
            lut4_delay: 0.54,
            dff_clk_to_q: 0.76,
            dff_setup: 0.15,
            dff_hold: 0.0,
            carry_delay: 0.08,
            io_input_delay: 1.0,
            io_output_delay: 2.2,
            local_wire_delay: 0.04,
            span4_delay: 0.18,
            span12_delay: 0.35,
            global_clock_delay: 0.08,
            pip_delay: 0.09,
            ram_read_delay: 3.2,
            ram_write_delay: 0.0,
        }
    }

    /// Delay model for iCE40 LP series (lower power)
    pub fn ice40_lp() -> Self {
        Self {
            lut4_delay: 0.65,
            dff_clk_to_q: 0.95,
            dff_setup: 0.20,
            dff_hold: 0.0,
            carry_delay: 0.10,
            io_input_delay: 1.4,
            io_output_delay: 2.8,
            local_wire_delay: 0.06,
            span4_delay: 0.22,
            span12_delay: 0.45,
            global_clock_delay: 0.12,
            pip_delay: 0.11,
            ram_read_delay: 3.8,
            ram_write_delay: 0.0,
        }
    }

    /// Delay model for iCE40 UP series (ultra-low power)
    pub fn ice40_up() -> Self {
        Self {
            lut4_delay: 0.70,
            dff_clk_to_q: 1.0,
            dff_setup: 0.22,
            dff_hold: 0.0,
            carry_delay: 0.11,
            io_input_delay: 1.5,
            io_output_delay: 3.0,
            local_wire_delay: 0.07,
            span4_delay: 0.25,
            span12_delay: 0.50,
            global_clock_delay: 0.15,
            pip_delay: 0.12,
            ram_read_delay: 4.0,
            ram_write_delay: 0.0,
        }
    }

    /// Get cell delay for a given cell type
    pub fn cell_delay(&self, cell_type: &str) -> f64 {
        if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
            self.lut4_delay
        } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
            self.dff_clk_to_q
        } else if cell_type.contains("CARRY") || cell_type.starts_with("SB_CARRY") {
            self.carry_delay
        } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
            self.io_input_delay // Conservative estimate
        } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
            self.ram_read_delay
        } else {
            // Default to LUT delay for unknown cells
            self.lut4_delay
        }
    }

    /// Get register clock-to-Q delay
    pub fn register_clock_to_q(&self) -> f64 {
        self.dff_clk_to_q
    }

    /// Get register setup time
    pub fn register_setup(&self) -> f64 {
        self.dff_setup
    }

    /// Get register hold time
    pub fn register_hold(&self) -> f64 {
        self.dff_hold
    }

    /// Get wire delay based on wire type
    pub fn wire_delay(&self, wire_type: &crate::device::WireType) -> f64 {
        match wire_type {
            crate::device::WireType::Local(_) => self.local_wire_delay,
            crate::device::WireType::Span4H(_) | crate::device::WireType::Span4V(_) => {
                self.span4_delay
            }
            crate::device::WireType::Span12H(_) | crate::device::WireType::Span12V(_) => {
                self.span12_delay
            }
            crate::device::WireType::Global(_) => self.global_clock_delay,
            crate::device::WireType::Neighbour => self.local_wire_delay,
            crate::device::WireType::CarryChain => self.carry_delay,
            crate::device::WireType::BelPin => 0.0, // No delay for BEL pins
        }
    }

    /// Get estimated path delay for a given distance
    pub fn estimated_path_delay(&self, manhattan_distance: u32) -> f64 {
        // Use a mix of span-4 and span-12 wires
        let span12_count = manhattan_distance / 12;
        let remaining = manhattan_distance % 12;
        let span4_count = remaining / 4;
        let local_count = remaining % 4;

        (span12_count as f64) * self.span12_delay
            + (span4_count as f64) * self.span4_delay
            + (local_count as f64) * self.local_wire_delay
            + ((span12_count + span4_count + local_count) as f64) * self.pip_delay
    }
}
