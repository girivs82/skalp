//! Top-Level Place and Route Orchestration
//!
//! Provides the high-level API for running the complete P&R flow.

use crate::bitstream::{Bitstream, BitstreamConfig, BitstreamGenerator};
use crate::device::ice40::{Ice40Device, Ice40Variant};
use crate::error::Result;
use crate::placer::{PlacementResult, Placer, PlacerConfig};
use crate::router::{Router, RouterConfig, RoutingResult};
use crate::timing::{TimingAnalyzer, TimingConfig, TimingReport};
use skalp_lir::gate_netlist::GateNetlist;

/// P&R configuration
#[derive(Debug, Clone)]
pub struct PnrConfig {
    /// Placer configuration
    pub placer: PlacerConfig,
    /// Router configuration
    pub router: RouterConfig,
    /// Bitstream configuration
    pub bitstream: BitstreamConfig,
    /// Timing configuration (optional)
    pub timing: Option<TimingConfig>,
}

impl Default for PnrConfig {
    fn default() -> Self {
        Self {
            placer: PlacerConfig::default(),
            router: RouterConfig::default(),
            bitstream: BitstreamConfig::default(),
            timing: Some(TimingConfig::default()),
        }
    }
}

impl PnrConfig {
    /// Create config for fast compilation (lower quality)
    pub fn fast() -> Self {
        Self {
            placer: PlacerConfig {
                max_iterations: 1000,
                initial_temperature: 50.0,
                ..Default::default()
            },
            router: RouterConfig {
                max_iterations: 20,
                ..Default::default()
            },
            bitstream: BitstreamConfig::default(),
            timing: None,
        }
    }

    /// Create config for high quality (slower)
    pub fn high_quality() -> Self {
        Self {
            placer: PlacerConfig {
                max_iterations: 10000,
                initial_temperature: 200.0,
                cooling_rate: 0.99,
                ..Default::default()
            },
            router: RouterConfig {
                max_iterations: 100,
                history_cost_factor: 0.7,
                ..Default::default()
            },
            bitstream: BitstreamConfig::default(),
            timing: Some(TimingConfig {
                max_critical_paths: 20,
                ..Default::default()
            }),
        }
    }
}

/// P&R result
#[derive(Debug, Clone)]
pub struct PnrResult {
    /// Placement result
    pub placement: PlacementResult,
    /// Routing result
    pub routing: RoutingResult,
    /// Generated bitstream
    pub bitstream: Bitstream,
    /// Timing report (if enabled)
    pub timing: Option<TimingReport>,
}

/// Run place and route on a GateNetlist for iCE40
pub fn place_and_route(
    netlist: &GateNetlist,
    variant: Ice40Variant,
    config: PnrConfig,
) -> Result<PnrResult> {
    // Create device
    let device = Ice40Device::new(variant);

    // Run placement
    let mut placer = Placer::new(config.placer.clone(), device.clone());
    let placement = placer.place(netlist)?;

    // Run routing
    let mut router = Router::new(config.router.clone(), device.clone());
    let routing = router.route(netlist, &placement)?;

    // Run timing analysis if configured
    let timing = if let Some(timing_config) = config.timing.clone() {
        let mut analyzer = TimingAnalyzer::new(timing_config, device.clone());
        Some(analyzer.analyze_timing(netlist, &placement, &routing)?)
    } else {
        None
    };

    // Generate bitstream
    let generator = BitstreamGenerator::with_config(device.clone(), config.bitstream.clone());
    let bitstream = generator.generate(&placement, &routing)?;

    Ok(PnrResult {
        placement,
        routing,
        bitstream,
        timing,
    })
}

/// Convenience function for HX1K
pub fn place_and_route_hx1k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Hx1k, config)
}

/// Convenience function for HX8K
pub fn place_and_route_hx8k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Hx8k, config)
}

/// Convenience function for UP5K
pub fn place_and_route_up5k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Up5k, config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{Cell, GateNet};

    fn create_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "ice40".to_string());

        // Create input net
        let input_net_id = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "in".to_string(),
        ));

        // Create output net
        let output_net_id = netlist.add_net(GateNet::new_output(
            skalp_lir::gate_netlist::GateNetId(1),
            "out".to_string(),
        ));

        // Create a simple LUT cell
        let cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "top.lut0".to_string(),
            vec![input_net_id],
            vec![output_net_id],
        );
        netlist.add_cell(cell);

        netlist
    }

    #[test]
    fn test_basic_pnr_flow() {
        let netlist = create_simple_netlist();
        let config = PnrConfig::fast();

        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(result.is_ok());

        let pnr_result = result.unwrap();
        assert!(!pnr_result.placement.placements.is_empty());
        assert!(!pnr_result.bitstream.data.is_empty());
    }
}
