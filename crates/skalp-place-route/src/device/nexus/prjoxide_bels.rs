//! M1c: BEL model for the Nexus PLC (logic) tile, derived from the prjoxide
//! database's real site-pin wire names.
//!
//! A PLC tile holds 4 slices (A–D). Each slice provides two `OXIDE_COMB` LUT4s
//! (K0/K1) and two `OXIDE_FF` registers (REG0/REG1). Site-pin wires follow the
//! `J<pin>_SLICE<ch>` convention seen in `tiletypes/PLC.ron`:
//!   - LUT `l` inputs:  `J{A,B,C,D}{l}_SLICE{ch}`   output: `JF{l}_SLICE{ch}`
//!   - FF `r` data in:  `JDI{r}_SLICE{ch}`           out:    `JQ{r}_SLICE{ch}`
//!   - per-slice clock/CE/reset: `JCLK_SLICE{ch}` / `JCE_SLICE{ch}` / `JLSR_SLICE{ch}`
//!
//! These site wires are already nodes in the routing graph (they appear as fixed
//! `conns`), so each BEL pin resolves to an existing [`super::WireId`]. This module
//! produces, per PLC tile, the SKALP `Bel` list plus the `bel_wires` mapping keyed
//! by the conventional names the `Device` helper methods (`lut_output_wire`,
//! `lut_input_wire`, `clock_wire`) expect.

use super::super::{BelType, PinDirection};

/// Logical cells per PLC: 4 slices × 2 LUTs = 8 LUT4, 4 slices × 2 = 8 FF.
pub const SLICES: [&str; 4] = ["A", "B", "C", "D"];

/// One BEL of a PLC tile: its type, display name, and the pins mapping a
/// conventional `bel_wires` key to the site-relative wire it connects to.
#[derive(Debug, Clone)]
pub struct PlcBel {
    pub bel_type: BelType,
    /// Tile-local display name, e.g. `"LUT3"`, `"FF6"`.
    pub name: String,
    /// `(conventional_key, site_wire_name, direction)`. `conventional_key` is the
    /// `bel_wires` suffix the `Device` helpers look up (e.g. `"LUT3_Z"`, `"CLK"`).
    pub pins: Vec<(String, String, PinDirection)>,
}

/// Generate the BEL list for one PLC tile (site-relative wire names).
///
/// LUT `lc_idx = slice*2 + lut` (0..8); FF `ff_idx = slice*2 + reg` (0..8) —
/// matching the `lc_idx` convention of `Device::lut_output_wire`/`lut_input_wire`.
pub fn plc_bels() -> Vec<PlcBel> {
    use PinDirection::{Input, Output};
    let mut bels = Vec::with_capacity(16);

    for (si, ch) in SLICES.iter().enumerate() {
        // Two LUT4s per slice.
        for lut in 0..2 {
            let lc = si * 2 + lut;
            let mut pins = Vec::new();
            for (i, inp) in ["A", "B", "C", "D"].iter().enumerate() {
                let _ = i;
                pins.push((
                    format!("LUT{lc}_{inp}"),
                    format!("J{inp}{lut}_SLICE{ch}"),
                    Input,
                ));
            }
            pins.push((format!("LUT{lc}_Z"), format!("JF{lut}_SLICE{ch}"), Output));
            bels.push(PlcBel {
                bel_type: BelType::Lut4,
                name: format!("LUT{lc}"),
                pins,
            });
        }
        // Two FFs per slice.
        for reg in 0..2 {
            let ff = si * 2 + reg;
            let pins = vec![
                (format!("FF{ff}_D"), format!("JDI{reg}_SLICE{ch}"), Input),
                (format!("FF{ff}_Q"), format!("JQ{reg}_SLICE{ch}"), Output),
                // Per-slice control. "CLK" (unsuffixed, slice A) is what the coarse
                // Device::clock_wire helper returns; per-slice keys are also emitted.
                (format!("FF{ff}_CLK"), format!("JCLK_SLICE{ch}"), Input),
                (format!("FF{ff}_CE"), format!("JCE_SLICE{ch}"), Input),
                (format!("FF{ff}_LSR"), format!("JLSR_SLICE{ch}"), Input),
            ];
            bels.push(PlcBel {
                bel_type: BelType::Dff,
                name: format!("FF{ff}"),
                pins,
            });
        }
    }
    bels
}

/// The site wire that `Device::clock_wire` resolves for a PLC tile (slice-A clock).
pub fn plc_clock_site_wire() -> &'static str {
    "JCLK_SLICEA"
}

#[cfg(test)]
mod tests {
    use super::super::prjoxide_graph::build_routing_graph;
    use super::super::prjoxide_load::{find_database, load_tilegrid};
    use super::super::{resolve_wire, NexusVariant};
    use super::*;

    #[test]
    fn plc_bels_shape() {
        let bels = plc_bels();
        assert_eq!(bels.len(), 16, "8 LUT4 + 8 FF");
        assert_eq!(
            bels.iter().filter(|b| b.bel_type == BelType::Lut4).count(),
            8
        );
        assert_eq!(
            bels.iter().filter(|b| b.bel_type == BelType::Dff).count(),
            8
        );
        // LUT0 inputs/output map to slice-A K0 site wires.
        let lut0 = &bels[0];
        assert!(lut0
            .pins
            .iter()
            .any(|(k, w, _)| k == "LUT0_A" && w == "JA0_SLICEA"));
        assert!(lut0
            .pins
            .iter()
            .any(|(k, w, _)| k == "LUT0_Z" && w == "JF0_SLICEA"));
    }

    /// Every generated BEL-pin site wire must be a real node in the routing graph
    /// (i.e. it appears in the database's conns/pips). This validates the pin
    /// naming against actual silicon data.
    #[test]
    fn every_bel_pin_wire_exists_in_graph() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).expect("tilegrid");
        let plc = tiles
            .iter()
            .find(|t| t.tiletype == "PLC" && t.x > 5 && t.y > 5)
            .expect("PLC tile");
        let g = build_routing_graph(
            &db,
            variant.prjoxide_family(),
            &tiles,
            Some((plc.x, plc.y, plc.x, plc.y)),
        )
        .expect("graph");

        let bels = plc_bels();
        let mut checked = 0;
        for bel in &bels {
            for (key, site_wire, _dir) in &bel.pins {
                let node = resolve_wire(plc.x, plc.y, site_wire)
                    .unwrap_or_else(|| panic!("resolve {site_wire}"));
                let name = match &node {
                    super::super::NodeKey::Local { x, y, name } => format!("R{y}C{x}_{name}"),
                    super::super::NodeKey::Global(n) => format!("G:{n}"),
                    super::super::NodeKey::Special(s) => s.clone(),
                };
                assert!(
                    g.wire_names.contains_key(&name),
                    "BEL pin {key} → site wire {site_wire} ({name}) not in graph"
                );
                checked += 1;
            }
        }
        eprintln!(
            "M1c-part1 OK: all {checked} BEL pins of PLC@({},{}) resolve to real graph nodes",
            plc.x, plc.y
        );
    }
}
