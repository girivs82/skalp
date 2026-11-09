//! Nexus timing data for the generic analyzer ([`crate::timing::sta`]).
//!
//! Uses prjoxide's delay tables: per-pip interconnect classes and per-cell
//! timing arcs. CertusPro-NX has no tables of its own and shares the Nexus
//! fabric, so its numbers are CrossLink-NX's and are estimates.

use super::{ebr, NexusDevice, NexusVariant};
use crate::bitstream::prepare::{constant_value, is_pad};
use crate::device::{Device, PipId, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_LSR};
use crate::error::{PlaceRouteError, Result};
use crate::timing::sta::{TimingEndpoint, TimingModel};
use serde::Deserialize;
use skalp_lir::gate_netlist::Cell;
use skalp_lir::tech_library::CellFunction;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Deserialize)]
struct InterconnectFile {
    pip_classes: HashMap<String, PipClass>,
}

#[derive(Debug, Deserialize)]
struct PipClass {
    base: (i64, i64),
}

#[derive(Debug, Deserialize)]
struct CellsFile {
    celltypes: HashMap<String, CellTiming>,
}

#[derive(Debug, Deserialize)]
struct CellTiming {
    iopaths: Vec<IoPath>,
    #[serde(default)]
    setupholds: Vec<SetupHold>,
}

#[derive(Debug, Deserialize)]
struct IoPath {
    from_pin: String,
    to_pin: String,
    maxv: i64,
}

#[derive(Debug, Deserialize)]
struct SetupHold {
    pin: String,
    max_setup: i64,
}

/// Worst-case delays from prjoxide's timing database, in picoseconds.
pub struct TimingDb {
    pip_classes: HashMap<String, i64>,
    lut: i64,
    clk_to_q: i64,
    setup: HashMap<String, i64>,
    ccu2: HashMap<(String, String), i64>,
    ram_clk_to_q: i64,
    ram_setup: HashMap<String, i64>,
}

impl TimingDb {
    /// Load a device's delay tables from the prjoxide database.
    pub fn load(db_root: &Path, variant: NexusVariant) -> Result<Self> {
        let dir = db_root.join(variant.timing_family()).join("timing");
        let speed = variant.timing_speed_grade();
        let read = |name: String| -> Result<String> {
            std::fs::read_to_string(dir.join(&name))
                .map_err(|e| PlaceRouteError::BitstreamFailed(format!("reading {name}: {e}")))
        };
        let bad = |name: &str, e: serde_json::Error| {
            PlaceRouteError::BitstreamFailed(format!("parsing {name}: {e}"))
        };

        let interconnect: InterconnectFile =
            serde_json::from_str(&read(format!("interconnect_{speed}.json"))?)
                .map_err(|e| bad("interconnect", e))?;
        let cells: CellsFile = serde_json::from_str(&read(format!("cells_{speed}.json"))?)
            .map_err(|e| bad("cells", e))?;

        let cell = |name: &str| {
            cells.celltypes.get(name).ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(format!("No timing data for {name}"))
            })
        };
        let lut = cell("OXIDE_COMB:LUT4")?
            .iopaths
            .iter()
            .map(|p| p.maxv)
            .max()
            .unwrap_or(0);
        let ff = cell("OXIDE_FF:PPP:SYNC")?;
        let clk_to_q = ff.iopaths.iter().map(|p| p.maxv).max().unwrap_or(0);
        let setup = ff
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();
        let ccu2 = cell("OXIDE_COMB:CCU2")?
            .iopaths
            .iter()
            .map(|p| ((p.from_pin.clone(), p.to_pin.clone()), p.maxv))
            .collect();

        let ram = cell("PDPSC16K_MODE")?;
        let ram_clk_to_q = ram.iopaths.iter().map(|p| p.maxv).max().unwrap_or(0);
        let ram_setup = ram
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();

        Ok(Self {
            pip_classes: interconnect
                .pip_classes
                .into_iter()
                .map(|(name, class)| (name, class.base.1))
                .collect(),
            lut,
            clk_to_q,
            setup,
            ccu2,
            ram_clk_to_q,
            ram_setup,
        })
    }
}

/// The name and tile of a routing wire, split for pip classification.
fn wire_parts(name: &str) -> (i32, i32, String) {
    if let Some(global) = name.strip_prefix("G:") {
        return (0, 0, global.to_string());
    }
    let parsed = name.strip_prefix('R').and_then(|rest| {
        let (row, rest) = rest.split_once('C')?;
        let (column, base) = rest.split_once('_')?;
        Some((column.parse().ok()?, row.parse().ok()?, base.to_string()))
    });
    parsed.unwrap_or((0, 0, name.to_string()))
}

/// Name of a `CCU2` input by index.
fn ccu2_input(index: usize) -> String {
    match index {
        8 => "FCI".to_string(),
        i => format!("{}{}", ['A', 'B', 'C', 'D'][i % 4], i / 4),
    }
}

fn ccu2_output(index: usize) -> &'static str {
    match index {
        0 => "F0",
        1 => "F1",
        _ => "FCO",
    }
}

/// [`TimingModel`] for a Nexus device.
pub struct NexusTiming<'a> {
    /// Real device model the design was routed on
    pub device: &'a NexusDevice,
    /// Delay tables for the device
    pub db: &'a TimingDb,
}

impl NexusTiming<'_> {
    fn is_dist_ram(cell: &Cell) -> bool {
        cell.function == Some(CellFunction::DistRam16x4)
    }

    fn is_carry(cell: &Cell) -> bool {
        cell.function == Some(CellFunction::Ccu2)
    }

    /// Setup time of a block RAM pin, from the table's bus-wide entries.
    fn ram_setup_ps(&self, pin: &str) -> i64 {
        let bus = pin.trim_end_matches(|c: char| c.is_ascii_digit());
        let key = match (bus, pin[bus.len()..].parse::<usize>()) {
            ("ADA" | "ADB", Ok(bit)) if bit < 5 => format!("{bus}_4_0"),
            ("ADA" | "ADB", Ok(_)) => format!("{bus}_13_5"),
            _ => bus.to_string(),
        };
        self.db.ram_setup.get(&key).copied().unwrap_or(0)
    }
}

impl TimingModel for NexusTiming<'_> {
    fn interconnect_ps(&self, pip: PipId) -> i64 {
        let Some(info) = self.device.pip(pip) else {
            return 0;
        };
        let (Some(src), Some(dst)) = (
            self.device.wire(info.src_wire),
            self.device.wire(info.dst_wire),
        ) else {
            return 0;
        };
        let (sx, sy, source) = wire_parts(&src.name);
        let (dx, dy, destination) = wire_parts(&dst.name);
        prjoxide::pip_classes::classify_pip(0, 0, &source, dx - sx, dy - sy, &destination)
            .and_then(|class| self.db.pip_classes.get(&class).copied())
            .unwrap_or(0)
    }

    fn launch_ps(&self, cell: &Cell, _output: usize) -> Option<i64> {
        if is_pad(cell) || constant_value(cell).is_some() {
            return Some(0);
        }
        if Self::is_dist_ram(cell) {
            return None;
        }
        if cell.ram_ports().is_some() {
            return Some(self.db.ram_clk_to_q);
        }
        cell.clock.map(|_| self.db.clk_to_q)
    }

    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        if Self::is_dist_ram(cell) {
            return (9..=12).contains(&input).then_some(self.db.lut);
        }
        if Self::is_carry(cell) {
            let arc = (ccu2_input(input), ccu2_output(output).to_string());
            return self.db.ccu2.get(&arc).copied();
        }
        Some(self.db.lut)
    }

    fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint> {
        let setup = |name: &str| self.db.setup.get(name).copied().unwrap_or(0);
        if let Some(ports) = cell.ram_ports() {
            return (0..cell.inputs.len())
                .filter_map(|input| {
                    let name = ebr::input_pin(&ports, input)?;
                    (name != "CLKA" && name != "CLKB").then(|| TimingEndpoint {
                        input,
                        pin: ebr::router_pin(&name).unwrap_or(input),
                        setup_ps: self.ram_setup_ps(&name),
                    })
                })
                .collect();
        }
        if Self::is_dist_ram(cell) {
            return (0..=8)
                .map(|input| TimingEndpoint {
                    input,
                    pin: input,
                    setup_ps: 0,
                })
                .collect();
        }
        (0..cell.inputs.len())
            .map(|input| {
                let (pin, setup_ps) = match (cell.cell_type.as_str(), input) {
                    (_, 0) => (0, setup("M")),
                    ("OXIDE_FF_EN", 1) | ("OXIDE_FF_RSTE", 2) => (FF_PIN_CE, setup("CE")),
                    _ => (FF_PIN_LSR, setup("LSR")),
                };
                TimingEndpoint {
                    input,
                    pin,
                    setup_ps,
                }
            })
            .collect()
    }

    fn input_pins(&self, cell: &Cell, input: usize) -> Vec<usize> {
        if Self::is_dist_ram(cell) && (9..=12).contains(&input) {
            let base = DIST_RAM_PIN_RAD + 4 * (input - 9);
            return (0..4).map(|lut| base + lut).collect();
        }
        vec![input]
    }

    fn is_dedicated(&self, cell: &Cell, input: usize) -> bool {
        Self::is_carry(cell) && input == 8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_names_split_into_location_and_base() {
        assert_eq!(
            wire_parts("R2C97_H01W0300"),
            (97, 2, "H01W0300".to_string())
        );
        assert_eq!(wire_parts("G:VCC"), (0, 0, "VCC".to_string()));
    }

    #[test]
    fn carry_pins_are_named_by_half() {
        assert_eq!(ccu2_input(0), "A0");
        assert_eq!(ccu2_input(7), "D1");
        assert_eq!(ccu2_input(8), "FCI");
    }

    #[test]
    fn delays_come_from_the_database() {
        let Some(db) = super::super::prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let timing = TimingDb::load(&db, NexusVariant::Lfcpnx100).unwrap();
        assert_eq!(timing.lut, 213);
        assert_eq!(timing.clk_to_q, 307);
        assert_eq!(timing.setup.get("M"), Some(&0));
    }
}
