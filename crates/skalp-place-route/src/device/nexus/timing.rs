//! Nexus timing data for the generic analyzer ([`crate::timing::sta`]).
//!
//! Uses prjoxide's delay tables: per-pip interconnect classes and per-cell
//! timing arcs. A family without tables of its own (upstream prjoxide has
//! CrossLink-NX's only) borrows CrossLink-NX's, whose Nexus fabric it shares;
//! see [`NexusVariant::timing_tables`].

use super::{dsp, ebr, lram, NexusDevice, NexusVariant};
use crate::bitstream::prepare::{constant_value, is_pad};
use crate::collections::HashMap;
use crate::device::{Device, PipId, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_LSR, WIDE_LUT_PIN_SEL};
use crate::error::{PlaceRouteError, Result};
use crate::timing::sta::{TimingEndpoint, TimingModel};
use serde::Deserialize;
use skalp_lir::gate_netlist::Cell;
use skalp_lir::tech_library::CellFunction;
use std::path::Path;

/// Index of a wide LUT's select input; the inputs before it are data.
const WIDE_LUT_SELECT: usize = 4;

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
    minv: i64,
    maxv: i64,
}

#[derive(Debug, Deserialize)]
struct SetupHold {
    pin: String,
    max_setup: i64,
    #[serde(default)]
    max_hold: i64,
}

/// Which end of the process, voltage and temperature range a table covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Corner {
    /// Slowest silicon, lowest voltage, hottest: the speed grade's own
    /// tables, worst-case (largest) delays. Setup is checked here.
    Slow,
    /// Fastest silicon, highest voltage, coldest: prjoxide's `M` tables,
    /// best-case (smallest) delays. Hold is checked here.
    Fast,
}

/// Delays at one corner from prjoxide's timing database, in picoseconds.
/// Where a cell has several arcs and the model uses one number, that number
/// is the corner's extreme: the longest arc at the slow corner, the shortest
/// at the fast one.
pub struct TimingDb {
    pip_classes: HashMap<String, i64>,
    lut: i64,
    /// A wide LUT (two LUT4s and the slice mux): data inputs to the output
    wide_data: i64,
    /// A wide LUT's mux select to the output
    wide_select: i64,
    clk_to_q: i64,
    setup: HashMap<String, i64>,
    ccu2: HashMap<(String, String), i64>,
    multiplier: i64,
    ram_clk_to_q: i64,
    ram_setup: HashMap<String, i64>,
    ram_hold: HashMap<String, i64>,
    /// A large RAM's clock-to-read-data delay
    large_ram_clk_to_q: i64,
    /// Setup and hold time of each large RAM pin, by bus name
    large_ram_setup: HashMap<String, i64>,
    large_ram_hold: HashMap<String, i64>,
    /// Hold time of each register pin
    hold: HashMap<String, i64>,
}

impl TimingDb {
    /// Load a device's slow-corner delay tables from the prjoxide database.
    pub fn load(db_root: &Path, variant: NexusVariant) -> Result<Self> {
        Self::load_corner(db_root, variant, Corner::Slow)
    }

    /// Load a device's delay tables at `corner`.
    pub fn load_corner(db_root: &Path, variant: NexusVariant, corner: Corner) -> Result<Self> {
        // The delay of one arc at this corner, and the extreme of several
        let value = |p: &IoPath| match corner {
            Corner::Slow => p.maxv,
            Corner::Fast => p.minv,
        };
        let extreme = |values: &mut dyn Iterator<Item = i64>| {
            match corner {
                Corner::Slow => values.max(),
                Corner::Fast => values.min(),
            }
            .unwrap_or(0)
        };
        let tables = variant.timing_tables(db_root, corner == Corner::Fast);
        let Some((dir, grade)) = tables.first().cloned() else {
            return Err(PlaceRouteError::BitstreamFailed(format!(
                "no Nexus delay tables in {}",
                db_root.display()
            )));
        };
        if !dir.starts_with(db_root.join(variant.prjoxide_family())) {
            log::warn!(
                "no {} delay tables in {}: using CrossLink-NX's grade {grade}",
                variant.prjoxide_family(),
                db_root.display()
            );
        }
        let read = |dir: &Path, name: String| -> Result<String> {
            std::fs::read_to_string(dir.join(&name))
                .map_err(|e| PlaceRouteError::BitstreamFailed(format!("reading {name}: {e}")))
        };
        let bad = |name: &str, e: serde_json::Error| {
            PlaceRouteError::BitstreamFailed(format!("parsing {name}: {e}"))
        };

        // Pip classes from the first table that has each one: a family's own
        // tables can lack classes its characterisation designs never used.
        let mut classes: HashMap<String, PipClass> = HashMap::default();
        for (index, (dir, grade)) in tables.iter().enumerate() {
            let file: InterconnectFile =
                serde_json::from_str(&read(dir, format!("interconnect_{grade}.json"))?)
                    .map_err(|e| bad("interconnect", e))?;
            let before = classes.len();
            for (name, class) in file.pip_classes {
                classes.entry(name).or_insert(class);
            }
            if index > 0 && classes.len() > before {
                log::info!(
                    "{} pip classes missing from {}'s grade {} tables taken from {}",
                    classes.len() - before,
                    variant.prjoxide_family(),
                    tables[0].1,
                    dir.display()
                );
            }
        }
        let cells: CellsFile = serde_json::from_str(&read(&dir, format!("cells_{grade}.json"))?)
            .map_err(|e| bad("cells", e))?;

        let cell = |name: &str| {
            cells.celltypes.get(name).ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(format!("No timing data for {name}"))
            })
        };
        let lut = extreme(&mut cell("OXIDE_COMB:LUT4")?.iopaths.iter().map(value));
        let wide = cell("OXIDE_COMB:WIDEFN9")?;
        let wide_arc = |pins: &[&str]| {
            extreme(
                &mut wide
                    .iopaths
                    .iter()
                    .filter(|p| pins.contains(&p.from_pin.as_str()))
                    .map(value),
            )
        };
        let wide_data = wide_arc(&["A", "B", "C", "D"]);
        let wide_select = wide_arc(&["SEL"]);
        let ff = cell("OXIDE_FF:PPP:SYNC")?;
        let clk_to_q = extreme(&mut ff.iopaths.iter().map(value));
        let setup = ff
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();
        let hold = ff
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_hold))
            .collect();
        let ccu2 = cell("OXIDE_COMB:CCU2")?
            .iopaths
            .iter()
            .map(|p| ((p.from_pin.clone(), p.to_pin.clone()), value(p)))
            .collect();

        // The corner's extreme path through each of a multiplier's chained
        // blocks.
        let worst_arc =
            |name: &str| -> Result<i64> { Ok(extreme(&mut cell(name)?.iopaths.iter().map(value))) };
        let multiplier = worst_arc("PREADD9_CORE")?
            + worst_arc("MULT9_CORE")?
            + worst_arc("MULT18_CORE")?
            + worst_arc("REG18_CORE")?;

        let ram = cell("PDPSC16K_MODE")?;
        let ram_clk_to_q = extreme(&mut ram.iopaths.iter().map(value));
        let ram_setup = ram
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();
        let ram_hold = ram
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_hold))
            .collect();
        let large_ram = cell("LRAM_CORE")?;
        let large_ram_clk_to_q = extreme(
            &mut large_ram
                .iopaths
                .iter()
                .filter(|p| p.from_pin == "CLK" && (p.to_pin == "DOA" || p.to_pin == "DOB"))
                .map(value),
        );
        let large_ram_setup = large_ram
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();
        let large_ram_hold = large_ram
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_hold))
            .collect();

        Ok(Self {
            pip_classes: classes
                .into_iter()
                .map(|(name, class)| {
                    let delay = match corner {
                        Corner::Slow => class.base.1,
                        Corner::Fast => class.base.0,
                    };
                    (name, delay)
                })
                .collect(),
            lut,
            wide_data,
            wide_select,
            clk_to_q,
            setup,
            ccu2,
            multiplier,
            ram_clk_to_q,
            ram_setup,
            ram_hold,
            large_ram_clk_to_q,
            large_ram_setup,
            large_ram_hold,
            hold,
        })
    }
}

impl TimingDb {
    /// A logic register's clock-to-output delay and data setup time, in
    /// picoseconds
    pub fn register_ps(&self) -> (i64, i64) {
        (self.clk_to_q, self.setup.get("M").copied().unwrap_or(0))
    }

    /// Delay through `cell` from `input` to `output`, in picoseconds, or
    /// `None` when that input doesn't reach that output
    pub fn cell_arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        if dsp::is_multiplier(cell) {
            return dsp::input_pin(cell, input).map(|_| self.multiplier);
        }
        if NexusTiming::is_dist_ram(cell) {
            return (9..=12).contains(&input).then_some(self.lut);
        }
        if NexusTiming::is_carry(cell) {
            let arc = (ccu2_input(input), ccu2_output(output).to_string());
            return self.ccu2.get(&arc).copied();
        }
        if NexusTiming::is_wide_lut(cell) {
            return Some(if input < WIDE_LUT_SELECT {
                self.wide_data
            } else {
                self.wide_select
            });
        }
        Some(self.lut)
    }
}

/// A Nexus device's delays for retiming before placement
/// ([`skalp_lir::retime`]): cell arcs from the timing database, and one
/// measured routing delay per connection, as for two cells a couple of
/// tiles apart, since nothing is placed yet
pub struct NexusRetimeDelays {
    db: TimingDb,
    connection_ps: i64,
}

/// Extra routing delay per load on a net, in picoseconds
const RETIME_PER_LOAD_PS: i64 = 15;

impl NexusRetimeDelays {
    /// Load the timing database and measure a typical connection on a small
    /// logic region of `variant`
    pub fn load(db_root: &Path, variant: NexusVariant) -> Result<Self> {
        let db = TimingDb::load(db_root, variant)?;
        let tiles = super::prjoxide_load::load_tilegrid(db_root, variant)
            .map_err(PlaceRouteError::BitstreamFailed)?;
        let clocks = super::prjoxide_graph::ClockRegions::load(
            db_root,
            variant.prjoxide_family(),
            variant.prjoxide_device(),
        )
        .map_err(PlaceRouteError::BitstreamFailed)?;
        let region = super::sites::design_region(
            &tiles,
            &clocks,
            &[],
            1000,
            super::sites::HardBlockCounts::default(),
        );
        let mut device = NexusDevice::from_prjoxide_region(
            variant,
            db_root,
            region.window,
            Some(region.logic_rows),
        )
        .map_err(PlaceRouteError::BitstreamFailed)?;
        device.set_pip_delays(&db);
        let connection_ps = crate::timing::RouteDelayTable::measure(&device)
            .map(|table| (table.delay(2, 1) * 1000.0).round() as i64)
            .ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(
                    "no logic tiles to measure routing delays on".to_string(),
                )
            })?;
        Ok(Self { db, connection_ps })
    }

    /// The routing delay charged per connection, in picoseconds
    pub fn connection_ps(&self) -> i64 {
        self.connection_ps
    }
}

impl skalp_lir::retime::RetimeDelays for NexusRetimeDelays {
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        self.db.cell_arc_ps(cell, input, output)
    }
    fn clk_to_q_ps(&self) -> i64 {
        self.db.register_ps().0
    }
    fn setup_ps(&self) -> i64 {
        self.db.register_ps().1
    }
    fn net_ps(&self, fanout: usize) -> i64 {
        self.connection_ps + RETIME_PER_LOAD_PS * fanout.saturating_sub(1) as i64
    }
}

/// The name and tile of a routing wire, split for pip classification.
impl TimingDb {
    /// Delay of a PIP from the wire named `source` to the one named
    /// `destination`, by its prjoxide PIP class (0 for a class with no
    /// timing data)
    pub fn pip_delay_ps(&self, source: &str, destination: &str) -> i64 {
        let (sx, sy, source) = wire_parts(source);
        let (dx, dy, destination) = wire_parts(destination);
        prjoxide::pip_classes::classify_pip(0, 0, &source, dx - sx, dy - sy, &destination)
            .and_then(|class| self.pip_classes.get(&class).copied())
            .unwrap_or(0)
    }
}

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

    fn is_wide_lut(cell: &Cell) -> bool {
        cell.function == Some(CellFunction::Lut5)
    }

    /// The table key of a block RAM pin: address buses have bus-wide
    /// entries.
    fn ram_table_key(pin: &str) -> String {
        let bus = pin.trim_end_matches(|c: char| c.is_ascii_digit());
        match (bus, pin[bus.len()..].parse::<usize>()) {
            ("ADA" | "ADB", Ok(bit)) if bit < 5 => format!("{bus}_4_0"),
            ("ADA" | "ADB", Ok(_)) => format!("{bus}_13_5"),
            _ => bus.to_string(),
        }
    }

    /// Setup time of a block RAM pin.
    fn ram_setup_ps(&self, pin: &str) -> i64 {
        let key = Self::ram_table_key(pin);
        self.db.ram_setup.get(&key).copied().unwrap_or(0)
    }

    /// Hold time of a block RAM pin.
    fn ram_hold_ps(&self, pin: &str) -> i64 {
        let key = Self::ram_table_key(pin);
        self.db.ram_hold.get(&key).copied().unwrap_or(0)
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
        self.db.pip_delay_ps(&src.name, &dst.name)
    }

    fn launch_ps(&self, cell: &Cell, _output: usize) -> Option<i64> {
        if is_pad(cell) || constant_value(cell).is_some() {
            return Some(0);
        }
        if Self::is_dist_ram(cell) {
            return None;
        }
        if lram::is_large_ram(cell) {
            return Some(self.db.large_ram_clk_to_q);
        }
        if cell.ram_ports().is_some() {
            return Some(self.db.ram_clk_to_q);
        }
        cell.clock.map(|_| self.db.clk_to_q)
    }

    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        self.db.cell_arc_ps(cell, input, output)
    }

    fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint> {
        let setup = |name: &str| self.db.setup.get(name).copied().unwrap_or(0);
        let hold = |name: &str| self.db.hold.get(name).copied().unwrap_or(0);
        if let Some(ports) = cell.ram_ports().filter(|_| lram::is_large_ram(cell)) {
            // The large RAM's table has one entry per bus
            let bus = |pin: &str| {
                pin.trim_end_matches(|c: char| c.is_ascii_digit())
                    .to_string()
            };
            return (0..cell.inputs.len())
                .filter_map(|input| {
                    let name = self.device.large_ram_input_pin(cell, &ports, input)?;
                    (name != "CLK").then(|| TimingEndpoint {
                        input,
                        pin: lram::router_pin(&name).unwrap_or(input),
                        setup_ps: self
                            .db
                            .large_ram_setup
                            .get(&bus(&name))
                            .copied()
                            .unwrap_or(0),
                        hold_ps: self
                            .db
                            .large_ram_hold
                            .get(&bus(&name))
                            .copied()
                            .unwrap_or(0),
                    })
                })
                .collect();
        }
        if let Some(ports) = cell.ram_ports() {
            return (0..cell.inputs.len())
                .filter_map(|input| {
                    let name = ebr::input_pin(&ports, input)?;
                    (name != "CLKA" && name != "CLKB").then(|| TimingEndpoint {
                        input,
                        pin: ebr::router_pin(&name).unwrap_or(input),
                        setup_ps: self.ram_setup_ps(&name),
                        hold_ps: self.ram_hold_ps(&name),
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
                    hold_ps: 0,
                })
                .collect();
        }
        (0..cell.inputs.len())
            .map(|input| {
                let (pin, name) = match (cell.cell_type.as_str(), input) {
                    (_, 0) => (0, "M"),
                    ("OXIDE_FF_EN" | "OXIDE_FF_RSTE", 1) => (FF_PIN_CE, "CE"),
                    _ => (FF_PIN_LSR, "LSR"),
                };
                // Data reaches the register through the bypass (M) or from
                // the slice's own LUT (DI); take the stricter of the two.
                let (setup_ps, hold_ps) = if name == "M" {
                    (setup("M").max(setup("DI")), hold("M").max(hold("DI")))
                } else {
                    (setup(name), hold(name))
                };
                TimingEndpoint {
                    input,
                    pin,
                    setup_ps,
                    hold_ps,
                }
            })
            .collect()
    }

    fn input_pins(&self, cell: &Cell, input: usize) -> Vec<usize> {
        if dsp::is_multiplier(cell) {
            return dsp::input_pin(cell, input)
                .and_then(|pin| dsp::router_pin(&pin))
                .into_iter()
                .collect();
        }
        if Self::is_dist_ram(cell) && (9..=12).contains(&input) {
            let base = DIST_RAM_PIN_RAD + 4 * (input - 9);
            return (0..4).map(|lut| base + lut).collect();
        }
        if Self::is_wide_lut(cell) {
            // Data inputs feed both LUTs; the last input is the mux select.
            return if input < WIDE_LUT_SELECT {
                vec![input, WIDE_LUT_SELECT + input]
            } else {
                vec![WIDE_LUT_PIN_SEL]
            };
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
        let timing = TimingDb::load(&db, NexusVariant::Lifcl40).unwrap();
        assert_eq!(timing.lut, 213);
        assert_eq!(timing.clk_to_q, 307);
        assert_eq!(timing.setup.get("M"), Some(&0));
    }

    /// A family with tables of its own uses them for its cells, and fills the
    /// pip classes they lack from CrossLink-NX's.
    #[test]
    fn a_family_with_its_own_tables_uses_them() {
        let Some(db) = super::super::prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tables = variant.timing_tables(&db, false);
        if tables.len() < 2 {
            eprintln!("no LFCPNX delay tables in this database — skipping");
            return;
        }
        let (own, grade) = &tables[0];
        let read = |dir: &Path, name: String| std::fs::read_to_string(dir.join(name)).unwrap();
        let cells: CellsFile =
            serde_json::from_str(&read(own, format!("cells_{grade}.json"))).unwrap();
        let lut = cells.celltypes["OXIDE_COMB:LUT4"]
            .iopaths
            .iter()
            .map(|p| p.maxv)
            .max()
            .unwrap();
        let timing = TimingDb::load(&db, variant).unwrap();
        assert_eq!(timing.lut, lut);

        let classes = |dir: &Path, grade: &str| -> InterconnectFile {
            serde_json::from_str(&read(dir, format!("interconnect_{grade}.json"))).unwrap()
        };
        let own_classes = classes(own, grade);
        let (shared, shared_grade) = &tables[1];
        let shared_classes = classes(shared, shared_grade);
        for (name, class) in &shared_classes.pip_classes {
            let expected = own_classes.pip_classes.get(name).unwrap_or(class).base.1;
            assert_eq!(timing.pip_classes.get(name), Some(&expected), "{name}");
        }
    }

    /// The device's PIPs carry the delays timing analysis charges them, so
    /// the router minimises what the timing report measures
    #[test]
    fn device_pips_get_their_class_delays() {
        let Some(db) = super::super::prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let timing = TimingDb::load(&db, variant).unwrap();
        let mut device = NexusDevice::from_prjoxide_bbox(variant, &db, (20, 20, 28, 28)).unwrap();
        device.set_pip_delays(&timing);
        let model = NexusTiming {
            device: &device,
            db: &timing,
        };
        let mut distinct = std::collections::BTreeSet::new();
        let pips: Vec<PipId> = (0..device.wire_count())
            .flat_map(|w| {
                device
                    .wire_src_pips(crate::device::WireId(w as u32))
                    .to_vec()
            })
            .collect();
        assert!(!pips.is_empty());
        for pip in pips {
            let delay = device.pip(pip).unwrap().delay;
            assert_eq!(delay as i64, model.interconnect_ps(pip).max(0), "{pip:?}");
            distinct.insert(delay);
        }
        // Neighbour, span-2, span-6 and in-tile PIPs differ
        assert!(distinct.len() > 5, "{distinct:?}");
    }
}
