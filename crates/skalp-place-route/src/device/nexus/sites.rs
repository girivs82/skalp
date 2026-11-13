//! Where a Nexus device's pins and hard blocks are.
//!
//! Locations come from the device database wherever it has them. prjoxide has
//! pin data for CrossLink-NX; CertusPro-NX's comes from Lattice's own pinout.

use super::prjoxide_graph::ClockRegions;
use super::{NexusVariant, PrjoxideTile};
use crate::collections::{HashMap, HashMapExt};
use crate::device::PackagePins;
use serde::Deserialize;
use std::path::Path;

/// Pin name that assigns a clock input to the on-chip oscillator.
pub const OSC_PIN: &str = "OSC";

/// IO data generated from Lattice's pinout and BSDL, for devices prjoxide has
/// none for (see scripts/gen_nexus_iodb.py).
const LFCPNX_100_IODB: &str = include_str!("../../../data/lfcpnx-100-iodb.json");

#[derive(Debug, Deserialize)]
struct IoDatabase {
    #[serde(default)]
    packages: Vec<String>,
    #[serde(default)]
    pads: Vec<IoPad>,
    #[serde(default)]
    boundary: Vec<BoundaryCell>,
}

#[derive(Debug, Deserialize)]
struct BoundaryCell {
    bit: usize,
    pad: String,
}

#[derive(Debug, Deserialize)]
struct IoPad {
    offset: i64,
    #[serde(default)]
    name: Option<String>,
    pins: Vec<String>,
    /// PIO within the tile; negative for dedicated pins that have none
    pio: i32,
    side: String,
    #[serde(default)]
    bank: i32,
    /// Special functions the pad can take, such as `VREF3_1`
    #[serde(default)]
    func: Vec<String>,
}

impl IoPad {
    /// The pad's site, `(tile_x, tile_y, pio)`, on a `width` x `height` grid.
    fn site(&self, width: u32, height: u32) -> Option<(u32, u32, u8)> {
        let offset = u32::try_from(self.offset).ok()?;
        let pio = u8::try_from(self.pio).ok()?;
        let (x, y) = match self.side.as_str() {
            "L" => (0, offset),
            "R" => (width - 1, offset),
            "T" => (offset, 0),
            "B" => (offset, height - 1),
            _ => return None,
        };
        Some((x, y, pio))
    }
}

/// A pad that drives a bank's input reference voltage.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VrefDriver {
    /// The pad's name, such as `PB110A`, or its site when the data has none
    pub pad: String,
    /// `(tile_x, tile_y, pio)`
    pub site: (u32, u32, u8),
}

/// Whether an IO tile belongs to a high-performance (1.8 V and below) bank.
pub fn is_high_performance_io(tiletype: &str) -> bool {
    matches!(tiletype.as_bytes().get(7), Some(b'3'..=b'5')) && tiletype.starts_with("SYSIO_B")
}

/// The pins of each package, plus the oscillator pseudo-pin.
pub fn package_pins(
    db_root: &Path,
    variant: NexusVariant,
    tiles: &[PrjoxideTile],
) -> Result<HashMap<String, PackagePins>, String> {
    let io = load_io_database(db_root, variant)?;
    let (width, height) = variant.grid_size();
    let mut packages: HashMap<String, PackagePins> = HashMap::new();
    for (index, package) in io.packages.iter().enumerate() {
        let pins = io
            .pads
            .iter()
            .filter_map(|pad| {
                let pin = pad.pins.get(index)?;
                let site = pad.site(width, height)?;
                (pin != "-").then(|| (pin.clone(), site))
            })
            .collect();
        packages.insert(
            package.clone(),
            PackagePins {
                name: package.clone(),
                pins,
            },
        );
    }

    if let Some(osc) = tiles.iter().find(|t| t.tiletype == "EFB_1_OSC") {
        for package in packages.values_mut() {
            package.pins.insert(OSC_PIN.to_string(), (osc.x, osc.y, 0));
        }
    }
    Ok(packages)
}

/// The device's IO data: prjoxide's, or the bundled data for devices it has
/// none for.
fn load_io_database(db_root: &Path, variant: NexusVariant) -> Result<IoDatabase, String> {
    let path = db_root
        .join(variant.prjoxide_family())
        .join(variant.prjoxide_device())
        .join("iodb.json");
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {}", path.display(), e))?;
    let io: IoDatabase =
        serde_json::from_str(&text).map_err(|e| format!("parsing {}: {}", path.display(), e))?;
    if io.pads.is_empty() && variant == NexusVariant::Lfcpnx100 {
        return serde_json::from_str(LFCPNX_100_IODB)
            .map_err(|e| format!("parsing the bundled LFCPNX-100 pins: {e}"));
    }
    Ok(io)
}

/// The pad that drives each bank's first reference voltage (VREF1), by bank.
///
/// Inputs of the SSTL, HSTL and HSUL standards compare against it; the pad
/// carries the `VREF{bank}_1` function in the device's IO data.
pub fn vref_drivers(
    db_root: &Path,
    variant: NexusVariant,
) -> Result<HashMap<u32, VrefDriver>, String> {
    let io = load_io_database(db_root, variant)?;
    let (width, height) = variant.grid_size();
    let mut drivers = HashMap::new();
    for pad in &io.pads {
        let Ok(bank) = u32::try_from(pad.bank) else {
            continue;
        };
        if !pad.func.iter().any(|f| *f == format!("VREF{bank}_1")) {
            continue;
        }
        let Some(site) = pad.site(width, height) else {
            continue;
        };
        let name = pad
            .name
            .clone()
            .unwrap_or_else(|| format!("({}, {}) PIO {}", site.0, site.1, site.2));
        drivers.insert(bank, VrefDriver { pad: name, site });
    }
    Ok(drivers)
}

/// The balls of `package` whose pads the build drives to give a design's
/// reference-voltage inputs their bank's VREF1, each with its bank. A
/// board must allow these to be driven, as it must the design's outputs.
pub fn reference_voltage_pins(
    db_root: &Path,
    variant: NexusVariant,
    package: &str,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
) -> Result<Vec<(String, u32)>, String> {
    use skalp_lir::tech_library::CellFunction;

    let io = load_io_database(db_root, variant)?;
    let Some(index) = io.packages.iter().position(|p| p == package) else {
        return Ok(Vec::new());
    };
    let ball_of = |pad: &IoPad| pad.pins.get(index).filter(|b| *b != "-").cloned();
    let mut banks: Vec<u32> = netlist
        .cells
        .iter()
        .filter(|c| {
            matches!(
                c.function,
                Some(CellFunction::InputPad | CellFunction::ClockPad)
            )
        })
        .filter(|c| {
            c.parameters
                .get("IO_STANDARD")
                .is_some_and(|s| super::emit::vref_level(s).is_some())
        })
        .filter_map(|c| c.parameters.get("LOC"))
        .filter_map(|loc| {
            io.pads
                .iter()
                .find(|pad| ball_of(pad).as_deref() == Some(loc.as_str()))
        })
        .filter_map(|pad| u32::try_from(pad.bank).ok())
        .collect();
    banks.sort_unstable();
    banks.dedup();
    let mut pins = Vec::new();
    for bank in banks {
        let driver = io
            .pads
            .iter()
            .find(|pad| pad.func.iter().any(|f| *f == format!("VREF{bank}_1")));
        if let Some(ball) = driver.and_then(ball_of) {
            pins.push((ball, bank));
        }
    }
    Ok(pins)
}

/// Narrowest routing window, leaving logic room beside the clock path.
const MIN_REGION_COLUMNS: u32 = 40;

/// The part of a device a design is built on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DesignRegion {
    /// Grid window loaded for routing: `(x0, y0, x1, y1)`
    pub window: (u32, u32, u32, u32),
    /// Rows logic may be placed in
    pub logic_rows: (u32, u32),
}

/// LUTs in a logic tile.
const LUTS_PER_TILE: usize = 8;
/// Share of a region's LUTs a design should fill, leaving room to route.
const MAX_REGION_FILL: f64 = 0.6;

/// The region holding a design's pads, its logic, and its clock path.
///
/// The on-chip clock reaches the fabric through the TMID and CMUX tiles, so the
/// window always includes them. Logic stays within one clock spine's rows when
/// it fits, keeping the loaded part of the device small; a larger design gets
/// every spine and then the full width of the chip.
pub fn design_region(
    tiles: &[PrjoxideTile],
    clocks: &ClockRegions,
    sites: &[(u32, u32)],
    lut_count: usize,
    ram_count: usize,
    dsp_count: usize,
) -> DesignRegion {
    let clock_tiles = tiles
        .iter()
        .filter(|t| t.tiletype.starts_with("TMID") || t.tiletype.contains("_GSR"));
    let first_col = clock_tiles.clone().map(|t| t.x).min().unwrap_or(0);
    let last_clock_row = clock_tiles.map(|t| t.y).max().unwrap_or(0);
    let last_row = tiles.iter().map(|t| t.y).max().unwrap_or(0);
    let last_col = tiles.iter().map(|t| t.x).max().unwrap_or(0);

    let one_spine = sites
        .iter()
        .find_map(|&(_, y)| clocks.spine_rows(y))
        .or_else(|| clocks.first_spine_rows())
        .unwrap_or((0, last_clock_row));
    let all_spines = clocks.all_spine_rows().unwrap_or((0, last_row));

    let site_x0 = sites.iter().map(|s| s.0).min().unwrap_or(first_col);
    let site_x1 = sites.iter().map(|s| s.0).max().unwrap_or(first_col);
    let site_y1 = sites.iter().map(|s| s.1).max().unwrap_or(0);

    let build = |rows: (u32, u32), x0: u32, x1: u32| DesignRegion {
        window: (
            x0.min(site_x0),
            0,
            x1.max(site_x1),
            (rows.1.max(site_y1).max(last_clock_row) + 2).min(last_row),
        ),
        logic_rows: rows,
    };
    let fits = |region: &DesignRegion| {
        let (x0, _, x1, _) = region.window;
        let (from, to) = region.logic_rows;
        let inside = |t: &&PrjoxideTile| t.x >= x0 && t.x <= x1 && t.y >= from && t.y <= to;
        // Logic is kept off the window's inner edges (see
        // `NexusDevice::from_prjoxide_region`)
        let off_edge = |t: &&PrjoxideTile| (t.x > x0 || x0 == 0) && (t.x < x1 || x1 >= last_col);
        let capacity = tiles
            .iter()
            .filter(inside)
            .filter(off_edge)
            .filter(|t| t.tiletype == "PLC")
            .count()
            * LUTS_PER_TILE;
        let ram_sites = tiles
            .iter()
            .filter(inside)
            .filter(|t| super::ebr::site_of_tile(&t.tiletype).is_some())
            .count();
        let dsp_sites = tiles
            .iter()
            .filter(inside)
            .filter(|t| super::dsp::slot_of_tile(&t.tiletype).is_some())
            .count();
        lut_count as f64 <= capacity as f64 * MAX_REGION_FILL
            && ram_count <= ram_sites
            && dsp_count <= dsp_sites
    };

    let right_edge = (first_col + MIN_REGION_COLUMNS).max(site_x1);
    [
        build(one_spine, first_col, right_edge),
        build(one_spine, first_col, last_col),
        build(all_spines, first_col, last_col),
    ]
    .into_iter()
    .find(|region| fits(region))
    .unwrap_or_else(|| build(all_spines, 0, last_col))
}

/// Name of the pad a boundary-scan bit reads, with its ball on `package`.
/// The boundary-scan bit of each package ball that has one.
pub fn boundary_bits_by_ball(package: &str) -> HashMap<String, usize> {
    let Ok(io) = serde_json::from_str::<IoDatabase>(LFCPNX_100_IODB) else {
        return HashMap::new();
    };
    let Some(index) = io.packages.iter().position(|p| p == package) else {
        return HashMap::new();
    };
    io.boundary
        .iter()
        .filter_map(|cell| {
            let pad = io
                .pads
                .iter()
                .find(|p| p.name.as_deref() == Some(cell.pad.as_str()))?;
            let ball = pad.pins.get(index).filter(|pin| pin.as_str() != "-")?;
            Some((ball.clone(), cell.bit))
        })
        .collect()
}

pub fn boundary_pad_names(package: &str) -> HashMap<usize, String> {
    let Ok(io) = serde_json::from_str::<IoDatabase>(LFCPNX_100_IODB) else {
        return HashMap::new();
    };
    let Some(index) = io.packages.iter().position(|p| p == package) else {
        return HashMap::new();
    };
    let ball = |pad: &str| {
        io.pads
            .iter()
            .find(|p| p.name.as_deref() == Some(pad))
            .and_then(|p| p.pins.get(index))
            .filter(|pin| pin.as_str() != "-")
            .cloned()
    };
    io.boundary
        .iter()
        .map(|cell| {
            let label = match ball(&cell.pad) {
                Some(pin) => format!("{} (ball {pin})", cell.pad),
                None => cell.pad.clone(),
            };
            (cell.bit, label)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::super::prjoxide_load::{find_database, load_tilegrid};
    use super::*;

    #[test]
    fn large_designs_get_more_of_the_chip() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let tiles = load_tilegrid(&db, NexusVariant::Lfcpnx100).unwrap();
        let clocks = ClockRegions::load(&db, "LFCPNX", "LFCPNX-100").unwrap();
        let small = design_region(&tiles, &clocks, &[(159, 31)], 100, 0, 0);
        let large = design_region(&tiles, &clocks, &[(159, 31)], 45_000, 0, 0);
        assert_eq!(small.logic_rows, (1, 37));
        assert_eq!(large.logic_rows, (1, 73));
        assert!(large.window.3 > small.window.3);
    }

    #[test]
    fn lfcpnx_pins_and_oscillator_are_known() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).unwrap();
        let packages = package_pins(&db, variant, &tiles).unwrap();
        let pins = &packages["LFG672"].pins;
        assert_eq!(pins["R8"], (159, 31, 0));
        assert_eq!(pins["M4"], (159, 13, 1), "SEG_A, pad PR13B");
        assert_eq!(pins["AC21"], (46, 74, 0), "K_DIG1, pad PB46A");
        assert_eq!(pins["P24"], (0, 42, 0), "the 125 MHz clock, pad PL42A");
        assert_eq!(boundary_pad_names("LFG672")[&533], "PR31A (ball R8)");
        assert_eq!(pins[OSC_PIN], (137, 0, 0));
    }

    #[test]
    fn each_bank_has_its_reference_voltage_pad() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let drivers = vref_drivers(&db, variant).unwrap();
        let tiles = load_tilegrid(&db, variant).unwrap();
        let pins = &package_pins(&db, variant, &tiles).unwrap()["LFG672"].pins;
        // Lattice's pinout: VREF3_1 on AA12, VREF4_1 on AE22, VREF5_1 on V24
        for (bank, pad, ball) in [
            (3, "PB110A", "AA12"),
            (4, "PB50A", "AE22"),
            (5, "PB2A", "V24"),
        ] {
            assert_eq!(drivers[&bank].pad, pad);
            assert_eq!(drivers[&bank].site, pins[ball], "bank {bank}");
        }
        // CrossLink-NX's come from prjoxide's own data
        let lifcl = vref_drivers(&db, NexusVariant::Lifcl40).unwrap();
        assert!([3, 4, 5].iter().all(|bank| lifcl.contains_key(bank)));
    }

    #[test]
    fn reference_voltage_inputs_name_their_banks_driver_ball() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let mut netlist = skalp_lir::gate_netlist::GateNetlist::new("top".into(), "nexus".into());
        let pad = |loc: &str, standard: &str| {
            let mut cell = skalp_lir::gate_netlist::Cell::new_comb(
                skalp_lir::gate_netlist::CellId(0),
                "OXIDE_IB".into(),
                "nexus".into(),
                0.0,
                format!("top.{loc}"),
                vec![],
                vec![],
            );
            cell.function = Some(skalp_lir::tech_library::CellFunction::InputPad);
            cell.parameters.insert("LOC".into(), loc.into());
            cell.parameters
                .insert("IO_STANDARD".into(), standard.into());
            cell
        };
        // Two SSTL15 inputs in bank 4, one LVCMOS18H input in bank 5
        netlist.add_cell(pad("AF14", "SSTL15_I"));
        netlist.add_cell(pad("AF13", "SSTL15_I"));
        netlist.add_cell(pad("AD23", "LVCMOS18H"));
        let pins =
            reference_voltage_pins(&db, NexusVariant::Lfcpnx100, "LFG672", &netlist).unwrap();
        assert_eq!(pins, vec![("AE22".to_string(), 4)]);
    }

    #[test]
    fn lifcl_pins_come_from_the_database() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lifcl40;
        let tiles = load_tilegrid(&db, variant).unwrap();
        let packages = package_pins(&db, variant, &tiles).unwrap();
        assert!(packages.len() > 1);
        assert!(packages["CABGA400"].pins.len() > 100);
        assert!(packages["CABGA400"].pins.contains_key(OSC_PIN));
    }

    #[test]
    fn region_covers_the_clock_path_and_pads() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let tiles = load_tilegrid(&db, NexusVariant::Lfcpnx100).unwrap();
        let clocks = ClockRegions::load(&db, "LFCPNX", "LFCPNX-100").unwrap();
        let region = design_region(&tiles, &clocks, &[(159, 31)], 100, 0, 0);
        assert_eq!(region.window, (73, 0, 159, 40));
        assert_eq!(region.logic_rows, (1, 37));
    }
}
