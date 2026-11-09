//! Where a Nexus device's pins and hard blocks are.
//!
//! Locations come from the device database wherever it has them. Pin data
//! exists for CrossLink-NX; CertusPro-NX has none, so its pins are the ones
//! confirmed from the Versa board schematic and user guide (FPGA-EB-02053).

use super::{NexusVariant, PrjoxideTile};
use crate::device::PackagePins;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

/// Pin name that assigns a clock input to the on-chip oscillator.
pub const OSC_PIN: &str = "OSC";

/// Package pins of the LFCPNX-100 LFG672 with confirmed locations: the eight
/// user LEDs of the Versa board.
const LFCPNX_100_LFG672_PINS: [(&str, (u32, u32, u8)); 8] = [
    ("P8", (159, 26, 1)), // LED7, PR26B
    ("R4", (159, 28, 0)), // LED1, PR28A
    ("R5", (159, 28, 1)), // LED0, PR28B
    ("R6", (159, 29, 0)), // LED6, PR29A
    ("R7", (159, 29, 1)), // LED5, PR29B
    ("R8", (159, 31, 0)), // LED2, PR31A
    ("R9", (159, 31, 1)), // LED3, PR31B
    ("U8", (159, 33, 0)), // LED4, PR33A
];

#[derive(Debug, Deserialize)]
struct IoDatabase {
    #[serde(default)]
    packages: Vec<String>,
    #[serde(default)]
    pads: Vec<IoPad>,
}

#[derive(Debug, Deserialize)]
struct IoPad {
    offset: i64,
    pins: Vec<String>,
    /// PIO within the tile; negative for dedicated pins that have none
    pio: i32,
    side: String,
}

/// The pins of each package, plus the oscillator pseudo-pin.
pub fn package_pins(
    db_root: &Path,
    variant: NexusVariant,
    tiles: &[PrjoxideTile],
) -> Result<HashMap<String, PackagePins>, String> {
    let path = db_root
        .join(variant.prjoxide_family())
        .join(variant.prjoxide_device())
        .join("iodb.json");
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {}", path.display(), e))?;
    let io: IoDatabase =
        serde_json::from_str(&text).map_err(|e| format!("parsing {}: {}", path.display(), e))?;

    let (width, height) = variant.grid_size();
    let mut packages: HashMap<String, PackagePins> = HashMap::new();
    for (index, package) in io.packages.iter().enumerate() {
        let pins = io
            .pads
            .iter()
            .filter_map(|pad| {
                let pin = pad.pins.get(index)?;
                let offset = u32::try_from(pad.offset).ok()?;
                let pio = u8::try_from(pad.pio).ok()?;
                let location = match pad.side.as_str() {
                    "L" => (0, offset),
                    "R" => (width - 1, offset),
                    "T" => (offset, 0),
                    "B" => (offset, height - 1),
                    _ => return None,
                };
                (pin != "-").then(|| (pin.clone(), (location.0, location.1, pio)))
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

    if variant == NexusVariant::Lfcpnx100 && packages.is_empty() {
        packages.insert(
            "LFG672".to_string(),
            PackagePins {
                name: "LFG672".to_string(),
                pins: LFCPNX_100_LFG672_PINS
                    .iter()
                    .map(|(pin, location)| (pin.to_string(), *location))
                    .collect(),
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

/// Narrowest routing window, leaving logic room beside the clock path.
const MIN_REGION_COLUMNS: u32 = 40;

/// Grid window holding every site a design uses and its clock path.
///
/// The on-chip clock reaches the fabric through the TMID and CMUX tiles, so the
/// window always includes them; `sites` are the design's pad locations.
pub fn routing_region(tiles: &[PrjoxideTile], sites: &[(u32, u32)]) -> (u32, u32, u32, u32) {
    let clock_tiles = tiles
        .iter()
        .filter(|t| t.tiletype.starts_with("TMID") || t.tiletype.contains("_GSR"));
    let first_col = clock_tiles.clone().map(|t| t.x).min().unwrap_or(0);
    let last_row = clock_tiles.map(|t| t.y).max().unwrap_or(0);
    let x0 = sites
        .iter()
        .map(|s| s.0)
        .min()
        .unwrap_or(first_col)
        .min(first_col);
    let x1 = sites
        .iter()
        .map(|s| s.0)
        .max()
        .unwrap_or(first_col)
        .max(first_col + MIN_REGION_COLUMNS);
    let y1 = sites
        .iter()
        .map(|s| s.1)
        .max()
        .unwrap_or(last_row)
        .max(last_row)
        + 2;
    (x0, 0, x1, y1)
}

#[cfg(test)]
mod tests {
    use super::super::prjoxide_load::{find_database, load_tilegrid};
    use super::*;

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
        assert_eq!(pins[OSC_PIN], (137, 0, 0));
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
        assert_eq!(routing_region(&tiles, &[(159, 31)]), (73, 0, 159, 40));
    }
}
