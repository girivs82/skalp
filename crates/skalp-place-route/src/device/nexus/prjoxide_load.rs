//! Ingestion of the real Project Oxide (prjoxide) silicon database for Lattice Nexus.
//!
//! This replaces the *synthetic* (procedurally fabricated) Nexus floorplan with the
//! actual fuzzed device database, so that placement, routing, and — critically —
//! bitstream bit positions correspond to real silicon.
//!
//! Data sources (prjoxide repo, ISC-licensed):
//!   - `database/<FAMILY>/<DEVICE>/tilegrid.json` — every physical tile: its type,
//!     grid (x,y), and the *bit region* it occupies in the global bitstream
//!     (`start_frame`, `frames`, `start_bit`, `bits`).
//!   - `database/<FAMILY>/tiletypes/<TYPE>.ron` — per tile-type bit database
//!     (pips / words / enums / conns). Consumed by M1b (routing graph) and
//!     M2 (bitstream), not here.
//!
//! M1a (this file): load `tilegrid.json` into a typed, validated tile list and map
//! prjoxide tiletypes onto SKALP's `TileType`. Verified against the resource counts
//! in [`super::data`].
//!
//! Copyright (C) 2020-21 gatecat <gatecat@ds0.me> (database) — ISC.

use super::super::TileType;
use super::NexusVariant;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// One tile's bit region within the global bitstream, plus its position/type.
///
/// `frames` × `bits` is the rectangle of configuration bits owned by this tile;
/// its global coordinate is `(start_frame + local_frame, start_bit + local_bit)`.
/// This is the join key between the routing graph (M1b) and the packed bits (M2).
#[derive(Debug, Clone)]
pub struct PrjoxideTile {
    /// Full prjoxide tile name, e.g. `"R10C20:PLC"`. Unique per physical tile.
    pub name: String,
    /// prjoxide tile-type, e.g. `"PLC"`, `"SYSIO_B0_0"`, `"EBR_1"`. Indexes the `.ron` DB.
    pub tiletype: String,
    /// Grid column.
    pub x: u32,
    /// Grid row.
    pub y: u32,
    /// First bitstream frame this tile occupies.
    pub start_frame: u32,
    /// Number of frames spanned.
    pub frames: u32,
    /// First bit (within a frame) this tile occupies.
    pub start_bit: u32,
    /// Number of bits per frame spanned.
    pub bits: u32,
    /// Mapped SKALP tile class for placement.
    pub skalp_type: TileType,
}

// --- serde shapes for tilegrid.json -----------------------------------------

#[derive(Debug, Deserialize)]
struct TileGridFile {
    tiles: HashMap<String, RawTile>,
}

#[derive(Debug, Deserialize)]
struct RawTile {
    tiletype: String,
    x: u32,
    y: u32,
    start_frame: u32,
    frames: u32,
    start_bit: u32,
    bits: u32,
}

/// Map a prjoxide tiletype string to SKALP's coarse `TileType` for placement.
///
/// Many prjoxide tiletypes carry a positional suffix (`SYSIO_B0_0`, `EBR_1`,
/// `DSP_R...`); we classify on the leading family token. Routing/interconnect
/// tiles (CIB/MIB/TAP) and the clock network carry wires+pips but no placeable
/// BELs, so they map to `Empty`/`GlobalBuf` — they still contribute to the
/// routing graph (M1b) and own bit regions (captured regardless of class).
///
/// `(x, y)` and the grid extents pick the IO side for `SYSIO` tiles.
pub fn tiletype_to_skalp(tiletype: &str, x: u32, y: u32, _max_x: u32, max_y: u32) -> TileType {
    let fam = tiletype.split('_').next().unwrap_or(tiletype);
    match fam {
        "PLC" => TileType::Logic,
        "EBR" | "LRAM" => TileType::RamTop,
        "DSP" | "ALU" => TileType::Dsp,
        "GPLL" => TileType::Pll,
        "SYSIO" => {
            // Nexus SYSIO banks sit on the periphery; pick side by position.
            if y == 0 {
                TileType::IoBottom
            } else if y >= max_y - 1 {
                TileType::IoTop
            } else if x == 0 {
                TileType::IoLeft
            } else {
                TileType::IoRight
            }
        }
        // Clock distribution network: spines, central muxes, edge/perimeter clocks.
        "CMUX" | "SPINE" | "ECLK" | "PCLK" | "BMID" | "TMID" | "LMID" | "RMID" | "DOSCL"
        | "HROW" => TileType::GlobalBuf,
        // Hard IP and analog/config blocks.
        "PCS" | "SERDES" | "CDR0" | "CDR1" | "PCIE" | "EFB" | "DDR" | "DDR40" | "ADC" | "I2C"
        | "PMU" | "POR" | "IREF" | "DOSC" | "BANKREF0" | "BANKREF1" | "BANKREF2" | "BANKREF3"
        | "BANKREF4" | "BANKREF5" | "BANKREF6" | "BANKREF7" | "BANKREF" | "RBB" | "DLY30"
        | "DLY32" | "DLY40" | "DLY50" | "DLY52" => TileType::IpCon,
        // CIB / MIB (general routing fabric) and TAP (clock taps): routing-only.
        _ => TileType::Empty,
    }
}

/// Locate the prjoxide database root.
///
/// Order: `$PRJOXIDE_DB` env var, then a sibling `prjoxide/database` checkout
/// relative to the SKALP repo (dev convenience). Returns the directory that
/// directly contains the `<FAMILY>/` subdirectories.
pub fn find_database() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("PRJOXIDE_DB") {
        let pb = PathBuf::from(p);
        if pb.is_dir() {
            return Some(pb);
        }
    }
    // Dev fallback: ~/src/prjoxide/database (the clone made during bring-up).
    if let Some(home) = std::env::var_os("HOME") {
        let pb = Path::new(&home).join("src/prjoxide/database");
        if pb.is_dir() {
            return Some(pb);
        }
    }
    None
}

impl NexusVariant {
    /// prjoxide family directory name (`LIFCL` / `LFCPNX`).
    pub fn prjoxide_family(&self) -> &'static str {
        match self {
            NexusVariant::Lifcl40 => "LIFCL",
            NexusVariant::Lfcpnx100 => "LFCPNX",
        }
    }

    /// prjoxide device directory name (`LIFCL-40` / `LFCPNX-100`).
    pub fn prjoxide_device(&self) -> &'static str {
        self.name()
    }
}

/// Load and classify every tile from `tilegrid.json` for `variant`.
///
/// `db_root` is the directory containing the `<FAMILY>/` subdirs (see
/// [`find_database`]). Returns the full physical tile list (multiple tiles may
/// share a grid cell — PLC + overlapping routing tiles), each with its bit region.
pub fn load_tilegrid(db_root: &Path, variant: NexusVariant) -> Result<Vec<PrjoxideTile>, String> {
    let path = db_root
        .join(variant.prjoxide_family())
        .join(variant.prjoxide_device())
        .join("tilegrid.json");
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {}", path.display(), e))?;
    let grid: TileGridFile =
        serde_json::from_str(&text).map_err(|e| format!("parsing {}: {}", path.display(), e))?;

    let (gw, gh) = variant.grid_size();
    let mut tiles = Vec::with_capacity(grid.tiles.len());
    for (name, raw) in grid.tiles {
        let skalp_type = tiletype_to_skalp(&raw.tiletype, raw.x, raw.y, gw, gh);
        tiles.push(PrjoxideTile {
            name,
            tiletype: raw.tiletype,
            x: raw.x,
            y: raw.y,
            start_frame: raw.start_frame,
            frames: raw.frames,
            start_bit: raw.start_bit,
            bits: raw.bits,
            skalp_type,
        });
    }
    // Deterministic order (HashMap iteration is not): by (y, x, name).
    tiles.sort_by(|a, b| (a.y, a.x, &a.name).cmp(&(b.y, b.x, &b.name)));
    Ok(tiles)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// M1a acceptance: the real database reproduces the resource counts that
    /// `data.rs` hand-copied from prjoxide. Skipped if the DB isn't present.
    #[test]
    fn tilegrid_reproduces_lfcpnx100_resource_counts() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping (set PRJOXIDE_DB or clone prjoxide)");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).expect("load tilegrid");
        let die = variant.die_data();

        let count_fam = |fam: &str| {
            tiles
                .iter()
                .filter(|t| t.tiletype.split('_').next() == Some(fam))
                .count()
        };

        // Direct tile families map 1:1 to data.rs constants.
        assert_eq!(count_fam("PLC"), die.plc_tiles, "PLC tile count");
        assert_eq!(count_fam("EBR"), die.ebr_blocks * 8, "EBR tiles (8/block)");
        assert_eq!(count_fam("LRAM"), die.lram_blocks, "LRAM tiles");
        assert_eq!(count_fam("GPLL"), die.plls as usize, "GPLL tiles");
        assert_eq!(count_fam("PCS"), die.pcs_channels as usize, "PCS channels");

        // Grid bounds: no tile exceeds the declared extents.
        let (gw, gh) = variant.grid_size();
        assert!(
            tiles.iter().all(|t| t.x < gw && t.y < gh),
            "tiles in bounds"
        );

        // Multi-tile-per-cell is real (more tiles than grid cells).
        assert!(
            tiles.len() > (gw * gh) as usize,
            "expected overlapping tiles per cell: {} tiles vs {} cells",
            tiles.len(),
            gw * gh
        );

        // Every tile owns a non-empty bit region (join key for M2).
        assert!(
            tiles.iter().all(|t| t.frames > 0 && t.bits > 0),
            "all tiles have a bit region"
        );

        eprintln!(
            "M1a OK: {} physical tiles, {} PLC, {} EBR, {} DSP, {} SYSIO",
            tiles.len(),
            count_fam("PLC"),
            count_fam("EBR"),
            count_fam("DSP"),
            count_fam("SYSIO"),
        );
    }

    #[test]
    fn tiletype_mapping_is_total_and_sane() {
        assert_eq!(tiletype_to_skalp("PLC", 5, 5, 160, 75), TileType::Logic);
        assert_eq!(tiletype_to_skalp("EBR_1", 5, 5, 160, 75), TileType::RamTop);
        assert_eq!(tiletype_to_skalp("DSP_R0", 5, 5, 160, 75), TileType::Dsp);
        assert_eq!(tiletype_to_skalp("GPLL_ULC", 5, 5, 160, 75), TileType::Pll);
        assert_eq!(
            tiletype_to_skalp("SYSIO_B0_0", 5, 0, 160, 75),
            TileType::IoBottom
        );
        assert_eq!(tiletype_to_skalp("PCIE_X1", 5, 5, 160, 75), TileType::IpCon);
        assert_eq!(tiletype_to_skalp("CIB", 5, 5, 160, 75), TileType::Empty);
    }
}
