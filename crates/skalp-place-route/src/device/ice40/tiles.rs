//! iCE40 tile implementation

use super::super::{Bel, BelType, Tile, TileType};
use serde::{Deserialize, Serialize};

/// iCE40 tile implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ice40Tile {
    /// Tile type
    tile_type: TileType,
    /// X coordinate
    x: u32,
    /// Y coordinate
    y: u32,
    /// BELs in this tile
    bels: Vec<Bel>,
}

impl Ice40Tile {
    /// Create a new iCE40 tile
    pub fn new(tile_type: TileType, x: u32, y: u32, bels: Vec<Bel>) -> Self {
        Self {
            tile_type,
            x,
            y,
            bels,
        }
    }

    /// Get tile name
    pub fn name(&self) -> String {
        match self.tile_type {
            TileType::Logic => format!("LOGIC_{}_{}", self.x, self.y),
            TileType::IoTop => format!("IO_TOP_{}_{}", self.x, self.y),
            TileType::IoBottom => format!("IO_BOT_{}_{}", self.x, self.y),
            TileType::IoLeft => format!("IO_LEFT_{}_{}", self.x, self.y),
            TileType::IoRight => format!("IO_RIGHT_{}_{}", self.x, self.y),
            TileType::RamTop => format!("RAM_TOP_{}_{}", self.x, self.y),
            TileType::RamBottom => format!("RAM_BOT_{}_{}", self.x, self.y),
            TileType::Dsp => format!("DSP_{}_{}", self.x, self.y),
            TileType::Pll => format!("PLL_{}_{}", self.x, self.y),
            TileType::GlobalBuf => format!("GBUF_{}_{}", self.x, self.y),
            TileType::IpCon => format!("IPCON_{}_{}", self.x, self.y),
            TileType::Empty => format!("EMPTY_{}_{}", self.x, self.y),
        }
    }

    /// Check if this is a logic tile
    pub fn is_logic(&self) -> bool {
        self.tile_type == TileType::Logic
    }

    /// Check if this is an I/O tile
    pub fn is_io(&self) -> bool {
        matches!(
            self.tile_type,
            TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight
        )
    }

    /// Check if this is a RAM tile
    pub fn is_ram(&self) -> bool {
        matches!(self.tile_type, TileType::RamTop | TileType::RamBottom)
    }

    /// Get the number of LUTs in this tile
    pub fn lut_count(&self) -> usize {
        self.bels
            .iter()
            .filter(|b| matches!(b.bel_type, BelType::Lut4 | BelType::Lut6))
            .count()
    }

    /// Get the number of FFs in this tile
    pub fn ff_count(&self) -> usize {
        self.bels
            .iter()
            .filter(|b| {
                matches!(
                    b.bel_type,
                    BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE
                )
            })
            .count()
    }

    /// Get available BELs of a specific type that are not yet used
    pub fn get_bels_of_type(&self, bel_type: BelType) -> Vec<&Bel> {
        self.bels
            .iter()
            .filter(|b| b.bel_type == bel_type)
            .collect()
    }
}

impl Tile for Ice40Tile {
    fn tile_type(&self) -> TileType {
        self.tile_type
    }

    fn coords(&self) -> (u32, u32) {
        (self.x, self.y)
    }

    fn bels(&self) -> &[Bel] {
        &self.bels
    }

    fn bel(&self, idx: usize) -> Option<&Bel> {
        self.bels.get(idx)
    }

    fn available_bels(&self, bel_type: BelType) -> usize {
        self.bels.iter().filter(|b| b.bel_type == bel_type).count()
    }
}
