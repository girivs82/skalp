//! Nexus tile implementation

use super::super::{Bel, BelType, Tile, TileType};

/// A tile in a Nexus (CertusPro-NX) device
#[derive(Debug, Clone)]
pub struct NexusTile {
    /// Tile type
    tile_type: TileType,
    /// X coordinate
    x: u32,
    /// Y coordinate
    y: u32,
    /// BELs in this tile
    bels: Vec<Bel>,
}

impl NexusTile {
    /// Create a new Nexus tile
    pub fn new(tile_type: TileType, x: u32, y: u32, bels: Vec<Bel>) -> Self {
        Self {
            tile_type,
            x,
            y,
            bels,
        }
    }
}

impl Tile for NexusTile {
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
