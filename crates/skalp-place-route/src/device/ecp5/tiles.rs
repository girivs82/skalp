//! ECP5 tile implementation

use super::super::{Bel, BelType, Tile, TileType};

/// A tile in an ECP5 device
#[derive(Debug, Clone)]
pub struct Ecp5Tile {
    tile_type: TileType,
    x: u32,
    y: u32,
    bels: Vec<Bel>,
}

impl Ecp5Tile {
    pub fn new(tile_type: TileType, x: u32, y: u32, bels: Vec<Bel>) -> Self {
        Self {
            tile_type,
            x,
            y,
            bels,
        }
    }
}

impl Tile for Ecp5Tile {
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
