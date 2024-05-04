use crate::tile_kind::prelude_internal::*;
use serde::{Deserialize, Serialize};

/// The `TileKindMatrix` is a simple 2D matrix of tile kinds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TileKindMatrix {
  /// The width of the matrix.
  pub width: usize,
  /// The height of the matrix.
  pub height: usize,
  /// The tile kinds.
  pub kinds: Vec<TileKind>,
}

impl TileKindMatrix {
  /// Create a new `TileKindMatrix`.
  pub fn new(width: usize, height: usize, kinds: &[TileKind]) -> Self {
    let kinds = kinds.to_vec();
    Self { width, height, kinds }
  }

  /// Get the tile kind at the given position.
  pub fn get_tile_kind(&self, x: i32, y: i32) -> Option<TileKind> {
    if x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32 {
      Some(self.kinds[(y as usize * self.width) + x as usize])
    } else {
      None
    }
  }
}
