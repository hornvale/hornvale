use crate::tile_kind::prelude_internal::*;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// The `TileKindTable` is a hashmap of tile kinds.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TileKindTable(pub HashMap<IVec2, TileKind>);

impl TileKindTable {
  /// Create a new `TileKindMatrix`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Get the tile kind at the given position.
  pub fn get_tile_kind(&self, x: i32, y: i32) -> Option<TileKind> {
    self.0.get(&IVec2::new(x, y)).copied()
  }
}
