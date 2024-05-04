use crate::tile_kind::prelude_internal::*;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};

/// Any data source for looking up tile kinds by coordinates.
#[derive(Debug, Clone, Resource, Serialize, Deserialize)]
pub enum TileKindDataSource {
  /// A matrix of tile kinds.
  Matrix(TileKindMatrix),
  /// A table of tile kinds.
  Table(TileKindTable),
  /// Modulo the tile kind, really dumb algorithm.
  Modulo,
}

impl TileKindDataSource {
  /// Get the tile kind at the given position.
  pub fn get_tile_kind(&self, x: i32, y: i32) -> Option<TileKind> {
    match self {
      Self::Matrix(matrix) => matrix.get_tile_kind(x, y),
      Self::Table(table) => table.get_tile_kind(x, y),
      Self::Modulo => {
        let kind = match (x % 2, y % 2) {
          (0, 0) => TileKind::Wall,
          (1, 0) => TileKind::Floor,
          (0, 1) => TileKind::Floor,
          (1, 1) => TileKind::Wall,
          _ => TileKind::Wall,
        };
        Some(kind)
      },
    }
  }
}
