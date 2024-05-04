//! # Tile Kind
//!
//! This module contains the concept of a tile kind, which is used to draw the
//! tilemap view of the game.

use serde::{Deserialize, Serialize};

/// A data source for looking up tile kinds by coordinates.
pub mod data_source;
/// A matrix of tile kinds.
pub mod matrix;
/// The plugin for the tile kind.
pub mod plugin;
/// A table of tile kinds.
pub mod table;
/// A theme (tile kind => sprite) for the tilemap.
pub mod theme;

/// The kind of a tile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TileKind {
  /// A wall tile.
  Wall,
  /// A floor tile.
  Floor,
  /// A door tile.
  Door,
}

/// The prelude.
pub mod prelude {
  pub use super::data_source::*;
  pub use super::matrix::*;
  pub use super::plugin::*;
  pub use super::table::*;
  pub use super::theme::*;
  pub use super::TileKind;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
