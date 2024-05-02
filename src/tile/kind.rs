use bevy::prelude::*;

/// The kind of a tile.
#[derive(Debug, Clone, Copy, Component)]
pub enum TileKind {
  /// A wall tile.
  Wall,
  /// A floor tile.
  Floor,
  /// A door tile.
  Door,
}
