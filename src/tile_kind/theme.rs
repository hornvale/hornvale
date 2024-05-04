use crate::tile_kind::prelude_internal::*;
use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use std::collections::HashMap;

/// A theme (tile kind => sprite) for the tilemap.
#[derive(Debug, Clone, Resource)]
pub struct TileKindTheme {
  /// The sprite image handle.
  pub sprite: Handle<Image>,
  /// A map of tile kinds to sprite coordinates.
  pub map: HashMap<TileKind, TileTextureIndex>,
}

impl TileKindTheme {
  /// Get the tile texture index for the given tile kind.
  pub fn get_texture_index(&self, kind: TileKind) -> Option<TileTextureIndex> {
    self.map.get(&kind).copied()
  }
}
