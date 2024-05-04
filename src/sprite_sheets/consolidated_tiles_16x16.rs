use bevy::prelude::*;

/// Consolidated Tiles (16x16)
#[derive(Debug, Resource)]
pub struct ConsolidatedTiles16x16(pub Handle<TextureAtlasLayout>);

impl FromWorld for ConsolidatedTiles16x16 {
  fn from_world(world: &mut World) -> Self {
    let texture_atlas = TextureAtlasLayout::from_grid(Vec2::new(16.0, 16.0), 16, 16, None, None);
    let mut texture_atlases = world.get_resource_mut::<Assets<TextureAtlasLayout>>().unwrap();
    let texture_atlas_handle = texture_atlases.add(texture_atlas);
    Self(texture_atlas_handle)
  }
}
