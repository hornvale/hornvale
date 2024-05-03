use bevy::prelude::*;

/// Hexany Roguelike Tiles (16x16)
#[derive(Debug, Resource)]
pub struct Kenney1bit16x16(pub Handle<TextureAtlasLayout>);

impl FromWorld for Kenney1bit16x16 {
  fn from_world(world: &mut World) -> Self {
    let texture_atlas = TextureAtlasLayout::from_grid(Vec2::new(16.0, 16.0), 49, 22, None, None);
    let mut texture_atlases = world.get_resource_mut::<Assets<TextureAtlasLayout>>().unwrap();
    let texture_atlas_handle = texture_atlases.add(texture_atlas);
    Self(texture_atlas_handle)
  }
}
