use bevy::prelude::*;

/// Hexany Monster Menagerie (32x32)
#[derive(Debug, Resource)]
pub struct HexanyMm32x32(pub Handle<TextureAtlasLayout>);

impl FromWorld for HexanyMm32x32 {
  fn from_world(world: &mut World) -> Self {
    let texture_atlas = TextureAtlasLayout::from_grid(Vec2::new(32.0, 32.0), 8, 8, None, None);
    let mut texture_atlases = world.get_resource_mut::<Assets<TextureAtlasLayout>>().unwrap();
    let texture_atlas_handle = texture_atlases.add(texture_atlas);
    Self(texture_atlas_handle)
  }
}
