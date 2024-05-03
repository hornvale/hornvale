use crate::rogue_view::prelude_internal::*;
use bevy::prelude::*;
use bevy::window::WindowResized;
use bevy_ecs_tilemap::prelude::*;

/// The RogueView plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct RogueViewPlugin;

impl RogueViewPlugin {
  /// Resize the rogue view when the window is resized.
  pub fn on_resize_window(
    mut rogue_view_dimensions: ResMut<RogueViewDimensions>,
    mut resize_reader: EventReader<WindowResized>,
  ) {
    for e in resize_reader.read() {
      rogue_view_dimensions.0 = Vec2::new(e.width, e.height);
    }
  }
}

impl Plugin for RogueViewPlugin {
  fn build(&self, app: &mut App) {
    app
      .add_plugins(TilemapPlugin)
      .insert_resource(RogueViewDimensions(Vec2::new(800.0, 600.0)))
      .insert_resource(ChunkManager::default())
      .add_systems(Startup, (RogueViewCamera::on_startup, ChunkManager::on_startup))
      .add_systems(
        Update,
        (
          Self::on_resize_window,
          RogueViewCamera::on_update,
          ChunkManager::spawn_chunks,
          ChunkManager::despawn_chunks,
        ),
      );
  }
}
