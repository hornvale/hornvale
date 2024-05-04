use crate::rogue_view::prelude_internal::*;
use bevy::prelude::*;
use bevy_ecs_tilemap::prelude::*;

/// The RogueView plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct RogueViewPlugin;

impl RogueViewPlugin {}

impl Plugin for RogueViewPlugin {
  fn build(&self, app: &mut App) {
    app
      .add_plugins(TilemapPlugin)
      .insert_resource(RogueViewDimensions(Vec2::new(800.0, 600.0)))
      .insert_resource(RogueViewRenderDistance(Vec2::new(400.0, 300.0)))
      .insert_resource(RogueViewChunkManager::default())
      .add_systems(
        Startup,
        (RogueViewCamera::on_startup, RogueViewChunkManager::on_startup),
      )
      .add_systems(
        Update,
        (
          RogueViewDimensions::on_resize_window,
          RogueViewRenderDistance::on_resize_window,
          RogueViewChunkManager::spawn_chunks,
          RogueViewChunkManager::despawn_chunks,
        ),
      );
  }
}
