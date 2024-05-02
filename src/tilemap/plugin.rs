use crate::tilemap::prelude_internal::*;
use bevy::prelude::*;
use bevy::window::WindowResized;

/// The Tilemap plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct TilemapPlugin;

impl TilemapPlugin {
  /// Create the camera.
  pub fn on_startup(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(), TilemapCamera));
  }

  /// Resize the tilemap wind the window is resized.
  pub fn on_resize_window(
    mut tilemap_dimensions: ResMut<TilemapDimensions>,
    mut resize_reader: EventReader<WindowResized>,
  ) {
    for e in resize_reader.read() {
      tilemap_dimensions.0 = Vec2::new(e.width, e.height);
    }
  }
}

impl Plugin for TilemapPlugin {
  fn build(&self, app: &mut App) {
    app
      .insert_resource(TilemapDimensions(Vec2::new(800.0, 600.0)))
      .add_systems(Startup, Self::on_startup)
      .add_systems(Update, Self::on_resize_window);
  }
}
