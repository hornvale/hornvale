use crate::rogue_view::prelude_internal::*;
use bevy::prelude::*;
use bevy::window::WindowResized;

/// The RogueView plugin.
#[derive(Clone, Copy, Debug, Default)]
pub struct RogueViewPlugin;

impl RogueViewPlugin {
  /// Create the camera.
  pub fn on_startup(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(), RogueViewCamera));
  }

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
      .insert_resource(RogueViewDimensions(Vec2::new(800.0, 600.0)))
      .add_systems(Startup, Self::on_startup)
      .add_systems(Update, Self::on_resize_window);
  }
}
