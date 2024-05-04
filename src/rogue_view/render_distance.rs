use bevy::prelude::*;
use bevy::window::WindowResized;

/// The cutoff for rendering entities.
#[derive(Debug, Clone, Copy, Resource)]
#[repr(transparent)]
pub struct RogueViewRenderDistance(pub Vec2);

impl RogueViewRenderDistance {
  /// Resize the rogue view when the window is resized.
  pub fn on_resize_window(
    mut render_distance: ResMut<RogueViewRenderDistance>,
    mut resize_reader: EventReader<WindowResized>,
  ) {
    for e in resize_reader.read() {
      render_distance.0 = Vec2::new(e.width / 2.0, e.height / 2.0);
    }
  }
}
