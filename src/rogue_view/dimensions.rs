use bevy::prelude::*;
use bevy::window::WindowResized;

/// The dimensions of the "rogue view."
#[derive(Debug, Clone, Copy, Resource)]
#[repr(transparent)]
pub struct RogueViewDimensions(pub Vec2);

impl RogueViewDimensions {
  /// Resize the rogue view when the window is resized.
  pub fn on_resize_window(mut dimensions: ResMut<RogueViewDimensions>, mut resize_reader: EventReader<WindowResized>) {
    for e in resize_reader.read() {
      dimensions.0 = Vec2::new(e.width, e.height);
    }
  }
}
