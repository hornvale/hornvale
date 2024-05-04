use bevy::prelude::*;

/// The camera component for the "rogue view."
#[derive(Clone, Copy, Component, Debug)]
pub struct RogueViewCamera;

impl RogueViewCamera {
  /// Create the camera.
  pub fn on_startup(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(), RogueViewCamera));
  }
}
