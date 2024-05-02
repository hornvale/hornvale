use bevy::prelude::*;

/// The camera component.
#[derive(Clone, Copy, Component, Debug)]
struct Camera;

/// Camera setup.
#[derive(Debug, Clone, Copy)]
pub struct CameraPlugin;

impl Plugin for CameraPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, setup);
  }
}

/// Create the camera.
fn setup(mut commands: Commands) {
  commands.spawn((Camera2dBundle::default(), Camera));
}
