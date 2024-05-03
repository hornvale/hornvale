use bevy::prelude::*;

/// The camera component for the "rogue view."
#[derive(Clone, Copy, Component, Debug)]
pub struct RogueViewCamera;

impl RogueViewCamera {
  /// Create the camera.
  pub fn on_startup(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(), RogueViewCamera));
  }

  /// Control the player.
  pub fn on_update(
    time: Res<Time>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut query: Query<(&mut Transform, &mut OrthographicProjection), With<RogueViewCamera>>,
  ) {
    for (mut transform, _) in query.iter_mut() {
      let mut direction = Vec3::ZERO;
      if keyboard_input.pressed(KeyCode::KeyA) {
        log::info!("Moving left");
        direction -= Vec3::new(1.0, 0.0, 0.0);
      }
      if keyboard_input.pressed(KeyCode::KeyD) {
        log::info!("Moving right");
        direction += Vec3::new(1.0, 0.0, 0.0);
      }
      if keyboard_input.pressed(KeyCode::KeyW) {
        log::info!("Moving up");
        direction += Vec3::new(0.0, 1.0, 0.0);
      }
      if keyboard_input.pressed(KeyCode::KeyS) {
        log::info!("Moving down");
        direction -= Vec3::new(0.0, 1.0, 0.0);
      }
      let z = transform.translation.z;
      transform.translation += time.delta_seconds() * direction * 500.;
      // Important! We need to restore the Z values when moving the camera around.
      // Bevy has a specific camera setup and this can mess with how our layers are shown.
      transform.translation.z = z;
    }
  }
}
