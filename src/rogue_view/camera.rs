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
    _time: Res<Time>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut query: Query<(&mut Transform, &mut OrthographicProjection), With<RogueViewCamera>>,
  ) {
    for (mut transform, _) in query.iter_mut() {
      let mut direction = Vec3::ZERO;
      if keyboard_input.pressed(KeyCode::KeyA) {
        direction -= Vec3::new(1.0, 0.0, 0.0);
      } else if keyboard_input.pressed(KeyCode::KeyD) {
        direction += Vec3::new(1.0, 0.0, 0.0);
      }
      if keyboard_input.pressed(KeyCode::KeyW) {
        direction += Vec3::new(0.0, 1.0, 0.0);
      } else if keyboard_input.pressed(KeyCode::KeyS) {
        direction -= Vec3::new(0.0, 1.0, 0.0);
      }

      // Important! We need to restore the Z values when moving the camera around.
      // Bevy has a specific camera setup and this can mess with how our layers are shown.
      let z = transform.translation.z;
      transform.translation += direction;
      transform.translation.z = z;
    }
  }
}
