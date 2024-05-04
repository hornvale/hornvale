use crate::command_event::prelude::*;
use crate::player::prelude::*;
use bevy::input::common_conditions::input_pressed;
use bevy::prelude::*;

/// The input-handling plugin.
#[derive(Debug, Clone, Copy)]
pub struct InputPlugin;

impl InputPlugin {
  /// The startup system.
  pub fn on_startup() {}

  /// Control the player by responding to keyboard input.
  pub fn on_update(
    _time: Res<Time>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut query: Query<(Entity, &mut Sprite), With<Player>>,
    mut go_direction_command: EventWriter<GoDirectionCommand>,
  ) {
    let (entity, mut player_sprite) = query.single_mut();
    let mut direction = IVec3::ZERO;
    if keyboard_input.pressed(KeyCode::KeyA) {
      direction.x = -1;
    } else if keyboard_input.pressed(KeyCode::KeyD) {
      direction.x = 1;
    }
    if keyboard_input.pressed(KeyCode::KeyW) {
      direction.y = 1;
    } else if keyboard_input.pressed(KeyCode::KeyS) {
      direction.y = -1;
    }
    // Flip the sprite if we are heading right or if we're heading up or
    // down and already flipped.
    //
    // Should probably be an effect.
    player_sprite.flip_x = direction.x > 0 || (direction.x == 0 && player_sprite.flip_x);

    // Send the go-direction command.
    go_direction_command.send(GoDirectionCommand(entity, direction.into()));
  }
}

impl Plugin for InputPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, Self::on_startup).add_systems(
      Update,
      Self::on_update.run_if(
        input_pressed(KeyCode::KeyA)
          .or_else(input_pressed(KeyCode::KeyD))
          .or_else(input_pressed(KeyCode::KeyW))
          .or_else(input_pressed(KeyCode::KeyS)),
      ),
    );
  }
}
