use crate::action_event::prelude::*;
use crate::direction::Direction;
use crate::player::prelude::*;
use bevy::prelude::*;

/// The go-direction command event.
#[derive(Debug, Clone, Copy, Event)]
pub struct GoDirectionCommand(pub Entity, pub Direction);

impl GoDirectionCommand {
  /// When a go-direction command is received, attempt to move the player.
  pub fn apply(
    mut go_direction_commands: EventReader<GoDirectionCommand>,
    mut query: Query<&Transform, With<Player>>,
    mut go_direction_actions: EventWriter<GoDirectionAction>,
  ) {
    for GoDirectionCommand(entity, direction) in go_direction_commands.read() {
      if let Ok(_transform) = query.get_mut(*entity) {
        go_direction_actions.send(GoDirectionAction(*entity, *direction));
      }
    }
  }
}
