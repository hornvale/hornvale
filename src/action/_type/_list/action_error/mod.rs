use anyhow::Error as AnyError;
use specs::prelude::*;

use crate::action::Action;
use crate::effect::EffectBuilderError;
use crate::passage::PassageDirection;
use crate::room::Room;

/// The `ActionError` type.
#[derive(Debug, Error)]
pub enum ActionError {
  /// You do not seem to be located in a room at the moment.
  #[error("You do not seem to be located in a room at the moment.")]
  NotInARoom {
    /// The `Action`.
    action: Action,
  },
  /// You see no way to travel in that direction.
  #[error("You see no way to travel in that direction.")]
  NoPassageInThatDirection {
    /// The `Action`.
    action: Action,
    /// The current room.
    current_room_entity: Entity,
    /// The current room.
    current_room: Room,
    /// The `PassageDirection`.
    direction: PassageDirection,
  },
  /// Any error occurred.
  #[error(transparent)]
  AnyError(#[from] AnyError),
  /// An effect builder error occurred.
  #[error(transparent)]
  EffectBuilderError(#[from] EffectBuilderError),
}

impl From<AnyError> for Box<ActionError> {
  fn from(error: AnyError) -> Self {
    Box::new(ActionError::AnyError(error))
  }
}

impl From<EffectBuilderError> for Box<ActionError> {
  fn from(error: EffectBuilderError) -> Self {
    Box::new(ActionError::EffectBuilderError(error))
  }
}
