use anyhow::Error as AnyError;

use crate::action::Action;
use crate::entity_id::RoomId;
use crate::passage::PassageDirection;
use crate::room::Room;

/// The `ActionError` type.
#[derive(Debug, Error)]
pub enum Error {
  /// You do not seem to be located in a room at the moment.
  #[error("You do not seem to be located in a room at the moment.")]
  NotInARoom {
    /// The `Action`.
    action: Action,
    /// The current room's `RoomId`.
    current_room_id: RoomId,
  },
  /// You see no way to travel in that direction.
  #[error("You see no way to travel in that direction.")]
  NoPassageInThatDirection {
    /// The `Action`.
    action: Action,
    /// The current room's `RoomId`.
    current_room_id: RoomId,
    /// The current room.
    current_room: Room,
    /// The `PassageDirection`.
    direction: PassageDirection,
  },
  /// Any error occurred.
  #[error(transparent)]
  AnyError(#[from] AnyError),
}
