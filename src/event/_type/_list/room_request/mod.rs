use crate::room::Room;
use crate::room::RoomFactory;

/// The `RoomRequest` type.
///
/// This represents a request to build a room.
#[derive(Builder, Clone, Debug, Derivative)]
#[builder(derive(Debug))]
pub struct RoomRequest {
  pub room: Room,
  pub room_factory: RoomFactory,
}
