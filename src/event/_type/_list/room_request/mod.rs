use crate::entity_uuid::ChunkUuid;
use crate::room::Room;
use crate::room::RoomFactory;

/// The `RoomRequest` type.
///
/// This represents a request to build a room.
#[derive(Builder, Clone, Debug, Derivative)]
#[builder(derive(Debug))]
pub struct RoomRequest {
  pub chunk_uuid: ChunkUuid,
  pub room: Room,
  pub room_factory: RoomFactory,
}
