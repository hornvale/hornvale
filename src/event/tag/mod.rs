use crate::entity_id::EntityId;
use crate::entity_id::RoomId;

/// The `EventTag` enum.
///
/// Events have tags, which will be used to determine which subscribers will
/// receive them.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Tag {
  /// The ID of the entity that created the event.
  HasCreatorEntity(EntityId),
  /// The name of the event.
  HasName(String),
  /// The timestamp at which the event was created.
  HasTimestamp(u64),
  /// The ID of the room in which the event was created.
  OccursInRoom(RoomId),
}
