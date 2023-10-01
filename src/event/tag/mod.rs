use crate::entity_id::ActorId;
use crate::entity_id::RoomId;
use crate::event::EventType;

/// The `EventTag` enum.
///
/// Events have tags, which will be used to determine which subscribers will
/// receive them.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Tag {
  /// The event has a specific name.
  HasName(String),
  /// This tag is used for events that are not tagged.
  HasPrincipalActor(ActorId),
  /// The player is the principal actor in the event.
  HasPlayerAsPrincipalActor,
  /// The type of event.
  HasType(EventType),
  /// The ID of the room in which the event was created.
  OccursInRoom(RoomId),
}
