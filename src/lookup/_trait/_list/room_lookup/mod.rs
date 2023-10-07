use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `RoomLookup` trait.
///
/// This helps room-related lookups.
pub trait RoomLookup {
  /// List rooms in chunk.
  fn get_rooms_in_chunk(&self, chunk_id: &ChunkId) -> Vec<RoomId>;

  /// Get the room of an actor.
  fn get_room_of_actor(&self, actor_id: &ActorId) -> Option<RoomId>;

  /// Get the room of an entity.
  fn get_room_of_entity(&self, entity_id: &EntityId) -> Option<RoomId>;

  /// Get the room of an object.
  fn get_room_of_object(&self, object_id: &ObjectId) -> Option<RoomId>;

  /// Get room of player.
  fn get_room_of_player(&self, player_id: &PlayerId) -> Option<RoomId>;
}
