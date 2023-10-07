use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `ActorLookup` trait.
///
/// This helps actor-related lookups.
pub trait ActorLookup {
  /// List actors in room.
  fn get_actors_in_room(&self, room_id: &RoomId) -> Vec<ActorId>;

  /// Get actors in room with actor.
  fn get_actors_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<ActorId>;

  /// Get actors in room with entity.
  fn get_actors_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<ActorId>;

  /// Get actors in room with object.
  fn get_actors_in_room_with_object(&self, object_id: &ObjectId) -> Vec<ActorId>;

  /// Get actors in room with player.
  fn get_actors_in_room_with_player(&self, player_id: &PlayerId) -> Vec<ActorId>;
}
