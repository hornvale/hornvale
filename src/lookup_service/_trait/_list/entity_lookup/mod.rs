use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `EntityLookup` trait.
///
/// This helps entity-related lookups.
pub trait EntityLookup {
  /// Get entities in room.
  fn get_entities_in_room(&self, room_id: &RoomId) -> Vec<EntityId>;

  /// Get entities in room with entity.
  fn get_entities_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<EntityId>;

  /// Get entities in room with entity.
  fn get_entities_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<EntityId>;

  /// Get entities in room with object.
  fn get_entities_in_room_with_object(&self, object_id: &ObjectId) -> Vec<EntityId>;

  /// Get entities in room with player.
  fn get_entities_in_room_with_player(&self, player_id: &PlayerId) -> Vec<EntityId>;
}
