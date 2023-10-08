use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `ObjectLookup` trait.
///
/// This helps object-related lookups.
pub trait ObjectLookup {
  /// Get objects in room.
  fn get_objects_in_room(&self, room_id: &RoomId) -> Vec<ObjectId>;

  /// Get objects in room with actor.
  fn get_objects_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<ObjectId>;

  /// Get objects in room with entity.
  fn get_objects_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<ObjectId>;

  /// Get objects in room with object.
  fn get_objects_in_room_with_object(&self, object_id: &ObjectId) -> Vec<ObjectId>;

  /// Get objects in room with player.
  fn get_objects_in_room_with_player(&self, player_id: &PlayerId) -> Vec<ObjectId>;
}
