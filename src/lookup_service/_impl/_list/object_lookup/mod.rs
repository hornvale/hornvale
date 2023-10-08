use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup_service::LookupService;
use crate::lookup_service::ObjectLookupTrait;

/// Implementation of the `ObjectLookup` trait.
impl ObjectLookupTrait for LookupService {
  /// Get objects in room.
  fn get_objects_in_room(&self, room_id: &RoomId) -> Vec<ObjectId> {
    self
      .room2objects
      .get(room_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }

  /// Get objects in room with actor.
  fn get_objects_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<ObjectId> {
    self
      .actor2room
      .get(actor_id)
      .map(|room_id| self.get_objects_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get objects in room with entity.
  fn get_objects_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<ObjectId> {
    self
      .entity2room
      .get(entity_id)
      .map(|room_id| self.get_objects_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get objects in room with object.
  fn get_objects_in_room_with_object(&self, object_id: &ObjectId) -> Vec<ObjectId> {
    self
      .object2room
      .get(object_id)
      .map(|room_id| self.get_objects_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get objects in room with player.
  fn get_objects_in_room_with_player(&self, player_id: &PlayerId) -> Vec<ObjectId> {
    self
      .player2room
      .get(player_id)
      .map(|room_id| self.get_objects_in_room(room_id))
      .unwrap_or_default()
  }
}
