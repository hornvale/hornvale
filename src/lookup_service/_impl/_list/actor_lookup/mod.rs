use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup_service::ActorLookupTrait;
use crate::lookup_service::Lookup;

/// Implementation of the `ActorLookup` trait.
impl ActorLookupTrait for Lookup {
  /// List actors in room.
  fn get_actors_in_room(&self, room_id: &RoomId) -> Vec<ActorId> {
    self
      .room2actors
      .get(room_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }

  /// Get actors in room with actor.
  fn get_actors_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<ActorId> {
    self
      .actor2room
      .get(actor_id)
      .map(|room_id| self.get_actors_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get actors in room with entity.
  fn get_actors_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<ActorId> {
    self
      .entity2room
      .get(entity_id)
      .map(|room_id| self.get_actors_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get actors in room with object.
  fn get_actors_in_room_with_object(&self, object_id: &ObjectId) -> Vec<ActorId> {
    self
      .object2room
      .get(object_id)
      .map(|room_id| self.get_actors_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get actors in room with player.
  fn get_actors_in_room_with_player(&self, player_id: &PlayerId) -> Vec<ActorId> {
    self
      .player2room
      .get(player_id)
      .map(|room_id| self.get_actors_in_room(room_id))
      .unwrap_or_default()
  }
}
