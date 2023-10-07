use crate::entity_id::ChunkId;
use crate::entity_id::RoomId;
use crate::game_state::GameState;
use crate::game_state::RoomIdToChunkIdTrait;

/// Implementation of the `RoomIdToChunkId` trait.
impl RoomIdToChunkIdTrait for GameState {
  /// Returns the chunk ID for the given room ID.
  fn get_chunk_id(&self, room_id: &RoomId) -> Option<ChunkId> {
    self.room_id_to_chunk_id.get(room_id).cloned()
  }
  /// Sets the chunk ID for the given room ID.
  fn set_chunk_id(&mut self, room_id: &RoomId, chunk_id: &ChunkId) {
    self.room_id_to_chunk_id.insert(room_id.clone(), chunk_id.clone());
  }
  /// Sets the chunk ID for the given room IDs.
  fn set_chunk_ids(&mut self, room_ids: &[RoomId], chunk_id: &ChunkId) {
    for room_id in room_ids {
      self.room_id_to_chunk_id.insert(room_id.clone(), chunk_id.clone());
    }
  }
}
