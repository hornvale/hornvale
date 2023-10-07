use crate::entity_id::ChunkId;
use crate::entity_id::RoomId;

/// The `RoomIdToChunkId` trait.
///
/// This is used to keep track of which rooms are in which chunks.
pub trait RoomIdToChunkId {
  /// Returns the chunk ID for the given room ID.
  fn get_chunk_id(&self, room_id: &RoomId) -> Option<ChunkId>;
  /// Sets the chunk ID for the given room ID.
  fn set_chunk_id(&mut self, room_id: &RoomId, chunk_id: &ChunkId);
  /// Sets the chunk ID for the given room IDs.
  fn set_chunk_ids(&mut self, room_ids: &[RoomId], chunk_id: &ChunkId);
}
