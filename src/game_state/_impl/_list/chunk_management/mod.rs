use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::entity_id::ChunkId;
use crate::game_state::ChunkManagementTrait;
use crate::game_state::GameState;

/// Implementation of the `ChunkManagement` trait.
impl ChunkManagementTrait for GameState {
  /// Get loaded chunks.
  fn get_loaded_chunks(&self) -> &HashMap<ChunkId, Chunk> {
    &self.loaded_chunks
  }
  /// Get mutable loaded chunks.
  fn get_loaded_chunks_mut(&mut self) -> &mut HashMap<ChunkId, Chunk> {
    &mut self.loaded_chunks
  }
}
