use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::entity_id::ChunkId;

/// The `ChunkManagement` trait.
pub trait ChunkManagement {
  /// Get loaded chunks.
  fn get_loaded_chunks(&self) -> &HashMap<ChunkId, Chunk>;
  /// Get mutable loaded chunks.
  fn get_loaded_chunks_mut(&mut self) -> &mut HashMap<ChunkId, Chunk>;
}
