use anyhow::Error as AnyError;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkManagement` trait.
pub trait ChunkManagement {
  /// Load a chunk.
  fn load_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError>;
  /// Unload a chunk.
  fn unload_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError>;
  /// Load a chunk plane.
  fn load_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError>;
  /// Unload a chunk plane.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError>;
}
