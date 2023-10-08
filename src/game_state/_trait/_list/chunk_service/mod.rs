use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkService` trait.
pub trait ChunkService {
  /// Load a chunk plane.
  ///
  /// Note that this consumes the chunk plane.
  fn load_chunk_plane(&mut self, chunk_plane: ChunkPlane) -> Result<(), AnyError>;

  /// Unload a chunk plane.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError>;

  /// Load a chunk.
  ///
  /// Note that this consumes the chunk.
  fn load_chunk(&mut self, chunk: Chunk) -> Result<(), AnyError>;

  /// Unload a chunk.
  fn unload_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError>;
}
