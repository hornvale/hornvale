use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkFileService` trait.
pub trait ChunkFileService {
  /// Open a chunk.
  fn open_chunk(&mut self, chunk_id: &ChunkId) -> Result<Chunk, AnyError>;
  /// Save a chunk.
  fn save_chunk(&mut self, chunk: &Chunk) -> Result<(), AnyError>;
  /// Save multiple chunks.
  fn save_chunks(&mut self, chunks: &[Chunk]) -> Result<(), AnyError>;
  /// Open a chunk plane.
  fn open_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError>;
  /// Save a chunk plane.
  fn save_chunk_plane(&mut self, chunk_plane: &ChunkPlane) -> Result<(), AnyError>;
}
