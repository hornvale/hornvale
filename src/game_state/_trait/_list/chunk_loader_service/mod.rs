use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkLoaderService` trait.
pub trait ChunkLoaderService {
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

  /// Get an arbitrary chunk plane.
  fn get_arbitrary_chunk_plane(&self) -> Option<&ChunkPlane>;

  /// Get an arbitrary chunk plane mutably.
  fn get_arbitrary_chunk_plane_mut(&mut self) -> Option<&mut ChunkPlane>;

  /// Get an arbitrary chunk.
  fn get_arbitrary_chunk(&self) -> Option<&Chunk>;

  /// Get an arbitrary chunk mutably.
  fn get_arbitrary_chunk_mut(&mut self) -> Option<&mut Chunk>;

  /// Get an arbitrary startable chunk.
  fn get_arbitrary_startable_chunk(&self) -> Option<&Chunk>;

  /// Get an arbitrary startable chunk mutably.
  fn get_arbitrary_startable_chunk_mut(&mut self) -> Option<&mut Chunk>;
}
