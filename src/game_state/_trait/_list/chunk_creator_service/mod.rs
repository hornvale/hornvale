use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;

/// The `ChunkCreatorService` trait.
pub trait ChunkCreatorService {
  /// Creates a new `ChunkPlane`.
  fn create_chunk_plane(&mut self) -> Result<ChunkPlane, AnyError>;

  /// Generates the initial `Chunk`s for a `ChunkPlane`.
  fn generate_initial_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError>;

  /// Map empty chunks in a chunk plane.
  fn map_empty_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError>;

  /// Map chunks.
  fn map_chunks(&mut self, chunks: &mut Vec<Chunk>) -> Result<(), AnyError>;

  /// Map a chunk.
  fn map_chunk(&mut self, chunk: &mut Chunk) -> Result<(), AnyError>;
}
