use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

/// The `ChunkCreatorService` trait.
pub trait ChunkWorldCreatorService {
  /// Creates a new `ChunkWorld`.
  fn create_chunk_world(&mut self) -> Result<ChunkWorld, AnyError>;
}
