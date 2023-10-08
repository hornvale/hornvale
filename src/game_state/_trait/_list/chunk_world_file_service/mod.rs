use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

/// The `ChunkWorldFileService` trait.
pub trait ChunkWorldFileService {
  /// Opens the `ChunkWorld` from disk.
  fn open_chunk_world(&mut self) -> Result<ChunkWorld, AnyError>;
  /// Saves the `ChunkWorld` in a serialized form.
  fn save_chunk_world(&mut self, chunk_world: &ChunkWorld) -> Result<(), AnyError>;
}
