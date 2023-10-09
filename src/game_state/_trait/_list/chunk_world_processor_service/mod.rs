use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;

/// The `ChunkWorldProcessorService` trait.
pub trait ChunkWorldProcessorService {
  /// Process the world.
  fn process(&mut self, chunk_world: &mut ChunkWorld) -> Result<(), AnyError>;
}
