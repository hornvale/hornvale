use crate::chunk::Chunk;

/// The `ChunkFactoryStrategyTrait` trait.
pub trait ChunkFactoryStrategyTrait {
  /// Creates a new `Chunk`.
  fn create_chunk(&self) -> Chunk;
}
