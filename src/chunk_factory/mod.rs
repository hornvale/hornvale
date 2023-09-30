use crate::chunk::Chunk;

pub mod strategies;
pub use strategies::*;
pub mod strategy;
pub use strategy::ChunkFactoryStrategyTrait;

/// The `ChunkFactory` struct.
#[derive(Clone)]
pub struct ChunkFactory<S: ChunkFactoryStrategyTrait> {
  /// The strategy.
  pub strategy: S,
}

impl<S: ChunkFactoryStrategyTrait> ChunkFactory<S> {
  /// Creates a new `ChunkFactory`.
  pub fn new(strategy: S) -> Self {
    Self { strategy }
  }

  /// Creates a new `Chunk`.
  pub fn create_chunk(&self) -> Chunk {
    self.strategy.create_chunk()
  }
}
