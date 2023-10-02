use crate::chunk::Chunk;

pub mod strategies;
pub use strategies::*;
pub mod strategy;
pub use strategy::Strategy as ChunkFactoryStrategy;

/// The `ChunkFactory` struct.
#[derive(Clone)]
pub struct ChunkFactory {
  /// The strategy.
  pub strategy: ChunkFactoryStrategy,
}

impl ChunkFactory {
  /// Creates a new `ChunkFactory`.
  pub fn new(strategy: ChunkFactoryStrategy) -> Self {
    Self { strategy }
  }

  /// Creates a new `Chunk`.
  pub fn create_chunk(&self) -> Chunk {
    self.strategy.create_chunk()
  }
}
