use anyhow::Error as AnyError;

use crate::chunk::Chunk;

pub mod strategies;
pub use strategies::*;
pub mod strategy;
pub use strategy::Strategy as ChunkMapBuilderStrategy;

/// The `ChunkMapBuilder` struct.
///
/// This takes a default, empty `Chunk` and populates it with rooms.
#[derive(Clone)]
pub struct MapBuilder {
  /// The strategy.
  pub strategy: ChunkMapBuilderStrategy,
}

impl MapBuilder {
  /// Creates a new `ChunkFactory`.
  pub fn new(strategy: ChunkMapBuilderStrategy) -> Self {
    Self { strategy }
  }

  /// Maps an existing, empty `Chunk`.
  pub fn map_chunk(&self, chunk: &mut Chunk) -> Result<(), AnyError> {
    self.strategy.map_chunk(chunk)?;
    Ok(())
  }
}
