use anyhow::Error as AnyError;

use crate::chunk::Chunk;

pub mod strategies;
pub use strategies::*;
pub mod strategy;
pub use strategy::Strategy as ChunkFactoryStrategy;

/// The `ChunkFactory` struct.
///
/// This isn't a _true_ factory per se, because the chunks already exist. It's
/// more of a chunk modifier, as it lays out the initial room structure and
/// connections based on the chunk geometry.
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

  /// Modifies an existing `Chunk`.
  pub fn modify_chunk(&self, chunk: &mut Chunk) -> Result<(), AnyError> {
    self.strategy.modify_chunk(chunk)?;
    Ok(())
  }
}
