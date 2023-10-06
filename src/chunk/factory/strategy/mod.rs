use anyhow::Error as AnyError;

use crate::chunk::factory::strategies::CompassRoseStrategy;
use crate::chunk::Chunk;

/// The `ChunkFactoryStrategy` enum.
///
/// This is the strategy for creating a new `Chunk` in the `ChunkFactory`.
#[derive(Clone, Debug, Default)]
pub enum Strategy {
  /// The `Empty` strategy.
  #[default]
  Empty,
  /// The `CompassRose` strategy.
  CompassRose,
}

impl Strategy {
  /// Modifies an empty `Chunk`.
  pub fn modify_chunk(&self, chunk: &mut Chunk) -> Result<(), AnyError> {
    use Strategy::*;
    match self {
      Empty => Ok(()),
      CompassRose => CompassRoseStrategy {}.modify_chunk(chunk),
    }
  }
}
