use anyhow::Error as AnyError;

use crate::chunk::BlankFillStrategy;
use crate::chunk::Chunk;
use crate::chunk::CompassRoseStrategy;

/// The `ChunkMapBuilderStrategy` enum.
///
/// This is the strategy for mapping a new `Chunk` in the `ChunkMapBuilder`.
#[derive(Clone, Debug, Default)]
pub enum Strategy {
  /// The `Empty` strategy.
  #[default]
  Empty,
  /// Fill with a bunch of empty rooms.
  BlankFill,
  /// The `CompassRose` strategy.
  CompassRose,
}

impl Strategy {
  /// Maps an empty `Chunk`.
  pub fn map_chunk(&self, chunk: &mut Chunk) -> Result<(), AnyError> {
    use Strategy::*;
    match self {
      Empty => Ok(()),
      BlankFill => BlankFillStrategy {}.map_chunk(chunk),
      CompassRose => CompassRoseStrategy {}.map_chunk(chunk),
    }
  }
}
