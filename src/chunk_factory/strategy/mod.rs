use crate::chunk::Chunk;
use crate::chunk_factory::strategies::CompassRoseStrategy;

/// The `ChunkFactoryStrategyTrait` enum.
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
  /// Creates a new `Chunk`.
  pub fn create_chunk(&self) -> Chunk {
    use Strategy::*;
    match self {
      Empty => Chunk::default(),
      CompassRose => CompassRoseStrategy {}.create_chunk(),
    }
  }
}
