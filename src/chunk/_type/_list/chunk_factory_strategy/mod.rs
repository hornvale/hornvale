/// The `ChunkFactoryStrategy` enum.
#[derive(Clone, Debug, Default)]
pub enum ChunkFactoryStrategy {
  /// The `ChunkFactoryStrategy::Flat` variant.
  #[default]
  Flat,
  /// The `ChunkFactoryStrategy::Perlin` variant.
  Perlin,
}
