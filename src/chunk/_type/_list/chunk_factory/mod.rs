use crate::chunk::ChunkFactoryStrategy;

/// The `ChunkFactory` struct.
#[derive(Clone, Debug, Default)]
pub struct ChunkFactory {
  pub strategy: ChunkFactoryStrategy,
}

impl ChunkFactory {}
