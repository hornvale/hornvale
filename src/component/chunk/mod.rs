use specs::prelude::*;

/// The `Chunk` struct.
#[derive(Clone, Component, Debug)]
pub struct Chunk {
  /// The `Chunk`'s `ChunkPlane`'s ID.
  pub chunk_plane: Entity,
  /// The `Chunk`'s `ChunkSeed` ID.
  pub chunk_seed: Entity,
}
