use specs::prelude::*;

use crate::chunk::Chunk;
use crate::chunk::ChunkFactory;

/// The `ChunkRequest` type.
///
/// This represents a request to build a chunk.
#[derive(Builder, Clone, Debug, Derivative)]
#[builder(derive(Debug))]
pub struct ChunkRequest {
  pub chunk: Chunk,
  pub chunk_plane_entity: Entity,
  pub chunk_factory: ChunkFactory,
}
