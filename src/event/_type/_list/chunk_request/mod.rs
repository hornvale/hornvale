use crate::chunk::Chunk;

/// The `ChunkRequest` type.
///
/// This represents a request to build a chunk.
#[derive(Builder, Clone, Debug, Derivative)]
#[builder(derive(Debug))]
pub struct ChunkRequest {
  pub chunk_plane_uuid: String,
  pub chunk: Chunk,
}
