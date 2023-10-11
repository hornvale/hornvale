use crate::chunk::Chunk;

/// The `ChunkRequest` type.
///
/// This represents a request to build a chunk.
#[derive(Builder, Clone, Debug, Derivative, Deserialize, Hash, PartialEq, Serialize)]
pub struct ChunkRequest {
  pub chunk: Chunk,
}
