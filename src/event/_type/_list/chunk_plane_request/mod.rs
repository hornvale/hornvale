use crate::chunk::ChunkPlane;

/// The `ChunkPlaneRequest` type.
///
/// This represents a request to build a chunk plane.
#[derive(Builder, Clone, Debug, Derivative, Deserialize, Hash, PartialEq, Serialize)]
pub struct ChunkPlaneRequest {
  pub chunk_plane: ChunkPlane,
}
