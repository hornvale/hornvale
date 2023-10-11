use crate::chunk::ChunkPlane;

/// The `ChunkPlaneRequest` type.
///
/// This represents a request to build a chunk plane.
#[derive(Builder, Clone, Derivative, Deserialize, Hash, PartialEq, Serialize)]
#[derivative(Debug)]
pub struct ChunkPlaneRequest {
  #[derivative(Debug = "ignore")]
  pub chunk_plane: ChunkPlane,
}
