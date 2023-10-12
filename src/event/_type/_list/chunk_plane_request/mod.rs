use crate::chunk::ChunkPlaneBuilder;

/// The `ChunkPlaneRequest` type.
///
/// This represents a request to build a chunk plane.
#[derive(Builder, Clone, Debug, Derivative)]
#[builder(derive(Debug))]
pub struct ChunkPlaneRequest {
  pub builder: ChunkPlaneBuilder,
}
