use crate::chunk::ChunkCoordinates;
use crate::chunk::ChunkStatus;

/// The `Chunk` struct.
#[derive(Builder, Clone, Debug, Deserialize, Hash, PartialEq, Serialize)]
pub struct Chunk {
  /// The `Chunk`'s coordinates in the `ChunkPlane`.
  pub coordinates: ChunkCoordinates,
  /// The `Chunk`'s seed string.
  pub seed_string: String,
  /// The `Chunk`'s status.
  pub status: ChunkStatus,
  /// The `Chunk`'s name.
  pub name: String,
  /// The `Chunk`'s description.
  pub description: String,
}
