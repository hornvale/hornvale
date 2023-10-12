use uuid::Uuid;

use crate::chunk::ChunkCoordinates;
use crate::chunk::ChunkStatus;
use crate::entity_uuid::BaseUuidWrapperTrait;
use crate::entity_uuid::ChunkUuid;

/// The `Chunk` struct.
#[derive(Builder, Clone, Debug, Deserialize, Hash, PartialEq, Serialize)]
#[builder(derive(Debug))]
pub struct Chunk {
  /// The `Chunk`'s coordinates in the `ChunkPlane`.
  pub coordinates: ChunkCoordinates,
  /// The `Chunk`'s seed string.
  pub seed_string: String,
  /// The UUID of the chunk.
  #[builder(default = "ChunkUuid::new(Uuid::new_v4().to_string())")]
  pub uuid: ChunkUuid,
  /// The `Chunk`'s status.
  pub status: ChunkStatus,
  /// The `Chunk`'s name.
  pub name: String,
  /// The `Chunk`'s description.
  pub description: String,
}
