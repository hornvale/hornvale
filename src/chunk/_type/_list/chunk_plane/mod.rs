use uuid::Uuid;

use crate::entity_uuid::BaseUuidWrapperTrait;
use crate::entity_uuid::ChunkPlaneUuid;

/// The `ChunkPlane` component.
///
/// A `ChunkPlane` is the largest spatial unit in the game. It is a 2D plane
/// composed of `Chunk`s. Other areas, e.g. the Underdark, will be managed
/// in different planes.
#[derive(Builder, Clone, Debug, Deserialize, Hash, PartialEq, Serialize)]
#[builder(derive(Debug))]
pub struct ChunkPlane {
  /// The seed string, used for randomizing the chunk plane.
  pub seed_string: String,
  /// The UUID of the chunk plane.
  #[builder(default = "ChunkPlaneUuid::new(Uuid::new_v4().to_string())")]
  pub uuid: ChunkPlaneUuid,
  /// The chunk plane's name. Not displayed, but used for reference.
  pub name: String,
  /// The chunk plane's description. Not displayed, but used for reference.
  pub description: String,
}
