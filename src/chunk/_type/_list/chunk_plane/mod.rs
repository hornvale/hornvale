use specs::prelude::*;

/// The `ChunkPlane` component.
///
/// A `ChunkPlane` is the largest spatial unit in the game. It is a 2D plane
/// composed of `Chunk`s. Other areas, e.g. the Underdark, will be managed
/// in different planes.
#[derive(Builder, Clone, Component, Debug, Deserialize, Hash, PartialEq, Serialize)]
pub struct ChunkPlane {
  /// The seed string, used for randomizing the chunk plane.
  pub seed_string: String,
  /// The chunk plane's name. Not displayed, but used for reference.
  pub name: String,
  /// The chunk plane's description. Not displayed, but used for reference.
  pub description: String,
}
