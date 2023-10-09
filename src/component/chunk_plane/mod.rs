use specs::prelude::*;

/// The `ChunkPlane` component.
#[derive(Clone, Component, Debug, Deserialize, Serialize)]
pub struct ChunkPlane {
  /// The ID.
  pub id: String,
  /// The upper-left corner of the `ChunkPlane` in (i64, i64) plane.
  pub upper_left_corner: (i64, i64),
  /// The lower-right corner of the `ChunkPlane` in (i64, i64) plane.
  pub lower_right_corner: (i64, i64),
  /// Whether this chunk plane is seeded.
  pub is_seeded: bool,
}
