use specs::prelude::*;
use std::collections::HashSet;

pub mod chunk_seed_type;
pub use chunk_seed_type::ChunkSeedType;

/// The `ChunkSeed` component.
#[derive(Clone, Component, Debug, Deserialize, Serialize)]
pub struct ChunkSeed {
  /// The Chunk Plane ID.
  pub chunk_plane_id: String,
  /// The Chunk ID, if any.
  pub chunk_id: Option<String>,
  /// The ID.
  pub id: String,
  /// The coordinates in (i64, i64) plane.
  pub coordinates: (i64, i64),
  /// The points contained by this `ChunkSeed`.
  pub points: HashSet<(i64, i64)>,
  /// The adjacent `ChunkSeed` IDs.
  pub adjacent_chunk_seed_ids: Vec<String>,
  /// The `ChunkSeed`'s type.
  pub chunk_seed_type: ChunkSeedType,
}
