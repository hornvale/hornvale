use specs::prelude::*;
use std::collections::HashSet;

pub mod chunk_seed_status;
pub use chunk_seed_status::ChunkSeedStatus;

/// The `ChunkSeed` component.
#[derive(Clone, Component, Debug)]
pub struct ChunkSeed {
  /// The Chunk Plane ID.
  pub chunk_plane: Entity,
  /// The coordinates in (i64, i64) plane.
  pub coordinates: (i64, i64),
  /// The points contained by this `ChunkSeed`.
  pub points: HashSet<(i64, i64)>,
  /// The adjacent `ChunkSeed` IDs.
  pub neighbors: Vec<Entity>,
  /// The `ChunkSeed`'s status.
  pub status: ChunkSeedStatus,
}
