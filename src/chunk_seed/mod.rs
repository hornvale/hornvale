use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use serde_yaml::Error as SerdeError;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::ChunkSeedId;

pub mod builder;
pub use builder::Builder as ChunkSeedBuilder;
pub mod r#type;
pub use r#type::Type as ChunkSeedType;

/// The `ChunkSeed` struct.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ChunkSeed {
  /// The `ChunkSeed`'s ID.
  pub id: ChunkSeedId,
  /// The `ChunkSeed`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: ChunkPlaneId,
  /// If this `ChunkSeed` has a corresponding `Chunk`.
  pub chunk_id: Option<ChunkId>,
  /// The coordinates in (i64, i64) plane.
  pub coordinates: (i64, i64),
  /// The points contained by this `ChunkSeed`.
  pub points: HashSet<(i64, i64)>,
  /// The adjacent `ChunkSeed` IDs.
  pub adjacent_chunk_seed_ids: Vec<ChunkSeedId>,
  /// The `ChunkSeed`'s type.
  pub r#type: ChunkSeedType,
}

impl ChunkSeed {
  /// Creates a new `ChunkSeed`.
  pub fn new(id: ChunkSeedId, chunk_plane_id: &ChunkPlaneId, coordinates: (i64, i64), r#type: ChunkSeedType) -> Self {
    let chunk_plane_id = chunk_plane_id.clone();
    Self {
      id,
      chunk_plane_id,
      chunk_id: None,
      coordinates,
      points: HashSet::new(),
      adjacent_chunk_seed_ids: Vec::new(),
      r#type,
    }
  }

  /// Saves the `Chunk` in a serialized form.
  pub fn store(&self, file_path: &str) -> Result<(), SerdeError> {
    let yaml_string = serde_yaml_to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file
      .write_all(yaml_string.as_bytes())
      .expect("Unable to write chunk_seed data");
    Ok(())
  }

  /// Loads the `Chunk` from a serialized form.
  pub fn load(file_path: &str) -> Result<ChunkSeed, SerdeError> {
    let file = File::open(file_path).expect("Unable to open chunk_seed file");
    let chunk_seed: ChunkSeed = serde_yaml_from_reader(file)?;
    Ok(chunk_seed)
  }

  /// Whether this `ChunkSeed` contains the given coordinates.
  pub fn contains(&self, coordinates: (i64, i64)) -> bool {
    self.points.contains(&coordinates)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use anyhow::Error as AnyError;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_seed() -> Result<(), AnyError> {
    init();
    let mut chunk_seed = ChunkSeed::default();
    chunk_seed.id = ChunkSeedId::new();
    chunk_seed.chunk_plane_id = ChunkPlaneId::new();
    chunk_seed.chunk_id = Some(ChunkId::new());
    chunk_seed.coordinates = (0, 0);
    chunk_seed.r#type = ChunkSeedType::default();
    let file_path = format!("{}/test_chunk_seed.yml", TEMPORARY_TEST_DATA_DIRECTORY);
    chunk_seed.store(&file_path)?;
    let chunk_seed_loaded = ChunkSeed::load(&file_path)?;
    assert_eq!(chunk_seed, chunk_seed_loaded);
    Ok(())
  }
}
