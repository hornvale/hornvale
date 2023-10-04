use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use serde_yaml::Error as SerdeError;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::chunk::Chunk;
use crate::chunk_seed::ChunkSeed;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkPlane` struct.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ChunkPlane {
  /// The ID.
  pub id: ChunkPlaneId,
  /// The `Chunk` IDs.
  pub chunk_ids: Vec<ChunkId>,
  /// The `ChunkSeed`s.
  pub chunk_seeds: Vec<ChunkSeed>,
  /// The `Chunk`s, only managed as needed.
  #[serde(skip)]
  pub chunks: HashMap<ChunkId, Chunk>,
}

impl ChunkPlane {
  /// Saves the `ChunkPlane` in a serialized form.
  pub fn store(&self, file_path: &str) -> Result<(), SerdeError> {
    let yaml_string = serde_yaml_to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file
      .write_all(yaml_string.as_bytes())
      .expect("Unable to write chunk plane");
    Ok(())
  }

  /// Loads the `ChunkPlane` from a serialized form.
  pub fn load(file_path: &str) -> Result<ChunkPlane, SerdeError> {
    let file = File::open(file_path).expect("Unable to read chunk plane");
    let chunk_plane: ChunkPlane = serde_yaml_from_reader(file)?;
    Ok(chunk_plane)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use anyhow::Error as AnyError;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_plane() -> Result<(), AnyError> {
    init();
    let mut chunk_plane = ChunkPlane::default();
    chunk_plane.id = ChunkPlaneId::new();
    chunk_plane.chunk_ids.push(ChunkId::new());
    chunk_plane.chunk_ids.push(ChunkId::new());
    chunk_plane.chunk_ids.push(ChunkId::new());
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_plane.yaml");
    chunk_plane.store(&file_path)?;
    let chunk_plane = ChunkPlane::load(&file_path)?;
    assert_eq!(chunk_plane.chunk_ids.len(), 3);
    Ok(())
  }
}
