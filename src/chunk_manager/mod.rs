use anyhow::Context;
use anyhow::Error as AnyError;
use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use std::collections::HashMap;
use std::fs::create_dir_all;
use std::fs::remove_dir_all;
use std::fs::File;
use std::io::Write;

use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkManager` struct.
///
/// This indirectly manages most of the chunk-related functionality.
///
/// This struct is responsible for:
/// - Orchestrating the creation and destruction of `ChunkPlane`s
/// - Invoking creation of empty `Chunk`s by their `ChunkPlane`s
/// - Populating empty `Chunk`s with the `ChunkFactory`.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ChunkManager {
  /// The `ChunkPlane`s IDs.
  pub chunk_plane_ids: Vec<ChunkPlaneId>,
  /// The `ChunkPlane`s.
  #[serde(skip)]
  pub chunk_planes: HashMap<ChunkPlaneId, ChunkPlane>,
}

impl ChunkManager {
  /// Creates a new `ChunkManager`.
  pub fn new() -> Self {
    Self {
      chunk_plane_ids: Vec::new(),
      chunk_planes: HashMap::new(),
    }
  }

  /// Creates a new `ChunkPlane`.
  pub fn create_chunk_plane(&mut self, seed_string: &str) -> Result<ChunkPlaneId, AnyError> {
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_plane = ChunkPlane::new(&chunk_plane_id, seed_string);
    self.chunk_plane_ids.push(chunk_plane_id.clone());
    self.chunk_planes.insert(chunk_plane_id.clone(), chunk_plane);
    Ok(chunk_plane_id)
  }

  /// Loads an existing `ChunkPlane`.
  pub fn load_chunk_plane(&mut self, base_path: &str, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    let chunk_plane = ChunkPlane::load(base_path, chunk_plane_id)?;
    self.chunk_plane_ids.push(chunk_plane_id.clone());
    self.chunk_planes.insert(chunk_plane_id.clone(), chunk_plane);
    Ok(())
  }

  /// Unloads an existing `ChunkPlane`.
  pub fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    self.chunk_plane_ids.retain(|id| id != chunk_plane_id);
    self.chunk_planes.remove(chunk_plane_id);
    Ok(())
  }

  /// Stores a specified `ChunkPlane`.
  pub fn store_chunk_plane(&self, base_path: &str, chunk_plane: &ChunkPlane) -> Result<(), AnyError> {
    chunk_plane.store(base_path)?;
    Ok(())
  }

  /// Get the specified `ChunkPlane`.
  pub fn get_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Option<&ChunkPlane> {
    self.chunk_planes.get(chunk_plane_id)
  }

  /// Get the specified `ChunkPlane`, mutably.
  pub fn get_chunk_plane_mut(&mut self, chunk_plane_id: &ChunkPlaneId) -> Option<&mut ChunkPlane> {
    self.chunk_planes.get_mut(chunk_plane_id)
  }

  /// Store all of the `ChunkPlane`s.
  pub fn store_all_chunk_planes(&self, base_path: &str) -> Result<(), AnyError> {
    create_dir_all(base_path).with_context(|| format!("Unable to create directory at {}", base_path))?;
    for chunk_plane_id in &self.chunk_plane_ids {
      if let Some(chunk_plane) = self.get_chunk_plane(chunk_plane_id) {
        chunk_plane.store_all(base_path)?;
      }
    }
    Ok(())
  }

  /// Load all of the `ChunkPlane`s.
  pub fn load_all_chunk_planes(&mut self, base_path: &str) -> Result<(), AnyError> {
    for chunk_plane_id in &self.chunk_plane_ids.clone() {
      self.load_chunk_plane(base_path, chunk_plane_id)?;
    }
    Ok(())
  }

  /// Saves the `ChunkManager` data in a serialized form.
  pub fn store(&self, base_dir: &str) -> Result<(), AnyError> {
    create_dir_all(base_dir).with_context(|| format!("Unable to create directory at {}", base_dir))?;
    let file_path = format!("{}/{}", base_dir, "chunk_manager.yaml");
    let yaml_string = serde_yaml_to_string(self)?;
    let mut file =
      File::create(file_path.clone()).with_context(|| format!("Unable to create directory at {}", file_path))?;
    file
      .write_all(yaml_string.as_bytes())
      .expect("Unable to write chunk manager data");
    Ok(())
  }

  /// Loads the `ChunkManager` data from a serialized form.
  pub fn load(base_dir: &str) -> Result<ChunkManager, AnyError> {
    let file_path = format!("{}/{}", base_dir, "chunk_manager.yaml");
    let file = File::open(file_path.clone()).with_context(|| format!("Unable to create directory at {}", file_path))?;
    let chunk_manager: ChunkManager = serde_yaml_from_reader(file)?;
    Ok(chunk_manager)
  }

  /// Stores all of the (loaded) data in a serialized form.
  pub fn store_all(&self, base_path: &str) -> Result<(), AnyError> {
    remove_dir_all(base_path).ok();
    create_dir_all(base_path).with_context(|| format!("Unable to create directory at {}", base_path))?;
    let chunk_planes_path = format!("{}/{}", base_path, "chunk_planes");
    create_dir_all(chunk_planes_path.clone())
      .with_context(|| format!("Unable to create directory at {}", chunk_planes_path))?;
    self.store_all_chunk_planes(&chunk_planes_path)?;
    self.store(base_path)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use anyhow::Error as AnyError;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_manager() -> Result<(), AnyError> {
    init();
    let mut chunk_manager = ChunkManager::new();
    let chunk_plane_id = chunk_manager.create_chunk_plane("test").unwrap();
    {
      let chunk_plane = chunk_manager.get_chunk_plane_mut(&chunk_plane_id).unwrap();
      chunk_plane.generate_initial_chunks()?;
    }
    chunk_manager
      .store_all(&format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_manager"))
      .unwrap();
    Ok(())
  }

  #[test]
  fn test_chunk_manager_load() -> Result<(), AnyError> {
    init();
    let mut chunk_manager = ChunkManager::new();
    let base_dir = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_manager_load");
    let chunk_plane_id = chunk_manager.create_chunk_plane("test").unwrap();
    {
      let chunk_plane = chunk_manager.get_chunk_plane_mut(&chunk_plane_id).unwrap();
      chunk_plane.generate_initial_chunks().unwrap();
    }
    chunk_manager.store_all(&base_dir).unwrap();
    let mut chunk_manager = ChunkManager::load(&base_dir).unwrap();
    let chunk_plane_id = chunk_manager.create_chunk_plane("test").unwrap();
    {
      let chunk_plane = chunk_manager.get_chunk_plane_mut(&chunk_plane_id).unwrap();
      chunk_plane.generate_initial_chunks()?;
    }
    chunk_manager.store_all(&base_dir).unwrap();
    Ok(())
  }

  #[test]
  fn test_chunk_manager_store_all() -> Result<(), AnyError> {
    init();
    let base_dir = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_manager_store_all");
    let mut chunk_manager = ChunkManager::new();
    let chunk_plane_id = chunk_manager
      .create_chunk_plane("test_chunk_manager_store_all")
      .unwrap();
    {
      let chunk_plane = chunk_manager.get_chunk_plane_mut(&chunk_plane_id).unwrap();
      chunk_plane.generate_initial_chunks()?;
    }
    chunk_manager.store_all(&base_dir).unwrap();
    Ok(())
  }
}

/*
  /// The ID.
  pub id: ChunkPlaneId,
  /// The `Chunk` IDs.
  pub chunk_ids: Vec<ChunkId>,
  /// The `ChunkSeed`s.
  pub chunk_seeds: HashMap<ChunkSeedId, ChunkSeed>,
  /// The upper-left corner of the `ChunkPlane` in (i64, i64) plane.
  pub upper_left_corner: (i64, i64),
  /// The lower-right corner of the `ChunkPlane` in (i64, i64) plane.
  pub lower_right_corner: (i64, i64),
  /// The seed string.
  pub seed_string: String,
  /// The `Chunk`s, only managed as needed.
  #[serde(skip)]
  pub chunks: HashMap<ChunkId, Chunk>,
*/
