use anyhow::Context;
use anyhow::Error as AnyError;
use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::Write;

use crate::chunk_world::ChunkWorld;

/// The `ChunkWorldFileManager` struct.
///
/// This struct is responsible for:
/// - loading chunk worlds from disk
/// - saving chunk worlds to disk
#[derive(Clone, Debug, Default)]
pub struct FileManager {
  /// The base path.
  pub base_path: String,
}

impl FileManager {
  /// Creates a new `ChunkWorldFileManager`.
  pub fn new(base_path: &str) -> Self {
    Self {
      base_path: base_path.to_string(),
    }
  }

  /// Open a `ChunkWorld` from disk.
  pub fn open(&self) -> Result<ChunkWorld, AnyError> {
    let file_path = format!("{}/chunk_world.yaml", self.base_path);
    let file =
      File::open(file_path.clone()).with_context(|| format!("Unable to read chunk world file at {}", file_path))?;
    let chunk_world: ChunkWorld = serde_yaml_from_reader(file)?;
    Ok(chunk_world)
  }

  /// Saves the `ChunkWorld` in a serialized form.
  pub fn save(&self, chunk_world: &ChunkWorld) -> Result<(), AnyError> {
    create_dir_all(&self.base_path).with_context(|| format!("Unable to create directory at {}", self.base_path))?;
    let file_path = format!("{}/chunk_world.yaml", self.base_path);
    let yaml_string = serde_yaml_to_string(chunk_world)?;
    let mut file =
      File::create(file_path.clone()).with_context(|| format!("Unable to create file at {}", file_path))?;
    file
      .write_all(yaml_string.as_bytes())
      .with_context(|| format!("Unable to write chunk world at {}", file_path))?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::fs::remove_dir_all;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_world_file_manager() -> Result<(), AnyError> {
    init();
    let chunk_world_file_manager = FileManager::new(&format!(
      "{}/{}",
      TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_world_file_manager"
    ));
    remove_dir_all(&chunk_world_file_manager.base_path).ok();
    let chunk_world = ChunkWorld::default();
    chunk_world_file_manager.save(&chunk_world)?;
    let _chunk_world = chunk_world_file_manager.open()?;
    Ok(())
  }
}
