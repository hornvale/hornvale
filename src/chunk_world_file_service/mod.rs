use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::chunk_world::ChunkWorldFileManager;

/// The `ChunkWorldFileService` service.
#[derive(Clone, Debug)]
pub struct ChunkWorldFileService {
  /// The base path.
  pub base_path: String,
  /// The `ChunkWorld` file manager.
  pub file_manager: ChunkWorldFileManager,
}

impl ChunkWorldFileService {
  /// Creates a new `ChunkWorldFileService`.
  pub fn new(base_path: &str) -> Self {
    Self {
      base_path: base_path.to_string(),
      file_manager: ChunkWorldFileManager::new(base_path),
    }
  }

  /// Open a `ChunkWorld` from disk.
  pub fn open(&self) -> Result<ChunkWorld, AnyError> {
    let chunk_world = self.file_manager.open()?;
    Ok(chunk_world)
  }

  /// Saves the `ChunkWorld` in a serialized form.
  pub fn save(&self, chunk_world: &ChunkWorld) -> Result<(), AnyError> {
    self.file_manager.save(chunk_world)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_new_chunk_world_file_service() {
    init();
    let base_dir = format!("{}/test_new_chunk_world_file_service", TEMPORARY_TEST_DATA_DIRECTORY);
    let chunk_world_file_service = ChunkWorldFileService::new(&base_dir);
    assert_eq!(chunk_world_file_service.base_path, base_dir);
  }

  #[test]
  fn test_save() -> Result<(), AnyError> {
    init();
    let base_dir = format!("{}/test_open_chunk_world_file_service", TEMPORARY_TEST_DATA_DIRECTORY);
    let chunk_world_file_service = ChunkWorldFileService::new(&base_dir);
    let chunk_world = ChunkWorld::default();
    chunk_world_file_service.save(&chunk_world)?;
    let _chunk_world = chunk_world_file_service.open()?;
    Ok(())
  }
}
