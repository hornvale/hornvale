use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::chunk_world::ChunkWorldFileManager;

/// The `ChunkWorldCreatorService` service.
#[derive(Clone, Debug)]
pub struct ChunkWorldCreatorService {
  /// The base path.
  pub base_path: String,
  /// The `ChunkWorld` file manager.
  pub file_manager: ChunkWorldFileManager,
}

impl ChunkWorldCreatorService {
  /// Creates a new `ChunkWorldCreatorService`.
  pub fn new(base_path: &str) -> Self {
    Self {
      base_path: base_path.to_string(),
      file_manager: ChunkWorldFileManager::new(base_path),
    }
  }

  /// Creates a new `ChunkWorld`.
  pub fn create_chunk_world(&self) -> Result<ChunkWorld, AnyError> {
    let chunk_world = ChunkWorld::default();
    Ok(chunk_world)
  }
}
