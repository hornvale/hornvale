use anyhow::Error as AnyError;
use std::collections::HashSet;

use crate::chunk::Chunk;
use crate::chunk::ChunkFileManager;
use crate::chunk::ChunkMapBuilder;
use crate::chunk::ChunkMapBuilderStrategy;
use crate::chunk::ChunkStatus;
use crate::chunk_plane::ChunkPlane;
use crate::chunk_plane::ChunkPlaneFileManager;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkManager` struct.
///
/// This indirectly manages most of the chunk-related functionality.
///
/// This struct is responsible for orchestrating:
/// - creating and destroying `ChunkPlane`s
/// - creating new, empty `Chunk`s by their `ChunkPlane`s
/// - populating empty `Chunk`s with the `ChunkFactory`.
#[derive(Clone, Debug, Default)]
pub struct Manager {
  /// The seed string.
  pub seed_string: String,
  /// The base path.
  pub base_path: String,
  /// The `ChunkPlane` file manager.
  pub chunk_plane_file_manager: ChunkPlaneFileManager,
  /// The `Chunk` file manager.
  pub chunk_file_manager: ChunkFileManager,
  /// The `ChunkPlane`s IDs.
  pub chunk_plane_ids: HashSet<ChunkPlaneId>,
}

impl Manager {
  /// Creates a new `ChunkManager`.
  pub fn new(seed_string: &str, base_path: &str) -> Self {
    let seed_string = format!("{}{}", seed_string, "ChunkManager");
    let cpfm_base_path = format!("{}/{}", base_path, "chunk_planes");
    let cfm_base_path = format!("{}/{}", base_path, "chunks");
    let base_path = base_path.to_string();
    let chunk_plane_file_manager = ChunkPlaneFileManager::new(&cpfm_base_path);
    let chunk_file_manager = ChunkFileManager::new(&cfm_base_path);
    let chunk_plane_ids = HashSet::new();
    Self {
      seed_string,
      base_path,
      chunk_plane_file_manager,
      chunk_file_manager,
      chunk_plane_ids,
    }
  }

  /// Creates a new `ChunkPlane`.
  pub fn create_chunk_plane(&mut self) -> Result<ChunkPlane, AnyError> {
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_plane = ChunkPlane::new(&chunk_plane_id, &self.seed_string.clone());
    self.chunk_plane_ids.insert(chunk_plane_id);
    Ok(chunk_plane)
  }

  /// Loads an existing `ChunkPlane`.
  pub fn load_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError> {
    let chunk_plane = self.chunk_plane_file_manager.load(chunk_plane_id)?;
    self.chunk_plane_ids.insert(chunk_plane_id.clone());
    Ok(chunk_plane)
  }

  /// Stores an existing `ChunkPlane`.
  pub fn store_chunk_plane(&mut self, chunk_plane: &ChunkPlane) -> Result<(), AnyError> {
    self.chunk_plane_file_manager.store(chunk_plane)?;
    self.chunk_plane_ids.insert(chunk_plane.id.clone());
    Ok(())
  }

  /// Generates the initial `Chunk`s for a `ChunkPlane`.
  pub fn generate_initial_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError> {
    let chunks = chunk_plane.generate_initial_chunks()?;
    self.store_chunk_plane(chunk_plane)?;
    self.store_chunks(&chunks)?;
    Ok(chunks)
  }

  /// Stores the `Chunk`s for a `ChunkPlane`.
  pub fn store_chunks(&mut self, chunks: &Vec<Chunk>) -> Result<(), AnyError> {
    self.chunk_file_manager.store_multiple(chunks)?;
    Ok(())
  }

  /// Map empty chunks.
  pub fn map_empty_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<(), AnyError> {
    let chunk_ids = chunk_plane.chunk_ids.clone();
    let empty_chunks = self
      .chunk_file_manager
      .load_multiple(&chunk_ids.iter().cloned().collect())?
      .iter()
      .filter(|chunk| chunk.status == ChunkStatus::Empty)
      .cloned()
      .collect::<Vec<Chunk>>();
    for mut empty_chunk in empty_chunks {
      self.map_empty_chunk(&mut empty_chunk)?;
    }
    Ok(())
  }

  /// Map an empty chunk.
  pub fn map_empty_chunk(&mut self, chunk: &mut Chunk) -> Result<(), AnyError> {
    let chunk_map_builder = ChunkMapBuilder::new(ChunkMapBuilderStrategy::BlankFill);
    chunk_map_builder.map_chunk(chunk)?;
    chunk.status = ChunkStatus::Mapped;
    self.chunk_file_manager.store(chunk)?;
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
    let seed_string = "test_chunk_manager";
    let base_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_manager");
    let mut chunk_manager = Manager::new(seed_string, &base_path);
    let mut chunk_plane = chunk_manager.create_chunk_plane()?;
    let chunk_plane_id = chunk_plane.id.clone();
    chunk_plane.generate_initial_chunks()?;
    let chunk_count = chunk_plane.chunk_ids.len();
    assert_ge!(chunk_count, 5);
    chunk_manager.store_chunk_plane(&chunk_plane)?;
    let chunk_plane = chunk_manager.load_chunk_plane(&chunk_plane.id)?;
    assert_eq!(chunk_plane.id, chunk_plane_id);
    assert_eq!(chunk_plane.seed_string, "test_chunk_managerChunkManager");
    assert_eq!(chunk_plane.chunk_ids.len(), chunk_count);
    assert_ne!(chunk_plane.upper_left_corner, (0, 0));
    assert_ne!(chunk_plane.lower_right_corner, (0, 0));
    assert_ne!(chunk_plane.chunk_seeds.len(), 0);
    Ok(())
  }
}
