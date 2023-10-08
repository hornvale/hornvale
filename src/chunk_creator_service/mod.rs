use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk::ChunkFileManager;
use crate::chunk::ChunkMapBuilder;
use crate::chunk::ChunkMapBuilderStrategy;
use crate::chunk::ChunkStatus;
use crate::chunk_plane::ChunkPlane;
use crate::chunk_plane::ChunkPlaneFileManager;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkCreatorService` service.
///
/// This is responsible for creating chunks and chunk planes, particularly at
/// the start of the game.
#[derive(Clone, Debug, Default)]
pub struct ChunkCreatorService {
  /// The base path.
  pub base_path: String,
  /// The seed string.
  pub seed_string: String,
  /// The `ChunkPlane` file manager.
  pub chunk_plane_file_manager: ChunkPlaneFileManager,
  /// The `Chunk` file manager.
  pub chunk_file_manager: ChunkFileManager,
}

impl ChunkCreatorService {
  /// Creates a new `ChunkFileService`.
  pub fn new(base_path: &str, seed_string: &str) -> Self {
    let seed_string = format!("{}{}", seed_string, "ChunkCreatorService");
    let cpfm_base_path = format!("{}/{}", base_path, "chunk_planes");
    let cfm_base_path = format!("{}/{}", base_path, "chunks");
    let base_path = base_path.to_string();
    let chunk_plane_file_manager = ChunkPlaneFileManager::new(&cpfm_base_path);
    let chunk_file_manager = ChunkFileManager::new(&cfm_base_path);
    Self {
      base_path,
      seed_string,
      chunk_plane_file_manager,
      chunk_file_manager,
    }
  }

  /// Creates a new `ChunkPlane`.
  pub fn create_chunk_plane(&mut self) -> Result<ChunkPlane, AnyError> {
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_plane = ChunkPlane::new(&chunk_plane_id, &self.seed_string.clone());
    self.chunk_plane_file_manager.save(&chunk_plane)?;
    Ok(chunk_plane)
  }

  /// Generates the initial `Chunk`s for a `ChunkPlane`.
  pub fn generate_initial_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError> {
    let chunks = chunk_plane.generate_initial_chunks()?;
    Ok(chunks)
  }

  /// Map an empty chunk.
  pub fn map_empty_chunk(&mut self, chunk: &mut Chunk) -> Result<(), AnyError> {
    let chunk_map_builder = ChunkMapBuilder::new(ChunkMapBuilderStrategy::CompassRose);
    chunk_map_builder.map_chunk(chunk)?;
    chunk.status = ChunkStatus::Mapped;
    Ok(())
  }
}
