use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk::ChunkFileManager;
use crate::chunk_plane::ChunkPlane;
use crate::chunk_plane::ChunkPlaneFileManager;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkFileService` service.
///
/// This is the service that reads and writes chunks and chunk planes to and
/// from the filesystem.
///
/// This does not handle the creation of chunks or chunk planes. That is the
/// responsibility of the `ChunkCreationService`.
///
/// This also does not handle the loading of chunks or chunk planes. That is the
/// responsibility of the `ChunkLoaderService`.
#[derive(Clone, Debug, Default)]
pub struct ChunkFileService {
  /// The base path.
  pub base_path: String,
  /// The `ChunkPlane` file manager.
  pub chunk_plane_file_manager: ChunkPlaneFileManager,
  /// The `Chunk` file manager.
  pub chunk_file_manager: ChunkFileManager,
}

impl ChunkFileService {
  /// Creates a new `ChunkFileService`.
  pub fn new(base_path: &str) -> Self {
    let cpfm_base_path = format!("{}/{}", base_path, "chunk_planes");
    let cfm_base_path = format!("{}/{}", base_path, "chunks");
    let base_path = base_path.to_string();
    let chunk_plane_file_manager = ChunkPlaneFileManager::new(&cpfm_base_path);
    let chunk_file_manager = ChunkFileManager::new(&cfm_base_path);
    Self {
      base_path,
      chunk_plane_file_manager,
      chunk_file_manager,
    }
  }

  /// Opens an existing `ChunkPlane`.
  pub fn open_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError> {
    let chunk_plane = self.chunk_plane_file_manager.open(chunk_plane_id)?;
    Ok(chunk_plane)
  }

  /// Saves an existing `ChunkPlane`.
  pub fn save_chunk_plane(&mut self, chunk_plane: &ChunkPlane) -> Result<(), AnyError> {
    self.chunk_plane_file_manager.save(chunk_plane)?;
    Ok(())
  }

  /// Save a specific `Chunk`.
  pub fn save_chunk(&mut self, chunk: &Chunk) -> Result<(), AnyError> {
    self.chunk_file_manager.save(chunk)?;
    Ok(())
  }

  /// Save multiple `Chunk`s.
  pub fn save_chunks(&mut self, chunks: &Vec<Chunk>) -> Result<(), AnyError> {
    self.chunk_file_manager.save_multiple(chunks)?;
    Ok(())
  }

  /// Open a specific `Chunk`.
  pub fn open_chunk(&mut self, chunk_id: &ChunkId) -> Result<Chunk, AnyError> {
    let chunk = self.chunk_file_manager.open(chunk_id)?;
    Ok(chunk)
  }

  /// Open all of the `Chunk`s for a `ChunkPlane`.
  pub fn open_chunks(&mut self, chunk_plane: &ChunkPlane) -> Result<Vec<Chunk>, AnyError> {
    let chunk_ids = chunk_plane.chunk_ids.clone();
    let chunks = self
      .chunk_file_manager
      .open_multiple(&chunk_ids.iter().cloned().collect())?;
    Ok(chunks)
  }
}
