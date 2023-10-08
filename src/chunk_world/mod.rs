use std::collections::HashSet;

use crate::entity_id::ChunkPlaneId;

pub mod file_manager;
pub use file_manager::FileManager as ChunkWorldFileManager;

/// The `ChunkWorld` struct.
///
/// This struct contains a set of `ChunkPlane` structs.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct ChunkWorld {
  /// The set of `ChunkPlaneId`s.
  chunk_plane_ids: HashSet<ChunkPlaneId>,
  /// The primary `ChunkPlaneId`.
  primary_chunk_plane_id: Option<ChunkPlaneId>,
}

impl ChunkWorld {
  /// Adds a `ChunkPlaneId` to the `ChunkWorld`.
  pub fn add_chunk_plane_id(&mut self, chunk_plane_id: &ChunkPlaneId) {
    self.chunk_plane_ids.insert(chunk_plane_id.clone());
  }

  /// Gets the primary `ChunkPlaneId`.
  pub fn get_primary_chunk_plane_id(&self) -> Option<ChunkPlaneId> {
    self.primary_chunk_plane_id.clone()
  }

  /// Sets the primary `ChunkPlaneId`.
  pub fn set_primary_chunk_plane_id(&mut self, chunk_plane_id: &ChunkPlaneId) {
    self.chunk_plane_ids.insert(chunk_plane_id.clone());
    self.primary_chunk_plane_id = Some(chunk_plane_id.clone());
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    let chunk_world = ChunkWorld::default();
    assert_eq!(chunk_world.chunk_plane_ids.len(), 0);
    assert_eq!(chunk_world.primary_chunk_plane_id, None);
  }

  #[test]
  fn test_add_chunk_plane_id() {
    let mut chunk_world = ChunkWorld::default();
    let chunk_plane_id = ChunkPlaneId::default();
    chunk_world.add_chunk_plane_id(&chunk_plane_id);
    assert_eq!(chunk_world.chunk_plane_ids.len(), 1);
  }

  #[test]
  fn test_set_primary_chunk_plane_id() {
    let mut chunk_world = ChunkWorld::default();
    let chunk_plane_id = ChunkPlaneId::default();
    chunk_world.set_primary_chunk_plane_id(&chunk_plane_id);
    assert_eq!(chunk_world.primary_chunk_plane_id, Some(chunk_plane_id));
  }
}
