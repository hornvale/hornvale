use anyhow::Error as AnyError;
use std::path::PathBuf;

use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkPlaneId;
use crate::game_state::ChunkPlaneManagementTrait;
use crate::game_state::GameState;
use crate::game_state::LocalDataDirTrait;

/// Implementation of the `ChunkPlaneManagement` trait.
impl ChunkPlaneManagementTrait for GameState {
  /// Gets the path to the `ChunkPlane`.
  fn get_chunk_plane_path(&self, chunk_plane_id: &ChunkPlaneId) -> PathBuf {
    let mut path = self.get_local_data_dir().to_path_buf();
    path.push("chunk_planes");
    path.push(format!("{}.yml", chunk_plane_id));
    path
  }

  /// Loads the `ChunkPlane`.
  fn load_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError> {
    let path = self.get_chunk_plane_path(chunk_plane_id);
    let chunk_plane = ChunkPlane::load(&path.to_string_lossy())?;
    Ok(chunk_plane)
  }

  /// Unloads the `ChunkPlane`.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    self.chunk_planes.remove(chunk_plane_id);
    Ok(())
  }

  /// Stores the `ChunkPlane`.
  fn store_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    let path = self.get_chunk_plane_path(chunk_plane_id);
    let chunk_plane = self.get_chunk_plane(chunk_plane_id).unwrap();
    chunk_plane.store(&path.to_string_lossy())?;
    Ok(())
  }

  /// Checks if the `ChunkPlane` is loaded.
  fn is_chunk_plane_loaded(&self, chunk_plane_id: &ChunkPlaneId) -> bool {
    self.chunk_planes.contains_key(chunk_plane_id)
  }

  /// Gets the `ChunkPlane`.
  fn get_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Option<&ChunkPlane> {
    self.chunk_planes.get(chunk_plane_id)
  }

  /// Gets the `ChunkPlane` mutably.
  fn get_chunk_plane_mut(&mut self, chunk_plane_id: &ChunkPlaneId) -> Option<&mut ChunkPlane> {
    self.chunk_planes.get_mut(chunk_plane_id)
  }

  /// Sets the `ChunkPlane`.
  fn set_chunk_plane(&mut self, chunk_plane: ChunkPlane) -> Result<(), AnyError> {
    self.chunk_planes.insert(chunk_plane.id.clone(), chunk_plane);
    Ok(())
  }
}
