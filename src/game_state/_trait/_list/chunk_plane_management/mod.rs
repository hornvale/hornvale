use anyhow::Error as AnyError;
use std::path::PathBuf;

use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkPlaneManagement` trait.
pub trait ChunkPlaneManagement {
  /// Gets the path to the `ChunkPlane`.
  fn get_chunk_plane_path(&self, chunk_plane_id: &ChunkPlaneId) -> PathBuf;

  /// Loads the `ChunkPlane`.
  fn load_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError>;

  /// Unloads the `ChunkPlane`.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError>;

  /// Stores the `ChunkPlane`.
  fn store_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError>;

  /// Checks if the `ChunkPlane` is loaded.
  fn is_chunk_plane_loaded(&self, chunk_plane_id: &ChunkPlaneId) -> bool;

  /// Gets the `ChunkPlane`.
  fn get_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Option<&ChunkPlane>;

  /// Gets the `ChunkPlane` mutably.
  fn get_chunk_plane_mut(&mut self, chunk_plane_id: &ChunkPlaneId) -> Option<&mut ChunkPlane>;

  /// Sets the `ChunkPlane`.
  fn set_chunk_plane(&mut self, chunk_plane: ChunkPlane) -> Result<(), AnyError>;
}
