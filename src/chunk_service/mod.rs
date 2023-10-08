use anyhow::Error as AnyError;
use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkService` struct.
///
/// The Chunk Service is responsible for managing the space of the game world.
///
/// Chiefly, this means loading and unloading chunks and chunk planes as events
/// dictate. It is a passive service, in that it does not initiate any actions
/// on its own. It only responds to events.
///
/// These various actions will have repercussions throughout the system, and
/// therefore we try to keep this service as simple as possible.
///
/// This is concerned only with the in-memory operations. Filesystem operations
/// are handled by the Chunk Filesystem Service.
#[derive(Debug, Default)]
pub struct ChunkService {
  // We really only need two data structures here:
  // - A HashMap of ChunkPlaneId to ChunkPlane.
  // - A HashMap of ChunkId to Chunk.
  /// The chunk planes, keyed by their IDs.
  chunk_planes: HashMap<ChunkPlaneId, ChunkPlane>,
  /// The chunks, keyed by their IDs.
  chunks: HashMap<ChunkId, Chunk>,
}

impl ChunkService {
  /// Create a new Chunk Service.
  pub fn new() -> Self {
    Self {
      chunk_planes: HashMap::new(),
      chunks: HashMap::new(),
    }
  }

  /// Load a chunk plane.
  ///
  /// Note that this consumes the chunk plane.
  pub fn load_chunk_plane(&mut self, chunk_plane: ChunkPlane) -> Result<(), AnyError> {
    self.chunk_planes.insert(chunk_plane.id.clone(), chunk_plane);
    Ok(())
  }

  /// Unload a chunk plane.
  pub fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    self.chunk_planes.remove(chunk_plane_id);
    Ok(())
  }

  /// Load a chunk.
  ///
  /// Note that this consumes the chunk.
  pub fn load_chunk(&mut self, chunk: Chunk) -> Result<(), AnyError> {
    self.chunks.insert(chunk.id.clone(), chunk);
    Ok(())
  }

  /// Unload a chunk.
  pub fn unload_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError> {
    self.chunks.remove(chunk_id);
    Ok(())
  }

  /// Get a chunk plane.
  pub fn get_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Option<&ChunkPlane> {
    self.chunk_planes.get(chunk_plane_id)
  }

  /// Get a chunk plane mutably.
  pub fn get_chunk_plane_mut(&mut self, chunk_plane_id: &ChunkPlaneId) -> Option<&mut ChunkPlane> {
    self.chunk_planes.get_mut(chunk_plane_id)
  }

  /// Get a chunk.
  pub fn get_chunk(&self, chunk_id: &ChunkId) -> Option<&Chunk> {
    self.chunks.get(chunk_id)
  }

  /// Get a chunk mutably.
  pub fn get_chunk_mut(&mut self, chunk_id: &ChunkId) -> Option<&mut Chunk> {
    self.chunks.get_mut(chunk_id)
  }
}
