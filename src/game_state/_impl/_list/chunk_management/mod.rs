use anyhow::Error as AnyError;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkManagementTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// Implementation of the `ChunkManagement` trait.
impl ChunkManagementTrait for GameState {
  /// Load a chunk.
  fn load_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError> {
    // Load the chunk.
    let chunk = self.chunk_manager.load_chunk(chunk_id).unwrap();
    // Insert the chunk into the loaded chunks.
    self.loaded_chunks.insert(chunk_id.clone(), chunk);
    let event = EventBuilder::new()
      .r#type(EventType::ChunkIsLoaded(chunk_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
  /// Unload a chunk.
  fn unload_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError> {
    // Remove the chunk from the loaded chunks.
    self.loaded_chunks.remove(chunk_id);
    let event = EventBuilder::new()
      .r#type(EventType::ChunkIsUnloaded(chunk_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
  /// Load a chunk plane.
  fn load_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    // Load the chunk plane.
    let chunk_plane = self.chunk_manager.load_chunk_plane(chunk_plane_id)?;
    // Insert the chunk plane into the loaded chunk planes.
    self.loaded_chunk_planes.insert(chunk_plane_id.clone(), chunk_plane);
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsLoaded(chunk_plane_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
  /// Unload a chunk plane.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    // Remove the chunk plane from the loaded chunk planes.
    self.loaded_chunk_planes.remove(chunk_plane_id);
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsUnloaded(chunk_plane_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
}
