use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkService` trait.
impl ChunkServiceTrait for GameState {
  /// Load a chunk plane.
  ///
  /// Note that this consumes the chunk plane.
  fn load_chunk_plane(&mut self, chunk_plane: ChunkPlane) -> Result<(), AnyError> {
    let chunk_plane_id = chunk_plane.id.clone();
    self.chunk_service.load_chunk_plane(chunk_plane)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsLoaded(chunk_plane_id))
      .build();
    self.enqueue_event(event);
    Ok(())
  }

  /// Unload a chunk plane.
  fn unload_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<(), AnyError> {
    self.chunk_service.unload_chunk_plane(chunk_plane_id)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsUnloaded(chunk_plane_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }

  /// Load a chunk.
  ///
  /// Note that this consumes the chunk.
  fn load_chunk(&mut self, chunk: Chunk) -> Result<(), AnyError> {
    let chunk_id = chunk.id.clone();
    self.chunk_service.load_chunk(chunk)?;
    let event = EventBuilder::new().r#type(EventType::ChunkIsLoaded(chunk_id)).build();
    self.enqueue_event(event);
    Ok(())
  }

  /// Unload a chunk.
  fn unload_chunk(&mut self, chunk_id: &ChunkId) -> Result<(), AnyError> {
    self.chunk_service.unload_chunk(chunk_id)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkIsUnloaded(chunk_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
}
