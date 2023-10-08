use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkFileServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkFileService` trait.
impl ChunkFileServiceTrait for GameState {
  /// Open a chunk.
  fn open_chunk(&mut self, chunk_id: &ChunkId) -> Result<Chunk, AnyError> {
    let chunk = self.chunk_file_service.open_chunk(chunk_id)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkIsOpened(chunk_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(chunk)
  }

  /// Save multiple chunks.
  fn save_chunks(&mut self, chunks: &[Chunk]) -> Result<(), AnyError> {
    chunks.iter().for_each(|chunk| {
      self.save_chunk(chunk).unwrap();
    });
    Ok(())
  }

  /// Save a chunk.
  fn save_chunk(&mut self, chunk: &Chunk) -> Result<(), AnyError> {
    let chunk_id = chunk.id.clone();
    self.chunk_file_service.save_chunk(chunk)?;
    let event = EventBuilder::new().r#type(EventType::ChunkIsSaved(chunk_id)).build();
    self.enqueue_event(event);
    Ok(())
  }

  /// Open a chunk plane.
  fn open_chunk_plane(&mut self, chunk_plane_id: &ChunkPlaneId) -> Result<ChunkPlane, AnyError> {
    let chunk_plane = self.chunk_file_service.open_chunk_plane(chunk_plane_id)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsOpened(chunk_plane_id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(chunk_plane)
  }

  /// Save a chunk plane.
  fn save_chunk_plane(&mut self, chunk_plane: &ChunkPlane) -> Result<(), AnyError> {
    let chunk_plane_id = chunk_plane.id.clone();
    self.chunk_file_service.save_chunk_plane(chunk_plane)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsSaved(chunk_plane_id))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
}
