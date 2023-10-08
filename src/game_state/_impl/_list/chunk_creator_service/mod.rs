use anyhow::Error as AnyError;

use crate::chunk::Chunk;
use crate::chunk_plane::ChunkPlane;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkCreatorServiceTrait;
use crate::game_state::ChunkFileServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkCreatorService` trait.
impl ChunkCreatorServiceTrait for GameState {
  /// Creates a new `ChunkPlane`.
  fn create_chunk_plane(&mut self) -> Result<ChunkPlane, AnyError> {
    let chunk_plane = self.chunk_creator_service.create_chunk_plane()?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkPlaneIsCreated(chunk_plane.id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(chunk_plane)
  }

  /// Generates the initial `Chunk`s for a `ChunkPlane`.
  fn generate_initial_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError> {
    let chunks = self.chunk_creator_service.generate_initial_chunks(chunk_plane)?;
    for chunk in chunks.iter() {
      let event = EventBuilder::new()
        .r#type(EventType::ChunkIsCreated(chunk.id.clone()))
        .build();
      self.enqueue_event(event);
    }
    Ok(chunks)
  }

  /// Map empty chunks.
  fn map_empty_chunks(&mut self, chunk_plane: &mut ChunkPlane) -> Result<Vec<Chunk>, AnyError> {
    let mut chunks = chunk_plane
      .chunk_ids
      .iter()
      .map(|chunk_id| self.open_chunk(chunk_id).unwrap())
      .collect();
    self.map_chunks(&mut chunks)?;
    Ok(chunks)
  }

  /// Map chunks.
  fn map_chunks(&mut self, chunks: &mut Vec<Chunk>) -> Result<(), AnyError> {
    for chunk in chunks.iter_mut() {
      self.map_chunk(chunk)?;
    }
    Ok(())
  }

  /// Map a chunk.
  fn map_chunk(&mut self, chunk: &mut Chunk) -> Result<(), AnyError> {
    self.chunk_creator_service.map_empty_chunk(chunk)?;
    let event = EventBuilder::new()
      .r#type(EventType::ChunkIsMapped(chunk.id.clone()))
      .build();
    self.enqueue_event(event);
    Ok(())
  }
}
