use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkWorldFileServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkWorldFileService` trait.
impl ChunkWorldFileServiceTrait for GameState {
  /// Opens the `ChunkWorld` from disk.
  fn open_chunk_world(&mut self) -> Result<ChunkWorld, AnyError> {
    let chunk_world = self.chunk_world_file_service.open()?;
    let event = EventBuilder::new().event_type(EventType::ChunkWorldIsOpened).build();
    self.enqueue_event(event);
    Ok(chunk_world)
  }

  /// Saves the `ChunkWorld` in a serialized form.
  fn save_chunk_world(&mut self, chunk_world: &ChunkWorld) -> Result<(), AnyError> {
    self.chunk_world_file_service.save(chunk_world)?;
    let event = EventBuilder::new().event_type(EventType::ChunkWorldIsSaved).build();
    self.enqueue_event(event);
    Ok(())
  }
}
