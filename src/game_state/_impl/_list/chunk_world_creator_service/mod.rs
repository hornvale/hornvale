use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkWorldCreatorServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkWorldCreatorService` trait.
impl ChunkWorldCreatorServiceTrait for GameState {
  /// Creates a new `ChunkWorld`.
  fn create_chunk_world(&mut self) -> Result<ChunkWorld, AnyError> {
    let chunk_world = self.chunk_world_creator_service.create_chunk_world()?;
    let event = EventBuilder::new().event_type(EventType::ChunkWorldIsCreated).build();
    self.enqueue_event(event);
    Ok(chunk_world)
  }
}
