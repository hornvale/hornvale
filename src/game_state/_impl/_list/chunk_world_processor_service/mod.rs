use anyhow::Error as AnyError;

use crate::chunk_world::ChunkWorld;
use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::ChunkWorldProcessorServiceTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `ChunkWorldProcessorService` trait.
impl ChunkWorldProcessorServiceTrait for GameState {
  /// Process the world.
  fn process(&mut self, chunk_world: &mut ChunkWorld) -> Result<(), AnyError> {
    self.chunk_world_processor_service.process(chunk_world)?;
    let event = EventBuilder::new().event_type(EventType::ChunkWorldIsProcessed).build();
    self.enqueue_event(event);
    Ok(())
  }
}
