use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `EventQueue` trait.
pub trait EventQueue {
  /// Enqueues an event.
  fn enqueue_event(&mut self, event: Box<dyn EventTrait<GameState>>);
  /// Dequeues an event.
  fn dequeue_event(&mut self) -> Option<Box<dyn EventTrait<GameState>>>;
}
