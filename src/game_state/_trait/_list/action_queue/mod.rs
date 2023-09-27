use crate::action::ActionTrait;
use crate::game_state::GameState;

/// The `ActionQueue` trait.
pub trait ActionQueue {
  /// Enqueues a command.
  fn enqueue_action(&mut self, action: Box<dyn ActionTrait<GameState>>);
  /// Dequeues a command.
  fn dequeue_action(&mut self) -> Option<Box<dyn ActionTrait<GameState>>>;
}
