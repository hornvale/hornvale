use crate::command::CommandTrait;
use crate::game_state::GameState;

/// The `CommandQueue` trait.
pub trait CommandQueue {
  /// Enqueues a command.
  fn enqueue_command(&mut self, command: Box<dyn CommandTrait<GameState>>);
  /// Dequeues a command.
  fn dequeue_command(&mut self) -> Option<Box<dyn CommandTrait<GameState>>>;
}
