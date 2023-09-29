use crate::command::Command;

/// The `CommandQueue` trait.
pub trait CommandQueue {
  /// Enqueues a command.
  fn enqueue_command(&mut self, command: Command);
  /// Dequeues a command.
  fn dequeue_command(&mut self) -> Option<Command>;
}
