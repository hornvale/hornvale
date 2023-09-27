/// The `OutputQueue` trait.
pub trait OutputQueue {
  /// Enqueues an output.
  fn enqueue_output(&mut self, output: String);
  /// Dequeues an output.
  fn dequeue_output(&mut self) -> Option<String>;
}
