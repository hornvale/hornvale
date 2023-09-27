/// The `InputQueue` trait.
pub trait InputQueue {
  /// Enqueues input.
  fn enqueue_input(&mut self, input: String);
  /// Dequeues input.
  fn dequeue_input(&mut self) -> Option<String>;
}
