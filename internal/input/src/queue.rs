use std::collections::VecDeque;

/// A wrapper around a double-ended queue for input.
#[derive(Debug, Default)]
pub struct Queue {
  queue: VecDeque<String>,
}

impl Queue {
  /// Create a new Queue.
  pub fn new() -> Self {
    Self { queue: VecDeque::new() }
  }

  /// Enqueue a string.
  pub fn enqueue(&mut self, input: String) {
    self.queue.push_back(input);
  }

  /// Dequeue a string.
  pub fn dequeue(&mut self) -> Option<String> {
    self.queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_enqueue_dequeue() {
    init();
    let mut queue = Queue::new();
    queue.enqueue("test".to_string());
    queue.enqueue("test2".to_string());
    assert_eq!(queue.dequeue(), Some("test".to_string()));
    assert_eq!(queue.dequeue(), Some("test2".to_string()));
    assert_eq!(queue.dequeue(), None);
  }
}
