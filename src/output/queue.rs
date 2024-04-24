use std::collections::VecDeque;

/// A wrapper around a double-ended queue for output.
#[derive(Debug, Default)]
pub struct OutputQueue {
  queue: VecDeque<String>,
}

impl OutputQueue {
  /// Create a new Queue.
  pub fn new() -> Self {
    Self { queue: VecDeque::new() }
  }

  /// Enqueue a string.
  pub fn enqueue(&mut self, output: String) {
    self.queue.push_back(output);
  }

  /// Dequeue a string.
  pub fn dequeue(&mut self) -> Option<String> {
    self.queue.pop_front()
  }

  /// Drain the queue.
  pub fn drain(&mut self) -> Vec<String> {
    self.queue.drain(..).collect()
  }
}

/// A wrapper around a double-ended queue for standard output.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct StdoutQueue(pub OutputQueue);

/// A wrapper around a double-ended queue for error output.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct StderrQueue(pub OutputQueue);

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_enqueue_dequeue() {
    init();
    let mut queue = OutputQueue::new();
    queue.enqueue("test".to_string());
    queue.enqueue("test2".to_string());
    assert_eq!(queue.dequeue(), Some("test".to_string()));
    assert_eq!(queue.dequeue(), Some("test2".to_string()));
    assert_eq!(queue.dequeue(), None);
  }
}
