use crate::event::Event;

/// The `EventQueue` trait.
pub trait EventQueue {
  /// Enqueues an event.
  fn enqueue_event(&mut self, event: Event);
  /// Dequeues an event.
  fn dequeue_event(&mut self) -> Option<Event>;
}
