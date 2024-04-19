use crate::prelude::*;
use hecs::World;

/// Enqueue an output message.
pub fn enqueue_output(world: &mut World, message: String) {
  if let Some(queue) = world.query_mut::<&mut StdoutQueue>().into_iter().next() {
    queue.1 .0.enqueue(message);
  } else {
    let mut queue = StdoutQueue::default();
    queue.0.enqueue(message);
    world.spawn((queue,));
  }
}

/// Enqueue an error message.
pub fn enqueue_error(world: &mut World, message: String) {
  if let Some(queue) = world.query_mut::<&mut StderrQueue>().into_iter().next() {
    queue.1 .0.enqueue(message);
  } else {
    let mut queue = StderrQueue::default();
    queue.0.enqueue(message);
    world.spawn((queue,));
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_enqueue_output() {
    init();
    let mut world = World::new();
    enqueue_output(&mut world, "test".to_string());
    enqueue_output(&mut world, "test2".to_string());
    let queue = world.query_mut::<&mut StdoutQueue>().into_iter().next().unwrap();
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test");
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test2");
  }

  #[test]
  fn test_enqueue_error() {
    init();
    let mut world = World::new();
    enqueue_error(&mut world, "test".to_string());
    enqueue_error(&mut world, "test2".to_string());
    let queue = world.query_mut::<&mut StderrQueue>().into_iter().next().unwrap();
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test");
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test2");
  }
}
