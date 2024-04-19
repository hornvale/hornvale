use crate::prelude::*;
use hecs::{Entity, World};
use std::sync::OnceLock;

/// Get the output entity.
pub fn get_out_entity(world: &mut World) -> &'static Entity {
  static ENTITY: OnceLock<Entity> = OnceLock::new();
  ENTITY.get_or_init(|| world.spawn((StdoutQueue::default(),)))
}

/// Get the error output entity.
pub fn get_err_entity(world: &mut World) -> &'static Entity {
  static ENTITY: OnceLock<Entity> = OnceLock::new();
  ENTITY.get_or_init(|| world.spawn((StderrQueue::default(),)))
}

/// Enqueue an output message.
pub fn enqueue_output(world: &mut World, message: String) {
  let entity = get_out_entity(world);
  let queue = world.query_one_mut::<&mut StdoutQueue>(*entity).unwrap();
  queue.0.enqueue(message);
}

/// Enqueue an error message.
pub fn enqueue_error(world: &mut World, message: String) {
  let entity = get_err_entity(world);
  let queue = world.query_one_mut::<&mut StderrQueue>(*entity).unwrap();
  queue.0.enqueue(message);
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
    let entity = get_out_entity(&mut world);
    let queue = world.query_one_mut::<&mut StdoutQueue>(*entity).unwrap();
    assert_eq!(queue.0.dequeue(), Some("test".to_string()));
    assert_eq!(queue.0.dequeue(), Some("test2".to_string()));
  }

  #[test]
  fn test_enqueue_error() {
    init();
    let mut world = World::new();
    enqueue_error(&mut world, "test".to_string());
    enqueue_error(&mut world, "test2".to_string());
    let entity = get_err_entity(&mut world);
    let queue = world.query_one_mut::<&mut StderrQueue>(*entity).unwrap();
    assert_eq!(queue.0.dequeue(), Some("test".to_string()));
    assert_eq!(queue.0.dequeue(), Some("test2".to_string()));
  }
}
