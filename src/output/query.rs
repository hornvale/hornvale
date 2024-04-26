use crate::database::prelude::*;
use crate::output::prelude::*;

/// Enqueue an output message.
pub fn enqueue_output(database: &mut Database, message: String) {
  if let Some(queue) = database.world.query_mut::<&mut StdoutQueue>().into_iter().next() {
    queue.1 .0.enqueue(message);
  } else {
    let mut queue = StdoutQueue::default();
    queue.0.enqueue(message);
    database.world.spawn((queue,));
  }
}

/// Enqueue an error message.
pub fn enqueue_error(database: &mut Database, message: String) {
  if let Some(queue) = database.world.query_mut::<&mut StderrQueue>().into_iter().next() {
    queue.1 .0.enqueue(message);
  } else {
    let mut queue = StderrQueue::default();
    queue.0.enqueue(message);
    database.world.spawn((queue,));
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_enqueue_output() {
    init();
    let mut database = Database::default();
    enqueue_output(&mut database, "test".to_string());
    enqueue_output(&mut database, "test2".to_string());
    let queue = database
      .world
      .query_mut::<&mut StdoutQueue>()
      .into_iter()
      .next()
      .unwrap();
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test");
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test2");
  }

  #[test]
  fn test_enqueue_error() {
    init();
    let mut database = Database::default();
    enqueue_error(&mut database, "test".to_string());
    enqueue_error(&mut database, "test2".to_string());
    let queue = database
      .world
      .query_mut::<&mut StderrQueue>()
      .into_iter()
      .next()
      .unwrap();
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test");
    assert_eq!(queue.1 .0.dequeue().unwrap(), "test2");
  }
}
