use crate::input::prelude::InputError;
use crate::input::prelude::InputQueue;
use crate::input::prelude::InputSource;
use hecs::{Entity, World};

/// An object that manages the process of reading and storing input.
#[derive(Debug)]
pub struct InputReader<T>
where
  T: InputSource,
{
  /// The source for fetching input.
  pub source: T,
  /// The entity that stores the input queue.
  pub entity: Option<Entity>,
}

impl<T> InputReader<T>
where
  T: InputSource,
{
  /// Create a new reader with the given source.
  pub fn new(source: T) -> Self {
    Self { source, entity: None }
  }

  /// Read input from the source and enqueue it.
  pub fn read_input(&mut self, world: &mut World) -> Result<(), InputError> {
    if let Ok(input) = self.source.fetch_input() {
      let inputs = self.split_input(&input)?;
      self.enqueue_inputs(world, inputs)?;
    }
    Ok(())
  }

  /// Split input into one or more distinct strings.
  ///
  /// We split input on the following:
  /// - Periods.
  /// - Semicolons.
  /// - Newlines.
  /// - The " then " keyword.
  pub fn split_input(&self, input: &str) -> Result<Vec<String>, InputError> {
    let mut inputs = Vec::new();
    let mut current = String::new();
    let input = input.replace(" then ", ".");
    for c in input.chars() {
      match c {
        '.' | ';' | '\n' => {
          if !current.is_empty() {
            let trimmed = current.trim().to_string();
            inputs.push(trimmed);
            current.clear();
          }
        },
        _ => current.push(c),
      }
    }
    if !current.is_empty() {
      let trimmed = current.trim().to_string();
      inputs.push(trimmed);
    }
    Ok(inputs)
  }

  /// Enqueue inputs for parsing.
  pub fn enqueue_inputs(&mut self, world: &mut World, inputs: Vec<String>) -> Result<(), InputError> {
    let entity = self.ensure_entity(world)?;
    let queue = world.query_one_mut::<&mut InputQueue>(entity).unwrap();
    for input in inputs {
      queue.enqueue(input);
    }
    Ok(())
  }

  /// Ensure that the input queue entity exists.
  pub fn ensure_entity(&mut self, world: &mut World) -> Result<Entity, InputError> {
    if self.entity.is_none() {
      let entity = world.spawn((InputQueue::new(),));
      self.entity = Some(entity);
    }
    Ok(self.entity.unwrap())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::input::prelude::StringSource;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_split_input() {
    init();
    let reader = InputReader::new(StringSource::new(vec![]));
    let inputs = reader.split_input("test. test2; test3\n test4 then test5.test6 ;test7");
    assert_eq!(
      inputs.unwrap(),
      vec!["test", "test2", "test3", "test4", "test5", "test6", "test7"]
    );
  }

  #[test]
  fn test_enqueue_inputs() {
    init();
    let mut world = World::new();
    let mut reader = InputReader::new(StringSource::new(vec![]));
    let inputs = vec!["test".to_string(), "test2".to_string()];
    reader.enqueue_inputs(&mut world, inputs).unwrap();
    let entity = reader.entity.unwrap();
    let queue = world.query_one_mut::<&mut InputQueue>(entity).unwrap();
    assert_eq!(queue.dequeue().unwrap(), "test".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test2".to_string());
    assert_eq!(queue.dequeue(), None);
  }

  #[test]
  fn test_ensure_entity() {
    init();
    let mut world = World::new();
    let mut reader = InputReader::new(StringSource::new(vec![]));
    reader.ensure_entity(&mut world).unwrap();
    assert!(reader.entity.is_some());
  }

  #[test]
  fn test_read_input() {
    init();
    let mut world = World::new();
    let mut reader = InputReader::new(StringSource::new(vec![
      "test".to_string(),
      "test2".to_string(),
      "test3;test4 then test5 .test6".to_string(),
    ]));
    reader.read_input(&mut world).unwrap();
    let entity = reader.entity.unwrap();
    let queue = world.query_one_mut::<&mut InputQueue>(entity).unwrap();
    assert_eq!(queue.dequeue().unwrap(), "test".to_string());
    assert_eq!(queue.dequeue(), None);
  }

  #[test]
  fn test_read_input_no_input() {
    init();
    let mut world = World::new();
    let mut reader = InputReader::new(StringSource::new(vec![]));
    reader.read_input(&mut world).unwrap();
    assert!(reader.entity.is_none());
  }

  #[test]
  fn test_read_input_all() {
    init();
    let mut world = World::new();
    let mut reader = InputReader::new(StringSource::new(vec![
      "test".to_string(),
      "test2".to_string(),
      "test3;test4 then test5 .test6".to_string(),
    ]));
    reader.read_input(&mut world).unwrap();
    reader.read_input(&mut world).unwrap();
    reader.read_input(&mut world).unwrap();
    let entity = reader.entity.unwrap();
    let queue = world.query_one_mut::<&mut InputQueue>(entity).unwrap();
    assert_eq!(queue.dequeue().unwrap(), "test".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test2".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test3".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test4".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test5".to_string());
    assert_eq!(queue.dequeue().unwrap(), "test6".to_string());
    assert_eq!(queue.dequeue(), None);
  }
}
