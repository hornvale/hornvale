use crate::output::prelude::*;
use hecs::{Entity, World};

/// An object that manages the process of writing output.
#[derive(Debug)]
pub struct OutputWriter<O, E>
where
  O: OutputSink,
  E: OutputSink,
{
  /// The sink for writing standard output.
  pub out_sink: O,
  /// The sink for writing error output.
  pub err_sink: E,
  /// The entity that stores the standard output queue.
  pub out_entity: Option<Entity>,
  /// The entity that stores the error output queue.
  pub err_entity: Option<Entity>,
}

impl<O, E> OutputWriter<O, E>
where
  O: OutputSink,
  E: OutputSink,
{
  /// Create a new reader with the given source.
  pub fn new(out_sink: O, err_sink: E) -> Self {
    let out_entity = None;
    let err_entity = None;
    Self {
      out_sink,
      err_sink,
      out_entity,
      err_entity,
    }
  }

  /// Write output to the output sink.
  pub fn write_output(&mut self, world: &mut World) -> Result<(), OutputError> {
    let out_entity = self.ensure_out_entity(world)?;
    let queue = world.query_one_mut::<&mut StdoutQueue>(out_entity).unwrap();
    for output in queue.0.drain() {
      self.out_sink.send_output(output)?;
    }
    Ok(())
  }

  /// Write error output to the error sink.
  pub fn write_error(&mut self, world: &mut World) -> Result<(), OutputError> {
    let err_entity = self.ensure_err_entity(world)?;
    let queue = world.query_one_mut::<&mut StderrQueue>(err_entity).unwrap();
    for error_output in queue.0.drain() {
      self.err_sink.send_output(error_output)?;
    }
    Ok(())
  }

  /// Enqueue standard output.
  pub fn enqueue_out(&mut self, world: &mut World, outputs: Vec<String>) -> Result<(), OutputError> {
    let entity = self.ensure_out_entity(world)?;
    let queue = world.query_one_mut::<&mut StdoutQueue>(entity).unwrap();
    for output in outputs {
      queue.0.enqueue(output);
    }
    Ok(())
  }

  /// Enqueue error output.
  pub fn enqueue_err(&mut self, world: &mut World, outputs: Vec<String>) -> Result<(), OutputError> {
    let entity = self.ensure_err_entity(world)?;
    let queue = world.query_one_mut::<&mut StderrQueue>(entity).unwrap();
    for error_output in outputs {
      queue.0.enqueue(error_output);
    }
    Ok(())
  }

  /// Ensure that the output queue entity exists.
  pub fn ensure_out_entity(&mut self, world: &mut World) -> Result<Entity, OutputError> {
    if self.out_entity.is_none() {
      let entity = world.spawn((StdoutQueue::default(),));
      self.out_entity = Some(entity);
    }
    Ok(self.out_entity.unwrap())
  }

  /// Ensure that the error queue entity exists.
  pub fn ensure_err_entity(&mut self, world: &mut World) -> Result<Entity, OutputError> {
    if self.err_entity.is_none() {
      let entity = world.spawn((StderrQueue::default(),));
      self.err_entity = Some(entity);
    }
    Ok(self.err_entity.unwrap())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_write_output() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer.write_output(&mut world).unwrap();
  }

  #[test]
  fn test_write_error() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer.write_error(&mut world).unwrap();
  }

  #[test]
  fn test_enqueue_out() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer
      .enqueue_out(&mut world, vec!["test".to_string(), "test2".to_string()])
      .unwrap();
    writer.write_output(&mut world).unwrap();
    assert_eq!(writer.out_sink.outputs, vec!["test".to_string(), "test2".to_string()]);
    assert!(writer.err_sink.outputs.is_empty());
  }

  #[test]
  fn test_enqueue_err() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer
      .enqueue_err(&mut world, vec!["test".to_string(), "test2".to_string()])
      .unwrap();
    writer.write_error(&mut world).unwrap();
    assert!(writer.out_sink.outputs.is_empty());
    assert_eq!(writer.err_sink.outputs, vec!["test".to_string(), "test2".to_string()]);
  }

  #[test]
  fn test_ensure_out_entity() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer.ensure_out_entity(&mut world).unwrap();
  }

  #[test]
  fn test_ensure_err_entity() {
    init();
    let mut world = World::new();
    let mut writer = OutputWriter::new(StringSink::new(), StringSink::new());
    writer.ensure_err_entity(&mut world).unwrap();
  }
}
