use anyhow::Error as AnyError;
use hecs::{Entity, World};
use hornvale_core::prelude::*;

/// A command that does nothing at all.
#[derive(Clone, Copy, Debug)]
pub struct NoOpCommand;

impl Command for NoOpCommand {
  const NAME: &'static str = "no-op";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "A command that always succeeds but does nothing.";
  const DESCRIPTION: &'static str = "A command that always succeeds but does nothing; useful for testing.";
  const FORM: CommandForm = CommandForm::Default;

  /// Do nothing.
  fn execute(_world: &mut World, _context: &Entity) -> Result<(), AnyError> {
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_execute() {
    init();
    let mut world = World::new();
    let entity = world.spawn(());
    let result = NoOpCommand::execute(&mut world, &entity);
    assert!(result.is_ok());
  }
}
