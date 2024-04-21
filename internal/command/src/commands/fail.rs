use crate::prelude::CommandError;
use anyhow::Error as AnyError;
use hecs::{Entity, World};
use hornvale_core::prelude::*;

/// A command that always fails.
#[derive(Clone, Copy, Debug)]
pub struct FailCommand;

impl Command for FailCommand {
  const NAME: &'static str = "fail";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "A command that always fails.";
  const DESCRIPTION: &'static str = "A command that always fails; useful for testing.";
  const FORM: CommandForm = CommandForm::Default;

  /// Fail.
  fn execute(_world: &mut World, _context: &Entity) -> Result<(), AnyError> {
    Err(CommandError::UnknownError.into())
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
    let result = FailCommand::execute(&mut world, &entity);
    assert!(result.is_err());
  }
}
