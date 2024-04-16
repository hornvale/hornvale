use crate::prelude::{Command, CommandContext, CommandError};
use hecs::World;

/// A command that always fails.
#[derive(Clone, Copy, Debug)]
pub struct FailCommand;

impl Command for FailCommand {
  const NAME: &'static str = "fail";
  const DESCRIPTION: &'static str = "A command that always fails.";
  const ALIASES: &'static [&'static str] = &[];

  /// Fail.
  fn execute(_world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
    Err(CommandError::UnknownError)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::CommandError;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_execute() {
    init();
    let mut world = World::new();
    let result = FailCommand::execute(&mut world, &Default::default());
    assert_eq!(result, Err(CommandError::UnknownError));
  }
}
