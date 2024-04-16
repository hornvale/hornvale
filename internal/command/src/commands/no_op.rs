use crate::prelude::{Command, CommandContext, CommandError};
use hecs::World;

/// A command that does nothing at all.
#[derive(Clone, Copy, Debug)]
pub struct NoOpCommand;

impl Command for NoOpCommand {
  const NAME: &'static str = "fail";
  const SYNONYMS: &'static [&'static str] = &[];
  const BRIEF: &'static str = "A command that always succeeds but does nothing.";
  const DESCRIPTION: &'static str = "A command that always succeeds but does nothing; useful for testing.";

  /// Do nothing.
  fn execute(_world: &mut World, _context: &CommandContext) -> Result<(), CommandError> {
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
    let result = NoOpCommand::execute(&mut world, &Default::default());
    assert_eq!(result, Ok(()));
  }
}
