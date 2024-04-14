use crate::prelude::{Command, CommandError};
use hecs::World;

/// A command that always fails.
#[derive(Clone, Copy, Debug)]
pub struct FailCommand;

impl Command for FailCommand {
  fn name(&self) -> &str {
    "fail"
  }
  fn execute(&self, _world: &World) -> Result<(), CommandError> {
    Err(CommandError::UnknownError)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::CommandError;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_name() {
    init();
    let command = FailCommand;
    assert_eq!(command.name(), "fail");
  }

  #[test]
  fn test_execute() {
    init();
    let command = FailCommand;
    let world = World::new();
    let result = command.execute(&world);
    assert_eq!(result, Err(CommandError::UnknownError));
  }
}
