use crate::prelude::{Command, CommandError};
use hecs::World;

/// A command that does nothing at all.
#[derive(Clone, Copy, Debug)]
pub struct NoOpCommand;

impl Command for NoOpCommand {
  fn name(&self) -> &str {
    "no-op"
  }
  fn execute(&self, _world: &World) -> Result<(), CommandError> {
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_name() {
    init();
    let command = NoOpCommand;
    assert_eq!(command.name(), "no-op");
  }

  #[test]
  fn test_execute() {
    init();
    let command = NoOpCommand;
    let world = World::new();
    let result = command.execute(&world);
    assert_eq!(result, Ok(()));
  }
}
