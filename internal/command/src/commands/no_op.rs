use crate::prelude::{Command, CommandContext, CommandError, SyntaxElement, SyntaxPattern};
use hecs::World;

/// A command that does nothing at all.
#[derive(Clone, Copy, Debug)]
pub struct NoOpCommand;

impl Command for NoOpCommand {
  fn name(&self) -> &str {
    "no-op"
  }
  fn syntax_patterns(&self) -> Vec<SyntaxPattern> {
    vec![SyntaxPattern {
      elements: vec![SyntaxElement::Command],
    }]
  }
  fn description(&self) -> &str {
    "A command that does nothing at all."
  }
  fn execute(&self, _world: &World, _context: &CommandContext) -> Result<(), CommandError> {
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
    let result = command.execute(&world, &Default::default());
    assert_eq!(result, Ok(()));
  }
}
