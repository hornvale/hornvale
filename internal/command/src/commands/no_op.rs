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
