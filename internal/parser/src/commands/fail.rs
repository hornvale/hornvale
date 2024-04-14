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
