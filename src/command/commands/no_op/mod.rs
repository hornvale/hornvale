use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;

/// The `NoOp` command struct.
#[derive(Clone, Debug, Default)]
pub struct NoOp {}

impl Command for NoOp {
  fn get_name(&self) -> &str {
    "no-op"
  }

  fn execute(&self, _context: &mut CommandContext) -> Result<(), CommandError> {
    Ok(())
  }
}
