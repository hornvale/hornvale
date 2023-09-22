use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandResult;

/// The `Wait` command struct.
#[derive(Clone, Debug, Default)]
pub struct Wait {}

impl Command for Wait {
  fn get_name(&self) -> &str {
    "wait"
  }

  fn execute(&self, _context: &mut CommandContext) -> Result<CommandResult, CommandError> {
    Ok(CommandResult::SucceededWithOutput("Time passes...".to_string()))
  }
}
