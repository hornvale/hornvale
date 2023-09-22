use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandResult;

/// The `Look` command struct.
#[derive(Clone, Debug, Default)]
pub struct Look {}

impl Command for Look {
  fn get_name(&self) -> &str {
    "look"
  }

  fn execute(&self, _context: &mut CommandContext) -> Result<CommandResult, CommandError> {
    Ok(CommandResult::SucceededWithOutput("You look around.".to_string()))
  }
}
