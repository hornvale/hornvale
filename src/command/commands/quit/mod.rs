use crate::action::QuitAction;
use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandResult;

/// The `Quit` command struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl Command for Quit {
  fn get_name(&self) -> &str {
    "quit"
  }

  fn execute(&self, _context: &mut CommandContext) -> Result<CommandResult, CommandError> {
    Ok(CommandResult::Action(Box::new(QuitAction {})))
  }
}
