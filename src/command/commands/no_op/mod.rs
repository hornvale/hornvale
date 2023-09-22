use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandResult;

/// The `NoOp` command struct.
#[derive(Clone, Debug, Default)]
pub struct NoOp {}

impl Command for NoOp {
  fn get_name(&self) -> &str {
    "no-op"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<CommandResult, CommandError> {
    write_output_event!(context.all_data, "NoOp command executed.");
    Ok(CommandResult::SucceededQuietly)
  }
}
