use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;

/// The `Look` command struct.
#[derive(Clone, Debug, Default)]
pub struct Look {}

impl Command for Look {
  fn get_name(&self) -> &str {
    "look"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_output_event!(context.data, "You look around.");
    Ok(())
  }
}
