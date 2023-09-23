use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;

/// The `Wait` command struct.
#[derive(Clone, Debug, Default)]
pub struct Wait {}

impl Command for Wait {
  fn get_name(&self) -> &str {
    "wait"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_output_event!(context.data, "Time passes...");
    Ok(())
  }
}
