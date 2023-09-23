use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandTrait;

/// The `Wait` command struct.
#[derive(Clone, Debug, Default)]
pub struct Wait {}

impl CommandTrait for Wait {
  fn get_name(&self) -> &str {
    "wait"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_output_event!(context.data, "Time passes...");
    Ok(())
  }
}
