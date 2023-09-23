use crate::command::CommandContext;
use crate::command::CommandError;
use crate::command::CommandTrait;

/// The `Look` command struct.
#[derive(Clone, Debug, Default)]
pub struct Look {}

impl CommandTrait for Look {
  fn get_name(&self) -> &str {
    "look"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_output_event!(context.data, "You look around.");
    Ok(())
  }
}
