use std::sync::Arc;

use crate::action::QuitAction;
use crate::command::CommandContext;
use crate::command::CommandContextTrait;
use crate::command::CommandError;
use crate::command::CommandTrait;

/// The `Quit` command struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl CommandTrait for Quit {
  fn get_name(&self) -> &str {
    "quit"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_action_event!(context.get_data_mut(), Arc::new(QuitAction {}));
    Ok(())
  }
}
