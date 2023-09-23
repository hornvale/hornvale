use std::sync::Arc;

use crate::action::QuitAction;
use crate::command::Command;
use crate::command::CommandContext;
use crate::command::CommandError;

/// The `Quit` command struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl Command for Quit {
  fn get_name(&self) -> &str {
    "quit"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_action_event!(context.data, Arc::new(QuitAction {}));
    Ok(())
  }
}
