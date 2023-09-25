use std::sync::Arc;

use crate::command::CommandContext;
use crate::command::CommandContextTrait;
use crate::command::CommandError;
use crate::command::CommandTrait;
use crate::effect::SetInputReadyFlagEffect;

/// The `Look` command struct.
#[derive(Clone, Debug, Default)]
pub struct Look {}

impl CommandTrait for Look {
  fn get_name(&self) -> &str {
    "look"
  }

  fn execute(&self, context: &mut CommandContext) -> Result<(), CommandError> {
    write_output_event!(context.get_data_mut(), "You look around.");
    write_effect_event!(
      context.get_data_mut(),
      Arc::new(SetInputReadyFlagEffect { value: true })
    );
    Ok(())
  }
}
