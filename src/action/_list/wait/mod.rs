use std::sync::Arc;

use crate::action::ActionContext;
use crate::action::ActionContextTrait;
use crate::action::ActionError;
use crate::action::ActionTrait;
use crate::effect::DelayedEffect;
use crate::effect::SetInputReadyFlagEffect;

/// The `Wait` action struct.
///
/// The entity waits for some length of time.
#[derive(Clone, Debug, Default)]
pub struct Wait {}

impl ActionTrait for Wait {
  fn execute(&self, context: &mut ActionContext) -> Result<(), ActionError> {
    write_output_event!(context.get_data_mut(), "Time passes...");
    write_effect_event!(
      context.get_data_mut(),
      Arc::new(DelayedEffect::new(SetInputReadyFlagEffect { value: true }, 100,))
    );
    Ok(())
  }
}
