use std::sync::Arc;

use crate::action::ActionContext;
use crate::action::ActionError;
use crate::action::ActionTrait;
use crate::effect::QuitGameEffect;

/// The `Quit` action struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl ActionTrait for Quit {
  fn execute(&self, context: &mut ActionContext) -> Result<(), ActionError> {
    write_effect_event!(
      context.data,
      Arc::new(QuitGameEffect {
        message: "You quit the game.".to_string(),
      })
    );
    Ok(())
  }
}
