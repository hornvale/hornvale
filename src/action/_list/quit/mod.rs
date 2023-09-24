use std::sync::Arc;

use crate::action::ActionContextTrait;
use crate::action::ActionError;
use crate::action::ActionTrait;
use crate::effect::QuitGameEffect;

/// The `Quit` action struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl ActionTrait for Quit {
  fn execute(&self, context: &mut dyn ActionContextTrait) -> Result<(), ActionError> {
    write_effect_event!(
      context.get_data_mut(),
      Arc::new(QuitGameEffect {
        message: "You quit the game.".to_string(),
      })
    );
    Ok(())
  }
}
