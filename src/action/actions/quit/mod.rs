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

#[cfg(test)]
mod test {

  use crate::action::_trait::traits::action_context::MockActionContext;
  use crate::action::_trait::traits::action_context_data::MockActionContextDataMerged;
  use crate::action::_trait::traits::*;
  use crate::test::init;

  #[test]
  fn test_execute() {
    init();
    let action = super::Quit::default();
    let mut context = MockActionContext::new();
    context.expect_get_data_mut().times(1).returning(|| {
      let mut data = MockActionContextDataMerged::new();
      data.expect_write_effect_event().times(1).return_const(());
      Box::new(data)
    });
    action.execute(&mut context).unwrap();
  }
}
