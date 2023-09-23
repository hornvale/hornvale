use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;
use crate::ecs::system::data::_trait::traits::*;
use crate::effect::EffectTrait;

/// The `AllData` trait, which inherits from all other data traits.
pub trait AllData: WriteActionEventTrait + WriteEffectEventTrait {}

mock! {
  pub AllDataMerged {}
  impl WriteActionEventTrait for AllDataMerged {
    fn write_action_event(&mut self, action: Arc<dyn ActionTrait>);
  }
  impl WriteEffectEventTrait for AllDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl AllData for AllDataMerged {}
}

#[cfg(test)]
mod test {
  use std::sync::Arc;

  use super::*;
  use crate::action::QuitAction;
  use crate::effect::QuitGameEffect;
  use crate::test::init;

  #[test]
  fn test_write_action_event() {
    init();
    let mut mock = MockAllDataMerged::new();
    mock.expect_write_action_event().times(1).return_const(());
    mock.write_action_event(Arc::new(QuitAction {}));
  }

  #[test]
  fn test_write_effect_event() {
    init();
    let mut mock = MockAllDataMerged::new();
    mock.expect_write_effect_event().times(1).return_const(());
    mock.write_effect_event(Arc::new(QuitGameEffect {
      message: "You quit the game.".to_string(),
    }));
  }
}
