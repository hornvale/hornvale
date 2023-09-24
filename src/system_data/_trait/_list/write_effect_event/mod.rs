use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::effect::EffectTrait;

/// The `WriteEffectEvent` trait, which allows for writing an effect event.
#[automock]
pub trait WriteEffectEvent {
  fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::effect::QuitGameEffect;
  use crate::test::init;

  #[test]
  fn test_write_effect_event() {
    init();
    let mut mock = MockWriteEffectEvent::new();
    mock.expect_write_effect_event().times(1).return_const(());
    mock.write_effect_event(Arc::new(QuitGameEffect {
      message: "You quit the game.".to_string(),
    }));
  }
}
