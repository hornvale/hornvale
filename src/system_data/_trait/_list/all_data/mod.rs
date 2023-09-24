use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;
use crate::command::CommandTrait;
use crate::effect::EffectTrait;
use crate::system_data::_trait::_list::*;

/// The `AllData` trait, which inherits from all other data traits.
pub trait AllData: WriteActionEventTrait + WriteEffectEventTrait {}

mock! {
  pub AllDataMerged {}
  impl WriteActionEventTrait for AllDataMerged {
    fn write_action_event(&mut self, action: Arc<dyn ActionTrait>);
  }
  impl WriteCommandEventTrait for AllDataMerged {
    fn write_command_event(&mut self, command: Arc<dyn CommandTrait>);
  }
  impl WriteEffectEventTrait for AllDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl WriteInputEventTrait for AllDataMerged {
    fn write_input_event(&mut self, input: String);
  }
  impl WriteOutputEventTrait for AllDataMerged {
    fn write_output_event(&mut self, output: String);
  }
  impl AllData for AllDataMerged {}
}

#[cfg(test)]
mod test {
  use std::sync::Arc;

  use super::*;
  use crate::action::QuitAction;
  use crate::command::QuitCommand;
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
  fn test_write_command_event() {
    init();
    let mut mock = MockAllDataMerged::new();
    mock.expect_write_command_event().times(1).return_const(());
    mock.write_command_event(Arc::new(QuitCommand {}));
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

  #[test]
  fn test_write_input_event() {
    init();
    let mut mock = MockAllDataMerged::new();
    mock.expect_write_input_event().times(1).return_const(());
    mock.write_input_event("Test".into());
  }

  #[test]
  fn test_write_output_event() {
    init();
    let mut mock = MockAllDataMerged::new();
    mock.expect_write_output_event().times(1).return_const(());
    mock.write_output_event("Test".into());
  }
}
