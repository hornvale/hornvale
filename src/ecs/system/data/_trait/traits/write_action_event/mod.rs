use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;

/// The `WriteActionEvent` trait, which allows for writing an action event.
#[automock]
pub trait WriteActionEvent {
  fn write_action_event(&mut self, action: Arc<dyn ActionTrait>);
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::action::QuitAction;
  use crate::test::init;

  #[test]
  fn test_write_action_event() {
    init();
    let mut mock = MockWriteActionEvent::new();
    mock.expect_write_action_event().times(1).return_const(());
    mock.write_action_event(Arc::new(QuitAction {}));
  }
}
