use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::command::CommandTrait;

/// The `WriteCommandEvent` trait, which allows for writing a command event.
#[automock]
pub trait WriteCommandEvent {
  fn write_command_event(&mut self, command: Arc<dyn CommandTrait>);
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::command::QuitCommand;
  use crate::test::init;

  #[test]
  fn test_write_command_event() {
    init();
    let mut mock = MockWriteCommandEvent::new();
    mock.expect_write_command_event().times(1).return_const(());
    mock.write_command_event(Arc::new(QuitCommand {}));
  }
}
