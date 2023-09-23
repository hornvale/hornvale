use mockall::predicate::*;
use mockall::*;

/// The `WriteInputEvent` trait, which allows for writing an input event.
#[automock]
pub trait WriteInputEvent {
  fn write_input_event(&mut self, input: String);
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::test::init;

  #[test]
  fn test_write_action_event() {
    init();
    let mut mock = MockWriteInputEvent::new();
    mock.expect_write_input_event().times(1).return_const(());
    mock.write_input_event("Test".into());
  }
}
