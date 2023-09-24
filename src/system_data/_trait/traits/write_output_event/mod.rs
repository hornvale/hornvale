use mockall::predicate::*;
use mockall::*;

/// The `WriteOutputEvent` trait, which allows for writing an output event.
#[automock]
pub trait WriteOutputEvent {
  fn write_output_event(&mut self, output: String);
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::test::init;

  #[test]
  fn test_write_action_event() {
    init();
    let mut mock = MockWriteOutputEvent::new();
    mock.expect_write_output_event().times(1).return_const(());
    mock.write_output_event("Test".into());
  }
}
