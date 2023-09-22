use crate::action::Action;

/// The `Result` enum.
///
/// This encapsulates the result of attempting to execute a command.
#[derive(Debug)]
pub enum Result {
  /// This command was executed successfully and has no output.
  SucceededQuietly,
  /// This command was executed successfully and has output.
  SucceededWithOutput(String),
  /// This command returned an Action.
  Action(Box<dyn Action>),
}
