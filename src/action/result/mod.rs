/// The `Result` enum.
///
/// This encapsulates the result of attempting to execute an action.
#[derive(Debug)]
pub enum Result {
  /// This action was executed successfully and has no output.
  SucceededQuietly,
  /// This action was executed successfully and has output.
  SucceededWithOutput(String),
}
