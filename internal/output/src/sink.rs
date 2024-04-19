use crate::prelude::OutputError;

/// A trait for synchronous output sinks, such as standard output or a file.
pub trait OutputSink {
  /// Send the output to the sink.
  ///
  /// This method sends output to the sink synchronously. It returns nothing or
  /// an error if the output cannot be sent.
  fn send_output(&mut self, output: String) -> Result<(), OutputError>;
}
