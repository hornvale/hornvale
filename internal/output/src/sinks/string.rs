use crate::prelude::{OutputError, OutputSink};

/// An output sink that writes to a list of strings.
#[derive(Debug, Default)]
pub struct StringSink {
  /// The list of strings to which output is written.
  pub outputs: Vec<String>,
}

impl StringSink {
  /// Create a new StringSink.
  pub fn new() -> Self {
    let outputs = Vec::new();
    Self { outputs }
  }
}

impl OutputSink for StringSink {
  fn send_output(&mut self, output: String) -> Result<(), OutputError> {
    self.outputs.push(output);
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_send_output() {
    init();
    let mut sink = StringSink::new();
    sink.send_output("test".to_string()).unwrap();
    sink.send_output("test2".to_string()).unwrap();
    assert_eq!(sink.outputs, vec!["test".to_string(), "test2".to_string()]);
  }
}
