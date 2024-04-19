use crate::prelude::InputError;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// A trait for synchronous input sources, such as standard input or a file.
///
/// Input may come from a variety of sources, such as standard input, a file,
/// or a network connection.
pub trait InputSource {
  /// Fetch the input from the source.
  ///
  /// This method fetches input from the source; it blocks until input is ready
  /// or returns an error if the input cannot be fetched.
  ///
  /// The implementation of this method will vary depending on the input source
  /// (e.g., reading from stdin, a network socket, or a test script).
  fn fetch_input(&mut self) -> Result<String, InputError>;
}

impl Debug for dyn InputSource {
  fn fmt(&self, f: &mut Formatter) -> FmtResult {
    write!(f, "Source")
  }
}
