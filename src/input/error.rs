use std::io::Error as IoError;
use thiserror::Error as ThisError;

/// A mechanical or systemic error that can occur when processing input.
///
/// This error type is used to represent any error that can occur when
/// processing input. This includes errors that occur when reading from a file,
/// network connection, or other input source. We will distinguish between
/// these mechanical errors and errors that occur due to the content of the
/// input itself.
#[derive(Debug, ThisError)]
pub enum InputError {
  /// Conversion from an IO error. This is a common error that can occur when
  /// reading from a file or other input source. This is not a result of a
  /// problem with the input itself, but rather a problem with the input
  /// mechanism.
  #[error("An I/O error occurred: {0}")]
  IoError(#[from] IoError),
  /// No input is available. This error occurs when the input source has been
  /// exhausted and no more input is available.
  #[error("No input available")]
  NoInput,
}
