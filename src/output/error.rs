use std::io::Error as IoError;
use thiserror::Error as ThisError;

/// A mechanical or systemic error that can occur when processing output.
///
/// This error type is used to represent any error that can occur when
/// processing output. This includes errors that occur when writing to a file,
/// network connection, or other output source. We will distinguish between
/// these mechanical errors and errors that occur due to the content of the
/// output itself.
#[derive(ThisError, Debug)]
pub enum OutputError {
  /// Conversion from an IO error. This is a common error that can occur when
  /// writing to a file or other output source. This is not a result of a
  /// problem with the output itself, but rather a problem with the output
  /// mechanism.
  #[error("An I/O error occurred: {0}")]
  IoError(#[from] IoError),
}
