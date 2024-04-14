use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to parsing operations.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum ParserError {
  /// Could not parse the input.
  #[error("could not parse the input")]
  CouldNotParseInput,
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
