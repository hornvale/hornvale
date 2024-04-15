use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to parsing operations.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum ParserError {
  /// Could not parse the input.
  #[error("could not parse the input")]
  CouldNotParseInput,
  /// An unexpected character was encountered.
  #[error("unexpected character: {0}")]
  UnexpectedCharacter(char),
  /// Unterminated string literal.
  #[error("unterminated string literal")]
  UnterminatedStringLiteral,
  /// Requested a character out of bounds.
  #[error("requested a character out of bounds")]
  CharacterOutOfBounds,
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
