use crate::prelude::TokenKind;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to parsing operations.
#[derive(Clone, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum ParserError {
  /// No input was provided.
  #[error("no input provided")]
  NoInput,
  /// No verb was provided.
  #[error("no verb provided")]
  NoVerb,
  /// Could not parse the input.
  #[error("could not parse the input")]
  CouldNotParseInput,
  /// Could not _classify_ the input. This may not be a fatal error.
  #[error("could not classify the input")]
  CouldNotClassifyInput,
  /// No preposition found.
  #[error("no preposition found")]
  NoPreposition,
  /// An unexpected character was encountered.
  #[error("unexpected character: {0}")]
  UnexpectedCharacter(char),
  /// An unexpected token was encountered.
  #[error("unexpected token: expected {0}, found {1}; {2}")]
  UnexpectedToken(TokenKind, TokenKind, String),
  /// Index out of bounds.
  #[error("index out of bounds")]
  IndexOutOfBounds,
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
