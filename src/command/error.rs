use crate::command::prelude::*;
use anyhow::Error as AnyError;
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Debug, ThisError)]
pub enum CommandError {
  /// An unknown command was encountered.
  #[error("unknown command: {0}")]
  UnknownCommand(String),
  /// Invalid argument.
  #[error("invalid argument: {0}")]
  InvalidArgument(String),
  /// Invalid actor.
  #[error("invalid actor: {0}")]
  InvalidActor(String),
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
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
  /// An unknown form of a command was encountered.
  #[error("unknown form: {0} of command: {1} (known forms: {2:?})")]
  UnknownCommandModifier(String, String, Vec<String>),
  /// An unexpected token was encountered.
  #[error("unexpected token: expected {0}, found {1}; {2}")]
  UnexpectedToken(TokenKind, TokenKind, String),
  /// Could not consume the direct object.
  #[error("could not consume the direct object {0}")]
  CouldNotConsumeDirectObject(String),
  /// Can't perform actions with two different directions.
  #[error("can't perform actions with two different directions")]
  TwoDifferentDirections,
  /// Can't bind "here" if the actor is nowhere.
  #[error("can't bind \"here\" if the actor is nowhere")]
  ActorIsNowhere,
  /// Index out of bounds.
  #[error("index out of bounds")]
  IndexOutOfBounds,
  /// Invalid token sequence.
  #[error("invalid token sequence: {0}")]
  InvalidTokenSequence(String),
  /// Unterminated string literal.
  #[error("unterminated string literal")]
  UnterminatedStringLiteral,
  /// Requested a character out of bounds.
  #[error("requested a character out of bounds")]
  CharacterOutOfBounds,
  /// Any error.
  #[error(transparent)]
  Any(#[from] AnyError),
}