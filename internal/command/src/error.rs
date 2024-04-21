use anyhow::Error as AnyError;
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Debug, ThisError)]
pub enum CommandError {
  /// Unknown command.
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
  /// Convert from any error.
  #[error(transparent)]
  Any(#[from] AnyError),
}
