use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Clone, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum CommandError {
  /// Unknown command.
  #[error("unknown command: {0}")]
  UnknownCommand(String),
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
