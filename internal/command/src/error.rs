use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum CommandError {
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
