use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Moon-related errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum MoonError {
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
