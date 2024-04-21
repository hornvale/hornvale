use anyhow::Error as AnyError;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Core error type.
#[derive(Clone, Debug, ThisError, Deserialize, Serialize)]
pub enum CoreError {
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
  /// Convert from any error.
  #[error(transparent)]
  Any(#[from] AnyError),
}
