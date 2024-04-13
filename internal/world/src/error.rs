use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Clone, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum WorldError {
  /// The region generator does not exist.
  #[error("the region generator \"{0}\" does not exist")]
  UnknownRegionGenerator(String),
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
