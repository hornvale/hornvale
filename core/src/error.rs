use anyhow::Error as AnyError;
use thiserror::Error as ThisError;

/// Errors related to world operations.
#[derive(Debug, ThisError)]
pub enum WorldError {
  /// The region generator does not exist.
  #[error("the region generator \"{0}\" does not exist")]
  UnknownRegionGenerator(String),
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
  /// Any error.
  #[error(transparent)]
  Any(#[from] AnyError),
}
