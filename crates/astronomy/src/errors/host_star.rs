use super::close_binary_star::CloseBinaryStarError;
use super::star::StarError;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Host star-related errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum HostStarError {
  /// An error occurred while calculating the stars' properties.
  #[error("an error occurred while calculating the stars' properties: {0}")]
  CloseBinaryStarError(#[from] CloseBinaryStarError),
  /// An error occurred while calculating the star's properties.
  #[error("an error occurred while calculating the star's properties: {0}")]
  StarError(#[from] StarError),
}
