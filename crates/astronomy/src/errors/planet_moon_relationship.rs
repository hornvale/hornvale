use super::moon::MoonError;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Planet-Moon relationship-related errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum PlanetMoonRelationshipError {
  /// An error occurred with the moon.
  #[error("a moon error occurred: {0}")]
  MoonError(#[from] MoonError),
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
