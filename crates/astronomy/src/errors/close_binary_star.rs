use super::star::StarError;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Close binary star-related errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum CloseBinaryStarError {
  /// Lower than MINIMUM_CLOSE_BINARY_STAR_SEPARATION.
  #[error("the stars are too close together to be stable")]
  BinaryStarsTooCloseForComfort,
  /// The habitable zone is contained within the forbidden zone.
  #[error("the stars' habitable zone is contained within their forbidden zone")]
  HabitableZoneContainedWithinForbiddenZone,
  /// The habitable zone isn't sufficiently far from the host stars.
  #[error("the stars' habitable zone is too close to the host stars")]
  HabitableZoneContainedWithinDangerZone,
  /// No habitable conditions found anywhere in StarSubsystem.
  #[error("the stars do not have a habitable zone")]
  NoHabitableZoneFound,
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
  /// An error occurred while calculating the stars' properties.
  #[error("an error occurred while calculating the stars' properties: {0}")]
  StarError(#[from] StarError),
}
