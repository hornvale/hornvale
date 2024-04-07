use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Star-related errors, mostly enforcing constraints on star properties.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum StarError {
  /// Lower than MAIN_SEQUENCE_STAR_MASS_LOWER_BOUND.
  #[error("its mass is too low to be a main-sequence star")]
  MassTooLowForMainSequence,
  /// Higher than MAIN_SEQUENCE_STAR_MASS_UPPER_BOUND.
  #[error("its mass is too high to be a main-sequence star")]
  MassTooHighForMainSequence,
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
