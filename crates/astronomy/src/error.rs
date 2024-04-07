use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Errors related to astronomy.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum AstronomyError {
  /// Pluto, also Minnesota.
  #[error("not habitable because it is too cold")]
  PlanetTooColdToSupportConventionalLife,
  /// Hell, or Las Vegas.
  #[error("not habitable because it is too hot")]
  PlanetTooHotToSupportConventionalLife,
  /// Hard to fight when people keep floating off into space.
  #[error("not habitable because its gravity is too low")]
  PlanetGravityTooLowToSupportConventionalLife,
  /// Just sounds kinda lame.
  #[error("not habitable because its gravity is too high")]
  PlanetGravityTooHighToSupportConventionalLife,
  /// Oxygen unstable in this atmosphere.
  #[error("not habitable because it cannot retain oxygen")]
  PlanetAtmosphereUnstableForOxygen,
  /// Carbon Dioxide unstable in this atmosphere.
  #[error("not habitable because it cannot retain carbon dioxide")]
  PlanetAtmosphereUnstableForCarbonDioxide,
  /// Argon unstable in this atmosphere.
  #[error("not habitable because it cannot retain argon")]
  PlanetAtmosphereUnstableForArgon,
  /// Nitrogen unstable in this atmosphere.
  #[error("not habitable because it cannot retain nitrogen")]
  PlanetAtmosphereUnstableForNitrogen,
  /// The habitable zone is contained within the forbidden zone.
  #[error("the stars' habitable zone is contained within their forbidden zone")]
  StarHabitableZoneContainedWithinForbiddenZone,
  /// The habitable zone isn't sufficiently far from the host stars.
  #[error("the stars' habitable zone is too close to the host stars")]
  StarHabitableZoneContainedWithinDangerZone,
  /// No habitable conditions found anywhere around the stars.
  #[error("the star does not have a habitable zone")]
  StarDoesNotHaveHabitableZone,
  /// Lower than MINIMUM_STAR_AGE_TO_SUPPORT_LIFE.
  #[error("the star is too young to support life")]
  StarTooYoungToSupportLife,
  /// Lower than MINIMUM_STAR_MASS_TO_SUPPORT_LIFE.
  #[error("the star's mass is too low to support life")]
  StarMassTooLowToSupportLife,
  /// Higher than MAXIMUM_STAR_MASS_TO_SUPPORT_LIFE.
  #[error("the star's mass is too high to support life")]
  StarMassTooHighToSupportLife,
  /// Lower than MAIN_SEQUENCE_STAR_MASS_LOWER_BOUND.
  #[error("its mass is too low to be a main-sequence star")]
  StarMassTooLowForMainSequence,
  /// Higher than MAIN_SEQUENCE_STAR_MASS_UPPER_BOUND.
  #[error("its mass is too high to be a main-sequence star")]
  StarMassTooHighForMainSequence,
  /// Lower than MINIMUM_CLOSE_BINARY_STAR_SEPARATION.
  #[error("the stars are too close together to be stable")]
  BinaryStarsTooCloseForComfort,
  /// An unknown error occurred.
  #[error("an unknown error occurred")]
  UnknownError,
}
