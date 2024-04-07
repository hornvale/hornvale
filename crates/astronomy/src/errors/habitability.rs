use super::close_binary_star::CloseBinaryStarError;
use super::star::StarError;
use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Habitability errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum HabitabilityError {
  /// The stars are not suitable or stable.
  #[error("one or both of the stars are not suitable or stable: {0}")]
  StarsNotSuitableOrStable(#[from] CloseBinaryStarError),
  /// The star itself is not suitable or stable.
  #[error("the star is not suitable or stable: {0}")]
  StarNotSuitableOrStable(#[from] StarError),
  /// Pluto, also Minnesota.
  #[error("not habitable because it is too cold")]
  TooColdToSupportConventionalLife,
  /// Hell, or Las Vegas.
  #[error("not habitable because it is too hot")]
  TooHotToSupportConventionalLife,
  /// Hard to fight when people keep floating off into space.
  #[error("not habitable because its gravity is too low")]
  GravityTooLowToSupportConventionalLife,
  /// Just sounds kinda lame.
  #[error("not habitable because its gravity is too high")]
  GravityTooHighToSupportConventionalLife,
  /// Oxygen unstable in this atmosphere.
  #[error("not habitable because it cannot retain oxygen")]
  AtmosphereUnstableForOxygen,
  /// Carbon Dioxide unstable in this atmosphere.
  #[error("not habitable because it cannot retain carbon dioxide")]
  AtmosphereUnstableForCarbonDioxide,
  /// Argon unstable in this atmosphere.
  #[error("not habitable because it cannot retain argon")]
  AtmosphereUnstableForArgon,
  /// Nitrogen unstable in this atmosphere.
  #[error("not habitable because it cannot retain nitrogen")]
  AtmosphereUnstableForNitrogen,
  /// The habitable zone is contained within the forbidden zone.
  #[error("the stars' habitable zone is contained within their forbidden zone")]
  HabitableZoneContainedWithinForbiddenZone,
  /// The habitable zone isn't sufficiently far from the host stars.
  #[error("the stars' habitable zone is too close to the host stars")]
  HabitableZoneContainedWithinDangerZone,
  /// No habitable conditions found anywhere in StarSubsystem.
  #[error("the stars do not have a habitable zone")]
  NoHabitableZoneFound,
  /// Lower than MINIMUM_STAR_AGE_TO_SUPPORT_LIFE.
  #[error("the star is too young to support life")]
  TooYoungToSupportLife,
  /// Lower than MINIMUM_STAR_MASS_TO_SUPPORT_LIFE.
  #[error("the star's mass is too low to support life")]
  MassTooLowToSupportLife,
  /// Higher than MAXIMUM_STAR_MASS_TO_SUPPORT_LIFE.
  #[error("the star's mass is too high to support life")]
  MassTooHighToSupportLife,
}
