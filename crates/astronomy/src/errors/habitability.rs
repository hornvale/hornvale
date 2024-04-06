use serde::{Deserialize, Serialize};
use thiserror::Error as ThisError;

/// Habitability errors.
#[derive(Clone, Copy, Debug, Deserialize, Eq, ThisError, Hash, PartialEq, Serialize)]
pub enum HabitabilityError {
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
}
