use crate::terrestrial_planet::TerrestrialPlanetError;
use thiserror::Error as ThisError;

/// An error type for the astronomy crate.
#[derive(ThisError, Debug)]
pub enum AstronomyError {
  /// An error from a terrestrial planet.
  #[error("terrestrial planet error: {0}")]
  TerrestrialPlanetError(TerrestrialPlanetError),
}
