use crate::error::AstronomyError;
use crate::types::prelude::MassOfSol;

/// The `StellarMassable` trait, used to sum the masses of the stars.
pub trait StellarMassable {
  /// Get the number of stars in this object.
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError>;
}
