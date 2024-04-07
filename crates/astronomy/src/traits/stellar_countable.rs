use crate::error::AstronomyError;

/// The `StellarCountable` trait, used to count the stars in an object.
pub trait StellarCountable {
  /// Get the number of stars in this object.
  fn get_stellar_count(&self) -> Result<u8, AstronomyError>;
}
