use crate::error::AstronomyError;

/// The `MaybeHabitable` trait, used to determine if something is habitable.
pub trait MaybeHabitable {
  /// Check if the object is habitable.
  fn check_habitability(&self) -> Result<(), AstronomyError>;
  /// Determine if the object is habitable.
  fn is_habitable(&self) -> bool {
    self.check_habitability().is_ok()
  }
}
