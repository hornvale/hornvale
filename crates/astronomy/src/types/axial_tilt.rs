use super::latitude::Latitude;
use super::rotation_direction::RotationDirection;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `AxialTilt` newtype, representing an axial tilt.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct AxialTilt(pub f64);

impl AxialTilt {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }

  /// Get the latitude of the northern polar zone (Arctic Circle).
  pub fn get_northern_polar_zone(self) -> Latitude {
    Latitude(90.0 - self.0)
  }

  /// Get the latitude of the southern polar zone (Antarctic Circle).
  pub fn get_southern_polar_zone(self) -> Latitude {
    Latitude(-90.0 + self.0)
  }

  /// Get the latitude of the northern tropic zone (Tropic of Cancer).
  pub fn get_northern_tropic_zone(self) -> Latitude {
    Latitude(self.0)
  }

  /// Get the latitude of the southern tropic zone (Tropic of Capricorn).
  pub fn get_southern_tropic_zone(self) -> Latitude {
    Latitude(-self.0)
  }

  /// Returns the direction of rotation.
  pub fn rotation_direction(self) -> RotationDirection {
    if self.0 >= 0.0 {
      RotationDirection::Prograde
    } else {
      RotationDirection::Retrograde
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(AxialTilt(-1.0).abs(), 1.0);
    assert_approx_eq!(AxialTilt(0.0).abs(), 0.0);
    assert_approx_eq!(AxialTilt(1.0).abs(), 1.0);
  }

  #[test]
  fn test_get_northern_polar_zone() {
    assert_approx_eq!(AxialTilt(23.5).get_northern_polar_zone().0, 66.5);
    assert_approx_eq!(AxialTilt(0.0).get_northern_polar_zone().0, 90.0);
    assert_approx_eq!(AxialTilt(90.0).get_northern_polar_zone().0, 0.0);
  }

  #[test]
  fn test_get_southern_polar_zone() {
    assert_approx_eq!(AxialTilt(23.5).get_southern_polar_zone().0, -66.5);
    assert_approx_eq!(AxialTilt(0.0).get_southern_polar_zone().0, -90.0);
    assert_approx_eq!(AxialTilt(90.0).get_southern_polar_zone().0, 0.0);
  }

  #[test]
  fn test_get_northern_tropic_zone() {
    assert_approx_eq!(AxialTilt(23.5).get_northern_tropic_zone().0, 23.5);
    assert_approx_eq!(AxialTilt(0.0).get_northern_tropic_zone().0, 0.0);
    assert_approx_eq!(AxialTilt(90.0).get_northern_tropic_zone().0, 90.0);
  }

  #[test]
  fn test_get_southern_tropic_zone() {
    assert_approx_eq!(AxialTilt(23.5).get_southern_tropic_zone().0, -23.5);
    assert_approx_eq!(AxialTilt(0.0).get_southern_tropic_zone().0, 0.0);
    assert_approx_eq!(AxialTilt(90.0).get_southern_tropic_zone().0, -90.0);
  }
}
