use super::longitude_direction::LongitudeDirection;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `Longitude` newtype, representing a longitude.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct Longitude(pub f64);

impl Longitude {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }

  /// Returns the direction.
  pub fn direction(self) -> LongitudeDirection {
    if self.0 >= 0.0 {
      LongitudeDirection::East
    } else {
      LongitudeDirection::West
    }
  }

  /// Returns the value in degrees.
  pub fn degrees(self) -> f64 {
    self.0
  }

  /// Returns the value in radians.
  pub fn radians(self) -> f64 {
    self.0.to_radians()
  }

  /// Returns the value in gradians.
  pub fn gradians(self) -> f64 {
    self.0 * 10.0 / 9.0
  }

  /// Returns the value in degrees, minutes, and seconds.
  pub fn dms(self) -> (f64, f64, f64) {
    let degrees = self.0.floor();
    let minutes = (self.0 - degrees) * 60.0;
    let seconds = (minutes - minutes.floor()) * 60.0;
    (degrees, minutes.floor(), seconds)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(Longitude(-1.0).abs(), 1.0);
    assert_approx_eq!(Longitude(0.0).abs(), 0.0);
    assert_approx_eq!(Longitude(1.0).abs(), 1.0);
  }

  #[test]
  fn test_direction() {
    assert_eq!(Longitude(-1.0).direction(), LongitudeDirection::West);
    assert_eq!(Longitude(0.0).direction(), LongitudeDirection::East);
    assert_eq!(Longitude(1.0).direction(), LongitudeDirection::East);
  }

  #[test]
  fn test_degrees() {
    assert_approx_eq!(Longitude(-1.0).degrees(), -1.0);
    assert_approx_eq!(Longitude(0.0).degrees(), 0.0);
    assert_approx_eq!(Longitude(1.0).degrees(), 1.0);
  }

  #[test]
  fn test_radians() {
    assert_approx_eq!(Longitude(-1.0).radians(), -1.0_f64.to_radians());
    assert_approx_eq!(Longitude(0.0).radians(), 0.0_f64.to_radians());
    assert_approx_eq!(Longitude(1.0).radians(), 1.0_f64.to_radians());
  }

  #[test]
  fn test_gradians() {
    assert_approx_eq!(Longitude(-1.0).gradians(), -1.0 * 10.0 / 9.0);
    assert_approx_eq!(Longitude(0.0).gradians(), 0.0);
    assert_approx_eq!(Longitude(1.0).gradians(), 1.0 * 10.0 / 9.0);
  }

  #[test]
  fn test_dms() {
    assert_eq!(Longitude(-1.0).dms(), (-1.0, 0.0, 0.0));
    assert_eq!(Longitude(0.0).dms(), (0.0, 0.0, 0.0));
    assert_eq!(Longitude(1.0).dms(), (1.0, 0.0, 0.0));
  }
}
