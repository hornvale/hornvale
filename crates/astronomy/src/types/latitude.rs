use super::latitude_direction::LatitudeDirection;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `Latitude` newtype, representing a latitude.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct Latitude(pub f64);

impl Latitude {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }

  /// From a value in degrees.
  pub fn from_degrees(degrees: f64) -> Self {
    Self(degrees)
  }

  /// From a value in degrees, minutes, and seconds.
  pub fn from_dms(degrees: f64, minutes: f64, seconds: f64) -> Self {
    Self(degrees + minutes / 60.0 + seconds / 3600.0)
  }

  /// Returns the direction.
  pub fn direction(self) -> LatitudeDirection {
    if self.0 >= 0.0 {
      LatitudeDirection::North
    } else {
      LatitudeDirection::South
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
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(Latitude(-1.0).abs(), 1.0);
    assert_approx_eq!(Latitude(0.0).abs(), 0.0);
    assert_approx_eq!(Latitude(1.0).abs(), 1.0);
  }

  #[test]
  fn direction() {
    assert_eq!(Latitude(0.0).direction(), LatitudeDirection::North);
    assert_eq!(Latitude(-1.0).direction(), LatitudeDirection::South);
  }

  #[test]
  fn degrees() {
    assert_approx_eq!(Latitude(-1.0).degrees(), -1.0);
    assert_approx_eq!(Latitude(0.0).degrees(), 0.0);
    assert_approx_eq!(Latitude(1.0).degrees(), 1.0);
  }

  #[test]
  fn radians() {
    assert_approx_eq!(Latitude(-1.0).radians(), -1.0_f64.to_radians());
    assert_approx_eq!(Latitude(0.0).radians(), 0.0_f64.to_radians());
    assert_approx_eq!(Latitude(1.0).radians(), 1.0_f64.to_radians());
  }

  #[test]
  fn gradians() {
    assert_approx_eq!(Latitude(-1.0).gradians(), -1.0 * 10.0 / 9.0);
    assert_approx_eq!(Latitude(0.0).gradians(), 0.0);
    assert_approx_eq!(Latitude(1.0).gradians(), 1.0 * 10.0 / 9.0);
  }

  #[test]
  fn dms() {
    assert_eq!(Latitude(-1.0).dms(), (-1.0, 0.0, 0.0));
    assert_eq!(Latitude(0.0).dms(), (0.0, 0.0, 0.0));
    assert_eq!(Latitude(1.0).dms(), (1.0, 0.0, 0.0));
  }
}
