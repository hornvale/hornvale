use super::radius_of_earth::RadiusOfEarth;
use super::radius_of_jupiter::RadiusOfJupiter;
use super::radius_of_luna::RadiusOfLuna;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `RadiusOfSol` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct RadiusOfSol(pub f64);

impl RadiusOfSol {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<RadiusOfEarth> for RadiusOfSol {
  fn from(original: RadiusOfEarth) -> Self {
    Self(original.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_SOL_RADIUS.0)
  }
}

impl From<RadiusOfJupiter> for RadiusOfSol {
  fn from(original: RadiusOfJupiter) -> Self {
    Self(original.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_SOL_RADIUS.0)
  }
}

impl From<RadiusOfLuna> for RadiusOfSol {
  fn from(original: RadiusOfLuna) -> Self {
    Self(original.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_SOL_RADIUS.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(RadiusOfSol(-1.0).abs(), 1.0);
    assert_approx_eq!(RadiusOfSol(0.0).abs(), 0.0);
    assert_approx_eq!(RadiusOfSol(1.0).abs(), 1.0);
  }

  #[test]
  fn from_radius_of_earth() {
    assert_approx_eq!(
      RadiusOfSol::from(RadiusOfEarth(1.0)),
      RadiusOfSol(1.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_SOL_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_jupiter() {
    assert_approx_eq!(
      RadiusOfSol::from(RadiusOfJupiter(1.0)),
      RadiusOfSol(1.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_SOL_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_luna() {
    assert_approx_eq!(
      RadiusOfSol::from(RadiusOfLuna(1.0)),
      RadiusOfSol(1.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_SOL_RADIUS.0)
    );
  }
}
