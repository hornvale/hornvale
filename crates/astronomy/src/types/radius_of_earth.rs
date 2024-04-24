use super::length_in_km::LengthInKm;
use super::radius_of_jupiter::RadiusOfJupiter;
use super::radius_of_luna::RadiusOfLuna;
use super::radius_of_sol::RadiusOfSol;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `RadiusOfEarth` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct RadiusOfEarth(pub f64);

impl RadiusOfEarth {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInKm> for RadiusOfEarth {
  fn from(original: LengthInKm) -> Self {
    Self(original.0 / KM_PER_EARTH_RADIUS.0)
  }
}

impl From<RadiusOfJupiter> for RadiusOfEarth {
  fn from(original: RadiusOfJupiter) -> Self {
    Self(original.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
  }
}

impl From<RadiusOfLuna> for RadiusOfEarth {
  fn from(original: RadiusOfLuna) -> Self {
    Self(original.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
  }
}

impl From<RadiusOfSol> for RadiusOfEarth {
  fn from(original: RadiusOfSol) -> Self {
    Self(original.0 * KM_PER_SOL_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(RadiusOfEarth(-1.0).abs(), 1.0);
    assert_approx_eq!(RadiusOfEarth(0.0).abs(), 0.0);
    assert_approx_eq!(RadiusOfEarth(1.0).abs(), 1.0);
  }

  #[test]
  fn from_length_in_km() {
    assert_approx_eq!(
      RadiusOfEarth::from(LengthInKm(1.0)),
      RadiusOfEarth(1.0 / KM_PER_EARTH_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_jupiter() {
    assert_approx_eq!(
      RadiusOfEarth::from(RadiusOfJupiter(1.0)),
      RadiusOfEarth(1.0 * KM_PER_JUPITER_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_luna() {
    assert_approx_eq!(
      RadiusOfEarth::from(RadiusOfLuna(1.0)),
      RadiusOfEarth(1.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_sol() {
    assert_approx_eq!(
      RadiusOfEarth::from(RadiusOfSol(1.0)),
      RadiusOfEarth(1.0 * KM_PER_SOL_RADIUS.0 / KM_PER_EARTH_RADIUS.0)
    );
  }
}
