use super::length_in_km::LengthInKm;
use super::radius_of_earth::RadiusOfEarth;
use super::radius_of_luna::RadiusOfLuna;
use super::radius_of_sol::RadiusOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `RadiusOfJupiter` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct RadiusOfJupiter(pub f64);

impl RadiusOfJupiter {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInKm> for RadiusOfJupiter {
  fn from(original: LengthInKm) -> Self {
    Self(original.0 / KM_PER_JUPITER_RADIUS.0)
  }
}

impl From<RadiusOfEarth> for RadiusOfJupiter {
  fn from(original: RadiusOfEarth) -> Self {
    Self(original.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
  }
}

impl From<RadiusOfLuna> for RadiusOfJupiter {
  fn from(original: RadiusOfLuna) -> Self {
    Self(original.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
  }
}

impl From<RadiusOfSol> for RadiusOfJupiter {
  fn from(original: RadiusOfSol) -> Self {
    Self(original.0 * KM_PER_SOL_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn from_length_in_km() {
    assert_approx_eq!(
      RadiusOfJupiter::from(LengthInKm(1.0)),
      RadiusOfJupiter(1.0 / KM_PER_JUPITER_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_earth() {
    assert_approx_eq!(
      RadiusOfJupiter::from(RadiusOfEarth(1.0)),
      RadiusOfJupiter(1.0 * KM_PER_EARTH_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_luna() {
    assert_approx_eq!(
      RadiusOfJupiter::from(RadiusOfLuna(1.0)),
      RadiusOfJupiter(1.0 * KM_PER_LUNA_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
    );
  }

  #[test]
  fn from_radius_of_sol() {
    assert_approx_eq!(
      RadiusOfJupiter::from(RadiusOfSol(1.0)),
      RadiusOfJupiter(1.0 * KM_PER_SOL_RADIUS.0 / KM_PER_JUPITER_RADIUS.0)
    );
  }
}
