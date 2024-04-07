use super::mass_in_kg::MassInKg;
use super::mass_of_earth::MassOfEarth;
use super::mass_of_jupiter::MassOfJupiter;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `MassOfSol` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct MassOfSol(pub f64);

impl MassOfSol {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<MassInKg> for MassOfSol {
  fn from(original: MassInKg) -> Self {
    Self(original.0 / KG_PER_SOLAR_MASS.0)
  }
}

impl From<MassOfJupiter> for MassOfSol {
  fn from(original: MassOfJupiter) -> Self {
    Self(original.0 / JUPITER_MASS_PER_SOLAR_MASS.0)
  }
}

impl From<MassOfEarth> for MassOfSol {
  fn from(original: MassOfEarth) -> Self {
    Self(original.0 / EARTH_MASS_PER_SOLAR_MASS.0)
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_m_earth_to_m_sol() {
    init();
    let actual: MassOfSol = MassOfEarth(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / EARTH_MASS_PER_SOLAR_MASS.0, 0.01);
  }

  #[test]
  fn test_m_kg_to_m_sol() {
    init();
    let actual: MassOfSol = MassInKg(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / KG_PER_SOLAR_MASS.0, 0.01);
  }
}
