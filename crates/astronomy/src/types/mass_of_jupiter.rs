use super::mass_in_kg::MassInKg;
use super::mass_of_earth::MassOfEarth;
use super::mass_of_luna::MassOfLuna;
use super::mass_of_sol::MassOfSol;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `MassOfJupiter` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct MassOfJupiter(pub f64);

impl MassOfJupiter {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<MassOfEarth> for MassOfJupiter {
  fn from(original: MassOfEarth) -> Self {
    Self(original.0 / EARTH_MASS_PER_JUPITER_MASS.0)
  }
}

impl From<MassOfLuna> for MassOfJupiter {
  fn from(original: MassOfLuna) -> Self {
    Self(original.0 * LUNA_MASS_PER_EARTH_MASS.0 / EARTH_MASS_PER_JUPITER_MASS.0)
  }
}

impl From<MassOfSol> for MassOfJupiter {
  fn from(original: MassOfSol) -> Self {
    Self(original.0 * JUPITER_MASS_PER_SOLAR_MASS.0)
  }
}

impl From<MassInKg> for MassOfJupiter {
  fn from(original: MassInKg) -> Self {
    Self(original.0 / KG_PER_JUPITER_MASS.0)
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_m_earth_to_m_jupiter() {
    init();
    let actual: MassOfJupiter = MassOfEarth(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / EARTH_MASS_PER_JUPITER_MASS.0, 0.01);
  }

  #[test]
  pub fn test_m_luna_to_m_jupiter() {
    init();
    let actual: MassOfJupiter = MassOfLuna(1.0).into();
    assert_approx_eq!(
      actual.0,
      LUNA_MASS_PER_EARTH_MASS.0 / EARTH_MASS_PER_JUPITER_MASS.0,
      0.01
    );
  }

  #[test]
  pub fn test_m_sol_to_m_jupiter() {
    init();
    let actual: MassOfJupiter = MassOfSol(1.0).into();
    assert_approx_eq!(actual.0, JUPITER_MASS_PER_SOLAR_MASS.0, 0.01);
  }
}
