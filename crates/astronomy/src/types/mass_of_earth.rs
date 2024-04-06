use super::mass_in_kg::MassInKg;
use super::mass_of_jupiter::MassOfJupiter;
use super::mass_of_luna::MassOfLuna;
use super::mass_of_sol::MassOfSol;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `MassOfEarth` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct MassOfEarth(pub f64);

impl From<MassInKg> for MassOfEarth {
  fn from(original: MassInKg) -> Self {
    Self(original.0 / KG_PER_EARTH_MASS.0)
  }
}

impl From<MassOfJupiter> for MassOfEarth {
  fn from(original: MassOfJupiter) -> Self {
    Self(EARTH_MASS_PER_JUPITER_MASS.0 * original.0)
  }
}

impl From<MassOfLuna> for MassOfEarth {
  fn from(original: MassOfLuna) -> Self {
    Self(original.0 / LUNA_MASS_PER_EARTH_MASS.0)
  }
}

impl From<MassOfSol> for MassOfEarth {
  fn from(original: MassOfSol) -> Self {
    Self(original.0 * EARTH_MASS_PER_SOLAR_MASS.0)
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_m_jupiter_to_m_earth() {
    init();
    let actual: MassOfEarth = MassOfJupiter(1.0).into();
    assert_approx_eq!(actual.0, EARTH_MASS_PER_JUPITER_MASS.0, 0.01);
  }

  #[test]
  pub fn test_m_luna_to_m_earth() {
    init();
    let actual: MassOfEarth = MassOfLuna(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / LUNA_MASS_PER_EARTH_MASS.0, 0.01);
  }

  #[test]
  pub fn test_m_kg_to_m_earth() {
    init();
    let actual: MassOfEarth = MassInKg(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / KG_PER_EARTH_MASS.0, 0.01);
  }
}
