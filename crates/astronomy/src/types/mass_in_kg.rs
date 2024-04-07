use super::mass_of_earth::MassOfEarth;
use super::mass_of_jupiter::MassOfJupiter;
use super::mass_of_luna::MassOfLuna;
use super::mass_of_sol::MassOfSol;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `MassInKg` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct MassInKg(pub f64);

impl From<MassOfSol> for MassInKg {
  fn from(original: MassOfSol) -> Self {
    Self(original.0 * KG_PER_SOLAR_MASS.0)
  }
}

impl From<MassOfJupiter> for MassInKg {
  fn from(original: MassOfJupiter) -> Self {
    Self(original.0 * KG_PER_JUPITER_MASS.0)
  }
}

impl From<MassOfEarth> for MassInKg {
  fn from(original: MassOfEarth) -> Self {
    Self(original.0 * KG_PER_EARTH_MASS.0)
  }
}

impl From<MassOfLuna> for MassInKg {
  fn from(original: MassOfLuna) -> Self {
    Self(original.0 * KG_PER_LUNAR_MASS.0)
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_m_sol_to_m_kg() {
    init();
    let actual: MassInKg = MassOfSol(1.0).into();
    assert_approx_eq!(actual.0, KG_PER_SOLAR_MASS.0, 0.01);
  }

  #[test]
  fn test_m_earth_to_m_kg() {
    init();
    let actual: MassInKg = MassOfEarth(1.0).into();
    assert_approx_eq!(actual.0, KG_PER_EARTH_MASS.0, 0.01);
  }

  #[test]
  fn test_m_jupiter_to_m_kg() {
    init();
    let actual: MassInKg = MassOfJupiter(1.0).into();
    assert_approx_eq!(actual.0, KG_PER_JUPITER_MASS.0, 0.01);
  }

  #[test]
  fn test_m_luna_to_m_kg() {
    init();
    let actual: MassInKg = MassOfLuna(1.0).into();
    assert_approx_eq!(actual.0, KG_PER_LUNAR_MASS.0, 0.01);
  }
}
