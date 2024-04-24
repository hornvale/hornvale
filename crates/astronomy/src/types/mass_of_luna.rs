use super::mass_in_kg::MassInKg;
use super::mass_of_earth::MassOfEarth;
use crate::constants::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `MassOfLuna` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct MassOfLuna(pub f64);

impl MassOfLuna {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<MassOfEarth> for MassOfLuna {
  fn from(original: MassOfEarth) -> Self {
    Self(original.0 * LUNA_MASS_PER_EARTH_MASS.0)
  }
}

impl From<MassInKg> for MassOfLuna {
  fn from(original: MassInKg) -> Self {
    Self(original.0 / KG_PER_LUNAR_MASS.0)
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_m_earth_to_m_luna() {
    init();
    let actual: MassOfLuna = MassOfEarth(1.0).into();
    assert_approx_eq!(actual.0, LUNA_MASS_PER_EARTH_MASS.0, 0.01);
  }

  #[test]
  fn test_m_kg_to_m_luna() {
    init();
    let actual: MassOfLuna = MassInKg(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / KG_PER_LUNAR_MASS.0, 0.01);
  }

  #[test]
  fn test_m_luna_to_m_earth() {
    init();
    let actual: MassOfEarth = MassOfLuna(1.0).into();
    assert_approx_eq!(actual.0, 1.0 / LUNA_MASS_PER_EARTH_MASS.0, 0.01);
  }

  #[test]
  fn test_m_luna_to_m_kg() {
    init();
    let actual: MassInKg = MassOfLuna(1.0).into();
    assert_approx_eq!(actual.0, KG_PER_LUNAR_MASS.0, 0.01);
  }

  #[test]
  fn test_m_luna_abs() {
    init();
    let actual: f64 = MassOfLuna(-1.0).abs();
    assert_approx_eq!(actual, 1.0, 0.01);
  }

  #[test]
  fn test_m_luna_default() {
    init();
    let actual: MassOfLuna = Default::default();
    assert_approx_eq!(actual.0, 0.0, 0.01);
  }
}
