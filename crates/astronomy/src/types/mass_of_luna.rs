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
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_m_earth_to_m_luna() {
    init();
    let actual: MassOfLuna = MassOfEarth(1.0).into();
    assert_approx_eq!(actual.0, LUNA_MASS_PER_EARTH_MASS.0, 0.01);
  }
}
