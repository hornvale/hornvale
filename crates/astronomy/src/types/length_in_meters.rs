use super::length_in_au::LengthInAu;
use super::length_in_km::LengthInKm;
use super::length_in_lyr::LengthInLyr;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `LengthInMeters` newtype, representing length in meters.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct LengthInMeters(pub f64);

impl LengthInMeters {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<LengthInAu> for LengthInMeters {
  fn from(original: LengthInAu) -> Self {
    Self(original.0 * KM_PER_AU.0 * 1000.0)
  }
}

impl From<LengthInKm> for LengthInMeters {
  fn from(original: LengthInKm) -> Self {
    Self(original.0 * 1000.0)
  }
}

impl From<LengthInLyr> for LengthInMeters {
  fn from(original: LengthInLyr) -> Self {
    Self(original.0 * KM_PER_LYR.0 * 1000.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(LengthInMeters(-1.0).abs(), 1.0);
    assert_approx_eq!(LengthInMeters(0.0).abs(), 0.0);
    assert_approx_eq!(LengthInMeters(1.0).abs(), 1.0);
  }

  #[test]
  fn from_length_in_au() {
    assert_approx_eq!(
      LengthInMeters::from(LengthInAu(1.0)),
      LengthInMeters(1.0 * KM_PER_AU.0 * 1000.0)
    );
  }

  #[test]
  fn from_length_in_km() {
    assert_approx_eq!(LengthInMeters::from(LengthInKm(1.0)), LengthInMeters(1.0 * 1000.0));
  }

  #[test]
  fn from_length_in_lyr() {
    assert_approx_eq!(
      LengthInMeters::from(LengthInLyr(1.0)),
      LengthInMeters(1.0 * KM_PER_LYR.0 * 1000.0)
    );
  }
}
