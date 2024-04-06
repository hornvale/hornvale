use super::escape_velocity_of_earth::EscapeVelocityOfEarth;
use super::speed_in_km_per_sec::SpeedInKmPerSec;
use crate::constants::prelude::*;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `EscapeVelocityOfLuna` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct EscapeVelocityOfLuna(pub f64);

impl EscapeVelocityOfLuna {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<EscapeVelocityOfEarth> for EscapeVelocityOfLuna {
  fn from(original: EscapeVelocityOfEarth) -> Self {
    EscapeVelocityOfLuna(original.0 * ESCAPE_VELOCITY_EARTH.0 / ESCAPE_VELOCITY_LUNA.0)
  }
}

impl From<SpeedInKmPerSec> for EscapeVelocityOfLuna {
  fn from(original: SpeedInKmPerSec) -> Self {
    EscapeVelocityOfLuna(original.0 / ESCAPE_VELOCITY_LUNA.0)
  }
}
