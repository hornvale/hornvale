use super::temperature_in_fahrenheit::TemperatureInFahrenheit;
use super::temperature_in_kelvin::TemperatureInKelvin;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TemperatureInCelsius` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TemperatureInCelsius(pub f64);

impl TemperatureInCelsius {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TemperatureInFahrenheit> for TemperatureInCelsius {
  fn from(original: TemperatureInFahrenheit) -> Self {
    Self((original.0 - 32.0) * 5.0 / 9.0)
  }
}

impl From<TemperatureInKelvin> for TemperatureInCelsius {
  fn from(original: TemperatureInKelvin) -> Self {
    Self(original.0 - 273.15)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(TemperatureInCelsius(-1.0).abs(), 1.0);
    assert_approx_eq!(TemperatureInCelsius(0.0).abs(), 0.0);
    assert_approx_eq!(TemperatureInCelsius(1.0).abs(), 1.0);
  }

  #[test]
  fn from_temperature_in_fahrenheit() {
    assert_approx_eq!(
      TemperatureInCelsius::from(TemperatureInFahrenheit(32.0)),
      TemperatureInCelsius(0.0)
    );
  }

  #[test]
  fn from_temperature_in_kelvin() {
    assert_approx_eq!(
      TemperatureInCelsius::from(TemperatureInKelvin(273.15)),
      TemperatureInCelsius(0.0)
    );
  }
}
