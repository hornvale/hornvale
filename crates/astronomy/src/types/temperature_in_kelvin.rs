use super::temperature_in_celsius::TemperatureInCelsius;
use super::temperature_in_fahrenheit::TemperatureInFahrenheit;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TemperatureInKelvin` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TemperatureInKelvin(pub f64);

impl TemperatureInKelvin {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TemperatureInCelsius> for TemperatureInKelvin {
  fn from(original: TemperatureInCelsius) -> Self {
    Self(original.0 + 273.15)
  }
}

impl From<TemperatureInFahrenheit> for TemperatureInKelvin {
  fn from(original: TemperatureInFahrenheit) -> Self {
    Self((original.0 - 32.0) * 5.0 / 9.0 + 273.15)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(TemperatureInKelvin(-1.0).abs(), 1.0);
    assert_approx_eq!(TemperatureInKelvin(0.0).abs(), 0.0);
    assert_approx_eq!(TemperatureInKelvin(1.0).abs(), 1.0);
  }

  #[test]
  fn from_temperature_in_celsius() {
    assert_approx_eq!(
      TemperatureInKelvin::from(TemperatureInCelsius(0.0)),
      TemperatureInKelvin(273.15)
    );
  }

  #[test]
  fn from_temperature_in_fahrenheit() {
    assert_approx_eq!(
      TemperatureInKelvin::from(TemperatureInFahrenheit(32.0)),
      TemperatureInKelvin(273.15)
    );
  }
}
