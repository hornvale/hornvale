use super::temperature_in_celsius::TemperatureInCelsius;
use super::temperature_in_kelvin::TemperatureInKelvin;
use derive_more::{Add, Display, Div, Mul, Sub};
use serde::{Deserialize, Serialize};

/// The `TemperatureInFahrenheit` newtype.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Display, Div, Mul, PartialEq, PartialOrd, Serialize, Sub)]
#[repr(transparent)]
pub struct TemperatureInFahrenheit(pub f64);

impl TemperatureInFahrenheit {
  /// Returns the absolute value.
  pub fn abs(self) -> f64 {
    self.0.abs()
  }
}

impl From<TemperatureInCelsius> for TemperatureInFahrenheit {
  fn from(original: TemperatureInCelsius) -> Self {
    Self((original.0 * 9.0 / 5.0) + 32.0)
  }
}

impl From<TemperatureInKelvin> for TemperatureInFahrenheit {
  fn from(original: TemperatureInKelvin) -> Self {
    Self((original.0 - 273.15) * 9.0 / 5.0 + 32.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_abs() {
    assert_approx_eq!(TemperatureInFahrenheit(-1.0).abs(), 1.0);
    assert_approx_eq!(TemperatureInFahrenheit(0.0).abs(), 0.0);
    assert_approx_eq!(TemperatureInFahrenheit(1.0).abs(), 1.0);
  }

  #[test]
  fn from_temperature_in_celsius() {
    assert_approx_eq!(
      TemperatureInFahrenheit::from(TemperatureInCelsius(0.0)),
      TemperatureInFahrenheit(32.0)
    );
  }

  #[test]
  fn from_temperature_in_kelvin() {
    assert_approx_eq!(
      TemperatureInFahrenheit::from(TemperatureInKelvin(273.15)),
      TemperatureInFahrenheit(32.0)
    );
  }
}
