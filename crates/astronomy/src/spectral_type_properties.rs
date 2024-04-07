use super::spectral_type::SpectralType;
use crate::types::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::OnceLock;

/// The `SpectralTypeProperties` struct.
#[derive(Clone, Copy, Debug, Deserialize, Display, PartialEq, Serialize)]
#[display(fmt = "{:#?}", self)]
pub struct SpectralTypeProperties {
  /// The mass of the star.
  pub mass: MassOfSol,
  /// The radius of the star.
  pub radius: RadiusOfSol,
  /// The luminosity of the star.
  pub luminosity: LuminosityOfSol,
  /// The temperature of the star.
  pub temperature: TemperatureInKelvin,
}

macro_rules! define_spectral_type_properties {
  ($hash_map:ident, {
    $(($spectral_type:ident, $mass:expr, $radius:expr, $luminosity:expr, $temperature:expr)),* $(,)?
  }) => {{
    $hash_map.get_or_init(|| {
      let mut map = HashMap::new();
      $(
        map.insert(SpectralType::$spectral_type, SpectralTypeProperties {
          mass: MassOfSol($mass),
          radius: RadiusOfSol($radius),
          luminosity: LuminosityOfSol($luminosity),
          temperature: TemperatureInKelvin($temperature),
        });
      )*
      map
    })
  }};
}

impl SpectralTypeProperties {
  /// Get the spectral type properties hashmap.
  pub fn hash_map() -> &'static HashMap<SpectralType, SpectralTypeProperties> {
    static HASH_MAP: OnceLock<HashMap<SpectralType, SpectralTypeProperties>> = OnceLock::new();
    define_spectral_type_properties!(HASH_MAP, {
      (O3V, 120.00, 15.00, 1_400_000.0, 44_900.0),
      (O4V, 85.31, 13.43, 1_073_019.0, 42_900.0),
      (O5V, 60.00, 12.0, 790_000.0, 41_400.0),
      (O6V, 43.71, 10.71, 540_422.0, 39_500.0),
      (O7V, 30.85, 9.52, 317_322.0, 37_100.0),
      (O8V, 23.00, 8.50, 170_000.0, 35_100.0),
      (O9V, 19.63, 7.51, 92_762.0, 33_300.0),
      (B0V, 17.70, 7.16, 44_668.0, 31_400.0),
      (B1V, 11.00, 5.71, 13_490.0, 26_000.0),
      (B2V, 7.30, 4.06, 2_692.0, 20_600.0),
      (B3V, 5.40, 3.61, 977.0, 17_000.0),
      (B4V, 5.10, 3.46, 776.0, 16_400.0),
      (B5V, 4.70, 3.36, 589.0, 15_700.0),
      (B6V, 4.30, 3.27, 372.0, 14_500.0),
      (B7V, 3.92, 2.94, 302.0, 14_000.0),
      (B8V, 3.38, 2.86, 155.0, 12_300.0),
      (B9V, 2.75, 2.49, 72.0, 10_700.0),
      (A0V, 2.18, 2.193, 38.02, 9_700.0),
      (A1V, 2.05, 2.136, 30.90, 9_300.0),
      (A2V, 1.98, 2.117, 23.99, 8_800.0),
      (A3V, 1.93, 1.861, 16.98, 8_600.0),
      (A4V, 1.88, 1.794, 13.49, 8_250.0),
      (A5V, 1.86, 1.785, 12.30, 8_100.0),
      (A6V, 1.83, 1.775, 11.22, 7_910.0),
      (A7V, 1.81, 1.750, 10.00, 7_760.0),
      (A8V, 1.77, 1.748, 9.12, 7_590.0),
      (A9V, 1.75, 1.747, 8.32, 7_400.0),
      (F0V, 1.61, 1.728, 7.24, 7_220.0),
      (F1V, 1.50, 1.679, 6.17, 7_020.0),
      (F2V, 1.46, 1.622, 5.13, 6_820.0),
      (F3V, 1.44, 1.578, 4.68, 6_750.0),
      (F4V, 1.38, 1.533, 4.17, 6_670.0),
      (F5V, 1.33, 1.473, 3.63, 6_550.0),
      (F6V, 1.25, 1.359, 2.69, 6_350.0),
      (F7V, 1.21, 1.324, 2.45, 6_280.0),
      (F8V, 1.18, 1.221, 1.95, 6_180.0),
      (F9V, 1.13, 1.167, 1.66, 6_050.0),
      (G0V, 1.06, 1.100, 1.35, 5_930.0),
      (G1V, 1.03, 1.060, 1.20, 5_860.0),
      (G2V, 1.00, 1.012, 1.02, 5_770.0),
      (G3V, 0.99, 1.002, 0.98, 5_720.0),
      (G4V, 0.985, 0.991, 0.91, 5_680.0),
      (G5V, 0.98, 0.977, 0.89, 5_660.0),
      (G6V, 0.97, 0.949, 0.79, 5_600.0),
      (G7V, 0.95, 0.927, 0.74, 5_550.0),
      (G8V, 0.94, 0.914, 0.68, 5_480.0),
      (G9V, 0.90, 0.853, 0.55, 5_380.0),
      (K0V, 0.88, 0.813, 0.46, 5_270.0),
      (K1V, 0.86, 0.797, 0.41, 5_170.0),
      (K2V, 0.82, 0.783, 0.37, 5_100.0),
      (K3V, 0.78, 0.755, 0.28, 4_830.0),
      (K4V, 0.73, 0.713, 0.20, 4_600.0),
      (K5V, 0.70, 0.701, 0.17, 4_440.0),
      (K6V, 0.69, 0.669, 0.14, 4_300.0),
      (K7V, 0.64, 0.630, 0.10, 4_100.0),
      (K8V, 0.62, 0.615, 0.087, 3_990.0),
      (K9V, 0.59, 0.608, 0.079, 3_930.0),
      (M0V, 0.57, 0.588, 0.069, 3_850.0),
      (M1V, 0.50, 0.501, 0.041, 3_660.0),
      (M2V, 0.44, 0.446, 0.029, 3_560.0),
      (M3V, 0.37, 0.361, 0.016, 3_430.0),
      (M4V, 0.23, 0.274, 7.2e-3, 3_210.0),
      (M5V, 0.162, 0.196, 3.0e-3, 3_060.0),
      (M6V, 0.102, 0.137, 1.0e-3, 2_810.0),
      (M7V, 0.090, 0.120, 6.5e-4, 2_680.0),
      (M8V, 0.085, 0.114, 5.2e-4, 2_570.0),
      (M9V, 0.079, 0.102, 3.0e-4, 2_380.0),
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::spectral_type::SpectralType;

  #[test]
  fn test_spectral_type_properties() {
    let spectral_type_properties = SpectralTypeProperties::hash_map();
    assert_eq!(spectral_type_properties.len(), 67);
    assert_eq!(
      spectral_type_properties[&SpectralType::O3V],
      SpectralTypeProperties {
        mass: MassOfSol(120.0),
        radius: RadiusOfSol(15.0),
        luminosity: LuminosityOfSol(1_400_000.0),
        temperature: TemperatureInKelvin(44_900.0),
      }
    );
    assert_eq!(
      spectral_type_properties[&SpectralType::M9V],
      SpectralTypeProperties {
        mass: MassOfSol(0.079),
        radius: RadiusOfSol(0.102),
        luminosity: LuminosityOfSol(3.0e-4),
        temperature: TemperatureInKelvin(2_380.0),
      }
    );
  }
}
