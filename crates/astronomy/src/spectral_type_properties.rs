use super::spectral_type::SpectralType;
use crate::constants::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use derive_more::Display;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::OnceLock;

/// The `SpectralTypeProperties` struct.
#[derive(Clone, Copy, Debug, Deserialize, Display, PartialEq, Serialize, Builder)]
#[display(fmt = "{:#?}", self)]
pub struct SpectralTypeProperties {
  /// The mass of the star.
  #[builder(default = "MassOfSol(1.0)")]
  pub mass: MassOfSol,
  /// The radius of the star.
  #[builder(default = "RadiusOfSol(1.0)")]
  pub radius: RadiusOfSol,
  /// The luminosity of the star.
  #[builder(default = "LuminosityOfSol(1.0)")]
  pub luminosity: LuminosityOfSol,
  /// The temperature of the star.
  #[builder(default = "SOL_TEMPERATURE_IN_KELVIN")]
  pub temperature: TemperatureInKelvin,
  /// The absolute RGB color of the star.
  #[builder(default = "(255, 252, 245)")]
  pub absolute_rgb: (u8, u8, u8),
  /// Whether this is a main sequence star.
  #[builder(default = "true")]
  pub is_main_sequence: bool,
}

macro_rules! define_spectral_type_properties {
  ($hash_map:ident, {
    $(($spectral_type:ident, $mass:expr, $radius:expr, $luminosity:expr, $temperature:expr, $absolute_rgb:expr, $is_main_sequence:expr)),* $(,)?
  }) => {{
    $hash_map.get_or_init(|| {
      let mut map = HashMap::new();
      $(
        map.insert(SpectralType::$spectral_type, SpectralTypeProperties {
          mass: MassOfSol($mass),
          radius: RadiusOfSol($radius),
          luminosity: LuminosityOfSol($luminosity),
          temperature: TemperatureInKelvin($temperature),
          absolute_rgb: $absolute_rgb,
          is_main_sequence: $is_main_sequence,
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
      (O3V, 120.00, 15.00, 1_400_000.0, 44_900.0, (215, 252, 255), true),
      (O4V, 85.31, 13.43, 1_073_019.0, 42_900.0, (215, 252, 255), true),
      (O5V, 60.00, 12.0, 790_000.0, 41_400.0, (215, 252, 255), true),
      (O6V, 43.71, 10.71, 540_422.0, 39_500.0, (216, 252, 255), true),
      (O7V, 30.85, 9.52, 317_322.0, 37_100.0, (216, 253, 255), true),
      (O8V, 23.00, 8.50, 170_000.0, 35_100.0, (217, 253, 255), true),
      (O9V, 19.63, 7.51, 92_762.0, 33_300.0, (218, 253, 255), true),
      (B0V, 17.70, 7.16, 44_668.0, 31_400.0, (218, 253, 255), true),
      (B1V, 11.00, 5.71, 13_490.0, 26_000.0, (221, 253, 255), true),
      (B2V, 7.30, 4.06, 2_692.0, 20_600.0, (224, 253, 255), true),
      (B3V, 5.40, 3.61, 977.0, 17_000.0, (226, 254, 255), true),
      (B4V, 5.10, 3.46, 776.0, 16_400.0, (227, 254, 255), true),
      (B5V, 4.70, 3.36, 589.0, 15_700.0, (228, 254, 255), true),
      (B6V, 4.30, 3.27, 372.0, 14_500.0, (229, 254, 255), true),
      (B7V, 3.92, 2.94, 302.0, 14_000.0, (230, 254, 255), true),
      (B8V, 3.38, 2.86, 155.0, 12_300.0, (232, 254, 255), true),
      (B9V, 2.75, 2.49, 72.0, 10_700.0, (235, 254, 255), true),
      (A0V, 2.18, 2.193, 38.02, 9_700.0, (240, 254, 255), true),
      (A1V, 2.05, 2.136, 30.90, 9_300.0, (241, 254, 255), true),
      (A2V, 1.98, 2.117, 23.99, 8_800.0, (242, 254, 255), true),
      (A3V, 1.93, 1.861, 16.98, 8_600.0, (242, 254, 255), true),
      (A4V, 1.88, 1.794, 13.49, 8_250.0, (243, 254, 255), true),
      (A5V, 1.86, 1.785, 12.30, 8_100.0, (244, 254, 255), true),
      (A6V, 1.83, 1.775, 11.22, 7_910.0, (244, 254, 255), true),
      (A7V, 1.81, 1.750, 10.00, 7_760.0, (244, 254, 255), true),
      (A8V, 1.77, 1.748, 9.12, 7_590.0, (245, 254, 255), true),
      (A9V, 1.75, 1.747, 8.32, 7_400.0, (245, 254, 255), true),
      (F0V, 1.61, 1.728, 7.24, 7_220.0, (248, 254, 255), true),
      (F1V, 1.50, 1.679, 6.17, 7_020.0, (250, 253, 255), true),
      (F2V, 1.46, 1.622, 5.13, 6_820.0, (251, 253, 255), true),
      (F3V, 1.44, 1.578, 4.68, 6_750.0, (252, 253, 255), true),
      (F4V, 1.38, 1.533, 4.17, 6_670.0, (253, 253, 255), true),
      (F5V, 1.33, 1.473, 3.63, 6_550.0, (254, 253, 255), true),
      (F6V, 1.25, 1.359, 2.69, 6_350.0, (255, 253, 255), true),
      (F7V, 1.21, 1.324, 2.45, 6_280.0, (255, 253, 255), true),
      (F8V, 1.18, 1.221, 1.95, 6_180.0, (255, 253, 255), true),
      (F9V, 1.13, 1.167, 1.66, 6_050.0, (255, 252, 255), true),
      (G0V, 1.06, 1.100, 1.35, 5_930.0, (255, 252, 250), true),
      (G1V, 1.03, 1.060, 1.20, 5_860.0, (255, 252, 247), true),
      (G2V, 1.00, 1.000, 1.000, 5_776.0, (255, 252, 245), true),
      (G3V, 0.99, 1.002, 0.98, 5_720.0, (255, 252, 244), true),
      (G4V, 0.985, 0.991, 0.91, 5_680.0, (255, 252, 243), true),
      (G5V, 0.98, 0.977, 0.89, 5_660.0, (255, 252, 243), true),
      (G6V, 0.97, 0.949, 0.79, 5_600.0, (255, 252, 242), true),
      (G7V, 0.95, 0.927, 0.74, 5_550.0, (255, 251, 241), true),
      (G8V, 0.94, 0.914, 0.68, 5_480.0, (255, 251, 240), true),
      (G9V, 0.90, 0.853, 0.55, 5_380.0, (255, 251, 237), true),
      (K0V, 0.88, 0.813, 0.46, 5_270.0, (255, 251, 235), true),
      (K1V, 0.86, 0.797, 0.41, 5_170.0, (255, 251, 233), true),
      (K2V, 0.82, 0.783, 0.37, 5_100.0, (255, 250, 229), true),
      (K3V, 0.78, 0.755, 0.28, 4_830.0, (255, 250, 225), true),
      (K4V, 0.73, 0.713, 0.20, 4_600.0, (255, 249, 220), true),
      (K5V, 0.70, 0.701, 0.17, 4_440.0, (255, 249, 216), true),
      (K6V, 0.69, 0.669, 0.14, 4_300.0, (255, 249, 215), true),
      (K7V, 0.64, 0.630, 0.10, 4_100.0, (255, 248, 208), true),
      (K8V, 0.62, 0.615, 0.087, 3_990.0, (255, 248, 205), true),
      (K9V, 0.59, 0.608, 0.079, 3_930.0, (255, 247, 201), true),
      (M0V, 0.57, 0.588, 0.069, 3_850.0, (255, 246, 197), true),
      (M1V, 0.50, 0.501, 0.041, 3_660.0, (255, 245, 185), true),
      (M2V, 0.44, 0.446, 0.029, 3_560.0, (255, 242, 172), true),
      (M3V, 0.37, 0.361, 0.016, 3_430.0, (255, 241, 163), true),
      (M4V, 0.23, 0.274, 7.2e-3, 3_210.0, (255, 238, 148), true),
      (M5V, 0.162, 0.196, 3.0e-3, 3_060.0, (255, 235, 136), true),
      (M6V, 0.102, 0.137, 1.0e-3, 2_810.0, (255, 231, 120), true),
      (M7V, 0.090, 0.120, 6.5e-4, 2_680.0, (255, 230, 115), true),
      (M8V, 0.085, 0.114, 5.2e-4, 2_570.0, (255, 229, 113), true),
      (M9V, 0.079, 0.102, 3.0e-4, 2_380.0, (255, 228, 110), true),
    })
  }

  /// Get the spectral type properties for a given spectral type.
  pub fn get(spectral_type: SpectralType) -> Option<SpectralTypeProperties> {
    Self::hash_map().get(&spectral_type).copied()
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
        absolute_rgb: (215, 252, 255),
        is_main_sequence: true,
      }
    );
    assert_eq!(
      spectral_type_properties[&SpectralType::M9V],
      SpectralTypeProperties {
        mass: MassOfSol(0.079),
        radius: RadiusOfSol(0.102),
        luminosity: LuminosityOfSol(3.0e-4),
        temperature: TemperatureInKelvin(2_380.0),
        absolute_rgb: (255, 228, 110),
        is_main_sequence: true,
      }
    );
  }
}
