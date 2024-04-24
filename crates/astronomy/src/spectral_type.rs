use super::spectral_type_properties::SpectralTypeProperties;
use crate::types::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};
use strum::{EnumIter, IntoEnumIterator};

/// The `SpectralType` enum, representing the spectral type of a star.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, EnumIter, Hash, PartialEq, Serialize)]
pub enum SpectralType {
  ///
  /// MAIN SEQUENCE STARS
  ///
  /// Type O main sequence stars are very hot and very luminous, with most of
  /// their output in the ultraviolet range. They are rare, with only a few
  /// thousand known in our galaxy.
  ///
  /// O3V
  O3V,
  /// O4V
  O4V,
  /// O5V
  O5V,
  /// O6V
  O6V,
  /// O7V
  O7V,
  /// O8V
  O8V,
  /// O9V
  O9V,
  /// Type B main sequence stars are very luminous and blue. Their spectra have
  /// neutral helium, which are most prominent at the B2V subclass.
  ///
  /// B0V
  B0V,
  /// B1V
  B1V,
  /// B2V
  B2V,
  /// B3V
  B3V,
  /// B4V
  B4V,
  /// B5V
  B5V,
  /// B6V
  B6V,
  /// B7V
  B7V,
  /// B8V
  B8V,
  /// B9V
  B9V,
  /// Type A main sequence stars are amongst the more common naked eye stars,
  /// and are white or bluish-white. They have strong hydrogen lines, at a
  /// maximum by A0, and also lines of ionized metals (Fe II, Mg II, Si II) at a
  /// maximum at A5. The presence of Ca II lines is notably strengthening by
  /// this point.
  ///
  /// A0V
  A0V,
  /// A1V
  A1V,
  /// A2V
  A2V,
  /// A3V
  A3V,
  /// A4V
  A4V,
  /// A5V
  A5V,
  /// A6V
  A6V,
  /// A7V
  A7V,
  /// A8V
  A8V,
  /// A9V
  A9V,
  /// Type F main sequence stars are slightly less massive than the Sun and
  /// somewhat hotter. They are main sequence stars that are white.
  ///
  /// F0V
  F0V,
  /// F1V
  F1V,
  /// F2V
  F2V,
  /// F3V
  F3V,
  /// F4V
  F4V,
  /// F5V
  F5V,
  /// F6V
  F6V,
  /// F7V
  F7V,
  /// F8V
  F8V,
  /// F9V
  F9V,
  /// Type G main sequence stars are the most common type of star, and are
  /// yellow. The Sun is a type G star.
  ///
  /// G0V
  G0V,
  /// G1V
  G1V,
  /// G2V
  #[default]
  G2V,
  /// G3V
  G3V,
  /// G4V
  G4V,
  /// G5V
  G5V,
  /// G6V
  G6V,
  /// G7V
  G7V,
  /// G8V
  G8V,
  /// G9V
  G9V,
  /// Type K main sequence stars are slightly cooler than the Sun and are
  /// orange. They make up about 12% of the main sequence stars in the solar
  /// neighborhood.
  ///
  /// K0V
  K0V,
  /// K1V
  K1V,
  /// K2V
  K2V,
  /// K3V
  K3V,
  /// K4V
  K4V,
  /// K5V
  K5V,
  /// K6V
  K6V,
  /// K7V
  K7V,
  /// K8V
  K8V,
  /// K9V
  K9V,
  /// Type M main sequence stars are the most common type of star, and are red.
  /// The late M dwarfs are the most common type of star in the galaxy.
  ///
  /// M0V
  M0V,
  /// M1V
  M1V,
  /// M2V
  M2V,
  /// M3V
  M3V,
  /// M4V
  M4V,
  /// M5V
  M5V,
  /// M6V
  M6V,
  /// M7V
  M7V,
  /// M8V
  M8V,
  /// M9V
  M9V,
}

impl SpectralType {
  /// Get properties for the spectral type.
  pub fn get_properties(&self) -> SpectralTypeProperties {
    *SpectralTypeProperties::hash_map().get(self).unwrap()
  }

  /// Construct a new `Star` object from the mass of the star.
  pub fn from_mass(mass: MassOfSol) -> SpectralType {
    let spectral_type_properties = SpectralTypeProperties::hash_map();
    let mut spectral_type = SpectralType::O3V;
    let mut min_diff = f64::MAX;
    for current_spectral_type in SpectralType::iter() {
      let current_properties = spectral_type_properties[&current_spectral_type];
      let current_mass = current_properties.mass;
      let diff = (mass.0 - current_mass.0).abs();
      if diff < min_diff {
        min_diff = diff;
        spectral_type = current_spectral_type;
      }
    }
    spectral_type
  }

  /// Get the mass of the star.
  pub fn get_mass(&self) -> MassOfSol {
    self.get_properties().mass
  }

  /// Get the radius of the star.
  pub fn get_radius(&self) -> RadiusOfSol {
    self.get_properties().radius
  }

  /// Get the luminosity of the star.
  pub fn get_luminosity(&self) -> LuminosityOfSol {
    self.get_properties().luminosity
  }

  /// Get the temperature of the star.
  pub fn get_temperature(&self) -> TemperatureInKelvin {
    self.get_properties().temperature
  }

  /// Get the density of the star.
  pub fn get_density(&self) -> DensityOfSol {
    let properties = self.get_properties();
    let mass = properties.mass;
    let radius = properties.radius;
    let result = mass.0 / radius.0.powf(3.0);
    DensityOfSol(result)
  }

  /// Get the absolute RGB color of the star.
  pub fn get_absolute_rgb(&self) -> (u8, u8, u8) {
    self.get_properties().absolute_rgb
  }
}

impl From<MassOfSol> for SpectralType {
  fn from(mass: MassOfSol) -> Self {
    SpectralType::from_mass(mass)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::star::Star;
  use crate::test_utilities::prelude::*;
  use strum::IntoEnumIterator;

  #[test]
  fn test_spectral_type_properties() {
    init();
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
    assert_approx_eq!(SpectralType::O3V.get_mass(), MassOfSol(120.0));
    assert_approx_eq!(SpectralType::O3V.get_radius(), RadiusOfSol(15.0));
    assert_approx_eq!(SpectralType::O3V.get_luminosity(), LuminosityOfSol(1_400_000.0));
    assert_approx_eq!(SpectralType::O3V.get_temperature(), TemperatureInKelvin(44_900.0));
    assert_approx_eq!(SpectralType::O3V.get_density(), DensityOfSol(0.03555555555555555));
  }

  #[test]
  fn print_rgb_values() {
    init();
    for spectral_type in SpectralType::iter() {
      let properties = spectral_type.get_properties();
      let mass = properties.mass;
      let rgb = Star::from(mass).get_absolute_rgb();
      println!("{:?} -> {:?}", spectral_type, rgb);
    }
    assert!(true);
  }
}
