use crate::constants::prelude::*;
use crate::error::AstronomyError;
use crate::spectral_type::SpectralType;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `Star` is a single main-sequence star.
///
/// This is intended to encompass the most useful information we can generate
/// about main-sequence stars.  Other types will use different structs; it's
/// useful to view and treat these as the default sense of "star", given their
/// centrality to our purpose.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct Star {
  /// The mass of the star, in solar masses.
  #[builder(default = "SpectralType::default()")]
  pub spectral_type: SpectralType,
  /// The age of the star, in gigayears.
  #[builder(default = "TimeInGigayears(4.6)")]
  pub current_age: TimeInGigayears,
}

impl Star {
  /// Create a new `Star` builder.
  pub fn builder() -> StarBuilder {
    StarBuilder::default()
  }

  /// Check the mass of the star to see if it's within the main sequence.
  pub fn check_mass(&self) -> Result<(), AstronomyError> {
    let mass = self.spectral_type.get_mass();
    if mass < MINIMUM_MAIN_SEQUENCE_STAR_MASS {
      return Err(AstronomyError::StarMassTooLowForMainSequence);
    }
    if mass > MAXIMUM_MAIN_SEQUENCE_STAR_MASS {
      return Err(AstronomyError::StarMassTooHighForMainSequence);
    }
    Ok(())
  }

  /// Get the temperature of the star.
  pub fn get_temperature(&self) -> Result<TemperatureInKelvin, AstronomyError> {
    self.check_mass()?;
    Ok(self.spectral_type.get_temperature())
  }

  /// Get the luminosity of the star.
  pub fn get_luminosity(&self) -> Result<LuminosityOfSol, AstronomyError> {
    self.check_mass()?;
    Ok(self.spectral_type.get_luminosity())
  }

  /// Get the radius of the star.
  pub fn get_radius(&self) -> Result<RadiusOfSol, AstronomyError> {
    self.check_mass()?;
    Ok(self.spectral_type.get_radius())
  }

  /// Get the density of the star.
  pub fn get_density(&self) -> Result<DensityOfSol, AstronomyError> {
    self.check_mass()?;
    Ok(self.spectral_type.get_density())
  }

  /// Get the absolute RGB of the star.
  pub fn get_absolute_rgb(&self) -> Result<(u8, u8, u8), AstronomyError> {
    self.check_mass()?;
    Ok(self.spectral_type.get_absolute_rgb())
  }

  /// Get the life expectancy of the star.
  pub fn get_life_expectancy(&self) -> Result<TimeInGigayears, AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?.0;
    let mass = self.get_mass()?.0;
    let result = mass / luminosity * 10.0;
    Ok(TimeInGigayears(result))
  }

  /// Get the habitable zone of the star.
  pub fn get_habitable_zone(&self) -> Result<(LengthInAu, LengthInAu), AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?;
    let inner_bound = (luminosity.0 / 1.1).sqrt();
    let outer_bound = (luminosity.0 / 0.53).sqrt();
    Ok((LengthInAu(inner_bound), LengthInAu(outer_bound)))
  }

  /// Get the satellite zone of the star.
  pub fn get_satellite_zone(&self) -> Result<(LengthInAu, LengthInAu), AstronomyError> {
    self.check_mass()?;
    let inner_bound = 0.1 * self.get_mass()?.0;
    let outer_bound = 40.0 * self.get_mass()?.0;
    Ok((LengthInAu(inner_bound), LengthInAu(outer_bound)))
  }

  /// Get the mass of the star.
  pub fn get_mass(&self) -> Result<MassOfSol, AstronomyError> {
    Ok(self.spectral_type.get_mass())
  }

  /// Get the frost line of the star.
  ///
  /// This uses the common formula for the frost line, which is 4.85 times the
  /// square root of the luminosity of the star.
  ///
  /// Other formulas exist, giving figures like ~2.75 AU for the frost line of
  /// our own solar system. This may be a more accurate figure during the early
  /// solar system, when the Sun was less luminous and gas and dust were more
  /// prevalent.
  pub fn get_frost_line(&self) -> Result<LengthInAu, AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?;
    let result = 4.85 * luminosity.0.sqrt();
    Ok(LengthInAu(result))
  }
}

impl Default for Star {
  fn default() -> Self {
    StarBuilder::default().build().unwrap()
  }
}

impl MaybeHabitable for Star {
  /// Indicate whether this star is capable of supporting conventional life.
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    self.check_mass()?;
    let mass = self.get_mass()?;
    if mass < MINIMUM_HABITABLE_STAR_MASS {
      return Err(AstronomyError::StarMassTooLowToSupportLife);
    }
    if mass > MAXIMUM_HABITABLE_STAR_MASS {
      return Err(AstronomyError::StarMassTooHighToSupportLife);
    }
    if self.current_age < MINIMUM_HABITABLE_STAR_AGE {
      return Err(AstronomyError::StarTooYoungToSupportLife);
    }
    Ok(())
  }
}

impl From<MassOfSol> for Star {
  fn from(mass: MassOfSol) -> Self {
    Star::builder()
      .spectral_type(SpectralType::from_mass(mass))
      .build()
      .unwrap()
  }
}

impl From<SpectralType> for Star {
  fn from(spectral_type: SpectralType) -> Self {
    Star::builder().spectral_type(spectral_type).build().unwrap()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_star_builder() {
    init();
    let star = Star::builder().build().unwrap();
    assert_eq!(star.get_mass().unwrap(), MassOfSol(1.0));
    assert_eq!(star.current_age, TimeInGigayears(4.6));
  }

  #[test]
  fn test_star_check_mass() {
    init();
    let star = Star::from(MassOfSol(0.5));
    assert!(star.check_mass().is_ok());
  }

  #[test]
  fn test_star_get_temperature() {
    init();
    // Jolly ol' Sol
    let mut mass = MassOfSol(1.0);
    let mut expected = SOL_TEMPERATURE_IN_KELVIN;
    let mut actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0);
    // M1V (Kepler-186)
    mass = MassOfSol(0.544);
    expected = TemperatureInKelvin(3850.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K3V
    mass = MassOfSol(0.78);
    expected = TemperatureInKelvin(4830.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.90);
    expected = TemperatureInKelvin(5380.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = TemperatureInKelvin(6350.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.70);
    expected = TemperatureInKelvin(7400.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(8.0);
    expected = TemperatureInKelvin(20_600.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // O8V
    mass = MassOfSol(25.0);
    expected = TemperatureInKelvin(35_100.0);
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
  }

  #[test]
  fn test_get_luminosity() {
    init();
    // Jolly ol' Sol
    let mut mass = MassOfSol(1.0);
    let mut expected = LuminosityOfSol(1.0);
    let mut actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0);
    // M1V (Kepler-186)
    mass = MassOfSol(0.544);
    expected = LuminosityOfSol(0.069);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K9V
    mass = MassOfSol(0.59);
    expected = LuminosityOfSol(0.079);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.95);
    expected = LuminosityOfSol(0.74);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = LuminosityOfSol(2.69);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.83);
    expected = LuminosityOfSol(11.22);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(4.7);
    expected = LuminosityOfSol(589.0);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
    // O8V
    mass = MassOfSol(23.0);
    expected = LuminosityOfSol(170_000.0);
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
  }

  #[test]
  fn test_star_get_radius() {
    init();
    // Jolly ol' Sol
    let mut mass = MassOfSol(1.0);
    let mut expected = RadiusOfSol(1.0);
    let mut actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0);
    // M1V (Kepler-186)
    mass = MassOfSol(0.544);
    expected = RadiusOfSol(0.588); // (actually about 0.501)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K9V
    mass = MassOfSol(0.59);
    expected = RadiusOfSol(0.608);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.95);
    expected = RadiusOfSol(0.927);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = RadiusOfSol(1.359);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.83);
    expected = RadiusOfSol(1.775);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(4.7);
    expected = RadiusOfSol(3.36);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
    // O8V
    mass = MassOfSol(23.0);
    expected = RadiusOfSol(8.50);
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
  }

  #[test]
  fn test_star_get_density() {
    assert_approx_eq!(Star::default().get_density().unwrap(), DensityOfSol(1.0));
  }

  #[test]
  fn test_star_get_life_expectancy() {
    assert_approx_eq!(Star::default().get_life_expectancy().unwrap(), TimeInGigayears(10.0));
  }

  #[test]
  fn test_star_get_habitable_zone() {
    assert_approx_eq!(Star::default().get_habitable_zone().unwrap().0 .0, 0.9534625892455924);
  }

  #[test]
  fn test_star_get_satellite_zone() {
    assert_approx_eq!(Star::default().get_satellite_zone().unwrap().0 .0, 0.1);
  }

  #[test]
  fn test_star_get_frost_line() {
    assert_approx_eq!(Star::default().get_frost_line().unwrap(), LengthInAu(4.85));
  }

  #[test]
  fn test_star_get_absolute_rgb() {
    assert_eq!(Star::default().get_absolute_rgb().unwrap(), (255, 252, 245));
  }
}
