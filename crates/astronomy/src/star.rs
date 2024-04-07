use crate::constants::prelude::*;
use crate::error::AstronomyError;
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
  #[builder(default = "MassOfSol(1.0)")]
  pub mass: MassOfSol,
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
    if self.mass < MINIMUM_STAR_MASS {
      return Err(AstronomyError::StarMassTooLowForMainSequence);
    }
    if self.mass > MAXIMUM_STAR_MASS {
      return Err(AstronomyError::StarMassTooHighForMainSequence);
    }
    Ok(())
  }

  /// Get the temperature of the star.
  pub fn get_temperature(&self) -> Result<TemperatureInKelvin, AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?;
    let radius = self.get_radius()?;
    let result = (luminosity.0 / radius.0.powf(2.0)).powf(0.25) * 5776.0;
    Ok(TemperatureInKelvin(result))
  }

  /// Get the luminosity of the star.
  pub fn get_luminosity(&self) -> Result<LuminosityOfSol, AstronomyError> {
    self.check_mass()?;
    let result = match self.mass {
      mass if mass.0 < 0.43 => 0.23 * mass.0.powf(2.3),
      mass if mass.0 < 2.0 => mass.0.powf(4.0),
      mass if mass.0 < 55.0 => 1.4 * mass.0.powf(3.5),
      mass if mass <= MAXIMUM_STAR_MASS => 32_000.0 * mass.0,
      _ => unreachable!(),
    };
    Ok(LuminosityOfSol(result))
  }

  /// Get the radius of the star.
  pub fn get_radius(&self) -> Result<RadiusOfSol, AstronomyError> {
    self.check_mass()?;
    let result = match self.mass {
      mass if mass.0 < 1.0 => mass.0.powf(0.80),
      mass if mass.0 >= 1.0 => mass.0.powf(0.57),
      _ => unreachable!(),
    };
    Ok(RadiusOfSol(result))
  }

  /// Get the density of the star.
  pub fn get_density(&self) -> Result<DensityOfSol, AstronomyError> {
    self.check_mass()?;
    let radius = self.get_radius()?;
    let result = self.mass.0 / radius.0.powf(3.0);
    Ok(DensityOfSol(result))
  }

  /// Get the life expectancy of the star.
  pub fn get_life_expectancy(&self) -> Result<TimeInGigayears, AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?;
    let result = self.mass.0 / luminosity.0 * 10.0;
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
    let inner_bound = 0.1 * self.mass.0;
    let outer_bound = 40.0 * self.mass.0;
    Ok((LengthInAu(inner_bound), LengthInAu(outer_bound)))
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

  /// Get the spectral class of a main-sequence star in Kelvin based on its Msol.
  pub fn get_spectral_class(&self) -> Result<String, AstronomyError> {
    self.check_mass()?;
    let temperature = self.get_temperature()?.0;
    let spectral_type = match temperature {
      temperature if temperature < 3_700.0 => 'M',
      temperature if temperature < 5_200.0 => 'K',
      temperature if temperature < 6_000.0 => 'G',
      temperature if temperature < 7_500.0 => 'F',
      temperature if temperature < 10_000.0 => 'A',
      temperature if temperature < 33_000.0 => 'B',
      temperature if temperature < 95_000.0 => 'O',
      _ => unreachable!(),
    };
    let decile = match temperature {
      temperature if temperature < 3_700.0 => 10.0 * (1.0 - ((temperature - 2_000.0) / 1_700.0)),
      temperature if temperature < 5_200.0 => 10.0 * (1.0 - ((temperature - 3_700.0) / 1_500.0)),
      temperature if temperature < 6_000.0 => 10.0 * (1.0 - ((temperature - 5_200.0) / 800.0)),
      temperature if temperature < 7_500.0 => 10.0 * (1.0 - ((temperature - 6_000.0) / 1_500.0)),
      temperature if temperature < 10_000.0 => 10.0 * (1.0 - ((temperature - 7_500.0) / 2_500.0)),
      temperature if temperature < 33_000.0 => 10.0 * (1.0 - ((temperature - 10_000.0) / 23_000.0)),
      temperature if temperature < 95_000.0 => 10.0 * (1.0 - ((temperature - 33_000.0) / 62_000.0)),
      _ => unreachable!(),
    };
    let result = format!("{}{:.0}V", spectral_type, decile);
    Ok(result)
  }

  /// Get the RGB color of a main-sequence star based on its Msol.
  ///
  /// This is going to calculate the absolute RGB of the star, which is going to
  /// be very pale and very subtly tinted.  To generate an apparent color, we are
  /// going to have to account for atmospheric scattering.  The specifics of
  /// that are going to depend upon the atmospheric characteristics of the planet
  /// from which we are observing the star.
  ///
  /// This came from StackOverflow: https://stackoverflow.com/q/21977786
  pub fn get_absolute_rgb(&self) -> Result<(u8, u8, u8), AstronomyError> {
    self.check_mass()?;
    let temperature = self.get_temperature()?.0;
    let x = match temperature {
      temperature if (1_667.0..=4_000.0).contains(&temperature) => {
        ((-0.2661239 * (10.0_f64).powf(9.0)) / temperature.powf(3.0))
          + ((-0.2343580 * (10.0_f64).powf(6.0)) / temperature.powf(2.0))
          + ((0.8776956 * (10.0_f64).powf(3.0)) / temperature)
          + 0.179910
      },
      temperature if temperature >= 4_000.0 => {
        ((-3.0258469 * (10.0_f64).powf(9.0)) / temperature.powf(3.0))
          + ((2.1070379 * (10.0_f64).powf(6.0)) / temperature.powf(2.0))
          + ((0.2226347 * (10.0_f64).powf(3.0)) / temperature)
          + 0.240390
      },
      _ => 0.0,
    };
    let y = match temperature {
      temperature if (1_667.0..2_222.0).contains(&temperature) => {
        -1.1063814 * x.powf(3.0) - 1.34811020 * x.powf(2.0) + 2.18555832 * x - 0.20219683
      },
      temperature if (2_222.0..4_000.0).contains(&temperature) => {
        -0.9549476 * x.powf(3.0) - 1.37418593 * x.powf(2.0) + 2.09137015 * x - 0.16748867
      },
      temperature if temperature >= 4_000.0 => {
        3.0817580 * x.powf(3.0) - 5.87338670 * x.powf(2.0) + 3.75112997 * x - 0.37001483
      },
      _ => 0.0,
    };
    let y2 = if y == 0.0 { 0.0 } else { 1.0 };
    let x2 = if y == 0.0 { 0.0 } else { (x * y2) / y };
    let z2 = if y == 0.0 { 0.0 } else { ((1.0 - x - y) * y2) / y };
    let r = 3.2406 * x2 - 1.5372 * y2 - 0.4986 * z2;
    let g = -0.9689 * x2 + 1.8758 * y2 + 0.0415 * z2;
    let b = 0.0557 * x2 - 0.2040 * y2 + 1.0570 * z2;
    let r2 = if r <= 0.0031308 {
      12.92 * r
    } else {
      1.055 * r.powf(1.0 / 2.4) - 0.055
    };
    let g2 = if g <= 0.0031308 {
      12.92 * g
    } else {
      1.055 * g.powf(1.0 / 2.4) - 0.055
    };
    let b2 = if b <= 0.0031308 {
      12.92 * b
    } else {
      1.055 * b.powf(1.0 / 2.4) - 0.055
    };
    let result = ((r2 * 255.0) as u8, (g2 * 255.0) as u8, (b2 * 255.0) as u8);

    Ok(result)
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
    if self.mass < MINIMUM_HABITABLE_STAR_MASS {
      return Err(AstronomyError::StarMassTooLowToSupportLife);
    }
    if self.mass > MAXIMUM_HABITABLE_STAR_MASS {
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
    Star::builder().mass(mass).build().unwrap()
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
    assert_eq!(star.mass, MassOfSol(1.0));
    assert_eq!(star.current_age, TimeInGigayears(4.6));
  }

  #[test]
  fn test_star_check_mass() {
    init();
    let star = Star::from(MassOfSol(0.008));
    assert!(star.check_mass().is_err());
    let star = Star::from(MassOfSol(0.5));
    assert!(star.check_mass().is_ok());
    let star = Star::from(MassOfSol(100000000.0));
    assert!(star.check_mass().is_err());
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
    expected = TemperatureInKelvin(4008.542794228607); // actually about 3,500
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K3V
    mass = MassOfSol(0.78);
    expected = TemperatureInKelvin(4976.040955507489); // actually about 4,830
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.90);
    expected = TemperatureInKelvin(5422.164512044873); // actually about 5,550
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = TemperatureInKelvin(6775.1332927588965); // actually about 6,300
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.70);
    expected = TemperatureInKelvin(8441.082858093216); // actually about 7,900
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(8.0);
    expected = TemperatureInKelvin(21428.03197741863); // not even close (actually about 15,000)
    actual = Star::from(mass).get_temperature().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // O8V
    mass = MassOfSol(25.0);
    expected = TemperatureInKelvin(41970.46671204058); // actually about 35,000
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
    expected = LuminosityOfSol(0.08757811609600002); // (actually about 0.055)
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K9V
    mass = MassOfSol(0.59);
    expected = LuminosityOfSol(0.12117360999999997); // (actually about 0.079)
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.95);
    expected = LuminosityOfSol(0.81450624); // (actually about 0.74)
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = LuminosityOfSol(2.4414); // (actually about 2.69)
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.83);
    expected = LuminosityOfSol(11.215131210000001); // weirdly accurate
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(4.7);
    expected = LuminosityOfSol(315.1160605); // (actually about 589)
    actual = Star::from(mass).get_luminosity().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
    // O8V
    mass = MassOfSol(23.0);
    expected = LuminosityOfSol(81691.23500180); // (actually about 170,000)
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
    expected = RadiusOfSol(0.6144394896464934); // (actually about 0.501)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // K9V
    mass = MassOfSol(0.59);
    expected = RadiusOfSol(0.6556644082892013); // (actually about 0.608)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // G7V
    mass = MassOfSol(0.95);
    expected = RadiusOfSol(0.9597958863520393); // (actually about 0.927)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // F6V
    mass = MassOfSol(1.25);
    expected = RadiusOfSol(1.1356348391893833); // (actually about 1.359)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // A6V
    mass = MassOfSol(1.83);
    expected = RadiusOfSol(1.4112277936262692); // (actually about 1.775)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1e-3f64);
    // B5V
    mass = MassOfSol(4.7);
    expected = RadiusOfSol(2.4159935973561537); // (actually about 3.36)
    actual = Star::from(mass).get_radius().unwrap();
    assert_approx_eq!(expected.0, actual.0, 1f64);
    // O8V
    mass = MassOfSol(23.0);
    expected = RadiusOfSol(5.972894812390997); // (actually about 8.50)
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
  fn test_star_star_mass_to_spectral_class() {
    assert_eq!(Star::default().get_spectral_class().unwrap(), "G3V"); // actually G2V
  }

  #[test]
  fn test_star_get_absolute_rgb() {
    assert_eq!(Star::default().get_absolute_rgb().unwrap(), (255, 252, 245));
  }
}
