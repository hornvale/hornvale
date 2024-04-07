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
  pub fn get_frost_line(&self) -> Result<LengthInAu, AstronomyError> {
    self.check_mass()?;
    let luminosity = self.get_luminosity()?;
    let result = 4.85 * luminosity.0.sqrt();
    Ok(LengthInAu(result))
  }

  /// Get the spectral class of a main-sequence star in Kelvin based on its Msol.
  pub fn star_mass_to_spectral_class(&self) -> Result<String, AstronomyError> {
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
