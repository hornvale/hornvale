use super::moon::Moon;
use super::planet::Planet;
use crate::constants::prelude::*;
use crate::error::AstronomyError;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `PlanetMoonRelationship` is a relationship between a planet and a moon.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct PlanetMoonRelationship {
  /// The planet.
  #[builder(default = "Planet::default()")]
  pub planet: Planet,
  /// The moon.
  #[builder(default = "Moon::default()")]
  pub moon: Moon,
  /// The semi-major axis of the moon's orbit, in KM.
  #[builder(default = "LengthInKm(384_000.0)")]
  pub semi_major_axis: LengthInKm,
  /// Orbital eccentricity (unitless).
  #[builder(default = "0.05")]
  pub orbital_eccentricity: f64,
  /// Orbital inclination.
  #[builder(default = "5.15")]
  pub orbital_inclination: f64,
}

impl PlanetMoonRelationship {
  /// Get the periapsis of the moon's orbit.
  pub fn get_periapsis(&self) -> Result<LengthInKm, AstronomyError> {
    Ok(LengthInKm((1.0 - self.orbital_eccentricity) * self.semi_major_axis.0))
  }

  /// Get the apoapsis of the moon's orbit.
  pub fn get_apoapsis(&self) -> Result<LengthInKm, AstronomyError> {
    Ok(LengthInKm((1.0 + self.orbital_eccentricity) * self.semi_major_axis.0))
  }

  /// Calculate the magnitude of the lunar tide.
  ///
  /// We do this by calculating the corrected lunar mass, which is the mass of the moon
  /// times the share of the gravitational parameter of Luna compared to the Earth.
  ///
  /// We then multiply this by the radius of the planet, and divide by the cube of the
  /// semi-major axis of the moon's orbit.
  pub fn get_lunar_tide(&self) -> Result<LengthInMeters, AstronomyError> {
    let corrected_lunar_mass = 2_230_000.0 * self.moon.mass.0 * LUNA_GRAVITATIONAL_PARAMETER_SHARE;
    let planet_radius = self.planet.get_radius();
    let numerator = corrected_lunar_mass * planet_radius.0;
    let denominator = (self.semi_major_axis.0 / KM_PER_EARTH_DIAMETER.0).powf(3.0);
    Ok(LengthInMeters(numerator / denominator))
  }

  /// Calculate the magnitude of the planetary tide.
  /// `moon_mass` - mass of the moon, in MLuna.
  /// `moon_radius`  - radius of the moon, in RLuna.
  /// `semi_major_axis` - semi-major axis of the moon's orbit, in KM.
  ///
  /// Returns a magnitude in meters.
  pub fn get_planetary_tide(&self) -> Result<LengthInMeters, AstronomyError> {
    let moon_mass = self.moon.mass;
    let moon_radius = self.moon.get_radius()?;
    let numerator = 2_230_000.0 * moon_mass.0 * moon_radius.0 * 0.027264;
    let denominator = (self.semi_major_axis.0 / KM_PER_EARTH_DIAMETER.0).powf(3.0);
    Ok(LengthInMeters(numerator / denominator))
  }
}

impl MaybeHabitable for PlanetMoonRelationship {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    self.planet.check_habitability()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_periapsis() {
    let relationship = PlanetMoonRelationshipBuilder::default().build().unwrap();
    assert_approx_eq!(relationship.get_periapsis().unwrap(), LengthInKm(364800.0));
  }

  #[test]
  fn test_apoapsis() {
    let relationship = PlanetMoonRelationshipBuilder::default().build().unwrap();
    assert_approx_eq!(relationship.get_apoapsis().unwrap(), LengthInKm(403200.0));
  }

  #[test]
  fn test_get_lunar_tide() {
    let relationship = PlanetMoonRelationshipBuilder::default().build().unwrap();
    assert_approx_eq!(relationship.get_lunar_tide().unwrap(), LengthInMeters(1.003), 1e-3);
  }

  #[test]
  fn test_get_planetary_tide() {
    let relationship = PlanetMoonRelationshipBuilder::default().build().unwrap();
    assert_approx_eq!(
      relationship.get_planetary_tide().unwrap(),
      LengthInMeters(2.2213324719545158)
    );
  }

  #[test]
  fn test_check_habitability() {
    let relationship = PlanetMoonRelationshipBuilder::default().build().unwrap();
    assert_eq!(relationship.check_habitability(), Ok(()));
  }
}
