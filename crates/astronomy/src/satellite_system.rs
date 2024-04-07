use super::moon::Moon;
use super::planet::Planet;
use super::planet_moon_relationship::PlanetMoonRelationship;
use super::traits::prelude::*;
use super::types::prelude::*;
use crate::error::AstronomyError;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `SatelliteSystem` is a planet and its moon or moons.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize, Builder)]
pub struct SatelliteSystem {
  /// The planet.
  #[builder(default = "Planet::default()")]
  pub planet: Planet,
  /// Any moon or moons.
  #[builder(default = "vec![Moon::default()]")]
  pub moons: Vec<Moon>,
  /// The semi-major axis of the moon's orbit, in KM.
  #[builder(default = "vec![LengthInKm(384_000.0)]")]
  pub semi_major_axes: Vec<LengthInKm>,
  /// Orbital eccentricity (unitless).
  #[builder(default = "vec![0.05]")]
  pub orbital_eccentricities: Vec<f64>,
  /// Orbital inclination.
  #[builder(default = "vec![5.15]")]
  pub orbital_inclinations: Vec<f64>,
}

impl SatelliteSystem {
  /// Create a new `SatelliteSystem` builder.
  pub fn builder() -> SatelliteSystemBuilder {
    SatelliteSystemBuilder::default()
  }

  /// Get a relationship between the planet and a single moon.
  pub fn get_planet_moon_relationship(&self, index: usize) -> Result<PlanetMoonRelationship, AstronomyError> {
    let moon = *self.moons.get(index).ok_or(AstronomyError::MoonIndexOutOfBounds)?;
    let semi_major_axis = *self
      .semi_major_axes
      .get(index)
      .ok_or(AstronomyError::MoonIndexOutOfBounds)?;
    let orbital_eccentricity = *self
      .orbital_eccentricities
      .get(index)
      .ok_or(AstronomyError::MoonIndexOutOfBounds)?;
    let orbital_inclination = *self
      .orbital_inclinations
      .get(index)
      .ok_or(AstronomyError::MoonIndexOutOfBounds)?;
    Ok(PlanetMoonRelationship {
      planet: self.planet,
      moon,
      semi_major_axis,
      orbital_eccentricity,
      orbital_inclination,
    })
  }
}

impl MaybeHabitable for SatelliteSystem {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    self.planet.check_habitability()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use anyhow::Result as AnyResult;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_planet_moon_relationship() -> AnyResult<()> {
    init();
    assert_eq!(
      SatelliteSystemBuilder::default()
        .planet(Planet::default())
        .moons(vec![Moon::default()])
        .build()?
        .get_planet_moon_relationship(0)
        .unwrap(),
      PlanetMoonRelationship {
        planet: Planet::default(),
        moon: Moon::default(),
        semi_major_axis: LengthInKm(384_000.0),
        orbital_eccentricity: 0.05,
        orbital_inclination: 5.15,
      }
    );
    Ok(())
  }
}
