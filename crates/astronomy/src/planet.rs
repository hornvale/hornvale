use super::terrestrial_planet::TerrestrialPlanet;
use crate::error::AstronomyError;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use serde::{Deserialize, Serialize};

/// A `Planet`.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub enum Planet {
  /// A terrestrial planet.
  TerrestrialPlanet(TerrestrialPlanet),
}

impl Planet {
  /// Get the radius of the planet.
  pub fn get_radius(&self) -> RadiusOfEarth {
    match self {
      Planet::TerrestrialPlanet(terrestrial_planet) => terrestrial_planet.get_radius(),
    }
  }
}

impl Default for Planet {
  fn default() -> Self {
    Planet::TerrestrialPlanet(TerrestrialPlanet::default())
  }
}

impl MaybeHabitable for Planet {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    match self {
      Planet::TerrestrialPlanet(terrestrial_planet) => terrestrial_planet.check_habitability(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_default() {
    let planet = Planet::default();
    assert_eq!(planet, Planet::TerrestrialPlanet(TerrestrialPlanet::default()));
  }

  #[test]
  fn test_get_radius() {
    let planet = Planet::default();
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 1e-2);
  }

  #[test]
  fn test_check_habitability() {
    let planet = Planet::default();
    assert_eq!(planet.check_habitability(), Ok(()));
  }
}
