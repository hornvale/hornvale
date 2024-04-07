use super::terrestrial_planet::TerrestrialPlanet;
use crate::types::prelude::*;
use serde::{Deserialize, Serialize};

/// A `Planet`.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Planet {
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
