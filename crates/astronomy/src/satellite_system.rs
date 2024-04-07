use super::moon::Moon;
use super::planet::Planet;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `SatelliteSystem` is a planet and its moon or moons.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct SatelliteSystem {
  /// The planet.
  pub planet: Planet,
  /// Any moon or moons.
  pub moons: Vec<Moon>,
}
