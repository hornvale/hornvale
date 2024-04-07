use crate::error::AstronomyError;
use crate::star_system::StarSystem;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// The `StellarNeighbor` class.
///
/// No, not someone who brings you brownies when you move into the area.
///
/// This is just a combination of a fully-fledged star system and a set of 3-D
/// coordinates so that we can place it relative to our primary star system.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct StellarNeighbor {
  /// Each coordinate (x,y,z) is a distance (in light years) from the origin.
  pub coordinates: (f64, f64, f64),
  /// The details of this particular star system.
  #[builder(default = "StarSystem::default()")]
  pub star_system: StarSystem,
}

impl StellarNeighbor {
  /// Create a new `StellarNeighbor` builder.
  pub fn builder() -> StellarNeighborBuilder {
    StellarNeighborBuilder::default()
  }

  /// Get the distance from the origin.
  pub fn get_distance(&self) -> LengthInLyr {
    let (x, y, z) = self.coordinates;
    LengthInLyr((x.powi(2) + y.powi(2) + z.powi(2)).sqrt())
  }
}

impl MaybeHabitable for StellarNeighbor {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    self.star_system.check_habitability()
  }
}

impl StellarCountable for StellarNeighbor {
  fn get_stellar_count(&self) -> Result<u8, AstronomyError> {
    self.star_system.get_stellar_count()
  }
}

impl StellarMassable for StellarNeighbor {
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError> {
    self.star_system.get_stellar_mass()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::planetary_system::PlanetarySystem;
  use crate::star_system::StarSystem;

  #[test]
  fn test_stellar_neighbor_builder() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    assert_eq!(neighbor.coordinates, (1.0, 2.0, 3.0));
    assert_eq!(
      neighbor.star_system,
      StarSystem::PlanetarySystem(PlanetarySystem::default())
    );
  }

  #[test]
  fn test_stellar_neighbor_get_distance() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    assert_eq!(neighbor.get_distance(), LengthInLyr(3.7416573867739413));
  }

  #[test]
  fn test_stellar_neighbor_check_habitability() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    neighbor.check_habitability().unwrap();
    assert!(neighbor.check_habitability().is_ok());
  }

  #[test]
  fn test_stellar_neighbor_get_stellar_count() {
    let neighbor = StellarNeighbor::builder().coordinates((1.0, 2.0, 3.0)).build().unwrap();
    assert_eq!(neighbor.get_stellar_count().unwrap(), 1);
  }

  #[test]
  fn test_stellar_neighbor_get_stellar_mass() {
    let neighbor = StellarNeighbor::builder().coordinates((1.0, 2.0, 3.0)).build().unwrap();
    assert_eq!(neighbor.get_stellar_mass().unwrap(), MassOfSol(1.0));
  }

  #[test]
  fn test_stellar_neighbor_get_stellar_mass_with_satellites() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    assert_eq!(neighbor.get_stellar_mass().unwrap(), MassOfSol(1.0));
  }
}
