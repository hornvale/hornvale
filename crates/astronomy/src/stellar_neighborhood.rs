use crate::error::AstronomyError;
use crate::stellar_neighbor::StellarNeighbor;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// The `StellarNeighborhood` type.
///
/// This is mostly a container for star systems.
///
/// We carve out a spherical section, a few light years or so in radius, and
/// generate some companion star systems.  These are likely to be other class V
/// stars, possibly with planets of their own.
///
/// Why?  Well, just to add a little color to the night sky.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct StellarNeighborhood {
  /// The radius of this neighborhood, measured in light years.
  pub radius: LengthInLyr,
  /// Stellar "neighbors", which is a glorified tuple of three-dimensional
  /// coordinates and a star system.
  pub neighbors: Vec<StellarNeighbor>,
}

impl StellarNeighborhood {
  /// Create a new `StellarNeighborhood` builder.
  pub fn builder() -> StellarNeighborhoodBuilder {
    StellarNeighborhoodBuilder::default()
  }
}

impl MaybeHabitable for StellarNeighborhood {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    for neighbor in &self.neighbors {
      if neighbor.star_system.is_habitable() {
        return Ok(());
      }
    }
    Err(AstronomyError::StellarNeighborhoodDoesNotHaveHabitableZone)
  }
}

impl StellarCountable for StellarNeighborhood {
  fn get_stellar_count(&self) -> Result<u8, AstronomyError> {
    let mut count = 0;
    for neighbor in &self.neighbors {
      count += neighbor.star_system.get_stellar_count()?;
    }
    Ok(count)
  }
}

impl StellarMassable for StellarNeighborhood {
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError> {
    let mut mass = 0.0_f64;
    for neighbor in &self.neighbors {
      mass += neighbor.star_system.get_stellar_mass()?.0;
    }
    Ok(MassOfSol(mass))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::planetary_system::PlanetarySystem;
  use crate::star_system::StarSystem;

  #[test]
  fn test_stellar_neighborhood_builder() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    let stellar_neighborhood = StellarNeighborhood::builder()
      .radius(LengthInLyr(1.0))
      .neighbors(vec![neighbor.clone()])
      .build()
      .unwrap();
    assert_eq!(stellar_neighborhood.radius, LengthInLyr(1.0));
    assert_eq!(stellar_neighborhood.neighbors.len(), 1);
    assert_eq!(stellar_neighborhood.neighbors[0], neighbor);
  }

  #[test]
  fn test_stellar_neighborhood_check_habitability() {
    let neighbor = StellarNeighbor::builder().coordinates((1.0, 2.0, 3.0)).build().unwrap();
    let stellar_neighborhood = StellarNeighborhood::builder()
      .radius(LengthInLyr(1.0))
      .neighbors(vec![neighbor.clone()])
      .build()
      .unwrap();
    assert!(stellar_neighborhood.check_habitability().is_ok());
  }

  #[test]
  fn test_stellar_neighborhood_get_stellar_count() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    let stellar_neighborhood = StellarNeighborhood::builder()
      .radius(LengthInLyr(1.0))
      .neighbors(vec![neighbor.clone()])
      .build()
      .unwrap();
    assert_eq!(stellar_neighborhood.get_stellar_count(), Ok(1));
  }

  #[test]
  fn test_stellar_neighborhood_get_stellar_mass() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    let stellar_neighborhood = StellarNeighborhood::builder()
      .radius(LengthInLyr(1.0))
      .neighbors(vec![neighbor.clone()])
      .build()
      .unwrap();
    assert_eq!(stellar_neighborhood.get_stellar_mass(), Ok(MassOfSol(1.0)));
  }

  #[test]
  fn test_stellar_neighborhood_get_stellar_mass_with_star_systems() {
    let neighbor = StellarNeighbor::builder()
      .coordinates((1.0, 2.0, 3.0))
      .star_system(StarSystem::PlanetarySystem(PlanetarySystem::default()))
      .build()
      .unwrap();
    let stellar_neighborhood = StellarNeighborhood::builder()
      .radius(LengthInLyr(1.0))
      .neighbors(vec![neighbor.clone()])
      .build()
      .unwrap();
    assert_eq!(stellar_neighborhood.get_stellar_mass(), Ok(MassOfSol(1.0)));
  }
}
