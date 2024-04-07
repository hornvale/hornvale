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
