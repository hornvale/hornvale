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
