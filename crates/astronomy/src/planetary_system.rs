use crate::error::AstronomyError;
use crate::host_star::HostStar;
use crate::satellite_system::SatelliteSystem;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `PlanetarySystem` is a `HostStar` and 0+ `SatelliteSystem` objects.
///
/// So a `PlanetarySystem` does not necessarily include planets.  This is
/// confusing and I don't really like it, but I don't have a better name
/// for it.  Yet.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct PlanetarySystem {
  pub host_star: HostStar,
  pub satellite_systems: Vec<SatelliteSystem>,
}

impl MaybeHabitable for PlanetarySystem {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    for satellite_system in &self.satellite_systems {
      if satellite_system.is_habitable() {
        return Ok(());
      }
    }
    Err(AstronomyError::PlanetarySystemDoesNotHaveHabitableZone)
  }
}

impl StellarCountable for PlanetarySystem {
  fn get_stellar_count(&self) -> Result<u8, AstronomyError> {
    self.host_star.get_stellar_count()
  }
}

impl StellarMassable for PlanetarySystem {
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError> {
    self.host_star.get_stellar_mass()
  }
}
