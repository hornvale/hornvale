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
  /// The host star of the planetary system.
  #[builder(default = "HostStar::default()")]
  pub host_star: HostStar,
  /// The satellite systems of the planetary system.
  #[builder(default = "vec![SatelliteSystem::default()]")]
  pub satellite_systems: Vec<SatelliteSystem>,
}

impl PlanetarySystem {
  /// Create a new `PlanetarySystem` builder.
  pub fn builder() -> PlanetarySystemBuilder {
    PlanetarySystemBuilder::default()
  }
}

impl MaybeHabitable for PlanetarySystem {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    self.host_star.check_habitability()?;
    for satellite_system in &self.satellite_systems {
      println!("{:#?}", satellite_system);
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

impl Default for PlanetarySystem {
  fn default() -> Self {
    PlanetarySystem::builder().build().unwrap()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_planetary_system_builder() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert_eq!(planetary_system.satellite_systems.len(), 1);
  }

  #[test]
  fn test_planetary_system_check_habitability() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert!(planetary_system.check_habitability().is_ok());
  }

  #[test]
  fn test_planetary_system_get_stellar_count() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert_eq!(planetary_system.get_stellar_count().unwrap(), 1);
  }

  #[test]
  fn test_planetary_system_get_stellar_mass() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert_eq!(planetary_system.get_stellar_mass().unwrap(), MassOfSol(1.0));
  }

  #[test]
  fn test_planetary_system_get_stellar_mass_with_satellites() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert_eq!(planetary_system.get_stellar_mass().unwrap(), MassOfSol(1.0));
  }

  #[test]
  fn test_planetary_system_get_stellar_mass_with_multiple_satellites() {
    let host_star = HostStar::default();
    let satellite_systems = vec![SatelliteSystem::builder().build().unwrap()];
    let planetary_system = PlanetarySystem::builder()
      .host_star(host_star)
      .satellite_systems(satellite_systems)
      .build()
      .unwrap();
    assert_eq!(planetary_system.get_stellar_mass().unwrap(), MassOfSol(1.0));
  }
}
