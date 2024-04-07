use crate::distant_binary_star::DistantBinaryStar;
use crate::error::AstronomyError;
use crate::planetary_system::PlanetarySystem;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use serde::{Deserialize, Serialize};

/// The `StarSystem` type.
///
/// This is probably a good place to include some notes on terminology.
///
/// For ease of programming, I'm conflating the concepts of "star" or "stellar"
/// systems and "planetary" systems.
///
/// Here, a "star system" means one or more stars gravitationally connected in
/// some interesting way, along with all of the planets and other satellites
/// bound to those stars in some interesting way.
///
/// And I use "solar system" only to refer to our (your and my) star system.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum StarSystem {
  /// A distant binary system.
  DistantBinaryStar(DistantBinaryStar),
  /// Any other planetary system.
  PlanetarySystem(PlanetarySystem),
}

impl MaybeHabitable for StarSystem {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    match self {
      StarSystem::DistantBinaryStar(distant_binary_star) => distant_binary_star.check_habitability(),
      StarSystem::PlanetarySystem(planetary_system) => planetary_system.check_habitability(),
    }
  }
}

impl StellarCountable for StarSystem {
  fn get_stellar_count(&self) -> Result<u8, AstronomyError> {
    match self {
      StarSystem::DistantBinaryStar(distant_binary_star) => distant_binary_star.get_stellar_count(),
      StarSystem::PlanetarySystem(planetary_system) => planetary_system.get_stellar_count(),
    }
  }
}

impl StellarMassable for StarSystem {
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError> {
    match self {
      StarSystem::DistantBinaryStar(distant_binary_star) => distant_binary_star.get_stellar_mass(),
      StarSystem::PlanetarySystem(planetary_system) => planetary_system.get_stellar_mass(),
    }
  }
}
