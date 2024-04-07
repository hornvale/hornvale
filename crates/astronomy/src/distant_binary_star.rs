use crate::error::AstronomyError;
use crate::planetary_system::PlanetarySystem;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `DistantBinaryStar` is actually a pair of `PlanetarySystem` objects.
///
/// This may seem counterintuitive, but each member of a distant binary star
/// can itself be a binary star with its own orbiting planets.  A distant
/// binary star is thus very different in critical ways from a close binary
/// star, and we have to treat them as completely distinct although they
/// sound and might seem very similar.
///
/// And let's not get started on how disappointing it is to call something a
/// planetary system when it may not actually have any planets, but I don't
/// think we have a better word or phrase for the idea.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct DistantBinaryStar {
  /// The primary planetary system is the one with greater mass.
  pub primary: PlanetarySystem,
  /// The secondary planetary system has less mass.
  pub secondary: PlanetarySystem,
}

impl MaybeHabitable for DistantBinaryStar {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    let primary_habitable = self.primary.is_habitable();
    let secondary_habitable = self.secondary.is_habitable();
    if primary_habitable || secondary_habitable {
      Ok(())
    } else {
      Err(AstronomyError::DistantBinaryStarDoesNotHaveHabitableZone)
    }
  }
}

impl StellarCountable for DistantBinaryStar {
  fn get_stellar_count(&self) -> Result<u8, AstronomyError> {
    Ok(self.primary.get_stellar_count()? + self.secondary.get_stellar_count()?)
  }
}

impl StellarMassable for DistantBinaryStar {
  fn get_stellar_mass(&self) -> Result<MassOfSol, AstronomyError> {
    Ok(self.primary.get_stellar_mass()? + self.secondary.get_stellar_mass()?)
  }
}
