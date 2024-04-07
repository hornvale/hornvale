use crate::errors::prelude::*;
use crate::star::Star;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `CloseBinaryStar` is a system of two `Star` objects.
///
/// This may seem counterintuitive, but a `CloseBinaryStar` is actually more
/// closely related to a `Star` than a `DistantBinaryStar`.  The reason for
/// this is that habitable planets can be in a circumbinary orbit around a
/// `CloseBinaryStar`, but can only be in an orbit around one member of a
/// `DistantBinaryStar`.  As a result, we handle `DistantBinaryStar` objects
/// with a different class.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct CloseBinaryStar {
  /// The primary star is the one with greater mass.
  pub primary: Star,
  /// The secondary star has less mass.
  pub secondary: Star,
  /// Average separation of the binary components, in AU.
  pub average_separation: LengthInAu,
  /// Orbital eccentricity of the components (unitless).
  pub orbital_eccentricity: f64,
}

impl CloseBinaryStar {
  /// Create from a pair of stars, average separation, and orbital eccentricity.
  pub fn from_stars(primary: Star, secondary: Star, average_separation: LengthInAu, orbital_eccentricity: f64) -> Self {
    CloseBinaryStar {
      primary,
      secondary,
      average_separation,
      orbital_eccentricity,
    }
  }

  /// Get the average distance from the barycenter of the components.
  pub fn get_average_distances_from_barycenter(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let combined_mass = self.primary.mass + self.secondary.mass;
    let d1 = LengthInAu(self.average_separation.0 * (self.secondary.mass.0 / combined_mass.0));
    let d2 = LengthInAu(self.average_separation.0 * (self.primary.mass.0 / combined_mass.0));
    Ok((d1, d2))
  }

  /// Get the minimum distance from the barycenter of the components.
  pub fn get_minimum_distances_from_barycenter(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let average_distances = self.get_average_distances_from_barycenter()?;
    let d1 = LengthInAu(average_distances.0 .0 * (1.0 - self.orbital_eccentricity));
    let d2 = LengthInAu(average_distances.1 .0 * (1.0 - self.orbital_eccentricity));
    Ok((d1, d2))
  }

  /// Get the maximum distance from the barycenter of the components.
  pub fn get_maximum_distances_from_barycenter(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let average_distances = self.get_average_distances_from_barycenter()?;
    let d1 = LengthInAu(average_distances.0 .0 * (1.0 + self.orbital_eccentricity));
    let d2 = LengthInAu(average_distances.1 .0 * (1.0 + self.orbital_eccentricity));
    Ok((d1, d2))
  }

  /// Get the minimum separation of the components.
  pub fn get_minimum_separation(&self) -> Result<LengthInAu, CloseBinaryStarError> {
    let min_distances = self.get_minimum_distances_from_barycenter()?;
    Ok(LengthInAu(min_distances.0 .0 + min_distances.1 .0))
  }

  /// Get the maximum separation of the components.
  pub fn get_maximum_separation(&self) -> Result<LengthInAu, CloseBinaryStarError> {
    let max_distances = self.get_maximum_distances_from_barycenter()?;
    Ok(LengthInAu(max_distances.0 .0 + max_distances.1 .0))
  }

  /// Get the habitable zone of the binary system.
  pub fn get_habitable_zone(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let luminosity = self.primary.get_luminosity()? + self.secondary.get_luminosity()?;
    let inner_bound = LengthInAu((luminosity / 1.1).0.sqrt());
    let outer_bound = LengthInAu((luminosity / 0.53).0.sqrt());
    Ok((inner_bound, outer_bound))
  }

  /// Calculate the frost line of a close binary system in AU.
  pub fn get_frost_line(&self) -> Result<LengthInAu, CloseBinaryStarError> {
    let luminosity = self.primary.get_luminosity()? + self.secondary.get_luminosity()?;
    Ok(LengthInAu(4.85 * luminosity.0.sqrt()))
  }

  /// Calculate the forbidden zone of a close binary system in AU.
  pub fn get_forbidden_zone(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let min_separation = self.get_minimum_separation()?;
    let max_separation = self.get_maximum_separation()?;
    Ok((LengthInAu(min_separation.0 / 3.0), LengthInAu(max_separation.0 * 3.0)))
  }

  /// Calculate the danger zone of a close binary system in AU.
  pub fn get_danger_zone(&self) -> Result<LengthInAu, CloseBinaryStarError> {
    let max_separation = self.get_maximum_separation()?;
    Ok(LengthInAu(max_separation.0 * 4.0))
  }

  /// Calculate the luminosity of the stars.
  pub fn get_luminosity(&self) -> Result<LuminosityOfSol, CloseBinaryStarError> {
    Ok(self.primary.get_luminosity()? + self.secondary.get_luminosity()?)
  }

  /// Calculate the combined mass of the stars.
  pub fn get_mass(&self) -> Result<MassOfSol, CloseBinaryStarError> {
    Ok(self.primary.mass + self.secondary.mass)
  }

  /// Calculate the satellite zone of a close binary system in AU.
  pub fn get_satellite_zone(&self) -> Result<(LengthInAu, LengthInAu), CloseBinaryStarError> {
    let combined_mass = self.get_mass()?;
    Ok((LengthInAu(0.1 * combined_mass.0), LengthInAu(40.0 * combined_mass.0)))
  }

  /// Get the age of the binary system.
  pub fn get_current_age(&self) -> Result<TimeInGigayears, CloseBinaryStarError> {
    Ok(self.primary.current_age)
  }

  /// Indicate whether this CloseBinaryStar is capable of supporting conventional life.
  pub fn check_habitable(&self) -> Result<(), HabitabilityError> {
    if self.get_habitable_zone()?.1 <= self.get_forbidden_zone()?.1 {
      return Err(HabitabilityError::HabitableZoneContainedWithinForbiddenZone);
    }
    if self.get_habitable_zone()?.1 <= self.get_danger_zone()? {
      return Err(HabitabilityError::HabitableZoneContainedWithinDangerZone);
    }
    self.primary.check_habitable()?;
    // Secondary stars can be very low mass or young but still habitable.
    match self.secondary.check_habitable() {
      Err(HabitabilityError::MassTooLowToSupportLife) => {},
      Ok(_) => {},
      Err(error) => return Err(error),
    }
    Ok(())
  }
}
