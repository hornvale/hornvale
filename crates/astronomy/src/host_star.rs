use crate::close_binary_star::CloseBinaryStar;
use crate::error::AstronomyError;
use crate::star::Star;
use crate::types::prelude::*;
use serde::{Deserialize, Serialize};

/// A `HostStar` is either a `Star` or a `CloseBinaryStar`.
///
/// This may seem counterintuitive, but a `CloseBinaryStar` is actually more
/// closely related to a `Star` than a `DistantBinaryStar`.  The reason for
/// this is that habitable planets can be in a circumbinary orbit around a
/// `CloseBinaryStar`, but can only be in an orbit around one member of a
/// `DistantBinaryStar`.  As a result, we handle `DistantBinaryStar` objects
/// with a distinct class.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum HostStar {
  /// A single star.
  Star(Box<Star>),
  /// A close binary star.
  CloseBinaryStar(Box<CloseBinaryStar>),
}

impl HostStar {
  /// Retrieve or calculate the age of the stars.
  ///
  /// Calculated in Gyr.
  pub fn get_current_age(&self) -> Result<TimeInGigayears, AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.current_age),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_current_age()?),
    }
  }

  /// Retrieve or calculate the total mass of the stars.
  ///
  /// Calculated in Msol.
  pub fn get_mass(&self) -> Result<MassOfSol, AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.mass),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_mass()?),
    }
  }

  /// Retrieve or calculate the total number of stars in the system.
  pub fn get_stellar_count(&self) -> u8 {
    match &self {
      HostStar::Star(_) => 1,
      HostStar::CloseBinaryStar(_) => 2,
    }
  }

  /// Retrieve or calculate the frost line.
  pub fn get_frost_line(&self) -> Result<LengthInAu, AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.get_frost_line()?),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_frost_line()?),
    }
  }

  /// Retrieve or calculate the habitable zone.
  pub fn get_habitable_zone(&self) -> Result<(LengthInAu, LengthInAu), AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.get_habitable_zone()?),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_habitable_zone()?),
    }
  }

  /// Retrieve or calculate the satellite zone.
  pub fn get_satellite_zone(&self) -> Result<(LengthInAu, LengthInAu), AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.get_satellite_zone()?),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_satellite_zone()?),
    }
  }

  /// Retrieve or calculate the luminosity.
  pub fn get_luminosity(&self) -> Result<LuminosityOfSol, AstronomyError> {
    match &self {
      HostStar::Star(star) => Ok(star.get_luminosity()?),
      HostStar::CloseBinaryStar(close_binary_star) => Ok(close_binary_star.get_luminosity()?),
    }
  }

  /// Indicate whether this star is capable of supporting conventional life.
  pub fn check_habitable(&self) -> Result<(), AstronomyError> {
    match &self {
      HostStar::Star(star) => star.check_habitable()?,
      HostStar::CloseBinaryStar(close_binary_star) => close_binary_star.check_habitable()?,
    }
    Ok(())
  }

  /// Indicate whether this star is capable of supporting conventional life.
  pub fn is_habitable(&self) -> bool {
    self.check_habitable().is_ok()
  }
}
