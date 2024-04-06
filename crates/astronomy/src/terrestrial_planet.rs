use crate::constants::prelude::*;
use crate::errors::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// The `TerrestrialPlanet` type.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct TerrestrialPlanet {
  /// Mass in MassOfEarth.
  #[builder(default = "MassOfEarth(1.0)")]
  pub mass: MassOfEarth,
  /// Core Mass Fraction (unitless).
  #[builder(default = "0.35")]
  pub core_mass_fraction: f64,
  /// Axial tilt (0-180º).
  #[builder(default = "AxialTilt(23.5)")]
  pub axial_tilt: AxialTilt,
  /// Bond albedo (unitless).
  #[builder(default = "0.29")]
  pub bond_albedo: f64,
  /// Greenhouse effect (unitless).
  #[builder(default = "1.0")]
  pub greenhouse_effect: f64,
}

impl TerrestrialPlanet {
  /// Calculate the density of a terrestrial planet.
  ///
  /// The CMF, or Core Mass Fraction, indicates what percentage of the planet's
  /// mass is contained within its iron core.
  ///
  /// Given that, we can calculate the overall density of the planet in DensityOfEarth.
  pub fn get_density(&self) -> DensityOfEarth {
    let d1 = 5.51 * self.mass.0.powf(0.189) / (1.07 - 0.21 * (self.core_mass_fraction)).powf(3.0);
    let d2 = 3.5 + 4.37 * self.core_mass_fraction;
    let result = match self.mass.0 {
      mass if mass > 0.6 => d1,
      _mass if d1 > d2 => d1,
      _ => d2,
    };
    DensityInGramsPerCm3(result).into()
  }

  /// Calculate the radius of the planet.
  pub fn get_radius(&self) -> RadiusOfEarth {
    RadiusOfEarth((self.mass.0 / self.get_density().0).powf(1.0 / 3.0))
  }

  /// Calculate the escape velocity of the planet.
  pub fn get_escape_velocity(&self) -> EscapeVelocityOfEarth {
    EscapeVelocityOfEarth((self.mass.0 / self.get_radius().0).sqrt())
  }

  /// Calculate the gravity of the planet.
  pub fn get_gravity(&self) -> GravityOfEarth {
    GravityOfEarth(self.mass.0 / self.get_radius().0.powf(2.0))
  }

  /// Calculate the rotation direction of the planet.
  pub fn get_rotation_direction(&self) -> RotationDirection {
    self.axial_tilt.rotation_direction()
  }

  /// Calculate the northern polar zone of the planet.
  pub fn get_northern_polar_zone(&self) -> Latitude {
    self.axial_tilt.get_northern_polar_zone()
  }

  /// Calculate the southern polar zone of the planet.
  pub fn get_southern_polar_zone(&self) -> Latitude {
    self.axial_tilt.get_southern_polar_zone()
  }

  /// Calculate the northern tropic zone of the planet.
  pub fn get_northern_tropic_zone(&self) -> Latitude {
    self.axial_tilt.get_northern_tropic_zone()
  }

  /// Calculate the southern tropic zone of the planet.
  pub fn get_southern_tropic_zone(&self) -> Latitude {
    self.axial_tilt.get_southern_tropic_zone()
  }

  /// Indicate whether this planet is capable of supporting conventional life.
  pub fn check_habitable(&self) -> Result<(), HabitabilityError> {
    {
      let gravity = self.get_gravity();
      if gravity <= MINIMUM_HABITABLE_GRAVITY {
        return Err(HabitabilityError::GravityTooLowToSupportConventionalLife);
      }
      if gravity >= MAXIMUM_HABITABLE_GRAVITY {
        return Err(HabitabilityError::GravityTooHighToSupportConventionalLife);
      }
      Ok(())
    }
  }

  /// Indicate whether this planet is capable of supporting conventional life.
  pub fn is_habitable(&self) -> bool {
    self.check_habitable().is_ok()
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_from_mass() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.35);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(1.01), 0.01);
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }
}
