use crate::constants::prelude::*;
use crate::types::prelude::*;
use serde::{Deserialize, Serialize};
pub mod error;
pub use error::TerrestrialPlanetError;
pub mod math;
use math::*;

/// The `TerrestrialPlanet` type.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct TerrestrialPlanet {
  /// Mass in MassOfEarth.
  pub mass: MassOfEarth,
  /// Core Mass Fraction (unitless).
  pub core_mass_fraction: f64,
  /// Density, in DensityOfEarth.
  pub density: DensityOfEarth,
  /// Escape velocity, in VEarth.
  pub escape_velocity: EscapeVelocityOfEarth,
  /// Gravity, in GravityOfEarth.
  pub gravity: GravityOfEarth,
  /// Radius, in Rearth.
  pub radius: RadiusOfEarth,
  /// Axial tilt (0-180º).
  pub axial_tilt: f64,
  /// Rotation.
  pub rotation_direction: RotationDirection,
  /// Semi-Major Axis.
  pub semi_major_axis: LengthInAu,
  /// Tropic Zone.
  pub tropic_zones: (f64, f64),
  /// Polar Zones.
  pub polar_zones: (f64, f64),
  /// Orbital eccentricity (units).
  pub orbital_eccentricity: f64,
  /// Perihelion.
  pub perihelion: LengthInAu,
  /// Aphelion.
  pub aphelion: LengthInAu,
  /// Orbital period, in Earth years.
  pub orbital_period: TimeInEarthYears,
  /// Bond albedo (unitless).
  pub bond_albedo: f64,
  /// Greenhouse effect (unitless).
  pub greenhouse_effect: f64,
  /// Equilibrium temperature, in Kelvin.
  pub equilibrium_temperature: TemperatureInKelvin,
  /// Whether we can retain the gases necessary for conventional life.
  pub is_atmospherically_stable: bool,
}

impl TerrestrialPlanet {
  pub fn from_mass(mass: MassOfEarth) -> Result<Self, TerrestrialPlanetError> {
    let core_mass_fraction: f64 = 0.35;
    let density = get_density(mass, core_mass_fraction);
    let radius = get_radius(mass, density);
    let escape_velocity = get_escape_velocity(mass, radius);
    let gravity = get_gravity(mass, radius);
    let axial_tilt = 23.5;
    let rotation_direction = RotationDirection::Prograde;
    let tropic_zones = (0.0, axial_tilt);
    let polar_zones = (90.0 - axial_tilt, 90.0);
    let bond_albedo = 0.29;
    let greenhouse_effect = 1.0;
    let host_star_luminosity = LuminosityOfSol(1.0);
    let host_star_distance = LengthInAu(1.0);
    let semi_major_axis = host_star_distance;
    let orbital_eccentricity = 0.0167;
    let perihelion = LengthInAu((1.0 - orbital_eccentricity) * semi_major_axis.0);
    let aphelion = LengthInAu((1.0 + orbital_eccentricity) * semi_major_axis.0);
    let orbital_period = TimeInEarthDays(semi_major_axis.0.powf(3.0).sqrt()).into();
    let equilibrium_temperature =
      get_equilibrium_temperature(bond_albedo, greenhouse_effect, host_star_luminosity, host_star_distance);
    let is_atmospherically_stable = is_atmospherically_stable(equilibrium_temperature, escape_velocity);
    let result = Self {
      mass,
      core_mass_fraction,
      density,
      escape_velocity,
      gravity,
      radius,
      axial_tilt,
      rotation_direction,
      semi_major_axis,
      tropic_zones,
      polar_zones,
      orbital_eccentricity,
      perihelion,
      aphelion,
      orbital_period,
      bond_albedo,
      greenhouse_effect,
      equilibrium_temperature,
      is_atmospherically_stable,
    };
    Ok(result)
  }

  /// Indicate whether this planet is capable of supporting conventional life.
  pub fn check_habitable(&self) -> Result<(), TerrestrialPlanetError> {
    {
      if self.equilibrium_temperature <= MINIMUM_HABITABLE_TEMPERATURE {
        // About 0ºC is too damned cold.
        return Err(TerrestrialPlanetError::TooColdToSupportConventionalLife);
      }
      if self.equilibrium_temperature >= MAXIMUM_HABITABLE_TEMPERATURE {
        // About 50ºC is too damned hot.
        return Err(TerrestrialPlanetError::TooHotToSupportConventionalLife);
      }
      if self.gravity <= MINIMUM_HABITABLE_GRAVITY {
        return Err(TerrestrialPlanetError::GravityTooLowToSupportConventionalLife);
      }
      if self.gravity >= MAXIMUM_HABITABLE_GRAVITY {
        return Err(TerrestrialPlanetError::GravityTooHighToSupportConventionalLife);
      }
      if !is_carbon_dioxide_stable(self.equilibrium_temperature, self.escape_velocity) {
        return Err(TerrestrialPlanetError::AtmosphereUnstableForCarbonDioxide);
      }
      if !is_argon_stable(self.equilibrium_temperature, self.escape_velocity) {
        return Err(TerrestrialPlanetError::AtmosphereUnstableForArgon);
      }
      if !is_oxygen_stable(self.equilibrium_temperature, self.escape_velocity) {
        return Err(TerrestrialPlanetError::AtmosphereUnstableForOxygen);
      }
      if !is_nitrogen_stable(self.equilibrium_temperature, self.escape_velocity) {
        return Err(TerrestrialPlanetError::AtmosphereUnstableForNitrogen);
      }
      Ok(())
    }
  }

  /// Indicate whether this planet is capable of supporting conventional life.
  pub fn is_habitable(&self) -> bool {
    match self.check_habitable() {
      Ok(()) => true,
      Err(_) => false,
    }
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_from_mass() -> Result<(), TerrestrialPlanetError> {
    init();
    let planet = TerrestrialPlanet::from_mass(MassOfEarth(1.0))?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.35);
    assert_approx_eq!(planet.density, DensityOfEarth(1.01), 0.01);
    assert_approx_eq!(planet.escape_velocity, EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.gravity, GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.radius, RadiusOfEarth(1.0), 0.01);
    Ok(())
  }
}
