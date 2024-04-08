use crate::constants::prelude::*;
use crate::error::AstronomyError;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use rand::prelude::*;
use serde::{Deserialize, Serialize};

/// The `TerrestrialPlanet` type.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize, Builder)]
#[builder(derive(Debug))]
pub struct TerrestrialPlanet {
  /// Mass in MassOfEarth.
  #[builder(default = "MassOfEarth(1.0)", setter(into))]
  pub mass: MassOfEarth,
  /// Core Mass Fraction (unitless).
  #[builder(default = "0.325")]
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
}

impl Default for TerrestrialPlanet {
  fn default() -> Self {
    TerrestrialPlanetBuilder::default().build().unwrap()
  }
}

impl Randomizable for TerrestrialPlanetBuilder {
  /// Get a random `TerrestrialPlanetBuilder`.
  fn get_random<R: Rng + ?Sized>(rng: &mut R) -> Self {
    TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt::get_random(rng))
      .clone()
  }

  /// Get a random habitable `TerrestrialPlanetBuilder`.
  fn get_random_habitable<R: Rng + ?Sized>(rng: &mut R) -> Self {
    TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt::get_random_habitable(rng))
      .clone()
  }

  /// Get a random exotic `TerrestrialPlanetBuilder`.
  fn get_random_exotic<R: Rng + ?Sized>(rng: &mut R) -> Self {
    TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt::get_random_exotic(rng))
      .clone()
  }

  /// Get a random exotic and habitable `TerrestrialPlanetBuilder`.
  fn get_random_exotic_habitable<R: Rng + ?Sized>(rng: &mut R) -> Self {
    TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt::get_random_exotic_habitable(rng))
      .clone()
  }

  /// Get a random earthlike `TerrestrialPlanetBuilder`.
  fn get_random_earthlike<R: Rng + ?Sized>(rng: &mut R) -> Self {
    TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt::get_random_earthlike(rng))
      .clone()
  }
}

impl MaybeHabitable for TerrestrialPlanet {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    let gravity = self.get_gravity();
    if gravity <= MINIMUM_HABITABLE_PLANET_GRAVITY {
      return Err(AstronomyError::PlanetGravityTooLowToSupportConventionalLife);
    }
    if gravity >= MAXIMUM_HABITABLE_PLANET_GRAVITY {
      return Err(AstronomyError::PlanetGravityTooHighToSupportConventionalLife);
    }
    Ok(())
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_mass() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.325);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(0.9947683215465892), 0.01);
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_from_core_mass_fraction() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().core_mass_fraction(0.5).build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.5);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(1.112802416227886), 0.01);
    assert_approx_eq!(
      planet.get_escape_velocity(),
      EscapeVelocityOfEarth(1.017973197118575),
      0.01
    );
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.07385433165991), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(0.9650000000000001), 0.01);
    Ok(())
  }

  #[test]
  fn test_from_axial_tilt() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(45.0))
      .build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.325);
    assert_approx_eq!(planet.axial_tilt.0, 45.0);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(0.9947683215465892), 0.01);
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_from_bond_albedo() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().bond_albedo(0.5).build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.325);
    assert_approx_eq!(planet.bond_albedo, 0.5);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(0.9947683215465892), 0.01);
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_from_greenhouse_effect() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().greenhouse_effect(0.5).build()?;
    assert_approx_eq!(planet.mass.0, 1.0);
    assert_approx_eq!(planet.core_mass_fraction, 0.325);
    assert_approx_eq!(planet.greenhouse_effect, 0.5);
    assert_approx_eq!(planet.get_density(), DensityOfEarth(0.9947683215465892), 0.01);
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_density() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.get_density(), DensityOfEarth(0.9947683215465892), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_radius() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.get_radius(), RadiusOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_escape_velocity() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.get_escape_velocity(), EscapeVelocityOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_gravity() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_approx_eq!(planet.get_gravity(), GravityOfEarth(1.0), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_rotation_direction() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(23.5))
      .build()?;
    assert_eq!(planet.get_rotation_direction(), RotationDirection::Prograde);
    Ok(())
  }

  #[test]
  fn test_get_northern_polar_zone() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(23.5))
      .build()?;
    assert_approx_eq!(planet.get_northern_polar_zone(), Latitude(66.5), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_southern_polar_zone() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(23.5))
      .build()?;
    assert_approx_eq!(planet.get_southern_polar_zone(), Latitude(-66.5), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_northern_tropic_zone() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(23.5))
      .build()?;
    assert_approx_eq!(planet.get_northern_tropic_zone(), Latitude(23.5), 0.01);
    Ok(())
  }

  #[test]
  fn test_get_southern_tropic_zone() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .axial_tilt(AxialTilt(23.5))
      .build()?;
    assert_approx_eq!(planet.get_southern_tropic_zone(), Latitude(-23.5), 0.01);
    Ok(())
  }

  #[test]
  fn test_check_habitability() -> Result<(), TerrestrialPlanetBuilderError> {
    init();
    let planet = TerrestrialPlanetBuilder::default().mass(MassOfEarth(1.0)).build()?;
    assert_eq!(planet.check_habitability(), Ok(()));
    Ok(())
  }

  #[test]
  #[should_panic]
  fn test_check_habitability_too_low() {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .mass(MassOfEarth(0.001))
      .build()
      .unwrap();
    planet.check_habitability().unwrap();
  }

  #[test]
  #[should_panic]
  fn test_check_habitability_too_high() {
    init();
    let planet = TerrestrialPlanetBuilder::default()
      .mass(MassOfEarth(100.0))
      .build()
      .unwrap();
    planet.check_habitability().unwrap();
  }

  #[test]
  fn test_get_random() {
    init();
    let planet = TerrestrialPlanetBuilder::get_random(&mut thread_rng()).build().unwrap();
    println!("{:#?}", planet);
  }

  #[test]
  fn test_get_random_habitable() {
    init();
    let planet = TerrestrialPlanetBuilder::get_random_habitable(&mut thread_rng())
      .build()
      .unwrap();
    println!("{:#?}", planet);
  }

  #[test]
  fn test_get_random_exotic() {
    init();
    let planet = TerrestrialPlanetBuilder::get_random_exotic(&mut thread_rng())
      .build()
      .unwrap();
    println!("{:#?}", planet);
  }

  #[test]
  fn test_get_random_exotic_habitable() {
    init();
    let planet = TerrestrialPlanetBuilder::get_random_exotic_habitable(&mut thread_rng())
      .build()
      .unwrap();
    println!("{:#?}", planet);
  }

  #[test]
  fn test_get_random_earthlike() {
    init();
    let planet = TerrestrialPlanetBuilder::get_random_earthlike(&mut thread_rng())
      .build()
      .unwrap();
    println!("{:#?}", planet);
  }
}
