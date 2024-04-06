use crate::constants::prelude::*;
use crate::types::prelude::*;
use std::f64::consts::PI;

/// Calculates whether a molecule can be stable in this atmosphere, given:
/// `mol_weight` - the weight of the molecule in kg/mol.
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn get_molecule_stability(
  mol_weight: KgPerMol,
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> f64 {
  ((3.0 * 8.3145 * (equilibrium_temperature.0 / 288.0) * 1500.0) / mol_weight.0).sqrt()
    / ((escape_velocity.0 * 11200.0) / 6.0)
}

/// Calculates whether a molecule can be stable in this atmosphere, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
/// `mol_weight` - the weight of the molecule in kg/mol.
pub fn is_molecule_stable(
  mol_weight: KgPerMol,
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> bool {
  let stability = get_molecule_stability(mol_weight, equilibrium_temperature, escape_velocity);
  stability < 1.0
}

/// Calculates whether oxygen is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn get_oxygen_stability(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> f64 {
  get_molecule_stability(OXYGEN_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Calculates whether carbon dioxide is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn get_carbon_dioxide_stability(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> f64 {
  get_molecule_stability(CO2_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Calculates whether argon is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn get_argon_stability(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> f64 {
  get_molecule_stability(ARGON_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Calculates whether nitrogen is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn get_nitrogen_stability(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> f64 {
  get_molecule_stability(NITROGEN_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Determines whether oxygen is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn is_oxygen_stable(equilibrium_temperature: TemperatureInKelvin, escape_velocity: EscapeVelocityOfEarth) -> bool {
  is_molecule_stable(OXYGEN_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Determines whether carbon dioxide is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn is_carbon_dioxide_stable(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> bool {
  is_molecule_stable(CO2_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Determines whether argon is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn is_argon_stable(equilibrium_temperature: TemperatureInKelvin, escape_velocity: EscapeVelocityOfEarth) -> bool {
  is_molecule_stable(ARGON_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Determines whether nitrogen is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn is_nitrogen_stable(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> bool {
  is_molecule_stable(NITROGEN_WEIGHT, equilibrium_temperature, escape_velocity)
}

/// Determines whether the atmosphere is stable, given:
/// `equilibrium_temperature` - of the body, in Kelvin.
/// `escape_velocity` - of the body, in VEarth.
pub fn is_atmospherically_stable(
  equilibrium_temperature: TemperatureInKelvin,
  escape_velocity: EscapeVelocityOfEarth,
) -> bool {
  is_oxygen_stable(equilibrium_temperature, escape_velocity)
    && is_carbon_dioxide_stable(equilibrium_temperature, escape_velocity)
    && is_argon_stable(equilibrium_temperature, escape_velocity)
    && is_nitrogen_stable(equilibrium_temperature, escape_velocity)
}

/// Calculate the density of terrestrial planet, given its mass and CMF.
///
/// The CMF, or Core Mass Fraction, indicates what percentage of the planet's
/// mass is contained within its iron core.
///
/// Given that, we can calculate the overall density of the planet in DensityOfEarth.
pub fn get_density(mass: MassOfEarth, cmf: f64) -> DensityOfEarth {
  let d1 = 5.51 * mass.0.powf(0.189) / (1.07 - 0.21 * (cmf)).powf(3.0);
  let d2 = 3.5 + 4.37 * cmf;
  let result = match mass.0 {
    mass if mass > 0.6 => d1,
    _mass if d1 > d2 => d1,
    _ => d2,
  };
  DensityInGramsPerCm3(result).into()
}

/// Calculate the escape velocity of a terrestrial planet.
///
/// Units are Mearth, Rearth, and Vearth.
pub fn get_escape_velocity(mass: MassOfEarth, radius: RadiusOfEarth) -> EscapeVelocityOfEarth {
  EscapeVelocityOfEarth((mass.0 / radius.0).sqrt())
}

/// Calculate the gravity of a terrestrial planet, given its mass and radius.
///
/// Units are Mearth, Rearth, and Gearth.
pub fn get_gravity(mass: MassOfEarth, radius: RadiusOfEarth) -> GravityOfEarth {
  GravityOfEarth(mass.0 / radius.0.powf(2.0))
}

/// Calculate the radius of a terrestrial planet, given its mass and density.
///
/// Units are Mearth, Dearth, and Rearth.
pub fn get_radius(mass: MassOfEarth, density: DensityOfEarth) -> RadiusOfEarth {
  RadiusOfEarth((mass.0 / density.0).powf(1.0 / 3.0))
}

/// Calculate the equilibrium temperature for a planet based on the host star's
/// luminosity, distance, etc.
/// Answer in Kelvin.
pub fn get_equilibrium_temperature(
  bond_albedo: f64,
  greenhouse_effect: f64,
  star_luminosity: LuminosityOfSol,
  star_distance: LengthInAu,
) -> TemperatureInKelvin {
  let luminosity = star_luminosity * ERGS_PER_SEC_PER_LSOL;
  let distance = star_distance * KM_PER_AU.0;
  let t_greenhouse = greenhouse_effect * GREENHOUSE_EFFECT;
  let absorption = ((1.0 - bond_albedo) * luminosity.0 / (16.0 * PI * STEFAN_BOLTZMANN_CONSTANT)).sqrt();
  let t_effective = absorption.sqrt() * (1.0 / distance.0.sqrt());
  let t_equilibrium = t_effective.powf(4.0) * (1.0 + (3.0 * t_greenhouse / 4.0));
  let t_surface = t_equilibrium / 0.9;
  TemperatureInKelvin(t_surface.powf(1.0 / 4.0))
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_get_oxygen_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let oxygen_stability = get_oxygen_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(oxygen_stability, 0.579, 0.001);
    assert!(is_oxygen_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  pub fn test_get_carbon_dioxide_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let carbon_dioxide_stability = get_carbon_dioxide_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(carbon_dioxide_stability, 0.494, 0.001);
    assert!(is_carbon_dioxide_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  pub fn test_get_argon_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let argon_stability = get_argon_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(argon_stability, 0.518, 0.001);
    assert!(is_argon_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  pub fn test_get_nitrogen_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let nitrogen_stability = get_nitrogen_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(nitrogen_stability, 0.619, 0.001);
    assert!(is_nitrogen_stable(equilibrium_temperature, escape_velocity));
  }
}
