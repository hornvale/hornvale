use crate::constants::prelude::*;
use crate::types::prelude::*;

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

#[cfg(test)]
mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_oxygen_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let oxygen_stability = get_oxygen_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(oxygen_stability, 0.579, 0.001);
    assert!(is_oxygen_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  fn test_get_carbon_dioxide_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let carbon_dioxide_stability = get_carbon_dioxide_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(carbon_dioxide_stability, 0.494, 0.001);
    assert!(is_carbon_dioxide_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  fn test_get_argon_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let argon_stability = get_argon_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(argon_stability, 0.518, 0.001);
    assert!(is_argon_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  fn test_get_nitrogen_stability() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    let nitrogen_stability = get_nitrogen_stability(equilibrium_temperature, escape_velocity);
    assert_approx_eq!(nitrogen_stability, 0.619, 0.001);
    assert!(is_nitrogen_stable(equilibrium_temperature, escape_velocity));
  }

  #[test]
  fn test_is_atmospherically_stable() {
    init();
    let equilibrium_temperature = TemperatureInKelvin(288.0);
    let escape_velocity = EscapeVelocityOfEarth(1.0);
    assert!(is_atmospherically_stable(equilibrium_temperature, escape_velocity));
  }
}
