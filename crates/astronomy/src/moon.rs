use crate::error::AstronomyError;
use crate::traits::prelude::*;
use crate::types::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// A `Moon`, mercifully, is a fairly simple concept.
///
/// It's possible that at some point, we might make moons habitable.
///
/// For instance, a habitable moon of a hot jupiter gas giant.
///
/// But for now, we're just staying with terrestrial planets, and we'll assume
/// that moons are just celestial features.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize, Builder)]
pub struct Moon {
  /// The mass of this moon, in MLuna.
  #[builder(default = "MassOfLuna(1.0)", setter(into))]
  pub mass: MassOfLuna,
  /// The density of this moon, in DLuna.
  #[builder(default = "DensityOfLuna(1.0)", setter(into))]
  pub density: DensityOfLuna,
  /// The Bond albedo of this moon (unitless).
  #[builder(default = "0.136")]
  pub bond_albedo: f64,
}

impl Moon {
  /// Create a new `Moon` builder.
  pub fn builder() -> MoonBuilder {
    MoonBuilder::default()
  }

  /// Get the radius of the moon in Lunar radii.
  pub fn get_radius(&self) -> Result<RadiusOfLuna, AstronomyError> {
    Ok(RadiusOfLuna((self.mass.0 / self.density.0).powf(1.0 / 3.0)))
  }

  /// Get the gravity of the moon in GLuna.
  pub fn get_gravity(&self) -> Result<GravityOfLuna, AstronomyError> {
    Ok(GravityOfLuna(self.mass.0 / self.get_radius()?.0.powf(2.0)))
  }

  /// Get the escape velocity of the moon in VLuna.
  pub fn get_escape_velocity(&self) -> Result<EscapeVelocityOfLuna, AstronomyError> {
    Ok(EscapeVelocityOfLuna((self.mass.0 / self.get_radius()?.0).sqrt()))
  }
}

impl MaybeHabitable for Moon {
  fn check_habitability(&self) -> Result<(), AstronomyError> {
    Err(AstronomyError::MoonsAreNotHabitable)
  }
}

impl Default for Moon {
  fn default() -> Self {
    MoonBuilder::default().build().unwrap()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use anyhow::Result as AnyResult;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_radius() -> AnyResult<()> {
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(1.0))
        .build()?
        .get_radius()
        .unwrap(),
      RadiusOfLuna(1.0)
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(2.0))
        .build()?
        .get_radius()
        .unwrap(),
      RadiusOfLuna(1.26),
      1e-4
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(3.0))
        .build()?
        .get_radius()
        .unwrap(),
      RadiusOfLuna(1.44),
      1e-2
    );
    Ok(())
  }

  #[test]
  fn test_get_gravity() -> AnyResult<()> {
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(1.0))
        .build()?
        .get_gravity()
        .unwrap(),
      GravityOfLuna(1.0)
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(2.0))
        .build()?
        .get_gravity()
        .unwrap(),
      GravityOfLuna(1.26),
      1e-4
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(3.0))
        .build()?
        .get_gravity()
        .unwrap(),
      GravityOfLuna(1.44),
      1e-2
    );
    Ok(())
  }

  #[test]
  fn test_get_escape_velocity() -> AnyResult<()> {
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(1.0))
        .build()?
        .get_escape_velocity()
        .unwrap(),
      EscapeVelocityOfLuna(1.0)
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(2.0))
        .build()?
        .get_escape_velocity()
        .unwrap(),
      EscapeVelocityOfLuna(1.26),
      1e-4
    );
    assert_approx_eq!(
      MoonBuilder::default()
        .mass(MassOfLuna(3.0))
        .build()?
        .get_escape_velocity()
        .unwrap(),
      EscapeVelocityOfLuna(1.44),
      1e-2
    );
    Ok(())
  }

  #[test]
  fn test_check_habitability() -> AnyResult<()> {
    assert_eq!(
      MoonBuilder::default().build()?.check_habitability(),
      Err(AstronomyError::MoonsAreNotHabitable)
    );
    Ok(())
  }

  #[test]
  fn test_default() {
    assert_eq!(Moon::default(), Moon::builder().build().unwrap());
  }
}
