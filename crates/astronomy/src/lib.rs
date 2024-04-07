/// Constants of interest for astronomy.
pub mod constants;
/// Errors for the astronomy crate.
pub mod errors;
/// Mathematical functions.
pub mod math;
/// A moon type.
pub mod moon;
/// A planet.
pub mod planet;
/// A relationship between a planet and a single moon.
pub mod planet_moon_relationship;
/// A satellite system, which is a planet and its moon or moons.
pub mod satellite_system;
/// A terrestrial planet type.
pub mod terrestrial_planet;
/// Types of interest for astronomy.
pub mod types;

/// The prelude for the astronomy crate.
pub mod prelude {
  pub use super::constants::prelude::*;
  pub use super::errors::prelude::*;
  pub use super::math::prelude::*;
  pub use super::moon::Moon;
  pub use super::planet::Planet;
  pub use super::planet_moon_relationship::PlanetMoonRelationship;
  pub use super::satellite_system::SatelliteSystem;
  pub use super::terrestrial_planet::TerrestrialPlanet;
  pub use super::types::prelude::*;
}
