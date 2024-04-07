//! Astronomy is a crate for astronomical calculations and simulations.

/// A close binary star.
pub mod close_binary_star;
/// Constants of interest for astronomy.
pub mod constants;
/// A distant binary star.
pub mod distant_binary_star;
/// Errors for the astronomy crate.
pub mod error;
/// A host star (single star or close binary star).
pub mod host_star;
/// Mathematical functions.
pub mod math;
/// A moon type.
pub mod moon;
/// A planet.
pub mod planet;
/// A relationship between a planet and a single moon.
pub mod planet_moon_relationship;
/// A planetary system.
pub mod planetary_system;
/// A satellite system, which is a planet and its moon or moons.
pub mod satellite_system;
/// A single star.
pub mod star;
/// A star system.
pub mod star_system;
/// A stellar neighbor.
pub mod stellar_neighbor;
/// A stellar neighborhood.
pub mod stellar_neighborhood;
/// A terrestrial planet type.
pub mod terrestrial_planet;
/// Traits for astronomy.
pub mod traits;
/// Types of interest for astronomy.
pub mod types;

/// The prelude for the astronomy crate.
pub mod prelude {
  pub use super::close_binary_star::CloseBinaryStar;
  pub use super::constants::prelude::*;
  pub use super::distant_binary_star::DistantBinaryStar;
  pub use super::error::AstronomyError;
  pub use super::host_star::HostStar;
  pub use super::math::prelude::*;
  pub use super::moon::Moon;
  pub use super::planet::Planet;
  pub use super::planet_moon_relationship::PlanetMoonRelationship;
  pub use super::planetary_system::PlanetarySystem;
  pub use super::satellite_system::SatelliteSystem;
  pub use super::star::Star;
  pub use super::star_system::StarSystem;
  pub use super::stellar_neighbor::StellarNeighbor;
  pub use super::stellar_neighborhood::StellarNeighborhood;
  pub use super::terrestrial_planet::TerrestrialPlanet;
  pub use super::traits::prelude::*;
  pub use super::types::prelude::*;
}
