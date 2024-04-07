/// An error in deriving a close binary star.
pub mod close_binary_star;
/// An error in deriving a habitable planet.
pub mod habitability;
/// An error in deriving a host star.
pub mod host_star;
/// An error in deriving a moon.
pub mod moon;
/// An error in calculating a planet-moon relationship.
pub mod planet_moon_relationship;
/// An error in calculating a star's properties.
pub mod star;

/// The prelude.
pub mod prelude {
  pub use super::close_binary_star::CloseBinaryStarError;
  pub use super::habitability::HabitabilityError;
  pub use super::host_star::HostStarError;
  pub use super::moon::MoonError;
  pub use super::planet_moon_relationship::PlanetMoonRelationshipError;
  pub use super::star::StarError;
}
