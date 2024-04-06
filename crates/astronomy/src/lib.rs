/// Constants of interest for astronomy.
pub mod constants;
/// Errors for the astronomy crate.
pub mod errors;
/// Mathematical functions.
pub mod math;
/// A terrestrial planet type.
pub mod terrestrial_planet;
/// Types of interest for astronomy.
pub mod types;

/// The prelude for the astronomy crate.
pub mod prelude {
  pub use super::constants::prelude::*;
  pub use super::errors::prelude::*;
  pub use super::math::prelude::*;
  pub use super::terrestrial_planet::TerrestrialPlanet;
  pub use super::types::prelude::*;
}
