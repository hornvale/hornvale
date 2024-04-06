/// Constants of interest for astronomy.
pub mod constants;
/// Errors for the astronomy crate.
pub mod error;
/// Types of interest for astronomy.
pub mod types;

/// The prelude for the astronomy crate.
pub mod prelude {
  pub use super::error::AstronomyError;
  pub use super::types::prelude::*;
}
