//! # Tile
//!
//! A tile is a single square within the tilemap.

/// The tile kind enumeration.
pub mod kind;

/// The prelude.
pub mod prelude {
  pub use super::kind::*;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
