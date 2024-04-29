//! # Geometry
//!
//! This module contains the geometry types and functions.

/// A direction in 4D space.
pub mod direction;
/// A 4D point.
pub mod point4d;
use point4d::Point4D;
/// A 4D vector.
pub mod vector4d;
use vector4d::Vector4D;

/// The prelude.
pub mod prelude {
  pub use super::direction::Direction;
  pub use super::point4d::Point4D;
  pub use super::vector4d::Vector4D;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
