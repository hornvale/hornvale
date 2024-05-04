//! # Direction
//!
//! This module contains the `Direction` enum, which represents the directions
//! in which the player can move.

use bevy::math::IVec3;

/// Traits and trait implementations for the direction enum.
pub mod traits;

/// The direction enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
  /// No direction at all.
  None,
  /// North.
  North,
  /// Northeast.
  Northeast,
  /// East.
  East,
  /// Southeast.
  Southeast,
  /// South.
  South,
  /// Southwest.
  Southwest,
  /// West.
  West,
  /// Northwest.
  Northwest,
  /// Up.
  Up,
  /// Down.
  Down,
}

impl Direction {
  /// Returns the opposite direction.
  pub fn opposite(&self) -> Self {
    match self {
      Self::None => Self::None,
      Self::North => Self::South,
      Self::Northeast => Self::Southwest,
      Self::East => Self::West,
      Self::Southeast => Self::Northwest,
      Self::South => Self::North,
      Self::Southwest => Self::Northeast,
      Self::West => Self::East,
      Self::Northwest => Self::Southeast,
      Self::Up => Self::Down,
      Self::Down => Self::Up,
    }
  }

  /// To IVec3.
  pub fn to_ivec3(&self) -> IVec3 {
    match self {
      Self::None => IVec3::new(0, 0, 0),
      Self::North => IVec3::new(0, 1, 0),
      Self::Northeast => IVec3::new(1, 1, 0),
      Self::East => IVec3::new(1, 0, 0),
      Self::Southeast => IVec3::new(1, -1, 0),
      Self::South => IVec3::new(0, -1, 0),
      Self::Southwest => IVec3::new(-1, -1, 0),
      Self::West => IVec3::new(-1, 0, 0),
      Self::Northwest => IVec3::new(-1, 1, 0),
      Self::Up => IVec3::new(0, 0, 1),
      Self::Down => IVec3::new(0, 0, -1),
    }
  }
}

/// The prelude.
pub mod prelude {
  pub use super::traits::*;
  pub use super::Direction;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
