use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// Trait implementations.
pub mod traits;

/// A direction.
#[derive(Clone, Copy, Debug, Deserialize, Display, EnumIter, Eq, PartialOrd, Ord, PartialEq, Hash, Serialize)]
pub enum Direction {
  /// North (y+1).
  North,
  /// Northeast (x+1, y+1).
  Northeast,
  /// East (x+1).
  East,
  /// Southeast (x+1, y-1).
  Southeast,
  /// South (y-1).
  South,
  /// Southwest (x-1, y-1).
  Southwest,
  /// West (x-1).
  West,
  /// Northwest (x-1, y+1).
  Northwest,
  /// Up (z+1).
  Up,
  /// Down (z-1).
  Down,
  /// In (w+1).
  In,
  /// Out (w-1).
  Out,
}

impl Direction {
  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    -(*self)
  }
}
