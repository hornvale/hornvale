use derive_more::Display;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

/// Trait implementtions.
pub mod traits;

/// A direction.
#[derive(Clone, Copy, Debug, Deserialize, Display, EnumIter, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize)]
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
