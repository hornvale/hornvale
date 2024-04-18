use serde::{Deserialize, Serialize};

/// A direction.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Hash, Serialize)]
pub enum Direction {
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
  /// In.
  In,
  /// Out.
  Out,
}
