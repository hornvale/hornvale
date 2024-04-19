use serde::{Deserialize, Serialize};
use std::ops::Neg;

/// A direction.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Hash, Serialize)]
pub enum CommandDirection {
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

impl CommandDirection {
  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    use CommandDirection::*;
    match self {
      North => South,
      Northeast => Southwest,
      East => West,
      Southeast => Northwest,
      South => North,
      Southwest => Northeast,
      West => East,
      Northwest => Southeast,
      Up => Down,
      Down => Up,
      In => Out,
      Out => In,
    }
  }
}

impl Neg for CommandDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    self.opposite()
  }
}
