use serde::{Deserialize, Serialize};

/// A direction in 4D space.
///
/// This enables us to easily interpret directions when parsing commands.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
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
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for Direction {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "north" | "n" => Ok(Self::North),
      "northeast" | "ne" => Ok(Self::Northeast),
      "east" | "e" => Ok(Self::East),
      "southeast" | "se" => Ok(Self::Southeast),
      "south" | "s" => Ok(Self::South),
      "southwest" | "sw" => Ok(Self::Southwest),
      "west" | "w" => Ok(Self::West),
      "northwest" | "nw" => Ok(Self::Northwest),
      "up" => Ok(Self::Up),
      "down" => Ok(Self::Down),
      "in" => Ok(Self::In),
      "out" => Ok(Self::Out),
      _ => Err(()),
    }
  }
}
