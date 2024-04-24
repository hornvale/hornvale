use crate::prelude::*;

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
