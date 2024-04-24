use crate::prelude::*;

impl TryFrom<&str> for Direction {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "north" => Ok(Self::North),
      "northeast" => Ok(Self::Northeast),
      "east" => Ok(Self::East),
      "southeast" => Ok(Self::Southeast),
      "south" => Ok(Self::South),
      "southwest" => Ok(Self::Southwest),
      "west" => Ok(Self::West),
      "northwest" => Ok(Self::Northwest),
      "up" => Ok(Self::Up),
      "down" => Ok(Self::Down),
      "in" => Ok(Self::In),
      "out" => Ok(Self::Out),
      _ => Err(()),
    }
  }
}
