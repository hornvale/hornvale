use crate::world::prelude::*;

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_try_from() {
    init();
    assert_eq!(Direction::try_from("north"), Ok(Direction::North));
    assert_eq!(Direction::try_from("n"), Ok(Direction::North));
    assert_eq!(Direction::try_from("northeast"), Ok(Direction::Northeast));
    assert_eq!(Direction::try_from("ne"), Ok(Direction::Northeast));
    assert_eq!(Direction::try_from("east"), Ok(Direction::East));
    assert_eq!(Direction::try_from("e"), Ok(Direction::East));
    assert_eq!(Direction::try_from("southeast"), Ok(Direction::Southeast));
    assert_eq!(Direction::try_from("se"), Ok(Direction::Southeast));
    assert_eq!(Direction::try_from("south"), Ok(Direction::South));
    assert_eq!(Direction::try_from("s"), Ok(Direction::South));
    assert_eq!(Direction::try_from("southwest"), Ok(Direction::Southwest));
    assert_eq!(Direction::try_from("sw"), Ok(Direction::Southwest));
    assert_eq!(Direction::try_from("west"), Ok(Direction::West));
    assert_eq!(Direction::try_from("w"), Ok(Direction::West));
    assert_eq!(Direction::try_from("northwest"), Ok(Direction::Northwest));
    assert_eq!(Direction::try_from("nw"), Ok(Direction::Northwest));
    assert_eq!(Direction::try_from("up"), Ok(Direction::Up));
    assert_eq!(Direction::try_from("down"), Ok(Direction::Down));
    assert_eq!(Direction::try_from("in"), Ok(Direction::In));
    assert_eq!(Direction::try_from("out"), Ok(Direction::Out));
    assert_eq!(Direction::try_from("invalid"), Err(()));
  }
}
