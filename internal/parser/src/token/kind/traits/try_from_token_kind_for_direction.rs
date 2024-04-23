use crate::prelude::*;
use hornvale_core::prelude::*;

impl TryFrom<TokenKind> for Direction {
  type Error = ();
  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      TokenKind::North => Ok(Direction::North),
      TokenKind::Northeast => Ok(Direction::Northeast),
      TokenKind::East => Ok(Direction::East),
      TokenKind::Southeast => Ok(Direction::Southeast),
      TokenKind::South => Ok(Direction::South),
      TokenKind::Southwest => Ok(Direction::Southwest),
      TokenKind::West => Ok(Direction::West),
      TokenKind::Northwest => Ok(Direction::Northwest),
      TokenKind::Up => Ok(Direction::Up),
      TokenKind::Down => Ok(Direction::Down),
      TokenKind::In => Ok(Direction::In),
      TokenKind::Out => Ok(Direction::Out),
      _ => Err(()),
    }
  }
}
