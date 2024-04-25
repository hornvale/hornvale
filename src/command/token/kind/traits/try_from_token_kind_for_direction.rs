use crate::command::prelude::*;
use crate::world::prelude::*;

impl TryFrom<TokenKind> for Direction {
  type Error = ();
  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      TokenKind::Direction(direction) => Ok(direction),
      _ => Err(()),
    }
  }
}
