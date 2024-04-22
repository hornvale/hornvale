use crate::prelude::*;

impl TryFrom<char> for TokenKind {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    match Character::try_from(character) {
      Ok(character) => Ok(Self::Character(character)),
      Err(_) => Err(()),
    }
  }
}
