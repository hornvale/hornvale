use crate::command::prelude::*;

impl TryFrom<char> for TokenKind {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    let char_token = Character::try_from(character)?;
    Ok(TokenKind::Character(char_token))
  }
}
