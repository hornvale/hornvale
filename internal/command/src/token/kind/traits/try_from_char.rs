use crate::prelude::*;

impl TryFrom<char> for TokenKind {
  type Error = ();

  fn try_from(character: char) -> Result<Self, Self::Error> {
    let char_token = CharacterToken::try_from(character)?;
    Ok(TokenKind::Character(char_token))
  }
}