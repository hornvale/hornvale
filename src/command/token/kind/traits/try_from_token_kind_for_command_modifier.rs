use crate::command::prelude::*;

impl TryFrom<TokenKind> for CommandModifier {
  type Error = ();

  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      TokenKind::CommandModifier(command_modifier) => Ok(command_modifier),
      _ => Err(()),
    }
  }
}
