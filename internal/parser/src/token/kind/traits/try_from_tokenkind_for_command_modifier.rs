use crate::prelude::*;
use hornvale_core::prelude::*;

impl TryFrom<TokenKind> for CommandModifier {
  type Error = ();
  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      TokenKind::Adverb(adverb) => Ok(CommandModifier::Adverb(adverb)),
      TokenKind::Preposition(preposition) => Ok(CommandModifier::Preposition(preposition)),
      _ => Err(()),
    }
  }
}
