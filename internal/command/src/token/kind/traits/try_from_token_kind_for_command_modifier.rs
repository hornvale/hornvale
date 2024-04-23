use crate::prelude::*;
use hornvale_core::prelude::*;

impl TryFrom<TokenKind> for CommandModifier {
  type Error = ();

  fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
    match kind {
      TokenKind::About => Ok(CommandModifier::About),
      TokenKind::Above => Ok(CommandModifier::Above),
      TokenKind::Across => Ok(CommandModifier::Across),
      TokenKind::Against => Ok(CommandModifier::Against),
      TokenKind::Along => Ok(CommandModifier::Along),
      TokenKind::Among => Ok(CommandModifier::Among),
      TokenKind::Around => Ok(CommandModifier::Around),
      TokenKind::As => Ok(CommandModifier::As),
      TokenKind::At => Ok(CommandModifier::At),
      TokenKind::Before => Ok(CommandModifier::Before),
      TokenKind::Behind => Ok(CommandModifier::Behind),
      TokenKind::Below => Ok(CommandModifier::Below),
      TokenKind::Beside => Ok(CommandModifier::Beside),
      TokenKind::Between => Ok(CommandModifier::Between),
      TokenKind::Beyond => Ok(CommandModifier::Beyond),
      TokenKind::By => Ok(CommandModifier::By),
      TokenKind::For => Ok(CommandModifier::For),
      TokenKind::From => Ok(CommandModifier::From),
      TokenKind::Here => Ok(CommandModifier::Here),
      TokenKind::In => Ok(CommandModifier::In),
      TokenKind::Into => Ok(CommandModifier::Into),
      TokenKind::Of => Ok(CommandModifier::Of),
      TokenKind::Off => Ok(CommandModifier::Off),
      TokenKind::On => Ok(CommandModifier::On),
      TokenKind::Out => Ok(CommandModifier::Out),
      TokenKind::Over => Ok(CommandModifier::Over),
      TokenKind::To => Ok(CommandModifier::To),
      TokenKind::Toward => Ok(CommandModifier::Toward),
      TokenKind::Under => Ok(CommandModifier::Under),
      TokenKind::Upon => Ok(CommandModifier::Upon),
      TokenKind::With => Ok(CommandModifier::With),
      TokenKind::Without => Ok(CommandModifier::Without),
      _ => Err(()),
    }
  }
}
