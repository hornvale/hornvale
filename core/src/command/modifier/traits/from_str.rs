use crate::prelude::*;

impl TryFrom<&str> for CommandModifier {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "about" => Ok(Self::About),
      "above" => Ok(Self::Above),
      "across" => Ok(Self::Across),
      "against" => Ok(Self::Against),
      "along" => Ok(Self::Along),
      "among" => Ok(Self::Among),
      "around" => Ok(Self::Around),
      "as" => Ok(Self::As),
      "at" => Ok(Self::At),
      "before" => Ok(Self::Before),
      "behind" => Ok(Self::Behind),
      "below" => Ok(Self::Below),
      "beside" => Ok(Self::Beside),
      "between" => Ok(Self::Between),
      "beyond" => Ok(Self::Beyond),
      "by" => Ok(Self::By),
      "for" => Ok(Self::For),
      "from" => Ok(Self::From),
      "here" => Ok(Self::Here),
      "in" => Ok(Self::In),
      "into" => Ok(Self::Into),
      "of" => Ok(Self::Of),
      "off" => Ok(Self::Off),
      "on" => Ok(Self::On),
      "out" => Ok(Self::Out),
      "over" => Ok(Self::Over),
      "to" => Ok(Self::To),
      "toward" => Ok(Self::Toward),
      "under" => Ok(Self::Under),
      "upon" => Ok(Self::Upon),
      "with" => Ok(Self::With),
      "without" => Ok(Self::Without),
      _ => Err(()),
    }
  }
}
