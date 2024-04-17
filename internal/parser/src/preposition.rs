use serde::{Deserialize, Serialize};

/// A preposition.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum Preposition {
  /// `at`.
  At,
  /// `across`.
  Across,
  /// `around`.
  Around,
  /// `about`.
  About,
  /// `among`.
  Among,
  /// `above`.
  Above,
  /// `against`.
  Against,
  /// `along`.
  Along,
  /// `before`.
  Before,
  /// `behind`.
  Behind,
  /// `beside`.
  Beside,
  /// `between`.
  Between,
  /// `below`.
  Below,
  /// `beyond`.
  Beyond,
  /// `by`.
  By,
  /// `down`.
  Down,
  /// `for`.
  For,
  /// `from`.
  From,
  /// `in`.
  In,
  /// `into`.
  Into,
  /// `off`.
  Off,
  /// `on`.
  On,
  /// `over`.
  Over,
  /// `to`.
  To,
  /// `toward`.
  Toward,
  /// `under`.
  Under,
  /// `up`.
  Up,
  /// `upon`.
  Upon,
  /// `with`.
  With,
  /// `without`.
  Without,
}

impl Preposition {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for Preposition {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "at" => Ok(Self::At),
      "across" => Ok(Self::Across),
      "around" => Ok(Self::Around),
      "about" => Ok(Self::About),
      "among" => Ok(Self::Among),
      "above" => Ok(Self::Above),
      "against" => Ok(Self::Against),
      "along" => Ok(Self::Along),
      "before" => Ok(Self::Before),
      "behind" => Ok(Self::Behind),
      "beside" => Ok(Self::Beside),
      "between" => Ok(Self::Between),
      "below" => Ok(Self::Below),
      "beyond" => Ok(Self::Beyond),
      "by" => Ok(Self::By),
      "down" => Ok(Self::Down),
      "for" => Ok(Self::For),
      "from" => Ok(Self::From),
      "in" => Ok(Self::In),
      "into" => Ok(Self::Into),
      "off" => Ok(Self::Off),
      "on" => Ok(Self::On),
      "over" => Ok(Self::Over),
      "to" => Ok(Self::To),
      "toward" => Ok(Self::Toward),
      "under" => Ok(Self::Under),
      "up" => Ok(Self::Up),
      "upon" => Ok(Self::Upon),
      "with" => Ok(Self::With),
      "without" => Ok(Self::Without),
      _ => Err(()),
    }
  }
}
