use serde::{Deserialize, Serialize};
use strum::Display;

/// Prepositions.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Preposition {
  /// e.g. "talk about the weather".
  About,
  /// e.g. "look above the door".
  Above,
  /// e.g. "walk across the bridge".
  Across,
  /// e.g. "lean against the wall".
  Against,
  /// e.g. "walk along the path".
  Along,
  /// e.g. "search among the trees".
  Among,
  /// e.g. "look around".
  Around,
  /// e.g. "remember goblin as dave".
  As,
  /// e.g. "look at the painting".
  At,
  /// e.g. "stand before the king".
  Before,
  /// e.g. "hide behind the curtain".
  Behind,
  /// e.g. "descend below the surface".
  Below,
  /// e.g. "stand beside the statue".
  Beside,
  /// e.g. "choose between the two".
  Between,
  /// e.g. "venture beyond the wall".
  Beyond,
  /// e.g. "pass by the guard".
  By,
  /// e.g. "look down the pipe".
  Down,
  /// e.g. "search for the treasure".
  For,
  /// e.g. "take from the chest".
  From,
  /// e.g. "take all here".
  Here,
  /// e.g. "hide in the shadows".
  In,
  /// e.g. "step into the portal".
  Into,
  /// e.g. "speak of the devil".
  Of,
  /// e.g. "get off the horse".
  Off,
  /// e.g. "put the pie on the oven".
  On,
  /// e.g. "get out of the house".
  Out,
  /// e.g. "climb over the wall".
  Over,
  /// e.g. "take sword then take shield".
  Then,
  /// e.g. "go to the castle".
  To,
  /// e.g. "walk toward the light".
  Toward,
  /// e.g. "hide under the bed".
  Under,
  /// e.g. "look up the chimney".
  Up,
  /// e.g. "stand upon the hill".
  Upon,
  /// e.g. "fight with the sword".
  With,
  /// e.g. "go without the sword".
  Without,
}

impl TryFrom<&str> for Preposition {
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
      "down" => Ok(Self::Down),
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
      "then" => Ok(Self::Then),
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
