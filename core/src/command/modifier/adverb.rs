use serde::{Deserialize, Serialize};
use strum::Display;

/// Adverbs.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Adverb {
  /// e.g. "look around".
  Around,
  /// e.g. "turn radio down".
  Down,
  /// e.g. "look here".
  Here,
  /// e.g. "turn radio off".
  Off,
  /// e.g. "turn radio on".
  On,
  /// e.g. "turn radio up".
  Up,
}

impl TryFrom<&str> for Adverb {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "around" => Ok(Self::Around),
      "down" => Ok(Self::Down),
      "here" => Ok(Self::Here),
      "off" => Ok(Self::Off),
      "on" => Ok(Self::On),
      "up" => Ok(Self::Up),
      _ => Err(()),
    }
  }
}
