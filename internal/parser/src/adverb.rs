use serde::{Deserialize, Serialize};

/// An adverb.
///
/// Adverbs are words that modify verbs, adjectives, or other adverbs. They are
/// used to provide additional information about the action being performed.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Adverb {
  /// `on`, e.g. `turn on the light` or `turn light on`.
  On,
  /// `off`, e.g. `turn off the light` or `turn light off`.
  Off,
  /// `up`, e.g. `pick up the sword` or `pick sword up`.
  Up,
  /// `down`, e.g. `put down the sword` or `put sword down`.
  Down,
  /// `around`, e.g. `look around the room` or `look around`.
  Around,
  /// `away`, e.g. `throw away the sword` or `throw sword away`.
  Away,
  /// `back`, e.g. `go back to the room` or `go back`.
  Back,
  /// `forward`, e.g. `move forward to the room` or `move forward`.
  Forward,
  /// `here`, e.g. `come here`.
  Here,
}

impl Adverb {
  /// If the string fits...
  pub fn fits(string: &str) -> bool {
    Self::try_from(string).is_ok()
  }
}

impl TryFrom<&str> for Adverb {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "on" => Ok(Self::On),
      "off" => Ok(Self::Off),
      "up" => Ok(Self::Up),
      "down" => Ok(Self::Down),
      _ => Err(()),
    }
  }
}
