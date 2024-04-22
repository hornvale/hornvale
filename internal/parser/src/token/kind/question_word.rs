use serde::{Deserialize, Serialize};
use strum::Display;

/// A question word, e.g. "who", "what", "where", "when", "why", "how".
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum QuestionWord {
  /// "How".
  How,
  /// "What".
  What,
  /// "When".
  When,
  /// "Where".
  Where,
  /// "Who".
  Who,
  /// "Why".
  Why,
}

impl QuestionWord {
  /// Can this token follow adjectives?
  pub fn can_follow_adjective(&self) -> bool {
    false
  }
}

impl TryFrom<&str> for QuestionWord {
  type Error = ();

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "how" => Ok(Self::How),
      "what" => Ok(Self::What),
      "when" => Ok(Self::When),
      "where" => Ok(Self::Where),
      "who" => Ok(Self::Who),
      "why" => Ok(Self::Why),
      _ => Err(()),
    }
  }
}
