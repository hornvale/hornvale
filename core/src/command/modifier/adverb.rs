use serde::{Deserialize, Serialize};
use strum::Display;

/// Adverbs.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Adverb {
  /// e.g. "turn radio down".
  Down,
  /// e.g. "turn radio off".
  Off,
  /// e.g. "turn radio on".
  On,
  /// e.g. "turn radio up".
  Up,
}
